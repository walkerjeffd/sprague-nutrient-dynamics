library(dplyr)
library(tidyr)
library(lubridate)
library(fluxr)
library(ggplot2)
library(gridExtra)
library(ggmap)
library(scales)
library(maptools)
library(gpclib)
library(sp)
theme_set(theme_bw())
gpclibPermit()

rm(list=ls())

# load data ----
load('kt_sprague.Rdata')
load('loads.Rdata')


# Subbasin Maps
load('gis.Rdata')

df_wyr <- loads_df$wyr
df_site <- loads_df$site

makeFootnote <- function(footnoteText=format(Sys.time(), "%Y-%m-%d %H:%M", tz="America/New_York"),size=0.7, color=grey(0.5)) {
  #   http://ryouready.wordpress.com/2009/02/17/r-good-practice-adding-footnotes-to-graphics/
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(2, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

dataset_levels <- names(loads)
subbasin_levels <- levels(subbasins_areas$SITE_NAME)
incbasin_levels <- list(RECENT=unique(incbasins_ivory_areas$INC_SITE_NAME),
                        POR=unique(incbasins_areas$INC_SITE_NAME))

map <- get_stamenmap(bbox=c(-122.1, 42.15, -120.6, 43), zoom=10)


stn <- select(stn.kt_sprague, SITE, SITE_NAME, LAT, LON) %>%
  arrange(SITE) %>%
  #   rbind(data.frame(SITE='WR1000', SITE_NAME='Sprague_Kirchers', LAT=42.567806, LON=-121.864472), .) %>%
  mutate(SITE=ordered(SITE, levels=SITE),
         SITE_NAME=ordered(SITE_NAME, levels=SITE_NAME))
stn <- left_join(stn, select(subbasins_areas, SITE, AREA_KM2))


# separate gis by dataset
stn <- list(RECENT=stn,
            POR=filter(stn, !(SITE_NAME %in% c('SF_Ivory', 'NF_Ivory'))))
subbasins <- list(RECENT=subbasins,
                  POR=filter(subbasins, !(SITE_NAME %in% c('SF_Ivory', 'NF_Ivory'))))
incbasins <- list(RECENT=incbasins_ivory,
                  POR=incbasins)

scale_fill_term <- list(Q=scale_fill_gradientn('Flow\n(hm3/yr)', colours=RColorBrewer::brewer.pal(n=6, name="Blues"), limits=c(0,NA)),
                        L=scale_fill_gradientn('Load\n(kg/yr)', colours=RColorBrewer::brewer.pal(n=6, name="Greens"), limits=c(0,NA)),
                        Q_AREA=scale_fill_gradientn('Flow per Area\n(cm/yr)', colours=RColorBrewer::brewer.pal(n=6, name="Blues"), limits=c(0,NA)),
                        L_AREA=scale_fill_gradientn('Load per Area\n(kg/km2/yr)', colours=RColorBrewer::brewer.pal(n=6, name="Greens"), limits=c(0,NA)),
                        C=scale_fill_gradientn('FWM Conc\n(ppb)', colours=RColorBrewer::brewer.pal(n=6, name="YlOrRd"), limits=c(0,NA)))

dataset <- 'POR'
variable <- 'TP'

term <- 'C'
map.c <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
  geom_polygon(aes(x = long, y = lat, group = group), data = select(incbasins[[dataset]], -SITE_NAME),
               color = 'grey50', fill = NA, size = 0.2) +
  geom_polygon(aes(x = long, y = lat, fill = VALUE),
               data = subbasins[[dataset]] %>%
                 left_join(filter(df_site, DATASET==dataset, VAR==variable, TERM==term, SITE_NAME %in% subbasin_levels), by='SITE_NAME') %>%
                 mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=subbasin_levels)),
               colour = 'black', size = 0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), data = basin,
               color = 'black', fill = NA, size = 0.2) +
  geom_point(aes(x = LON, y = LAT), data = stn[[dataset]], fill = 'deepskyblue', pch = 21, color = 'black', size = 3) +
  scale_fill_term[[term]] +
  facet_wrap(~SITE_NAME, nrow=1)
bar.c <- filter(df_site, DATASET==dataset, VAR==variable, TERM==term, SITE_NAME %in% subbasin_levels) %>%
  mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=rev(levels(SITE_NAME)))) %>%
  ggplot(aes(x=SITE_NAME, y=VALUE, fill=VALUE)) +
  geom_bar(stat='identity', color='grey50') +
  labs(x='', y=scale_fill_term[[term]]$name) +
  scale_fill_term[[term]] +
  guides(fill=FALSE) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        plot.margin=unit(c(30, 10, 0, 0), units="mm"))
term <- 'L_AREA'
map.l <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
  geom_polygon(aes(x = long, y = lat, group = group), data = select(incbasins[[dataset]], -SITE_NAME),
               color = 'grey50', fill = NA, size = 0.2) +
  geom_polygon(aes(x = long, y = lat, fill = VALUE),
               data = subbasins[[dataset]] %>%
                 left_join(filter(df_site, DATASET==dataset, VAR==variable, TERM==term, SITE_NAME %in% subbasin_levels), by='SITE_NAME') %>%
                 mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=subbasin_levels)),
               colour = 'black', size = 0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), data = basin,
               color = 'black', fill = NA, size = 0.2) +
  geom_point(aes(x = LON, y = LAT), data = stn[[dataset]], fill = 'deepskyblue', pch = 21, color = 'black', size = 3) +
  scale_fill_term[[term]] +
  facet_wrap(~SITE_NAME, nrow=1)
bar.l <- filter(df_site, DATASET==dataset, VAR==variable, TERM==term, SITE_NAME %in% subbasin_levels) %>%
  ggplot(aes(x=SITE_NAME, y=VALUE, fill=VALUE)) +
  geom_bar(stat='identity', color='grey50') +
  labs(x='', y=scale_fill_term[[term]]$name) +
  scale_fill_term[[term]] +
  guides(fill=FALSE) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        plot.margin=unit(c(30, 10, 0, 0), units="mm"))
term <- 'Q_AREA'
map.q <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
  geom_polygon(aes(x = long, y = lat, group = group), data = select(incbasins[[dataset]], -SITE_NAME),
               color = 'grey50', fill = NA, size = 0.2) +
  geom_polygon(aes(x = long, y = lat, fill = VALUE),
               data = subbasins[[dataset]] %>%
                 left_join(filter(df_site, DATASET==dataset, VAR==variable, TERM==term, SITE_NAME %in% subbasin_levels), by='SITE_NAME') %>%
                 mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=subbasin_levels)),
               colour = 'black', size = 0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), data = basin,
               color = 'black', fill = NA, size = 0.2) +
  geom_point(aes(x = LON, y = LAT), data = stn[[dataset]], fill = 'deepskyblue', pch = 21, color = 'black', size = 3) +
  scale_fill_term[[term]] +
  facet_wrap(~SITE_NAME, nrow=1)
bar.q <- filter(df_site, DATASET==dataset, VAR==variable, TERM==term, SITE_NAME %in% subbasin_levels) %>%
  ggplot(aes(x=SITE_NAME, y=VALUE, fill=VALUE)) +
  geom_bar(stat='identity', color='grey50') +
  labs(x='', y=scale_fill_term[[term]]$name) +
  scale_fill_term[[term]] +
  guides(fill=FALSE) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        plot.margin=unit(c(30, 10, 0, 0), units="mm"))
grid.arrange(arrangeGrob(bar.c, bar.l, bar.q, ncol=1),
             arrangeGrob(map.c, map.l, map.q, ncol=1),
             ncol=2, widths=c(1,3))