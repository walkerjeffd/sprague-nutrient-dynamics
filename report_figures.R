library(dplyr)
library(tidyr)
library(lubridate)
library(fluxr)
library(ggplot2)
theme_set(theme_bw())
library(gridExtra)

library(ggmap)
library(scales)
library(maptools)
library(gpclib)
library(sp)
gpclibPermit()


# load data ----
load('kt_sprague.Rdata')
load('loads.Rdata')

df_mon <- filter(loads_df[['mon']],
                     DATASET=="POR",
                     SITE_NAME %in% c('Power', 'Lone_Pine', 'Sycan', 'Godowa', 
                                      'SF', 'NF')) %>%
  filter(TERM %in% c("Q", "C")) %>%
  filter(VAR != "PP") %>%
  arrange(VAR, SITE_NAME) %>%
  mutate(SITE_NAME=ordered(as.character(SITE_NAME),
                           levels=c("Power", "Lone_Pine", "Godowa", "Sycan",
                                    "SF", "NF")),
         VAR_LABEL=ifelse(VAR=="FLOW", "Flow", paste0(VAR, " Conc")),
         VAR_LABEL=ordered(VAR_LABEL, levels=unique(VAR_LABEL)))

df_mon.tile <- mutate(df_mon,
                      MONTH=ordered(MONTH, levels=rev(c(10:12, 1:9))), 
                      VALUE=log10(VALUE)) %>%
  group_by(VAR_LABEL) %>%
  mutate(VALUE=scale(VALUE)) %>%
  ungroup

wyr_labels <- seq(2002, 2014)
wyr_labels[as.logical(wyr_labels %% 2)] <- ""

df_mon.tile %>%
  ggplot(aes(factor(WYEAR), MONTH, fill=VALUE)) +
  geom_tile() +
  facet_grid(VAR_LABEL~SITE_NAME) + 
  scale_fill_gradientn('Std. Value',
                       colours=rev(scales::brewer_pal(type = "seq", 
                                                      palette = 'GnBu')(9))) +
  scale_x_discrete(labels=wyr_labels) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=10),
        axis.text.y=element_text(size=8),
        panel.grid=element_blank()) +
  labs(x='Water Year', y='Month')


seasons <- list(Annual=1:12,
                'Fall (Oct-Dec)'=c(10:12),
                'Winter (Jan-Mar)'=c(1:3),
                'Spring (Apr-Jun)'=c(4:6),
                'Summer (Jul-Sep)'=c(7:9))
df_seasons <- lapply(names(seasons), function(s) {
  data.frame(MONTH=seasons[[s]], SEASON=s, stringsAsFactors=FALSE)
}) %>%
  rbind_all

df_wyr <- filter(loads_df[['mon']],
       DATASET=="POR",
       SITE_NAME %in% c('Power', 'Lone_Pine', 'Sycan', 'Godowa', 
                        'SF', 'NF'), 
       VAR != "PP") %>%
  filter(TERM %in% c("Q", "L")) %>%
  droplevels %>%
  spread(TERM, VALUE)
df_wyr.flow <- filter(df_wyr, VAR == "FLOW") %>%
  select(-VAR ,-L)
df_wyr.load <- filter(df_wyr, VAR != "FLOW") %>%
  select(-Q)
df_wyr <- left_join(df_wyr.load, df_wyr.flow)

df_wyr <- left_join(df_seasons, df_wyr, by='MONTH') %>%
  mutate(SEASON=ordered(SEASON, levels=names(seasons)))

# compute annual flows/loads/concs
df_wyr <- group_by(df_wyr, DATASET, VAR, SITE_NAME, SEASON, WYEAR) %>%
  summarise(N.MONTH=n(),
            N.DAY=sum(days_in_month(MONTHYEAR)),
            L=sum(L)/N.DAY,
            Q=sum(Q)/N.DAY,
            C=L/Q) %>%
  ungroup

df_wyr.flow <- select(df_wyr, -VAR, -L, -C) %>%
  unique %>%
  mutate(VAR="FLOW", TERM="Q") %>%
  rename(VALUE=Q)
df_wyr.wq <- select(df_wyr, -Q) %>%
  gather(TERM, VALUE, L, C) %>%
  filter(TERM != "L")
df_wyr <- rbind(df_wyr.flow, df_wyr.wq) %>%
  mutate(SITE_NAME=ordered(as.character(SITE_NAME),
                           levels=c("Power", "Lone_Pine", "Godowa", "Sycan",
                                    "SF", "NF")),
         VAR_LABEL=ifelse(VAR=="FLOW", "Flow", paste0(VAR, " Conc")),
         VAR_LABEL=ordered(VAR_LABEL, levels=unique(VAR_LABEL)))

df_wyr.tile <- mutate(df_wyr,
                      VALUE_LOG=log10(VALUE)) %>%
  group_by(VAR_LABEL) %>%
  mutate(VALUE_SCALE=scale(VALUE_LOG)) %>%
  ungroup

stopifnot(
  group_by(df_wyr.tile, SEASON, WYEAR, VAR_LABEL, SITE_NAME) %>%
    summarise(N=n()) %>%
    filter(N>1) %>%
    nrow == 0
)

df_wyr.tile %>%
  ggplot(aes(factor(WYEAR), SITE_NAME, fill=VALUE_SCALE)) +
  geom_tile() +
  facet_grid(VAR_LABEL~SEASON) + 
  scale_fill_gradientn('Std. Value',
                       colours=rev(scales::brewer_pal(type = "seq", 
                                                      palette = 'GnBu')(9))) +
  # scale_x_discrete(labels=wyr_labels) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=10),
        axis.text.y=element_text(size=10),
        panel.grid=element_blank()) +
  labs(x='Water Year', y='Station')

df_wyr.tile %>%
  ggplot(aes(WYEAR, VALUE, color=SEASON)) +
  geom_line() +
  facet_grid(VAR_LABEL~SITE_NAME, scales='free_y') + 
  scale_y_log10() +
  labs(x='Water Year', y='Value')
df_wyr.tile %>%
  ggplot(aes(WYEAR, VALUE, color=SITE_NAME)) +
  geom_line() +
  facet_grid(VAR_LABEL~SEASON, scales='free_y') + 
  scale_x_continuous(breaks=seq(2002, 2014, by=2)) +
  scale_y_log10() +
  labs(x='Water Year', y='Value')



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