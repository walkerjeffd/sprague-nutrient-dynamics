library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggmap)
theme_set(theme_bw())

rm(list=ls())

load("kt_synoptic.Rdata")
load("gis.Rdata")

wq.kt_synoptic <- mutate(wq.kt_synoptic,
                         VAR = ordered(as.character(VAR), levels=c("TP", "PO4", "TN", "NH4", "NO23")))

synoptic_stn <- c("SR0100", "SR0120", "SR0200")

map <- get_stamenmap(bbox=c(-122.1, 42.15, -120.6, 43), zoom=10)

makeFootnote <- function(footnoteText='Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.',
                         size=0.7, color=grey(0.5)) {
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

pdf(file.path("pdf", "synoptic-data.pdf"), width=11, height=8.5)

p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
  geom_polygon(aes(x = long, y = lat, group = group), data = basin,
               color = 'black', alpha = 0, size = 0.5) +
  geom_path(aes(x = long, y = lat, group = group), data = flowline,
            color='deepskyblue', size=0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), data = incbasin_ivory,
               color = 'orangered', fill = 'grey50', alpha = 0.2, size = 0.2) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE), data = filter(stn.kt_synoptic, SITE %in% synoptic_stn),
             shape=21, fill='deepskyblue', size=3) +
  geom_text(aes(x = LONGITUDE+0.02, y = LATITUDE, label = SITE_LABEL),
            data = (filter(stn.kt_synoptic, SITE %in% synoptic_stn) %>%
                      mutate(SITE_LABEL=paste0(SITE_DESCRIPTION, " (", SITE, ")"))),
            size=4, hjust=0) +
  ggtitle('Synoptic Stations')
print(p)
makeFootnote()

p <- filter(wq.kt_synoptic, SITE %in% synoptic_stn) %>%
  filter(VAR %in% c("TP", "PO4", "TN", "NO23", "NH4", "TSS")) %>%
  ggplot(aes(DATETIME, VALUE)) +
  geom_point() +
  facet_grid(VAR~SITE_DESCRIPTION, scales="free_y") +
  scale_x_datetime(labels=scales::date_format("%m/%Y")) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  labs(x="Date", y="Concentration (ppm)", title="Synoptic WQ Data")
print(p)

p <- filter(wq.kt_synoptic, SITE %in% synoptic_stn) %>%
  filter(VAR %in% c("TP", "PO4", "TN", "NO23", "NH4", "TSS")) %>%
  ggplot(aes(SITE_DESCRIPTION, VALUE)) +
  geom_boxplot() +
  facet_wrap(~VAR, scales="free_y") +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) +
  ylim(0, NA) +
  labs(x="", y="Concentration (ppm)", title="Distributions of Synoptic WQ Data")
print(p)

dev.off()