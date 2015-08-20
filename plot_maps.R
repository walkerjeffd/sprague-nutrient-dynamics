library(ggplot2)
library(ggmap)
library(dplyr)

rm(list=ls())

load("kt_sprague.Rdata")
load("gis.Rdata")
source("functions.R")

# maps ----
pdf(file.path('pdf', 'maps.pdf'), width=11, height=8.5)

p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
  geom_polygon(aes(x = long, y = lat, group = group), data = basin,
               color = 'orangered', alpha = 0, size = 0.5) +
  geom_path(aes(x = long, y = lat, group = group), data = flowline,
            color='deepskyblue', size=0.2) +
  ggtitle('Flowline and Basin')
print(p)
makeFootnote()

p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
  geom_polygon(aes(x = long, y = lat, group = group), data = basin,
               color = 'black', alpha = 0, size = 0.5) +
  geom_path(aes(x = long, y = lat, group = group), data = flowline,
            color='deepskyblue', size=0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), data = incbasin_ivory,
               color = 'orangered', fill = 'grey50', alpha = 0.2, size = 0.2) +
  geom_point(aes(x = LON, y = LAT), data = stn.kt_sprague,
             shape=21, fill='deepskyblue', size=3) +
  geom_text(aes(x = LON+0.02, y = LAT, label = SITE_LABEL),
            data = (filter(stn.kt_sprague, !(SITE_NAME %in% c("Godowa", "Sycan"))) %>%
              mutate(SITE_LABEL=paste0(SITE_NAME, " (", SITE, ")"))),
            size=4, hjust=0) +
  geom_text(aes(x = LON-0.02, y = LAT, label = SITE_LABEL),
            data = (filter(stn.kt_sprague, (SITE_NAME %in% c("Godowa", "Sycan"))) %>%
                      mutate(SITE_LABEL=paste0(SITE_NAME, " (", SITE, ")"))),
            size=4, hjust=1) +
  ggtitle('Klamath Tribes - Water Quality Stations and Subbasins')
print(p)
makeFootnote()

p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
  geom_polygon(aes(x = long, y = lat, group = group), data = basin,
               color = 'black', alpha = 0, size = 0.5) +
  geom_path(aes(x = long, y = lat, group = group), data = flowline,
            color='deepskyblue', size=0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), data = incbasin,
               color = 'orangered', fill = 'grey50', alpha = 0.2, size = 0.2) +
  geom_text(aes(x = long, y = lat, label = INC_SITE_ABBR),
            data = group_by(incbasin, INC_SITE_ABBR) %>%
              summarise(lat=mean(lat), long=mean(long))) +
  geom_point(aes(x = LON, y = LAT), data = filter(stn.kt_sprague, !(SITE_ABBR %in% c('NFI', 'SFI'))),
             shape=21, fill='deepskyblue', size=3) +
  ggtitle('Incremental Subbasins (w/o Ivory Stations)')
print(p)
makeFootnote()

p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
  geom_polygon(aes(x = long, y = lat, group = group), data = basin,
               color = 'black', alpha = 0, size = 0.5) +
  geom_path(aes(x = long, y = lat, group = group), data = flowline,
            color='deepskyblue', size=0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), data = incbasin_ivory,
               color = 'orangered', fill = 'grey50', alpha = 0.2, size = 0.2) +
  geom_text(aes(x = long, y = lat, label = INC_SITE_ABBR),
            data = group_by(incbasin_ivory, INC_SITE_ABBR) %>%
              summarise(lat=mean(lat), long=mean(long))) +
  geom_point(aes(x = LON, y = LAT), data = stn.kt_sprague,
             shape=21, fill='deepskyblue', size=3) +
  ggtitle('Incremental Subbasins (w/ Ivory Stations)')
print(p)
makeFootnote()

p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
  geom_polygon(aes(x = long, y = lat, group = group), data = basin,
               color = 'black', alpha = 0, size = 0.5) +
  geom_path(aes(x = long, y = lat, group = group), data = flowline,
            color='deepskyblue', size = 0.2, alpha = 0.5) +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = subbasin,
               color = 'orangered', fill = 'grey50', alpha = 0.1, size = 0.2) +
  geom_point(aes(x = LON, y = LAT), data = stn.kt_sprague,
             shape=21, fill='deepskyblue', size=3) +
  facet_wrap(~SITE_NAME) +
  ggtitle('Drainage Subbasins')
print(p)
makeFootnote()

dev.off()