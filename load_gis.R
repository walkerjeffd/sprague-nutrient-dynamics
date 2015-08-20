library(maptools)
library(gpclib)
library(sp)
library(ggplot2)
library(ggmap)
library(dplyr)
# gpclibPermit()

rm(list=ls())

# stations ----
load('kt_sprague.Rdata')

stn <- select(stn.kt_sprague, SITE, SITE_NAME, SITE_ABBR)

stn_incbasin_ivory <- stn %>%
  mutate(INC_SITE=plyr::revalue(SITE,
           c('SR0090'='SR0090-SR0080',
             'SR0080'='SR0080-SR0060-SR0070',
             'SR0060'='SR0060-SR0150-SR0140',
             'SR0070'='SR0070',
             'SR0050'='SR0050',
             'SR0040'='SR0040',
             'SR0150'='SR0150-SR0050',
             'SR0140'='SR0140-SR0040')),
         INC_SITE_NAME=plyr::revalue(SITE_NAME,
           c('Power'='Power-Lone_Pine',
             'Lone_Pine'='Lone_Pine-Godowa-Sycan',
             'Godowa'='Godowa-SF_Ivory-NF_Ivory',
             'Sycan'='Sycan',
             'SF_Ivory'='SF_Ivory-SF',
             'SF'='SF',
             'NF_Ivory'='NF_Ivory-NF',
             'NF'='NF')),
         INC_SITE_ABBR=plyr::revalue(SITE_ABBR,
           c('POW'='POW-LP',
             'LP'='LP-GOD-SYC',
             'GOD'='GOD-SFI-NFI',
             'SYC'='SYC',
             'SFI'='SFI-SF',
             'SF'='SF',
             'NFI'='NFI-NF',
             'NF'='NF'))) %>%
  mutate(INC_SITE=ordered(INC_SITE, levels=INC_SITE),
         INC_SITE_NAME=ordered(INC_SITE_NAME, levels=INC_SITE_NAME),
         INC_SITE_ABBR=ordered(INC_SITE_ABBR, levels=INC_SITE_ABBR))

stn_incbasin <- stn %>%
  mutate(INC_SITE=plyr::revalue(SITE,
           c('SR0090'='SR0090-SR0080',
             'SR0080'='SR0080-SR0060-SR0070',
             'SR0060'='SR0060-SR0050-SR0040',
             'SR0070'='SR0070',
             'SR0050'='SR0050',
             'SR0040'='SR0040')),
         INC_SITE_NAME=plyr::revalue(SITE_NAME,
           c('Power'='Power-Lone_Pine',
             'Lone_Pine'='Lone_Pine-Godowa-Sycan',
             'Godowa'='Godowa-SF-NF',
             'Sycan'='Sycan',
             'SF'='SF',
             'NF'='NF')),
         INC_SITE_ABBR=plyr::revalue(SITE_ABBR,
          c('POW'='POW-LP',
            'LP'='LP-GOD-SYC',
            'GOD'='GOD-SF-NF',
            'SYC'='SYC',
            'SF'='SF',
            'NF'='NF'))) %>%
  mutate(INC_SITE=ordered(INC_SITE, levels=INC_SITE),
         INC_SITE_NAME=ordered(INC_SITE_NAME, levels=INC_SITE_NAME),
         INC_SITE_ABBR=ordered(INC_SITE_ABBR, levels=INC_SITE_ABBR))

DATA_DIR <- getOption('UKL_DATA')
GIS_DIR <- file.path(DATA_DIR, '../gis/sprague/r_wgs84')
stopifnot(file.exists(GIS_DIR))

# basin ----
basin_shp <- readShapeSpatial(file.path(GIS_DIR, 'sprague_basin.shp'),
                              proj4string = CRS("+proj=longlat +datum=WGS84"))
basin <- fortify(basin_shp)
basin_area <- basin_shp@data %>%
  select(HUC8, NAME=Name, AREA_KM2=AreaSqKm)

# subbasins ----
subbasin_shp <- readShapeSpatial(file.path(GIS_DIR, 'sprague_subbasins.shp'),
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
subbasin <- fortify(subbasin_shp, region="SITE") %>%
  filter(id != 'WR1000') %>%
  mutate(SITE=id) %>%
  left_join(select(stn, SITE, SITE_NAME, SITE_ABBR), by='SITE') %>%
  mutate(SITE=ordered(SITE, levels=levels(stn$SITE)))
subbasin_area <- subbasin_shp@data %>%
  filter(SITE != 'WR1000') %>%
  rename(AREA_KM2=AreaSqKM) %>%
  left_join(select(stn, SITE, SITE_NAME, SITE_ABBR), by='SITE') %>%
  mutate(SITE=ordered(SITE, levels=levels(stn$SITE))) %>%
  arrange(SITE)

# incbasins w/ ivory ----
incbasin_ivory_shp <- readShapeSpatial(file.path(GIS_DIR, 'sprague_incbasins_ivory.shp'),
                                       proj4string = CRS("+proj=longlat +datum=WGS84"))
incbasin_ivory <- fortify(incbasin_ivory_shp, region="SITE") %>%
  filter(id != 'WR1000') %>%
  mutate(SITE=id) %>%
  left_join(stn_incbasin_ivory, by='SITE') %>%
  mutate(SITE=ordered(SITE, levels=levels(stn$SITE)))
incbasin_ivory_area <- incbasin_ivory_shp@data %>%
  filter(SITE != 'WR1000') %>%
  rename(AREA_KM2=AreaSqKM) %>%
  filter(!is.na(SITE)) %>%
  left_join(stn_incbasin_ivory, by='SITE') %>%
  mutate(SITE=ordered(SITE, levels=levels(stn$SITE))) %>%
  arrange(SITE)

# incbasins w/o ivory ----
incbasin_shp <- readShapeSpatial(file.path(GIS_DIR, 'sprague_incbasins.shp'),
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
incbasin <- fortify(incbasin_shp, region="SITE") %>%
  filter(id != 'WR1000') %>%
  mutate(SITE=id) %>%
  left_join(stn_incbasin, by='SITE') %>%
  mutate(SITE=ordered(SITE, levels=levels(stn$SITE)))
incbasin_area <- incbasin_shp@data %>%
  filter(SITE != 'WR1000') %>%
  rename(AREA_KM2=AreaSqKM) %>%
  filter(!is.na(SITE)) %>%
  left_join(stn_incbasin, by='SITE') %>%
  mutate(SITE=ordered(SITE, levels=levels(stn$SITE))) %>%
  arrange(SITE)

#flowlines ----
flowline_shp <- readShapeSpatial(file.path(GIS_DIR, 'sprague_flowlines.shp'),
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
flowline <- fortify(flowline_shp)

# basemap ----
map <- get_stamenmap(bbox=c(-122.1, 42.15, -120.6, 43), zoom=10)

# save ----
save(basin, subbasin, incbasin, incbasin_ivory, flowline,
     basin_area, subbasin_area, incbasin_area, incbasin_ivory_area,
     map, file='gis.Rdata')
