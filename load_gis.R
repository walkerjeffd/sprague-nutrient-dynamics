library(maptools)
library(gpclib)
library(sp)
library(ggplot2)
library(ggmap)
library(dplyr)
# gpclibPermit()

rm(list=ls())

cat(paste0(rep('=', 80), collapse=''), '\n')
cat("Loading GIS datasets...\n\n")

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
             'NF'='NF')))

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
            'NF'='NF')))

DATA_DIR <- getOption('UKL_DATA')
GIS_DIR <- file.path(DATA_DIR, '../gis/sprague/r_wgs84')
stopifnot(file.exists(GIS_DIR))

# basin ----
filename <- file.path(GIS_DIR, 'sprague_basin.shp')
cat("Loading shapefile:", filename, '\n')
basin_shp <- readShapeSpatial(filename,
                              proj4string = CRS("+proj=longlat +datum=WGS84"))
basin <- fortify(basin_shp)
basin_area <- basin_shp@data %>%
  select(HUC8, NAME=Name, AREA_KM2=AreaSqKm)

# subbasins ----
filename <- file.path(GIS_DIR, 'sprague_subbasins.shp')
cat("Loading shapefile:", filename, '\n')
subbasin_shp <- readShapeSpatial(filename,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
subbasin <- fortify(subbasin_shp, region="SITE") %>%
  filter(id != 'WR1000') %>%
  mutate(SITE=id) %>%
  left_join(select(stn, SITE, SITE_NAME, SITE_ABBR) %>% mutate(SITE=as.character(SITE)), by='SITE')
subbasin_area <- subbasin_shp@data %>%
  mutate(SITE=as.character(SITE)) %>%
  filter(SITE != 'WR1000') %>%
  rename(AREA_KM2=AreaSqKM) %>%
  left_join(select(stn, SITE, SITE_NAME, SITE_ABBR) %>% mutate(SITE=as.character(SITE)), by='SITE')

# incbasins w/ ivory ----
filename <- file.path(GIS_DIR, 'sprague_incbasins_ivory.shp')
cat("Loading shapefile:", filename, '\n')
incbasin_ivory_shp <- readShapeSpatial(filename,
                                       proj4string = CRS("+proj=longlat +datum=WGS84"))
incbasin_ivory <- fortify(incbasin_ivory_shp, region="SITE") %>%
  filter(id != 'WR1000') %>%
  mutate(SITE=id) %>%
  left_join(select(stn_incbasin_ivory, SITE, INC_SITE_NAME) %>%
              mutate(SITE=as.character(SITE),
                     INC_SITE_NAME=as.character(INC_SITE_NAME)), by='SITE') %>%
  droplevels()
incbasin_ivory_area <- incbasin_ivory_shp@data %>%
  mutate(SITE = as.character(SITE)) %>%
  filter(SITE != 'WR1000') %>%
  rename(AREA_KM2=AreaSqKM) %>%
  left_join(select(stn_incbasin_ivory, SITE, INC_SITE_NAME) %>%
              mutate(SITE=as.character(SITE),
                     INC_SITE_NAME=as.character(INC_SITE_NAME)), by='SITE') %>%
  select(SITE, INC_SITE_NAME, AREA_KM2)

# incbasins w/o ivory ----
filename <- file.path(GIS_DIR, 'sprague_incbasins.shp')
cat("Loading shapefile:", filename, '\n')
incbasin_shp <- readShapeSpatial(filename,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
incbasin <- fortify(incbasin_shp, region="SITE") %>%
  filter(id != 'WR1000') %>%
  mutate(SITE=id) %>%
  left_join(select(stn_incbasin, SITE, INC_SITE_NAME) %>%
              mutate(SITE=as.character(SITE),
                     INC_SITE_NAME=as.character(INC_SITE_NAME)), by='SITE') %>%
  droplevels()
incbasin_area <- incbasin_shp@data %>%
  mutate(SITE = as.character(SITE)) %>%
  filter(SITE != 'WR1000') %>%
  rename(AREA_KM2=AreaSqKM) %>%
  filter(!is.na(SITE)) %>%
  left_join(select(stn_incbasin, SITE, INC_SITE_NAME) %>%
              mutate(SITE=as.character(SITE),
                     INC_SITE_NAME=as.character(INC_SITE_NAME)), by='SITE') %>%
  select(SITE, INC_SITE_NAME, AREA_KM2)

# merge incbasins
incbasin_area <- rbind(incbasin_ivory_area, filter(incbasin_area, INC_SITE_NAME=="Godowa-SF-NF")) %>%
  mutate(INC_SITE_NAME=ordered(INC_SITE_NAME, levels=c("Power-Lone_Pine",
                                                       "Lone_Pine-Godowa-Sycan",
                                                       "Godowa-SF_Ivory-NF_Ivory",
                                                       "Godowa-SF-NF",
                                                       "Sycan",
                                                       "SF_Ivory-SF",
                                                       "SF",
                                                       "NF_Ivory-NF",
                                                       "NF"))) %>%
  arrange(INC_SITE_NAME)
incbasin <- rbind(incbasin_ivory, filter(incbasin, INC_SITE_NAME=="Godowa-SF-NF")) %>%
  mutate(INC_SITE_NAME=ordered(INC_SITE_NAME, levels=levels(incbasin_area$INC_SITE_NAME))) %>%
  arrange(INC_SITE_NAME)

#flowlines ----
filename <- file.path(GIS_DIR, 'sprague_flowlines.shp')
cat("Loading shapefile:", filename, '\n')
flowline_shp <- readShapeSpatial(filename,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
flowline <- fortify(flowline_shp)

# basemap ----
map <- get_stamenmap(bbox=c(-122.1, 42.15, -120.6, 43), zoom=10)

# save ----
filename <- 'gis.Rdata'
cat('\nSaving gis datasets to:', filename, '\n')
save(basin, subbasin, incbasin, flowline,
     basin_area, subbasin_area, incbasin_area,
     map, file=filename)

cat('\n\n')

