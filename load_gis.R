library(maptools)
#library(gpclib)
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

#DATA_DIR <- getOption('UKL_DATA')
#GIS_DIR <- file.path(DATA_DIR, '../gis/sprague/r_wgs84')
#stopifnot(file.exists(GIS_DIR))

DATA_DIR <- './data'
GIS_DIR <- file.path(DATA_DIR, 'sprague', 'gis')
stopifnot(file.exists(GIS_DIR))

# basin ----
filename <- file.path(GIS_DIR, 'sprague_basin.shp')
cat("Loading shapefile:", filename, '\n')

#systemfile <- system.file(filename, package = "sf")
#basin_shp <- st_read(systemfile)

### something about automating the filepath is goofing st_read when using the DATA_DIR AND GIS_DIR material
### troubleshoot this issue


#basin_shp <- st_read(dsn = filename, layer = "+proj=longlat +datum=WGS84")
#basin_shp <- readShapeSpatial(filename,
                              #proj4string = CRS("+proj=longlat +datum=WGS84"))

basin_shp <- st_read("./data/sprague/gis/sprague_basin.shp")
#basin_shp <- st_set_crs(basin_shp, "+proj=longlat +datum=WGS84")

st_crs(basin_shp)

basin <- fortify(basin_shp)
#st_crs(basin)
#basin_area <- basin_shp@data %>%
 # select(HUC8, NAME=Name, AREA_KM2=AreaSqKm)
basin_area <- basin_shp %>%
 select(HUC8, NAME=Name, AREA_KM2=AreaSqKm)

# subbasins ----
filename <- file.path(GIS_DIR, 'sprague_subbasins.shp')
cat("Loading shapefile:", filename, '\n')
#subbasin_shp <- st_read(filename)

#subbasin_shp <- readShapeSpatial(filename,
 #                                proj4string = CRS("+proj=longlat +datum=WGS84"))

subbasin_shp <- st_read("./data/sprague/gis/sprague_subbasins.shp")
#subbasin_shp <- st_set_crs(subbasin_shp, "+proj=longlat +datum=WGS84")

subbasin <- fortify(subbasin_shp, region="SITE") %>%
  filter(SITE != 'WR1000') %>%
 # mutate(SITE=id) %>%
  left_join(select(stn, SITE, SITE_NAME, SITE_ABBR) %>% mutate(SITE=as.character(SITE)), by='SITE')


subbasin_area <- subbasin_shp %>%
  mutate(SITE=as.character(SITE)) %>%
  filter(SITE != 'WR1000') %>%
  dplyr::rename(AREA_KM2=AreaSqKM) %>%
  left_join(select(stn, SITE, SITE_NAME, SITE_ABBR) %>% mutate(SITE=as.character(SITE)), by='SITE')

# incbasins w/ ivory ----
filename <- file.path(GIS_DIR, 'sprague_incbasins_ivory.shp')
cat("Loading shapefile:", filename, '\n')
#incbasin_ivory_shp <- readShapeSpatial(filename,
 #                                      proj4string = CRS("+proj=longlat +datum=WGS84"))

incbasin_ivory_shp <- st_read("./data/sprague/gis/sprague_incbasins_ivory.shp")

incbasin_ivory <- fortify(incbasin_ivory_shp, region="SITE") %>%
  filter(SITE != 'WR1000') %>%
 # mutate(SITE=id) %>%
  left_join(select(stn_incbasin_ivory, SITE, INC_SITE_NAME) %>%
              mutate(SITE=as.character(SITE),
                     INC_SITE_NAME=as.character(INC_SITE_NAME)), by='SITE')
 # droplevels() no applicable levels for this function
incbasin_ivory_area <- incbasin_ivory_shp %>%
  mutate(SITE = as.character(SITE)) %>%
  filter(SITE != 'WR1000') %>%
  dplyr::rename(AREA_KM2=AreaSqKM) %>%
  left_join(select(stn_incbasin_ivory, SITE, INC_SITE_NAME) %>%
              mutate(SITE=as.character(SITE),
                     INC_SITE_NAME=as.character(INC_SITE_NAME)), by='SITE') %>%
  select(SITE, INC_SITE_NAME, AREA_KM2)

# incbasins w/o ivory ----
filename <- file.path(GIS_DIR, 'sprague_incbasins.shp')
cat("Loading shapefile:", filename, '\n')
#incbasin_shp <- readShapeSpatial(filename,
 #                                proj4string = CRS("+proj=longlat +datum=WGS84"))
incbasin_shp <- st_read("./data/sprague/gis/sprague_incbasins.shp")
incbasin <- fortify(incbasin_shp, region="SITE") %>%
  filter(SITE != 'WR1000') %>%
 # mutate(SITE=id) %>%
  left_join(select(stn_incbasin, SITE, INC_SITE_NAME) %>%
              mutate(SITE=as.character(SITE),
                     INC_SITE_NAME=as.character(INC_SITE_NAME)), by='SITE')



 # droplevels()
incbasin_area <- incbasin_shp %>%
  mutate(SITE = as.character(SITE)) %>%
  filter(SITE != 'WR1000') %>%
  dplyr::rename(AREA_KM2=AreaSqKM) %>%
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
#flowline_shp <- readShapeSpatial(filename,
 #                                proj4string = CRS("+proj=longlat +datum=WGS84"))
flowline_shp <- st_read("./data/sprague/gis/sprague_flowlines.shp")
flowline <- fortify(flowline_shp)

# basemap ----
map <- get_stamenmap(bbox=c(-122.1, 42.15, -120.6, 43), zoom=10)

# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")),
                       c("ymin", "xmin", "ymax", "xmax"))

  # Coonvert the bbox to an sf polygon, transform it to 3857,
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))

  # Overwrite the bbox of the ggmap object with the transformed coordinates
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

# Use the function:
#map <- ggmap_bbox(map)


# save ----
filename <- 'gis.Rdata'
cat('\nSaving gis datasets to:', filename, '\n')
save(basin, subbasin, incbasin, flowline,
     basin_area, subbasin_area, incbasin_area,
     map, file=filename)

cat('\n\n')

