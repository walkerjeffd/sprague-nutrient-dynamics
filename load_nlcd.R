library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())

rm(list=ls())

cat(paste0(rep('=', 80), collapse=''), '\n')
cat("Loading NLCD dataset...\n\n")

DATA_DIR <- getOption('UKL_DATA')

load('kt_sprague.Rdata')
load('gis.Rdata')

# load data ----
filename <- file.path(DATA_DIR, 'sprague', 'nlcd', 'metadata.csv')
cat("Loading metadata from:", filename, "\n")
nlcd_codes <- read.csv(filename,
                       stringsAsFactors=FALSE) %>%
  dplyr::select(-LULC_DESCRIPTION)

filename <- file.path(DATA_DIR, 'sprague', 'nlcd', 'nlcd_basin.csv')
cat("Loading NLCD basin data from:", filename, "\n")
nlcd_basin <- read.csv(filename,
                       stringsAsFactors=FALSE) %>%
  mutate(EXTENT='basin')

filename <- file.path(DATA_DIR, 'sprague', 'nlcd', 'nlcd_valley.csv')
cat("Loading NLCD valley data from:", filename, "\n")
nlcd_valley <- read.csv(filename,
                        stringsAsFactors=FALSE) %>%
  mutate(EXTENT='valley') %>%
  filter(!(SITE %in% c('SR0050', 'SR0040')))

# note: nlcd zonal stats based on delineation with ivory stations
nlcd <- rbind_list(nlcd_basin, nlcd_valley) %>%
  dplyr::select(-X__fid__) %>%
  gather(LULC, COUNT, -SITE, -AreaSqKM, -EXTENT) %>%
  mutate(LULC=as.integer(extract_numeric(LULC)),
         AREA_KM2=COUNT*30*30/1000000) %>% # 30m x 30m cell size
  filter(!is.na(AREA_KM2), LULC != 0)

# stations ----
stn_ivory <- left_join(mutate(stn.kt_sprague, SITE=as.character(SITE)),
                       select(incbasin_area, SITE, INC_SITE_NAME,
                              INC_AREA_KM2=AREA_KM2) %>%
                         filter(INC_SITE_NAME != "Godowa-SF-NF") %>%
                         mutate(SITE=as.character(SITE)),
                       by='SITE') %>%
  mutate(SITE=as.character(SITE),
         INC_SITE_NAME=as.character(INC_SITE_NAME))
stn <- filter(stn.kt_sprague, !(SITE_NAME %in% c('SF_Ivory', 'NF_Ivory'))) %>%
  mutate(SITE=as.character(SITE)) %>%
  left_join(select(incbasin_area, SITE, INC_SITE_NAME, INC_AREA_KM2=AREA_KM2) %>%
              filter(!(INC_SITE_NAME %in% c("Godowa-SF_Ivory-NF_Ivory", "SF_Ivory-SF", "NF_Ivory-NF"))),
            by='SITE') %>%
  mutate(SITE=as.character(SITE),
         INC_SITE_NAME=as.character(INC_SITE_NAME))

# add join columns
nlcd <- nlcd %>%
  filter(SITE != 'WR1000') %>%
  left_join(select(nlcd_codes, LULC, LULC_CAT, LULC_GRP), by='LULC') %>%
  left_join(select(stn_ivory, SITE, SITE_NAME, INC_SITE_NAME),
            by='SITE')

# set factor order
nlcd <- mutate(nlcd,
               LULC_CAT=ordered(LULC_CAT,
                                levels=c('Developed', 'Planted/Cultivated',
                                         'Herbaceous', 'Shrubland', 'Barren',
                                         'Forest', 'Wetlands', 'Water')),
               LULC_GRP=factor(LULC_GRP))

# rename columns
nlcd <- select(nlcd, EXTENT, INC_SITE_NAME, LULC_CAT, LULC_GRP, AREA_KM2)

# add Godowa-SF-NF
nlcd <- spread(nlcd, INC_SITE_NAME, AREA_KM2) %>%
  # replace NA with 0
  gather(INC_SITE_NAME, AREA_KM2, -EXTENT, -LULC_CAT, -LULC_GRP) %>%
  mutate(AREA_KM2=ifelse(is.na(AREA_KM2), 0, AREA_KM2)) %>%
  spread(INC_SITE_NAME, AREA_KM2) %>%
  mutate(`Godowa-SF-NF`=`Godowa-SF_Ivory-NF_Ivory`+`SF_Ivory-SF`+`NF_Ivory-NF`) %>%
  gather(INC_SITE_NAME, AREA_KM2, -EXTENT, -LULC_CAT, -LULC_GRP) %>%
  mutate(INC_SITE_NAME=as.character(INC_SITE_NAME))

# expand grid
nlcd_unique <- expand.grid(EXTENT=unique(nlcd$EXTENT),
                           INC_SITE_NAME=unique(nlcd$INC_SITE_NAME),
                           LULC_CAT=unique(nlcd$LULC_CAT)) %>%
  left_join(select(nlcd, LULC_CAT, LULC_GRP) %>% unique, by='LULC_CAT') %>%
  mutate(EXTENT=as.character(EXTENT),
         INC_SITE_NAME=as.character(INC_SITE_NAME),
         LULC_CAT=as.character(LULC_CAT),
         LULC_GRP=as.character(LULC_GRP))

nlcd <- mutate(nlcd,
               EXTENT=as.character(EXTENT),
               INC_SITE_NAME=as.character(INC_SITE_NAME),
               LULC_CAT=as.character(LULC_CAT),
               LULC_GRP=as.character(LULC_GRP)) %>%
  full_join(nlcd_unique,
            by=c('EXTENT', 'INC_SITE_NAME', 'LULC_CAT', 'LULC_GRP')) %>%
  mutate(AREA_KM2=ifelse(is.na(AREA_KM2), 0, AREA_KM2))
stopifnot(all(table(nlcd$EXTENT, nlcd$INC_SITE_NAME)==14))

# compute area fraction
nlcd <- group_by(nlcd, EXTENT, INC_SITE_NAME) %>%
  mutate(SUM_AREA_KM2=sum(AREA_KM2),
         AREA_FRAC=ifelse(SUM_AREA_KM2>0, AREA_KM2/SUM_AREA_KM2, 0)) %>%
  ungroup

# split into grp and cat datasets
nlcd.grp <- nlcd %>%
  mutate(SOURCE="NLCD_GRP") %>%
  select(SOURCE, EXTENT, INC_SITE_NAME, LANDUSE=LULC_GRP, SUM_AREA_KM2, AREA_KM2, AREA_FRAC)
group_by(nlcd.grp, EXTENT, INC_SITE_NAME, SUM_AREA_KM2) %>%
  summarise(AREA_FRAC=sum(AREA_FRAC))

nlcd.cat <- nlcd %>%
  mutate(SOURCE="NLCD_CAT") %>%
  select(SOURCE, EXTENT, INC_SITE_NAME, LANDUSE=LULC_CAT, SUM_AREA_KM2, AREA_KM2, AREA_FRAC) %>%
  group_by(SOURCE, EXTENT, INC_SITE_NAME, SUM_AREA_KM2, LANDUSE) %>%
  summarise(AREA_KM2=sum(AREA_KM2)) %>%
  ungroup %>%
  mutate(AREA_FRAC=ifelse(SUM_AREA_KM2>0, AREA_KM2/SUM_AREA_KM2, 0))
group_by(nlcd.cat, EXTENT, INC_SITE_NAME, SUM_AREA_KM2) %>%
  summarise(AREA_FRAC=sum(AREA_FRAC))

nlcd <- rbind(nlcd.grp, nlcd.cat)

# check area sums from GIS
filter(nlcd, EXTENT=='basin') %>%
  group_by(SOURCE, INC_SITE_NAME) %>%
  summarise(NLCD_AREA_KM2=sum(AREA_KM2)) %>%
  ungroup %>%
  left_join(select(incbasin_area, INC_SITE_NAME, GIS_AREA_KM2=AREA_KM2) %>%
              mutate(INC_SITE_NAME=as.character(INC_SITE_NAME)), by='INC_SITE_NAME') %>%
  mutate(AREA_DIFF=NLCD_AREA_KM2-GIS_AREA_KM2)

# check that sum of area_frac is unity
nlcd %>%
  group_by(SOURCE, EXTENT, INC_SITE_NAME) %>%
  summarise(SUM=sum(AREA_FRAC)) %>%
  filter(SUM != 0) %>%
  (function(x) { stopifnot(all(x$SUM > 0.99999)) })

# summary of basin/valley areas
group_by(nlcd, SOURCE, EXTENT, INC_SITE_NAME) %>%
  summarise(AREA_KM2=sum(AREA_KM2)) %>%
  spread(EXTENT, AREA_KM2)

# cumulative areas
nlcd.subbasin <- filter(nlcd, !(INC_SITE_NAME %in% "Godowa-SF-NF")) %>%
  select(SOURCE, EXTENT, INC_SITE_NAME, LANDUSE, AREA_KM2) %>%
  spread(INC_SITE_NAME, AREA_KM2) %>%
  mutate(NF_Ivory=`NF_Ivory-NF`+NF,
         SF_Ivory=`SF_Ivory-SF`+SF,
         Godowa=`Godowa-SF_Ivory-NF_Ivory`+SF_Ivory+NF_Ivory,
         Lone_Pine=`Lone_Pine-Godowa-Sycan`+Godowa+Sycan,
         Power=`Power-Lone_Pine`+Lone_Pine) %>%
  gather(INC_SITE_NAME, AREA_KM2, -SOURCE, -EXTENT, -LANDUSE) %>%
  filter(INC_SITE_NAME %in% levels(subbasin_area$SITE_NAME)) %>%
  rename(SITE_NAME=INC_SITE_NAME) %>%
  group_by(SOURCE, EXTENT, SITE_NAME) %>%
  mutate(TOTAL_AREA_KM2=sum(AREA_KM2)) %>%
  ungroup %>%
  mutate(SITE_NAME=as.character(SITE_NAME),
         AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2))

# check that area fracs sum to 1
group_by(nlcd.subbasin, SOURCE, EXTENT, SITE_NAME) %>%
  summarise(AREA_FRAC=sum(AREA_FRAC)) %>%
  filter(AREA_FRAC != 0) %>%
  (function(x) { stopifnot(all(x$AREA_FRAC > 0.99999)) })

filter(nlcd.subbasin, EXTENT=="basin") %>%
  select(SOURCE, SITE_NAME, TOTAL_AREA_KM2) %>%
  unique %>%
  left_join(select(subbasin_area, SITE_NAME, GIS_AREA_KM2=AREA_KM2) %>%
              mutate(SITE_NAME=as.character(SITE_NAME)),
            by="SITE_NAME") %>%
  mutate(AREA_DIFF=GIS_AREA_KM2-TOTAL_AREA_KM2)

stopifnot(all(table(nlcd.subbasin$EXTENT, nlcd.subbasin$SITE_NAME)==22))

nlcd.incbasin <- nlcd
stopifnot(all(table(nlcd.incbasin$EXTENT, nlcd.incbasin$INC_SITE_NAME)==22))

# extract categories
nlcd.subbasin <- filter(nlcd.subbasin, SOURCE=="NLCD_CAT") %>%
  mutate(LANDUSE=ordered(LANDUSE, levels=c('Developed', 'Planted/Cultivated',
                                           'Herbaceous', 'Shrubland', 'Barren',
                                           'Forest', 'Wetlands', 'Water')),
         SITE_NAME=ordered(as.character(SITE_NAME), levels=levels(stn.kt_sprague$SITE_NAME)))
nlcd.incbasin <- filter(nlcd.incbasin, SOURCE=="NLCD_CAT") %>%
  mutate(LANDUSE=ordered(LANDUSE, levels=levels(nlcd.subbasin$LANDUSE)),
         INC_SITE_NAME=ordered(INC_SITE_NAME, levels=levels(incbasin_area$INC_SITE_NAME)))

stopifnot(all(table(nlcd.subbasin$SITE_NAME, nlcd.subbasin$EXTENT)==8))
stopifnot(all(table(nlcd.incbasin$INC_SITE_NAME, nlcd.incbasin$EXTENT)==8))

# save nlcd ----
filename <- 'nlcd.Rdata'
cat('\nSaving NLCD dataset to:', filename, '\n')
save(nlcd.incbasin, nlcd.subbasin, file=filename)

cat('\n\n')


