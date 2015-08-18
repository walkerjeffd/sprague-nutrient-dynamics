library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())

DATA_DIR <- getOption('UKL_DATA')

load('kt_sprague.Rdata')
load('gis.Rdata')

# stations ----
stn_ivory <- left_join(stn.kt_sprague,
                       select(incbasin_ivory_area, SITE, INC_SITE,
                              INC_SITE_NAME, INC_SITE_ABBR,
                              INC_AREA_KM2=AREA_KM2),
                       by='SITE')
stn <- filter(stn.kt_sprague, !(SITE_ABBR %in% c('SFI', 'NFI'))) %>%
  left_join(select(incbasin_area, SITE, INC_SITE, INC_SITE_NAME,
                   INC_SITE_ABBR, INC_AREA_KM2=AREA_KM2),
            by='SITE')

# load data ----
nlcd_codes <- read.csv(file.path(DATA_DIR, 'sprague', 'nlcd', 'metadata.csv'),
                       stringsAsFactors=FALSE) %>%
  dplyr::select(-LULC_DESCRIPTION)

nlcd_basin <- read.csv(file.path(DATA_DIR, 'sprague', 'nlcd', 'nlcd_basin.csv'),
                       stringsAsFactors=FALSE) %>%
  mutate(EXTENT='basin')

nlcd_valley <- read.csv(file.path(DATA_DIR, 'sprague', 'nlcd', 'nlcd_valley.csv'),
                        stringsAsFactors=FALSE) %>%
  mutate(EXTENT='valley') %>%
  filter(!(SITE %in% c('SR0050', 'SR0040')))

nlcd <- rbind_list(nlcd_basin, nlcd_valley) %>%
  dplyr::select(-X__fid__) %>%
  gather(LULC, COUNT, -SITE, -AreaSqKM, -EXTENT) %>%
  mutate(LULC=as.integer(extract_numeric(LULC)),
         AREA_KM2=COUNT*30*30/1000000) %>% # 30m x 30m cell size
  filter(!is.na(AREA_KM2), LULC != 0)

# add join columns
nlcd <- nlcd %>%
  filter(SITE != 'WR1000') %>%
  left_join(dplyr::select(nlcd_codes, LULC, LULC_CAT, LULC_GRP), by='LULC') %>%
  left_join(dplyr::select(stn_ivory, SITE, SITE_NAME, INC_SITE, INC_SITE_NAME,
                          INC_SITE_ABBR),
            by='SITE') %>%
  mutate(SITE=ordered(SITE, levels=stn_ivory$SITE))

# set factor order
nlcd <- mutate(nlcd,
               LULC_CAT=ordered(LULC_CAT,
                                levels=c('Developed', 'Planted/Cultivated',
                                         'Herbaceous', 'Shrubland', 'Barren',
                                         'Forest', 'Wetlands', 'Water')),
               LULC_GRP=factor(LULC_GRP))

# rename columns
nlcd <- mutate(nlcd, SITE_NAME=INC_SITE_NAME) %>%
  select(EXTENT, SITE_NAME, LULC_CAT, LULC_GRP, AREA_KM2)

# add Godowa-SF-NF
nlcd <- spread(nlcd, SITE_NAME, AREA_KM2) %>%
  # replace NA with 0
  gather(SITE_NAME, AREA_KM2, -EXTENT, -LULC_CAT, -LULC_GRP) %>%
  mutate(AREA_KM2=ifelse(is.na(AREA_KM2), 0, AREA_KM2)) %>%
  spread(SITE_NAME, AREA_KM2) %>%
  mutate(`Godowa-SF-NF`=`Godowa-SF_Ivory-NF_Ivory`+`SF_Ivory-SF`+`NF_Ivory-NF`) %>%
  gather(SITE_NAME, AREA_KM2, -EXTENT, -LULC_CAT, -LULC_GRP)

# expand grid
nlcd_unique <- expand.grid(EXTENT=unique(nlcd$EXTENT),
                           SITE_NAME=unique(nlcd$SITE_NAME),
                           LULC_CAT=unique(nlcd$LULC_CAT)) %>%
  left_join(select(nlcd, LULC_CAT, LULC_GRP) %>% unique, by='LULC_CAT')

nlcd <- full_join(nlcd, nlcd_unique,
                  by=c('EXTENT', 'SITE_NAME', 'LULC_CAT', 'LULC_GRP')) %>%
  mutate(AREA_KM2=ifelse(is.na(AREA_KM2), 0, AREA_KM2))
stopifnot(all(table(nlcd$EXTENT, nlcd$SITE_NAME)==14))

# compute area fraction
nlcd <- group_by(nlcd, EXTENT, SITE_NAME) %>%
  mutate(SUM_AREA_KM2=sum(AREA_KM2),
         AREA_FRAC=ifelse(SUM_AREA_KM2>0, AREA_KM2/SUM_AREA_KM2, 0)) %>%
  ungroup

# split into grp and cat datasets
nlcd.grp <- nlcd %>%
  mutate(SOURCE="NLCD_GRP") %>%
  select(SOURCE, EXTENT, SITE_NAME, LANDUSE=LULC_GRP, SUM_AREA_KM2, AREA_KM2, AREA_FRAC)
group_by(nlcd.grp, EXTENT, SITE_NAME, SUM_AREA_KM2) %>%
  summarise(AREA_FRAC=sum(AREA_FRAC))

nlcd.cat <- nlcd %>%
  mutate(SOURCE="NLCD_CAT") %>%
  select(SOURCE, EXTENT, SITE_NAME, LANDUSE=LULC_CAT, SUM_AREA_KM2, AREA_KM2, AREA_FRAC) %>%
  group_by(SOURCE, EXTENT, SITE_NAME, SUM_AREA_KM2, LANDUSE) %>%
  summarise(AREA_KM2=sum(AREA_KM2)) %>%
  ungroup %>%
  mutate(AREA_FRAC=ifelse(SUM_AREA_KM2>0, AREA_KM2/SUM_AREA_KM2, 0))
group_by(nlcd.cat, EXTENT, SITE_NAME, SUM_AREA_KM2) %>%
  summarise(AREA_FRAC=sum(AREA_FRAC))

nlcd <- rbind(nlcd.grp, nlcd.cat)

# check area sums from GIS
incbasin_area <- rbind(incbasin_area, incbasin_ivory_area) %>%
  unique %>%
  select(SITE_NAME=INC_SITE_NAME,
         GIS_AREA_KM2=AREA_KM2)

filter(nlcd, EXTENT=='basin') %>%
  group_by(SOURCE, SITE_NAME) %>%
  summarise(NLCD_AREA_KM2=sum(AREA_KM2)) %>%
  ungroup %>%
  left_join(incbasin_area, by='SITE_NAME') %>%
  mutate(AREA_DIFF=NLCD_AREA_KM2-GIS_AREA_KM2)

# check that sum of area_frac is unity
filter(nlcd, SITE_NAME %in% stn_ivory$INC_SITE_NAME) %>%
  group_by(SOURCE, EXTENT, SITE_NAME) %>%
  summarise(SUM=sum(AREA_FRAC)) %>%
  filter(SUM != 0) %>%
  (function(x) { all(x$SUM > 0.99999) })
filter(nlcd, SITE_NAME %in% stn$INC_SITE_NAME) %>%
  group_by(SOURCE, EXTENT, SITE_NAME) %>%
  summarise(SUM=sum(AREA_FRAC)) %>%
  filter(SUM != 0) %>%
  (function(x) { all(x$SUM > 0.99999) })

# summary of basin/valley areas
group_by(nlcd, SOURCE, EXTENT, SITE_NAME) %>%
  summarise(AREA_KM2=sum(AREA_KM2)) %>%
  spread(EXTENT, AREA_KM2)

# cumulative areas
nlcd.subbasin <- filter(nlcd, SITE_NAME %in% stn_ivory$INC_SITE_NAME) %>%
  select(SOURCE, EXTENT, SITE_NAME, LANDUSE, AREA_KM2) %>%
  spread(SITE_NAME, AREA_KM2) %>%
  mutate(NF_Ivory=`NF_Ivory-NF`+NF,
         SF_Ivory=`SF_Ivory-SF`+SF,
         Godowa=`Godowa-SF_Ivory-NF_Ivory`+SF_Ivory+NF_Ivory,
         Lone_Pine=`Lone_Pine-Godowa-Sycan`+Godowa+Sycan,
         Power=`Power-Lone_Pine`+Lone_Pine) %>%
  gather(SITE_NAME, AREA_KM2, -SOURCE, -EXTENT, -LANDUSE) %>%
  filter(SITE_NAME %in% subbasin_area$SITE_NAME) %>%
  mutate(SITE_NAME = ordered(as.factor(SITE_NAME), levels=levels(subbasin_area$SITE_NAME))) %>%
  group_by(SOURCE, EXTENT, SITE_NAME) %>%
  mutate(TOTAL_AREA_KM2=sum(AREA_KM2)) %>%
  ungroup %>%
  mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2))

# check that area fracs sum to 1
group_by(nlcd.subbasin, SOURCE, EXTENT, SITE_NAME) %>%
  summarise(AREA_FRAC=sum(AREA_FRAC)) %>%
  filter(AREA_FRAC != 0) %>%
  (function(x) { all(x$AREA_FRAC > 0.99999) })

filter(nlcd.subbasin, EXTENT=="basin") %>%
  select(SOURCE, SITE_NAME, TOTAL_AREA_KM2) %>%
  unique %>%
  left_join(select(subbasin_area, SITE_NAME, GIS_AREA_KM2=AREA_KM2), by="SITE_NAME") %>%
  mutate(AREA_DIFF=GIS_AREA_KM2-TOTAL_AREA_KM2)

stopifnot(all(table(nlcd.subbasin$EXTENT, nlcd.subbasin$SITE_NAME)==22))

nlcd.incbasin <- nlcd
stopifnot(all(table(nlcd.incbasin$EXTENT, nlcd.incbasin$SITE_NAME)==22))

# save nlcd ----
save(nlcd.incbasin, nlcd.subbasin, file='nlcd.Rdata')
