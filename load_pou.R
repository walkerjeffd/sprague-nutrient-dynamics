library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())

rm(list=ls())

DATA_DIR <- getOption('UKL_DATA')

load('kt_sprague.Rdata')

pou.basin <- read.csv(file.path(DATA_DIR, 'sprague/water_rights/pou_irrigation_basin.csv'),
                      stringsAsFactors=FALSE) %>%
  select(SITE, AREA_KM2=AreaSqKM) %>%
  mutate(EXTENT="basin")
pou.valley <- read.csv(file.path(DATA_DIR, 'sprague/water_rights/pou_irrigation_valley.csv'),
                       stringsAsFactors=FALSE) %>%
  select(SITE, AREA_KM2=AreaSqKM) %>%
  mutate(EXTENT="valley")

pou <- rbind(pou.basin, pou.valley) %>%
  left_join(select(stn.kt_sprague, SITE, SITE_NAME), by="SITE") %>%
  filter(!is.na(SITE_NAME)) %>%
  rbind(data.frame(EXTENT=c('basin', 'valley', 'valley'),
                   SITE_NAME=c(rep('NF', 2), 'SF'),
                   SITE=c(rep('SR0040', 2), 'SR0050'),
                   AREA_KM2=rep(0, 3))) %>%
  select(-SITE) %>%
  arrange(EXTENT, SITE_NAME)

pou <- spread(pou, SITE_NAME, AREA_KM2) %>%
  mutate(SF_Ivory=SF_Ivory+SF,
         NF_Ivory=NF_Ivory+NF,
         Godowa=Godowa+NF_Ivory+SF_Ivory,
         Lone_Pine=Lone_Pine+Godowa+Sycan,
         Power=Power+Lone_Pine,
         'Godowa+Sycan'=Godowa+Sycan,
         'SF+NF'=SF+NF,
         'SF_Ivory+NF_Ivory'=SF_Ivory+NF_Ivory) %>%
  gather(SITE_NAME, AREA_KM2, -EXTENT) %>%
  mutate(LANDUSE="POU", SOURCE="POU") %>%
  select(SOURCE, EXTENT, SITE_NAME, LANDUSE, AREA_KM2)

ggplot(pou, aes(SITE_NAME, AREA_KM2, fill=EXTENT)) +
  geom_bar(stat='identity', position='dodge')

save(pou, file='pou.Rdata')
