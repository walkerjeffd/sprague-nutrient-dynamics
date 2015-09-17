library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())

rm(list=ls())

load('gis.Rdata')

stn_names <- select(subbasin_area, SITE, SITE_NAME)

geomorph_incbasin <- read.csv('~/Dropbox/Work/klamath/data/sprague/gis/incremental_basins_geomorphology_clip.csv', stringsAsFactors=FALSE) %>%
  rename(AREA_KM2=AreaSqKM) %>%
  rbind(data.frame(SITE=c('SR0040'), AREA_KM2=0)) %>%
  left_join(stn_names, by="SITE") %>%
  filter(SITE != "WR1000") %>%
  select(SITE, SITE_NAME, AREA_KM2) %>%
  left_join(select(incbasin_ivory_area, SITE_NAME, INC_SITE_NAME,
                   BASIN_AREA_KM2=AREA_KM2),
            by="SITE_NAME") %>%
  select(SITE_NAME, INC_SITE_NAME, VALLEY_AREA_KM2=AREA_KM2, BASIN_AREA_KM2)

geomorph_subbasin <- geomorph_incbasin %>%
  select(SITE_NAME, AREA_KM2=VALLEY_AREA_KM2) %>%
  mutate(IDX=1) %>%
  spread(SITE_NAME, AREA_KM2) %>%
  mutate(NF_Ivory=NF_Ivory+NF,
         SF_Ivory=SF_Ivory+SF,
         Godowa=Godowa+NF_Ivory+SF_Ivory,
         Lone_Pine=Lone_Pine+Godowa+Sycan,
         Power=Power+Lone_Pine) %>%
  gather(SITE_NAME, VALLEY_AREA_KM2, -IDX) %>%
  select(-IDX)

geomorph_subbasin <- geomorph_subbasin %>%
  left_join(select(subbasin_area, SITE_NAME, BASIN_AREA_KM2=AREA_KM2),
            by="SITE_NAME") %>%
  mutate(FRAC_AREA=VALLEY_AREA_KM2/BASIN_AREA_KM2)

geomorph_incbasin <- geomorph_incbasin %>%
  select(INC_SITE_NAME, VALLEY_AREA_KM2, BASIN_AREA_KM2) %>%
  gather(VAR, AREA, -INC_SITE_NAME) %>%
  spread(INC_SITE_NAME, AREA) %>%
  mutate(`Godowa-SF-NF`=`Godowa-SF_Ivory-NF_Ivory`+`SF_Ivory-SF`+`NF_Ivory-NF`) %>%
  gather(INC_SITE_NAME, AREA, -VAR) %>%
  spread(VAR, AREA) %>%
  mutate(FRAC_AREA=VALLEY_AREA_KM2/BASIN_AREA_KM2)

incbasin_names <- select(incbasin_ivory_area, INC_SITE, INC_SITE_NAME) %>%
  rbind(select(incbasin_area, INC_SITE, INC_SITE_NAME)) %>%
  unique
geomorph_incbasin <- left_join(geomorph_incbasin, incbasin_names,
                               by="INC_SITE_NAME") %>%
  select(INC_SITE, INC_SITE_NAME, VALLEY_AREA_KM2, BASIN_AREA_KM2, FRAC_AREA) %>%
  mutate(INC_SITE_NAME=ordered(INC_SITE_NAME,
                               levels=c('Power-Lone_Pine',
                                        'Lone_Pine-Godowa-Sycan',
                                        'Godowa-SF_Ivory-NF_Ivory',
                                        'Sycan',
                                        'Godowa-SF-NF',
                                        'SF_Ivory-SF',
                                        'SF',
                                        'NF_Ivory-NF',
                                        'NF'))) %>%
  arrange(INC_SITE_NAME)
geomorph_incbasin

subbasin_names <- select(subbasin_area, SITE, SITE_NAME)
geomorph_subbasin <- left_join(geomorph_subbasin, subbasin_names, by="SITE_NAME") %>%
  select(SITE, SITE_NAME, VALLEY_AREA_KM2, BASIN_AREA_KM2, FRAC_AREA)




# plots ----

geomorph_subbasin %>%
  ggplot(aes(SITE_NAME, FRAC_AREA)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent) +
  coord_flip() +
  labs(y="Fraction Area (%)", x="Station")

geomorph_incbasin %>%
  ggplot(aes(INC_SITE_NAME, FRAC_AREA)) +
  geom_bar(stat="identity") +
  scale_y_continuous(labels=scales::percent) +
  coord_flip() +
  labs(y="Fraction Area (%)", x="Incremental Basin")

# save ----
write.csv(geomorph_subbasin, file='csv/geomorph_subbasin.csv', row.names=FALSE)
write.csv(geomorph_incbasin, file='csv/geomorph_incbasin.csv', row.names=FALSE)

save(geomorph_subbasin, geomorph_incbasin, file="geomorph.Rdata")
