library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())

rm(list=ls())

cat(paste0(rep('=', 80), collapse=''), '\n')
cat("Loading Geomorphology Delineation...\n\n")

load('gis.Rdata')


# load ----
#DATA_DIR <- getOption('UKL_DATA')
#GIS_DIR <- file.path(DATA_DIR, '../gis/sprague/geomorph')
DATA_DIR <- './data'
GIS_DIR <- file.path(DATA_DIR, 'sprague', 'geomorphology')

filename <- file.path(GIS_DIR, 'incremental_basins_geomorphology_clip.csv')
cat("Loading geomorphology areas from:", filename, '\n')
geomorph <- read.csv(filename, stringsAsFactors=FALSE) %>%
  dplyr::rename(AREA_KM2=AreaSqKM) %>%
  rbind(data.frame(SITE=c('SR0040'), AREA_KM2=0)) %>%
  filter(SITE != "WR1000") %>%
  left_join(filter(incbasin_area, INC_SITE_NAME != "Godowa-SF-NF") %>%
              dplyr::rename(BASIN_AREA_KM2=AREA_KM2), by="SITE") %>%
  select(INC_SITE_NAME, VALLEY_AREA_KM2=AREA_KM2, BASIN_AREA_KM2)

geomorph <- geomorph %>%
  pivot_longer(c(VALLEY_AREA_KM2,BASIN_AREA_KM2),names_to="VAR",values_to="VALUE") %>%  # gather(geomorph, VAR, VALUE, -INC_SITE_NAME) %>%
  pivot_wider(names_from = INC_SITE_NAME, values_from = VALUE) %>%   #spread(INC_SITE_NAME, VALUE) %>%
  mutate(`Godowa-SF-NF`=`Godowa-SF_Ivory-NF_Ivory`+`SF_Ivory-SF`+`NF_Ivory-NF`,
         NF_Ivory=`NF_Ivory-NF`+NF,
         SF_Ivory=`SF_Ivory-SF`+SF,
         Godowa=`Godowa-SF-NF`+SF+NF,
         Lone_Pine=`Lone_Pine-Godowa-Sycan`+Godowa+Sycan,
         Power=`Power-Lone_Pine`+Lone_Pine) %>%
  pivot_longer(c(SF:Power),names_to="SITE_NAME",values_to="VALUE") %>% #gather(SITE_NAME, VALUE, -VAR) %>%
  mutate(SITE_NAME=as.character(SITE_NAME)) %>%
  pivot_wider(names_from = VAR,values_from=VALUE) %>%  # spread(VAR, VALUE) %>%
  mutate(FRAC_AREA=VALLEY_AREA_KM2/BASIN_AREA_KM2)

geomorph_incbasin <- filter(geomorph, SITE_NAME %in% incbasin_area$INC_SITE_NAME) %>%
  dplyr::rename(INC_SITE_NAME=SITE_NAME) %>%
  mutate(INC_SITE_NAME=ordered(INC_SITE_NAME, levels=levels(incbasin_area$INC_SITE_NAME))) %>%
  arrange(INC_SITE_NAME)

geomorph_subbasin <- filter(geomorph, SITE_NAME %in% subbasin_area$SITE_NAME) %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=levels(subbasin_area$SITE_NAME))) %>%
  arrange(SITE_NAME)


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
cat('\nSaving Geomorphology areas to csv: csv/geomorph_subbasin.csv and csv/geomorph_incbasin.csv\n')
write.csv(geomorph_subbasin, file='csv/geomorph_subbasin.csv', row.names=FALSE)
write.csv(geomorph_incbasin, file='csv/geomorph_incbasin.csv', row.names=FALSE)

filename <- "geomorph.Rdata"
cat('Saving Geomorphology dataset to:', filename, '\n')
save(geomorph_subbasin, geomorph_incbasin, file=filename)

cat('\n\n')


