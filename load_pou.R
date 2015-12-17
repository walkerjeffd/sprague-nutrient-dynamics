library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())

rm(list=ls())

cat(paste0(rep('=', 80), collapse=''), '\n')
cat("Loading POU dataset...\n\n")

DATA_DIR <- getOption('UKL_DATA')

load('kt_sprague.Rdata')
load('gis.Rdata')
load('geomorph.Rdata')

# load ----
filename <- file.path(DATA_DIR, 'sprague/water_rights/pou_irrigation_basin.csv')
cat("Loading POU basin dataset from:", filename, "\n")
pou.basin <- read.csv(filename,
                      stringsAsFactors=FALSE) %>%
  filter(SITE != "WR1000") %>%
  select(SITE, AREA_KM2=AreaSqKM) %>%
  mutate(EXTENT="basin")

filename <- file.path(DATA_DIR, 'sprague/water_rights/pou_irrigation_valley.csv')
cat("Loading POU valley dataset from:", filename, "\n")
pou.valley <- read.csv(filename,
                       stringsAsFactors=FALSE) %>%
  filter(SITE != "WR1000") %>%
  select(SITE, AREA_KM2=AreaSqKM) %>%
  mutate(EXTENT="valley")

pou.basin <- full_join(pou.basin,
                       filter(incbasin_area, INC_SITE_NAME != "Godowa-SF-NF") %>%
                         rename(TOTAL_AREA_KM2=AREA_KM2,
                                SITE_NAME=INC_SITE_NAME),
                       by="SITE") %>%
  mutate(EXTENT="basin",
         AREA_KM2=ifelse(is.na(AREA_KM2), 0, AREA_KM2))

pou.valley <- full_join(pou.valley,
                        left_join(geomorph_incbasin,
                                  select(incbasin_area, SITE, INC_SITE_NAME),
                                  by="INC_SITE_NAME") %>%
                          filter(INC_SITE_NAME != "Godowa-SF-NF") %>%
                          select(TOTAL_AREA_KM2=VALLEY_AREA_KM2,
                                 SITE_NAME=INC_SITE_NAME,
                                 SITE),
                         by="SITE") %>%
  mutate(EXTENT="valley",
         AREA_KM2=ifelse(is.na(AREA_KM2), 0, AREA_KM2))

pou <- rbind(pou.basin, pou.valley) %>%
  select(-SITE) %>%
  gather(VAR, VALUE, AREA_KM2, TOTAL_AREA_KM2) %>%
  spread(SITE_NAME, VALUE) %>%
  mutate(`Godowa-SF-NF`=`Godowa-SF_Ivory-NF_Ivory`+`SF_Ivory-SF`+`NF_Ivory-NF`,
         NF_Ivory=`NF_Ivory-NF`+NF,
         SF_Ivory=`SF_Ivory-SF`+SF,
         Godowa=`Godowa-SF-NF`+SF+NF,
         Lone_Pine=`Lone_Pine-Godowa-Sycan`+Godowa+Sycan,
         Power=`Power-Lone_Pine`+Lone_Pine,
         `Godowa+Sycan`=Godowa+Sycan,
         `SF+NF`=SF+NF,
         `SF_Ivory+NF_Ivory`=SF_Ivory+NF_Ivory) %>%
  gather(SITE_NAME, VALUE, -EXTENT, -VAR) %>%
  mutate(SITE_NAME=as.character(SITE_NAME)) %>%
  spread(VAR, VALUE) %>%
  mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2))

pou_incbasin <- filter(pou, SITE_NAME %in% incbasin_area$INC_SITE_NAME) %>%
  rename(INC_SITE_NAME=SITE_NAME) %>%
  mutate(INC_SITE_NAME=ordered(INC_SITE_NAME, levels=levels(incbasin_area$INC_SITE_NAME))) %>%
  arrange(INC_SITE_NAME)

pou_subbasin <- filter(pou, SITE_NAME %in% c(levels(subbasin_area$SITE_NAME), "Godowa+Sycan", "SF_Ivory+NF_Ivory", "SF+NF")) %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=c('Power', 'Lone_Pine',
                                               'Godowa+Sycan', 'Godowa',
                                               'Sycan', 'SF_Ivory+NF_Ivory',
                                               'SF_Ivory', 'NF_Ivory',
                                               'SF+NF',
                                               'SF', 'NF'))) %>%
  arrange(SITE_NAME)

# plots ----
gather(pou_incbasin, VAR, VALUE, AREA_KM2:AREA_FRAC) %>%
  ggplot(aes(INC_SITE_NAME, VALUE, fill=EXTENT)) +
  geom_bar(stat='identity', position='dodge') +
  facet_wrap(~VAR, scales='free_y') +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

gather(pou_subbasin, VAR, VALUE, AREA_KM2:AREA_FRAC) %>%
  ggplot(aes(SITE_NAME, VALUE, fill=EXTENT)) +
  geom_bar(stat='identity', position='dodge') +
  facet_wrap(~VAR, scales='free_y') +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

# report figure ----
filename <- "report/pou-subbasin-composition.png"
cat('Saving subbasin composition to:', filename, '\n')
png(filename, width=8, height=4, res=200, units="in")
pou_subbasin %>%
  filter(!(SITE_NAME %in% c('Godowa+Sycan', 'SF_Ivory+NF_Ivory', 'SF+NF'))) %>%
  mutate(EXTENT=plyr::revalue(EXTENT, c("basin"="Total Basin", "valley"="Lower Valley"))) %>%
  ggplot(aes(SITE_NAME, AREA_FRAC)) +
  geom_bar(stat='identity', position='stack', fill='grey30') +
  facet_wrap(~EXTENT, scales='free_y') +
  labs(x='Station', y='Fraction of Drainage Area (%)') +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1),
        strip.background=element_blank(),
        strip.text=element_text(face="bold")) +
  scale_y_continuous(labels=scales::percent)
dev.off()

# save ----
filename <- "pou.Rdata"
cat('\nSaving POU dataset to:', filename, '\n')
save(pou_incbasin, pou_subbasin, file=filename)

cat('\n\n')
