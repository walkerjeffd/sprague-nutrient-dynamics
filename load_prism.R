library(dplyr)
library(tidyr)
library(lubridate)
library(fluxr)

rm(list=ls())

DATA_DIR <- getOption('UKL_DATA')

# load data ----
load('gis.Rdata')

prism <- read.csv(file.path(DATA_DIR, 'sprague', 'prism', 'ppt_basins.csv'), skip=1, stringsAsFactors=FALSE)
names(prism) <- toupper(names(prism))
prism <- rename(prism, PRCP=PPT) %>%
  filter(SITE != 'WR1000') %>%
  mutate(SITE=ordered(SITE, levels=levels(incbasin_ivory_area$SITE)),
         MONTHYEAR=as.Date(paste(YEAR, MONTH, 1, sep='-')),
         WYEAR=wyear(MONTHYEAR)) %>%
  select(MONTHYEAR, WYEAR, SITE, PRCP)

# join incbasin
prism_incbasin <- left_join(prism, incbasin_ivory_area, by='SITE') %>%
  select(MONTHYEAR, WYEAR, INC_SITE, INC_SITE_NAME, AREA_KM2, PRCP)

# compute subbasin
prism_subbasin <- mutate(prism_incbasin, PRCP_AREA=PRCP*AREA_KM2) %>%
  select(MONTHYEAR, WYEAR, INC_SITE, PRCP_AREA) %>%
  spread(INC_SITE, PRCP_AREA) %>%
  mutate(SR0140=`SR0140-SR0040`+SR0040,
         SR0150=`SR0150-SR0050`+SR0050,
         SR0060=`SR0060-SR0150-SR0140`+SR0150+SR0140,
         SR0080=`SR0080-SR0060-SR0070`+SR0060+SR0070,
         SR0090=`SR0090-SR0080`+SR0080) %>%
  select(MONTHYEAR, WYEAR, SR0090, SR0080, SR0070, SR0060, SR0150, SR0140, SR0050, SR0040) %>%
  gather(SITE, PRCP_AREA, SR0090:SR0040) %>%
  left_join(subbasin_area, by='SITE') %>%
  mutate(SITE=ordered(SITE, levels=levels(subbasin_area$SITE))) %>%
  mutate(PRCP=PRCP_AREA/AREA_KM2) %>%
  select(MONTHYEAR, WYEAR, SITE, SITE_NAME, AREA_KM2, PRCP)

# overall mean annual precip (in/yr) by subbasin
group_by(prism_subbasin, SITE_NAME, AREA_KM2, WYEAR) %>%
  summarise(N_MONTH=n(),
            PRCP=sum(PRCP)/25.4) %>%
  filter(N_MONTH == 12) %>%
  summarise(PRCP=mean(PRCP))

# overall mean annual precip (in/yr) by incbasin
group_by(prism_incbasin, INC_SITE_NAME, AREA_KM2, WYEAR) %>%
  summarise(N_MONTH=n(),
            PRCP=sum(PRCP)/25.4) %>%
  filter(N_MONTH == 12) %>%
  summarise(PRCP=mean(PRCP))

# report figures
png("report/prism-annual-precip.png", width=6, height=4, res=200, units="in")
filter(prism_subbasin, SITE_NAME=="Power") %>%
  group_by(WYEAR) %>%
  summarise(SUM=sum(PRCP/10), # mm/mon -> cm/mon
            N=n()) %>%
  filter(N==12) %>%
  ggplot(aes(WYEAR, SUM)) +
  geom_bar(stat="identity") +
  labs(x="Water Year", y="Annual Precipitation (cm/yr)") +
  scale_y_continuous(breaks=seq(0, 100, 10)) +
  scale_x_continuous(breaks=seq(1980, 2015, 5))
dev.off()

png("report/prism-monthly-precip.png", width=6, height=4, res=200, units="in")
filter(prism_subbasin, SITE_NAME=="Power") %>%
  group_by(WYEAR) %>%
  mutate(N=n()) %>%
  filter(N==12) %>%
  mutate(MONTH=month(MONTHYEAR),
         MONTH=ordered(as.character(MONTH), levels=as.character(c(10:12, 1:9))),
         PRCP=PRCP/10) %>%
  ggplot(aes(MONTH, PRCP)) +
  geom_boxplot(fill="grey80") +
  labs(x="Month", y="Monthly Precipitation (cm/mon)")
dev.off()


# save ----
save(prism_subbasin, prism_incbasin, file='prism.Rdata')

