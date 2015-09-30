library(dplyr)
library(tidyr)
library(lubridate)
library(fluxr)
library(ggplot2)
theme_set(theme_bw())

rm(list=ls())

cat(paste0(rep('=', 80), collapse=''), '\n')
cat("Loading PRISM dataset...\n\n")

DATA_DIR <- getOption('UKL_DATA')

# load data ----
load('gis.Rdata')

filename <- file.path(DATA_DIR, 'sprague', 'prism', 'ppt_basins.csv')
cat("Loading PRISM dataset from:", filename, '\n')
prism <- read.csv(filename, skip=1, stringsAsFactors=FALSE)
names(prism) <- toupper(names(prism))
prism <- rename(prism, PRCP=PPT) %>%
  filter(SITE != 'WR1000') %>%
  mutate(MONTHYEAR=as.Date(paste(YEAR, MONTH, 1, sep='-')),
         WYEAR=wyear(MONTHYEAR)) %>%
  select(MONTHYEAR, WYEAR, SITE, PRCP)

# join incbasin
prism <- left_join(prism,
                   filter(incbasin_area, INC_SITE_NAME != "Godowa-SF-NF") %>%
                     mutate(SITE=as.character(SITE),
                            INC_SITE_NAME=as.character(INC_SITE_NAME)),
                     by='SITE') %>%
  select(MONTHYEAR, WYEAR, INC_SITE_NAME, AREA_KM2, PRCP)

# compute subbasin
prism <- mutate(prism, PRCP_AREA=PRCP*AREA_KM2) %>%
  select(MONTHYEAR, WYEAR, INC_SITE_NAME, PRCP_AREA) %>%
  spread(INC_SITE_NAME, PRCP_AREA) %>%
  mutate(`Godowa-SF-NF`=`Godowa-SF_Ivory-NF_Ivory`+`SF_Ivory-SF`+`NF_Ivory-NF`,
         NF_Ivory=`NF_Ivory-NF`+NF,
         SF_Ivory=`SF_Ivory-SF`+SF,
         Godowa=`Godowa-SF-NF`+SF+NF,
         Lone_Pine=`Lone_Pine-Godowa-Sycan`+Godowa+Sycan,
         Power=`Power-Lone_Pine`+Lone_Pine) %>%
  gather(SITE_NAME, PRCP_AREA, -MONTHYEAR, -WYEAR) %>%
  mutate(SITE_NAME=as.character(SITE_NAME))

prism_incbasin <- rename(prism, INC_SITE_NAME=SITE_NAME) %>%
  inner_join(select(incbasin_area, INC_SITE_NAME, AREA_KM2) %>%
               mutate(INC_SITE_NAME=as.character(INC_SITE_NAME)),
             by="INC_SITE_NAME") %>%
  mutate(PRCP=PRCP_AREA/AREA_KM2,
         INC_SITE_NAME=ordered(INC_SITE_NAME, levels=levels(incbasin_area$INC_SITE_NAME))) %>%
  select(-PRCP_AREA)

prism_subbasin <- prism %>%
  inner_join(select(subbasin_area, SITE_NAME, AREA_KM2) %>%
               mutate(SITE_NAME=as.character(SITE_NAME)),
             by="SITE_NAME") %>%
  mutate(PRCP=PRCP_AREA/AREA_KM2,
         SITE_NAME=ordered(SITE_NAME, levels=levels(subbasin_area$SITE_NAME))) %>%
  select(-PRCP_AREA)

# mean annual precip
prism_subbasin_wyr <- group_by(prism_subbasin, SITE_NAME, AREA_KM2, WYEAR) %>%
  summarise(N_MONTH=n(),
            PRCP=sum(PRCP)/10) %>% # mm/yr -> cm/yr
  ungroup %>%
  filter(N_MONTH == 12)

# overall mean annual precip (cm/yr) by subbasin
prism_subbasin_site <- prism_subbasin_wyr %>%
  group_by(SITE_NAME, AREA_KM2) %>%
  summarise(PRCP=mean(PRCP))
mean_annual_precip <- filter(prism_subbasin_site, SITE_NAME=="Power")$PRCP
cat("\nMean annual precip at Power (cm/yr):", mean_annual_precip, '\n\n')

# overall mean annual precip (cm/yr) by incbasin
group_by(prism_incbasin, INC_SITE_NAME, AREA_KM2, WYEAR) %>%
  summarise(N_MONTH=n(),
            PRCP=sum(PRCP)/10) %>% # mm/yr -> cm/yr
  filter(N_MONTH == 12) %>%
  summarise(PRCP=mean(PRCP))

prism_subbasin_wyr %>%
  ggplot(aes(SITE_NAME, PRCP)) +
  geom_boxplot()

# report figures
filename <- "report/prism-annual-precip.png"
cat('Saving annual precip plot to:', filename, '\n')
png(filename, width=6, height=4, res=200, units="in")
p <- ggplot() +
  geom_bar(aes(WYEAR, PRCP), stat="identity",
           data=prism_subbasin_wyr %>%
             filter(SITE_NAME=="Power")) +
  geom_hline(yint=mean_annual_precip, color='black', linetype=2) +
  geom_text(aes(x=2013, y=mean_annual_precip+2), label="Mean", hjust=0, vjust=0, size=4) +
  labs(x="Water Year", y="Annual Precipitation (cm/yr)") +
  scale_y_continuous(breaks=seq(0, 100, 10)) +
  scale_x_continuous(breaks=seq(1980, 2015, 5))
print(p)
dev.off()

filename <- "report/prism-monthly-precip.png"
cat('Saving monthly precip boxplots to:', filename, '\n')
png(filename, width=6, height=4, res=200, units="in")
p <- filter(prism_subbasin, SITE_NAME=="Power") %>%
  group_by(WYEAR) %>%
  mutate(N=n()) %>%
  filter(N==12) %>%
  mutate(MONTH=month(MONTHYEAR),
         MONTH=ordered(as.character(MONTH), levels=as.character(c(10:12, 1:9))),
         PRCP=PRCP/10) %>%
  ggplot(aes(MONTH, PRCP)) +
  geom_boxplot(fill="grey80") +
  labs(x="Month", y="Monthly Precipitation (cm/mon)")
print(p)
dev.off()


# save ----
filename <- 'prism.Rdata'
cat('\nSaving PRISM dataset to:', filename, '\n')
save(prism_subbasin, prism_incbasin, file=filename)

cat('\n\n')

