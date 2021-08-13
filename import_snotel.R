library(tidyverse)
library(snotelr)
library(dplyr)
library(tidyr)
library(lubridate)
library(fluxr)
library(ggplot2)
theme_set(theme_bw())

rm(list=ls())

source('functions.R')

cat(paste0(rep('=', 80), collapse=''), '\n')
cat("Loading SNOTEL dataset...\n\n")

#DATA_DIR <- getOption('UKL_DATA')
DATA_DIR <- './data'

# load data ----
#snotel_dir <- file.path(DATA_DIR, 'met', 'snotel')
snotel_dir <- file.path(DATA_DIR, 'raw', 'snotel')
files <- c(#'QUARTZ MOUNTAIN'='706_QUARTZ MOUNTAIN_20150219.txt',
  'SUMMER RIM'='800_SUMMER RIM_20150219.txt',
  'TAYLOR BUTTE'='810_TAYLOR BUTTE_20150219.txt',
  'CRAZYMAN FLAT'='1010_CRAZYMAN FLAT_20150219.txt')
#'SWAN LAKE MTN'='1077_SWAN LAKE MTN_20150219.txt')

snotel <- lapply(names(files), function(site_name) {
  fname <- files[[site_name]]
  cat("Loading SNOTEL data from:", fname, '\n')
  df <- read.csv(file = file.path(snotel_dir, fname), header = TRUE, skip = 7)
  names(df) <- c('DATE', 'SWE_in')
  df$DATE <- ymd(df$DATE)
  df$SITE_NAME <- site_name
  df
}) %>%
  do.call(rbind, .)

water_day <- function(x, start_month = 10) {
  jd <- lubridate::yday(x)

  if (start_month == 1) {
    # same as julian day
    return(jd)
  } else {
    wy <- fluxr::wyear(x, start_month = start_month)
    wy_start_date <- as.Date(paste(wy - 1, start_month, 1, sep = "-"))
    wd <- as.numeric(difftime(x, wy_start_date, units = "day")) + 1
    return(wd)
  }
}


snotel <- snotel %>%
  mutate(
    SWE_cm=SWE_in*2.54,
    mutate(DATE = ymd(DATE)) ,
    WYEAR=fluxr::wyear(DATE),
    WDAY = water_day (DATE),
    SITE_NAME=as.factor(SITE_NAME),
    WDAY_LABEL=as.Date("2001-10-01")+days(WDAY))

snotel <- snotel %>%
  filter(WYEAR <= 2014)

# stations ----
stn.snotel <- read.csv(file.path(snotel_dir, 'snotel_stations.csv'), stringsAsFactors=FALSE)
stn.snotel <- filter(stn.snotel, SITE_NAME %in% unique(snotel$SITE_NAME))

# the appropriate site ID number is not present in this df

stn.snotel.ID <-  c("810","1010","800")

snotel.update <- lapply(stn.snotel.ID,function(x) {
  data <- snotel_download(site_id = x ,internal=T)
})

snotel.update <- do.call(bind_rows,snotel.update)

snotel.update <-  snotel.update %>%
  dplyr::rename(SWE_mm=snow_water_equivalent,SITE_NAME=site_name,DATE=date) %>%
  mutate(SITE_NAME=toupper(SITE_NAME),DATE=ymd(DATE),YEAR=year(DATE),MONTH=month(DATE),WYEAR=fluxr::wyear(DATE),SITE_NAME=sub("\\s+$", "", SITE_NAME)) %>%
  mutate(SITE_NAME=ifelse(SITE_NAME=="TAYLOR BUTTE ","TAYLOR BUTTE",
                          ifelse(SITE_NAME=="CRAZYMAN FLAT ","CRAZYMAN FLAT",
                                 ifelse(SITE_NAME=="SUMMER RIM ","SUMMER RIM",SITE_NAME)))) %>%
  mutate(SWE_cm=SWE_mm*0.1) %>%
  select(SITE_NAME,DATE,SWE_cm,YEAR,MONTH,WYEAR)



snotel.update %>%
  filter(WYEAR=="2013") %>%
  ggplot()+
  geom_point( aes(x=DATE,y=SWE_cm),color="red",alpha=0.5)+
  geom_point(data=filter(snotel, WYEAR=="2013"),aes(x=DATE,y=SWE_cm),color="blue",alpha=0.5)+
  facet_wrap(~SITE_NAME)+
  theme_bw()

# time series of SWE to verify the data are the same with the new and prior imports

names <- unique(snotel.update$WYEAR)
plot_list = list()
for (ii in names) {

  # colors <- c("GHCND original import"="blue","GHCND updated import"="red")

  p <-
    snotel.update %>%
    filter(WYEAR == ii) %>%
    ggplot()+
    geom_point( aes(x=DATE,y=SWE_cm),color="red",alpha=0.5)+
    geom_point(data=filter(snotel, WYEAR==ii),aes(x=DATE,y=SWE_cm),color="blue",alpha=0.5)+
    facet_wrap(~SITE_NAME)+
    theme_bw() +
    ggtitle(as.character(ii))+
    labs(x="Date",y=as.character(ii))
  plot_list[[ii]] = p
  ggsave(p, file=paste0("explore/snotel/snotel_SWE_", ii,".png"), width = 11, height = 8.5, units = "in")
}

filename <- "./pdf/import/thermo_snotel_SWE.pdf"
pdf(file = filename,height=8.5,width=11)

for(ii in names) {
  print(plot_list[[ii]])

}
dev.off()


