library(dplyr)
library(tidyr)
library(lubridate)
library(fluxr)

DATA_DIR <- getOption('UKL_DATA')

# load data ----
snotel_dir <- file.path(DATA_DIR, 'met', 'snotel')
files <- c('QUARTZ MOUNTAIN'='706_QUARTZ MOUNTAIN_20150219.txt',
           'SUMMER RIM'='800_SUMMER RIM_20150219.txt',
           'TAYLOR BUTTE'='810_TAYLOR BUTTE_20150219.txt',
           'CRAZYMAN FLAT'='1010_CRAZYMAN FLAT_20150219.txt',
           'SWAN LAKE MTN'='1077_SWAN LAKE MTN_20150219.txt')

snotel <- lapply(names(files), function(site_name) {
  fname <- files[[site_name]]
  df <- read.csv(file = file.path(snotel_dir, fname), header = TRUE, skip = 7)
  names(df) <- c('DATE', 'SWE_in')
  df$DATE <- ymd(df$DATE)
  df$SITE_NAME <- site_name
  df
}) %>%
  do.call(rbind, .)

snotel <- snotel %>%
  mutate(SITE_NAME=factor(SITE_NAME),
         WYEAR=wyear(DATE)) %>%
  filter(WYEAR <= 2014)

# stations ----
stn.snotel <- read.csv(file.path(snotel_dir, 'snotel_stations.csv'), stringsAsFactors=FALSE)
stn.snotel <- filter(stn.snotel, SITE_NAME %in% unique(snotel$SITE_NAME))

# aggregate by wyr ----
snotel.wyr <- snotel %>%
  group_by(SITE_NAME, WYEAR) %>%
  summarise(N=sum(!is.na(SWE_in)),
            SWE_MEAN=mean(SWE_in, na.rm=TRUE),
            SWE_MAX=max(SWE_in, na.rm=TRUE)) %>%
  filter(N>=300)

snotel.wyr <- snotel %>%
  filter(month(DATE)==4, day(DATE)==1) %>%
  select(SITE_NAME, WYEAR, SWE_APR=SWE_in) %>%
  left_join(snotel.wyr, by=c('SITE_NAME', 'WYEAR')) %>%
  select(SITE_NAME, WYEAR, N, SWE_APR, SWE_MEAN, SWE_MAX)

# save ----
save(snotel, snotel.wyr, stn.snotel, file='snotel.Rdata')
