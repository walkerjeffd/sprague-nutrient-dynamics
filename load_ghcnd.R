library(dplyr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())

rm(list=ls())

cat(paste0(rep('=', 80), collapse=''), '\n')
cat("Loading GHCND dataset...\n\n")

#DATA_DIR <- getOption('UKL_DATA')
DATA_DIR <- './data'


# load data ----
filename <- file.path(DATA_DIR, 'raw', 'ghcnd', 'ghcnd_sprague.csv')
cat('Loading GHCND dataset from:', filename, '\n\n')

ghcnd <- read.csv(filename, stringsAsFactors=FALSE, na.strings='-9999') %>%
  select(STATION, STATION_NAME, ELEVATION, LATITUDE, LONGITUDE, DATE, TMIN, TMAX, PRCP, SNOW) %>%
  mutate(TMIN=TMIN/10, # degC
         TMAX=TMAX/10, # degC
         PRCP=PRCP/10) # mm/day
ghcnd <- mutate(ghcnd,
                DATE=ymd(DATE),
                MONTH=month(DATE),
                WYEAR=fluxr::wyear(DATE))
stn.ghcnd <- select(ghcnd, STATION, STATION_NAME, ELEVATION, LATITUDE, LONGITUDE) %>% unique

df.ghcnd %>% select(STATION_ID) %>% unique

# plots ----
ggplot(ghcnd, aes(DATE, PRCP)) +
  geom_point(size=1) +
  facet_wrap(~STATION_NAME) +
  labs(x="Date", y="Precip (mm/day)",
       title="GHCND Daily Precipitation")

dplyr::group_by(ghcnd, STATION_NAME, WYEAR) %>%
  dplyr::summarise(N=sum(!is.na(PRCP)),
            N_NA=sum(is.na(PRCP)),
            PRCP=sum(PRCP, na.rm=TRUE)) %>%
  ggplot(aes(factor(WYEAR), N)) +
  geom_bar(stat='identity') +
  facet_wrap(~STATION_NAME) +
  labs(x="Water Year", y="Number of Daily Observations",
       title="GHCND Daily Observation Count by Water Year")

# save ----
cat('Saving GHCND dataset: ghcnd.Rdata\n')
save(ghcnd, stn.ghcnd, file='ghcnd.Rdata')

cat('\n\n')
