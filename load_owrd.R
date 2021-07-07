library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())

rm(list=ls())

cat(paste0(rep('=', 80), collapse=''), '\n')
cat("Loading OWRD dataset...\n\n")

#DATA_DIR <- getOption('UKL_DATA')
DATA_DIR <- './data'

# load stn ----
filename <- file.path(DATA_DIR, 'sprague', 'owrd', 'owrd_stations.csv')
cat("Loading stations from:", filename, "\n")
stn.owrd <- read.csv(filename, stringsAsFactors=FALSE)

stn.owrd <- mutate(stn.owrd,
                   LAT=as.numeric(gsub("[^0-9.]+", "", as.character(LAT))),
                   LON=as.numeric(gsub("[^0-9.]+", "", as.character(LON))))
stn.owrd <- mutate(stn.owrd,
                   LAT_DEG=floor(LAT/10000),
                   LAT_MIN=floor((LAT-LAT_DEG*10000)/100),
                   LAT_SEC=LAT-LAT_DEG*10000-LAT_MIN*100,
                   LON_DEG=floor(LON/10000),
                   LON_MIN=floor((LON-LON_DEG*10000)/100),
                   LON_SEC=LON-LON_DEG*10000-LON_MIN*100,
                   LAT=LAT_DEG+(LAT_MIN+LAT_SEC/60)/60,
                   LON=-(LON_DEG+(LON_MIN+LON_SEC/60)/60),
                   DRAINAGE_AREA_SQMI=DRAINAGE_AREA_MI2)
stn.owrd <- select(stn.owrd, STATION_ID, DESCRIPTION, LAT, LON, DRAINAGE_AREA_SQMI, SITE_NAME)

# load ----
q.owrd <- lapply(stn.owrd$STATION_ID, function(site) {
  filename <- file.path(DATA_DIR, 'sprague', 'owrd',
                        paste0('Station_', site, '_mean_daily_flow.txt'))
  cat("Loading flow data from:", filename, "\n")

  df <- read.table(filename,
                   sep='\t',
                   header=TRUE,
                   as.is=TRUE)
  return(df)
}) %>%
  do.call(rbind, .)

q.owrd <- mutate(q.owrd,
                 STATION_ID=station_nbr,
                 DATE=mdy(record_date),
                 FLOW=mean_daily_flow_cfs) %>%
  select(STATION_ID, DATE, FLOW) %>%
  mutate(SOURCE='OWRD') %>%
  left_join(select(stn.owrd, STATION_ID, SITE_NAME), by="STATION_ID")

stn_period <- group_by(q.owrd, STATION_ID) %>%
  dplyr::summarise(START_DATE=min(DATE),
            END_DATE=max(DATE))

stn.owrd <- filter(stn.owrd, STATION_ID %in% stn_period$STATION_ID) %>%
  left_join(stn_period, by="STATION_ID")

# plot ----
filter(q.owrd, !is.na(FLOW)) %>%
  ggplot(aes(DATE, FLOW)) +
  geom_line() +
  facet_wrap(~STATION_ID+SITE_NAME, scales='free_y', ncol=1) +
  labs(x='', y='Flow (cfs)', title='OWRD Daily Flow Data')

# save ----
filename <- file.path('csv', 'stn_owrd.csv')
cat('\nSaving OWRD stations to:', filename, '\n')
select(stn.owrd, SITE_NAME, STATION_ID, DESCRIPTION, LAT, LON,
       DRAINAGE_AREA_SQMI, START_DATE, END_DATE) %>%
  write.csv(file=filename, row.names=FALSE)

filename <- 'owrd.Rdata'
cat('Saving OWRD flow dataset to:', filename, '\n')
save(q.owrd, stn.owrd, file=filename)

cat('\n\n')

