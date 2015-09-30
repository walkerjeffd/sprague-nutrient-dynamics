library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())

rm(list=ls())

cat(paste0(rep('=', 80), collapse=''), '\n')
cat("Loading USGS dataset...\n\n")

DATA_DIR <- getOption('UKL_DATA')

# load stn ----
filename <- file.path(DATA_DIR, 'sprague', 'usgs', 'usgs_stations.rdb')
cat("Loading stations from:", filename, "\n")
stn.usgs <- read.table(filename,
                       sep='\t',
                       skip=77,
                       col.names=c("agency_cd","site_no","station_nm","site_tp_cd","lat_va","long_va","dec_lat_va","dec_long_va","coord_meth_cd","coord_acy_cd","coord_datum_cd","dec_coord_datum_cd","district_cd","state_cd","county_cd","country_cd","land_net_ds","map_nm","map_scale_fc","alt_va","alt_meth_cd","alt_acy_va","alt_datum_cd","huc_cd","basin_cd","topo_cd","data_types_cd","instruments_cd","construction_dt","inventory_dt","drain_area_va","contrib_drain_area_va","tz_cd","local_time_fg","reliability_cd","gw_file_cd","nat_aqfr_cd","aqfr_cd","aqfr_type_cd","well_depth_va","hole_depth_va","depth_src_cd","project_no","rt_bol","peak_begin_date","peak_end_date","peak_count_nu","qw_begin_date","qw_end_date","qw_count_nu","gw_begin_date","gw_end_date","gw_count_nu","sv_begin_date","sv_end_date","sv_count_nu"),
                       stringsAsFactors=FALSE) %>%
  select(STATION_ID=site_no, DESCRIPTION=station_nm, LAT=dec_lat_va, LON=dec_long_va, DRAINAGE_AREA_SQMI=drain_area_va) %>%
  mutate(STATION_ID=as.character(STATION_ID)) %>%
  mutate(SITE_NAME=plyr::revalue(STATION_ID,
                               c('11495800'='NF',
                                 '11497500'='Beatty',
                                 '11497550'='Godowa',
                                 '11501000'='Power')))

# load data ----
filename <- file.path(DATA_DIR, 'sprague', 'usgs',
                      '11495800_sprague_nf_1993-05-01_2012-10-10.txt')
cat("Loading flow data from:", filename, "\n")
q.11495800 <- read.table(file=filename,
                         skip=27, col.names=c('AGENCY', 'STATION_ID', 'DATE', 'FLOW', 'FLAG'), stringsAsFactors=FALSE)

# filename <- file.path(DATA_DIR, 'sprague', 'usgs',
#                       '11497500_sprague_beatty_1953-10-01_1991-09-30.txt')
# cat("Loading flow data from:", filename, "\n")
# q.11497500 <- read.table(file=filename,
#                          skip=27, col.names=c('AGENCY', 'STATION_ID', 'DATE', 'FLOW', 'FLAG'), stringsAsFactors=FALSE)

filename <- file.path(DATA_DIR, 'sprague', 'usgs',
                      '11501000_sprague_chiloquin_1921-03-01_2015-02-06.txt')
cat("Loading flow data from:", filename, "\n")
q.11501000 <- read.table(file=filename,
                         skip=28, col.names=c('AGENCY', 'STATION_ID', 'DATE', 'FLOW', 'FLAG'), stringsAsFactors=FALSE)

q.usgs <- rbind(q.11501000, q.11495800)

q.usgs <- mutate(q.usgs,
                 DATE=ymd(DATE),
                 STATION_ID=as.character(STATION_ID))

q.usgs <- rename(q.usgs, SOURCE=AGENCY)

q.usgs <- left_join(q.usgs,
                    select(stn.usgs, STATION_ID, SITE_NAME),
                    by='STATION_ID')

stn_period <- group_by(q.usgs, STATION_ID) %>%
  summarise(START_DATE=min(DATE),
            END_DATE=max(DATE))

stn.usgs <- filter(stn.usgs, STATION_ID %in% stn_period$STATION_ID) %>%
  left_join(stn_period, by="STATION_ID")

# plot ----
ggplot(q.usgs, aes(DATE, FLOW)) +
  geom_line() +
  facet_wrap(~STATION_ID+SITE_NAME, scales='free_y', ncol=1) +
  labs(x='', y='Flow (cfs)', title='USGS Daily Flow Data')

# save ----
filename <- file.path('csv', 'stn_usgs.csv')
cat('\nSaving USGS stations to:', filename, '\n')
select(stn.usgs, SITE_NAME, STATION_ID, DESCRIPTION, LAT, LON,
       DRAINAGE_AREA_SQMI, START_DATE, END_DATE) %>%
  write.csv(file=filename, row.names=FALSE)

filename <- 'usgs.Rdata'
cat('Saving USGS flow dataset to:', filename, '\n')
save(q.usgs, stn.usgs, file=filename)

cat('\n\n')
