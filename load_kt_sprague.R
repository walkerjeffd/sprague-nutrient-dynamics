library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(maptools)
library(ggplot2)
theme_set(theme_bw())

DATA_DIR <- getOption('UKL_DATA')

df <- read.csv(file.path(DATA_DIR, 'sprague', 'kt', 'Sprague River--Water Quality Dataset 2001_2014_revised_20150127.csv'), stringsAsFactors=FALSE)
df <- rename(df,
             INDEX=INDEX,
             DATE=DATE,
             LAT=GPS_N,
             LON=GPS_W,
             SITE_DESCRIPTION=Site.ID,
             SITE=SITE,
             JD=JD,
             CJD=CJD,
             MONTH=MO,
             DAY=DAY,
             YEAR=YR,
             TIME=TIME,
             FLOW_cfs=Q_CFS,
             STAGE_ft=STAFF_GAGE_FT,
             TEMP_degC=TEMP_C,
             TEMP_CALC=TEMP_CALC,
             TEMP_ACT=TEMP_ACT,
             TEMP_DIFF=TEMP_DIFF,
             COND_mScm=COND_MS_CM,
             COND_uScm=COND_US_CM,
             COND_CALC=COND_CALC,
             COND_ACT=COND_ACT,
             COND_DIFF=COND_DIFF,
             DO_ppm=DO_mgl,
             DO_CALC=DO_CALC,
             DO_ACT=DO_ACT,
             DO_DIFF=DO_DIFF,
             PH_su=PH,
             PH_CALC =PH_CALC,
             PH_ACT=PH_ACT,
             PH_DIFF=PH_DIFF,
             PSAT_pct=PSAT_perc,
             PSAT_CALC=PSAT_CALC,
             PSAT_ACT=PSAT_ACT,
             PSAT_DIFF=PSAT_DIFF,
             TP_ppm=TP_MGL,
             TP_DUP=TP_DUP,
             TP_SPIKE=TP_SPIKE,
             TP_DIFF=TP_DIFF,
             TP_RPD=TP_RPD,
             TP_PASS_FAIL=TP_PASS_FAIL,
             PO4_ppm=PO4_MGL,
             PO4_DUP=PO4_DUP,
             PO4_SPIKE=PO4_SPIKE,
             PO4_DIFF=PO4_DIFF,
             PO4_RPD=PO4_RPD,
             PO4_PASS_FAIL=PO4_PASS_FAIL,
             NH4_ppm=NH4_MGL,
             NH4_DUP=NH4_DUP,
             NH4_SPIKE=NH4_SPIKE,
             NH4_DIFF=NH4_DIFF,
             NH4_RPD=NH4_RPD,
             NH4_PASS_FAIL=NH4_PASS_FAIL,
             NO23_ppm=NO3NO2_MGL,
             NO23_DUP=NO3NO2_DUP,
             NO23_SPIKE=NO3NO2_SPIKE,
             NO23_DIFF=NO3NO2_DIFF,
             NO23_RPD=NO3NO2_RPD,
             NO23_PASS_FAIL=NO3NO2_PASS_FAIL,
             NO2_ppm=NO2_MGL,
             NO2_DUP=NO2_DUP,
             NO2_SPIKE=NO2_SPIKE,
             NO2_DIFF=NO2_DIFF,
             NO2_RPD=NO2_RPD,
             NO2_PASS_FAIL=NO2_PASS_FAIL,
             TN_ppm=TN_MGL,
             TN_DUP=TN_DUP,
             TN_SPIKE=TN_SPIKE,
             TN_DIFF=TN_DIFF,
             TN_RPD=TN_RPD,
             TN_PASS_FAIL=TN_PASS_FAIL,
             CL_ppm=CL_MGL,
             CL_DUP=CL_DUP,
             CL_SPIKE=CL_SPIKE,
             CL_DIFF=CL_DIFF,
             CL_RPD=CL_RPD,
             CL_PASS_FAIL=CL_PASS_FAIL,
             SIO2_ppm=SIO2.MGL,
             SIO2_DUP=SIO2_DUP,
             SIO2_SPIKE=SIO2_SPIKE,
             SIO2_DIFF=SIO2_DIFF,
             SIO2_RPD=SIO2_RPD,
             SIO2_PASS_FAIL=SIO2_PASS_FAIL,
             TSS_ppm=TSS_MGL,
             TSS_DUP=TSS_DUP,
             TSS_DIFF=TSS_DIFF,
             TSS_RPD=TSS_RPD,
             TSS_PASS_FAIL=TSS_PASS_FAIL,
             TURBIDITY_NTU=TURB_MGL,
             TURB_DUP=TURB_DUP,
             TURB_DIFF=TURB_DIFF,
             TURB_RPD=TURB_RPD,
             TURB_PASS_FAIL=TURB_PASS_FAIL,
             CHLA_ppb=CHLA_UGL,
             CHLA_DUP=CHLA_DUP,
             CHLA_DIFF=CHLA_DIFF,
             CHLA_RPD=CHLA_RPD,
             CHLA_PASS_FAIL=CHLA_PASS_FAIL,
             PHAE_ppb=PHAE_UGL,
             PHAE_DUP=PHAE_DUP,
             PHAE_DIFF=PHAE_DIFF,
             PHAE_RPD=PHAE_RPD,
             PHAE_PASS_FAIL=PHAE_PASS_FAIL,
             NOTES=Notes)
df.orig <- df

df <- select(df, DATE, TIME, LAT, LON, SITE_DESCRIPTION, SITE,
             FLOW_cfs, STAGE_ft, TEMP_degC, COND_uScm, DO_ppm, PH_su, PSAT_pct,
             TP_ppm, PO4_ppm, NH4_ppm, NO23_ppm, NO2_ppm, TN_ppm,
             TSS_ppm, TURBIDITY_NTU, NOTES) %>%
  mutate(STAGE_ft=as.double(STAGE_ft)) # 'n/a' converted to NA

# remove rows without date
idx.missing_date <- which(is.na(df$DATE))
if (length(idx.missing_date) > 0) {
  cat('Removing', length(idx.missing_date), 'rows without dates\n')
  df.missing_date <- df[idx.missing_date, ]
  df <- df[-idx.missing_date, ]
}

# remove QA samples
idx.qaqc_samples <- which(df$SITE %in% c("SR0110", "SR0550", "SR0990"))
if (length(idx.qaqc_samples)) {
  cat("Removing", length(idx.qaqc_samples), "QAQC sample rows\n")
  df.qaqc_samples <- df[idx.qaqc_samples, ]
  df <- df[-idx.qaqc_samples, ]
}

# fix dates
# df[which(df$DATE=="5/20/29"), 'DATE'] <- "5/20/09"

# fix times
if (length(which(df$TIME==8.35)) > 0) {
  cat('Fixing time: 8.35\n')
  df[which(df$TIME==8.35), "TIME"] <- 835
}
if (length(which(df$TIME==15.06)) > 0) {
  cat('Fixing time: 15.06\n')
  df[which(df$TIME==15.06), "TIME"] <- 1506
}

# parse dates and times
cat('Parsing dates and times\n')
df <- mutate(df, DATE=mdy(DATE))
df <- mutate(df, HOUR=floor(TIME/100),
                 MINUTE=TIME-HOUR*100)
df[['TIME']]  <- ifelse(is.na(df[['HOUR']]),
                        '12:00',
                        paste0(sprintf('%02d', df[['HOUR']]), ':',
                               sprintf('%02d', df[['MINUTE']])))

df <- mutate(df, DATETIME=paste0(format(DATE, '%Y-%m-%d'), ' ', TIME) %>%
                            ymd_hm())

# remove Whiskey Creek
idx.whiskey <- which(df$SITE == "SR3333")
if (length(idx.whiskey) > 0) {
  cat("Removing", length(idx.whiskey),"rows for Whisky Creek\n")
  df.whiskey <- df[idx.whiskey, ]
  df <- df[-idx.whiskey, ]
}

# remove early NF Ivory Pine data
idx.nf_ivory <- which(df$SITE == "SR0140" & year(df$DATE) <= 2008)
if (length(idx.nf_ivory) > 0) {
  cat("Removing", length(idx.nf_ivory),"rows for NF Ivory Pine before 2008\n")
  df.nf_ivory <- df[idx.nf_ivory, ]
  df <- df[-idx.nf_ivory, ]
}

# extract stations
cat('Extracting station info\n')
stn.raw <- select(df, SITE, SITE_DESCRIPTION, LAT, LON) %>%
  mutate_each(funs(str_trim)) %>%
  unique %>%
  arrange(SITE)
stn.raw <- mutate(stn.raw,
                  LAT_DMS=as.character(gsub("[^0-9]+", "", as.character(LAT))),
                  LON_DMS=format(as.character(gsub("[^0-9]+", "", as.character(LON))), digits=8),
                  LAT_D=substr(LAT_DMS, 1, 2),
                  LAT_M=substr(LAT_DMS, 3, 4),
                  LAT_S=substr(LAT_DMS, 5, 8),
                  LON_D=substr(LON_DMS, 1, 3),
                  LON_M=substr(LON_DMS, 4, 5),
                  LON_S=substr(LON_DMS, 6, 9),
                  LAT_DD=as.numeric(LAT_D) + (as.numeric(LAT_M) + as.numeric(LAT_S)/600)/60,
                  LON_DD=-(as.numeric(LON_D) + (as.numeric(LON_M) + as.numeric(LON_S)/600)/60)) %>%
  select(SITE, SITE_DESCRIPTION, LAT_DD, LON_DD) %>%
  unique

# save raw stations to csv
select(stn.raw, SITE, DESCRIPTION=SITE_DESCRIPTION, LAT=LAT_DD, LON=LON_DD) %>%
  unique %>%
  write.csv('./csv/kt_sprague_stations_raw.csv', row.names=FALSE)

# load manual stn list from csv
cat("Loading site table from csv\n")
stn <- read.csv(file.path(DATA_DIR, 'sprague', 'kt', 'kt_sprague_stations.csv'), stringsAsFactors=FALSE)

# load shapefile with subbasin areas
cat("Loading basins shapefile and adding drainage area to sites table\n")
subbasins_shp <- readShapeSpatial(file.path(DATA_DIR, '../gis/sprague/r_wgs84/sprague_subbasins.shp'),
                                  proj4string = CRS("+proj=longlat +datum=WGS84"))
subbasins <- fortify(subbasins_shp, region="SITE")

stn <- left_join(stn, subbasins_shp@data, by="SITE") %>%
  rename(DRAINAGE_AREA_SQKM=AreaSqKM)
stopifnot(sum(is.na(stn$DRAINAGE_AREA_SQKM))==0)

# check for duplicate site/dates
cat("Check for duplicate site/dates\n")
df.dups <- group_by(df, SITE, DATE) %>%
  summarise(N=n()) %>%
  filter(N>1)
stopifnot(nrow(df.dups) == 0)

# add site_name to df
cat('Add SITE_NAME to dataset\n')
df <- left_join(df, stn[, c('SITE', 'SITE_NAME')], by='SITE')

# reshape
cat("Reshaping dataset to long format\n")
df <- gather(df, VAR.UNITS, VALUE, FLOW_cfs:TURBIDITY_NTU) %>%
  separate(VAR.UNITS, c('VAR', 'UNITS'), sep='[_]') %>%
  select(DATE, DATETIME, SITE, SITE_NAME, VAR, UNITS, VALUE) %>%
  mutate(VAR=factor(VAR),
         UNITS=factor(UNITS)) %>%
  filter(!is.na(VALUE))

# order factors
cat("Setting SITE factor order\n")
stn <- mutate(stn,
              SITE=ordered(SITE, levels=stn$SITE),
              SITE_NAME=ordered(SITE_NAME, levels=stn$SITE_NAME),
              SITE_ABBR=ordered(SITE_ABBR, levels=stn$SITE_ABBR))
df <- mutate(df,
             SITE=ordered(SITE, levels=stn$SITE),
             SITE_NAME=ordered(SITE_NAME, levels=stn$SITE_NAME))

# QAQC ----
cat("Perform QAQC tests\n")
df$QAQC <- 'PASS'

detection_limits <- readRDS('detection_limits.Rdata')
# plot nitrogen values at detection limit in 2008 to find siwtch point
# df.limits <- filter(df, VAR %in% detection_limits$VAR) %>%
#   left_join(detection_limits) %>%
#   mutate(HALF_LOWERDL=LOWERDL/2,
#          HALF_UPPERDL=UPPERDL/2)
#
# filter(df.limits, VAR %in% c('TN', 'NH4', 'NO23'), year(DATE)==2008) %>%
#   ggplot(aes(DATE, VALUE, color=VALUE==HALF_LOWERDL)) +
#   geom_point() +
#   scale_y_log10(breaks=fluxr::log_breaks(c(1, 2, 3, 4, 5), 10^seq(-3, 1))) +
#   facet_wrap(~VAR, scales='free_y', ncol=1)
# NOTE: DL switch on 2008-04-01

# compute rpd test
df.rpd <- df.orig %>%
  select(YEAR, MONTH, DAY, SITE, TP_ppm, TP_DUP, PO4_ppm, PO4_DUP, TN_ppm, TN_DUP, NH4_ppm, NH4_DUP, NO23_ppm, NO23_DUP, TSS_ppm, TSS_DUP) %>%
  filter(SITE %in% stn$SITE) %>%
  mutate(DATE=ymd(paste(YEAR,MONTH,DAY,sep='-')),
         SITE=factor(SITE)) %>%
  select(SITE, DATE, TP_ppm:TSS_DUP) %>%
  gather(VAR_TYPE, VALUE, -SITE, -DATE) %>%
  separate(VAR_TYPE, c("VAR", "TYPE"), sep="[_]") %>%
  mutate(TYPE=plyr::revalue(TYPE, c("ppm"="PRIMARY", "DUP"="DUP"))) %>%
  spread(TYPE, VALUE) %>%
  filter(!is.na(DUP)) %>%
  left_join(detection_limits, by="VAR") %>%
  mutate(DL_PERIOD=ifelse(DATE < ymd("2008-04-01"), 'UPPER', 'LOWER'),
         DL=ifelse(DL_PERIOD=='UPPER', UPPERDL, LOWERDL),
         HALF_DL=DL/2,
         DIFF=abs(PRIMARY-DUP),
         RPD=abs(PRIMARY-DUP)/((PRIMARY+DUP)/2),
         RPD_TYPE=ifelse(PRIMARY > 5*DL & DUP > 5*DL, 'HIGH', 'LOW'),
         RPD_PASS=ifelse(RPD_TYPE=='HIGH', RPD<=0.2, DIFF<=DL))

# number of rejected samples by RPD type
filter(df.rpd, RPD_PASS==FALSE) %>%
  group_by(VAR, RPD_TYPE) %>%
  tally %>%
  spread(RPD_TYPE, n) %>%
  mutate(HIGH=ifelse(is.na(HIGH), 0, HIGH),
         LOW=ifelse(is.na(LOW), 0, LOW),
         TOTAL=HIGH+LOW)

# histogram of RPD fails
filter(df.rpd, !RPD_PASS) %>%
  mutate(RPD_TYPE=ordered(RPD_TYPE, levels=c('LOW', 'HIGH'))) %>%
  ggplot(aes(RPD, fill=RPD_TYPE)) +
  geom_histogram() +
  facet_grid(VAR~RPD_TYPE) +
  scale_x_continuous(labels=scales::percent)

# pdf plots
pdf(file.path('pdf', 'dataset-rpd-test.pdf'), width=11, height=8.5)

# Timeseries of raw data with RPD test results
select(df.rpd, SITE, DATE, VAR, PRIMARY, DUP, HALF_DL, RPD_TYPE, RPD_PASS) %>%
  gather(SAMPLE, VALUE, PRIMARY, DUP) %>%
  ggplot(aes(DATE, VALUE, color=RPD_PASS)) +
  geom_line(aes(y=HALF_DL), color='black', alpha=0.5) +
  geom_point(aes(size=RPD_PASS)) +
  scale_color_manual('Pass RPD?', values=c('TRUE'='grey50', 'FALSE'='orangered')) +
  scale_size_manual('Pass RPD?', values=c('TRUE'=1, 'FALSE'=2)) +
  scale_y_log10(breaks=fluxr::log_breaks(c(1, 2, 3, 4, 5, 7), 10^seq(-3, 3))) +
  labs(x='', y='Concentration (ppm)', title='Timeseries of RPD Test Results (All Sites)') +
  facet_wrap(~VAR, scales='free_y') +
  theme(panel.grid.minor.y=element_blank())

# PRIMARY vs DUP scatter plot, showing which values pass/fail RPD test
select(df.rpd, SITE, DATE, VAR, PRIMARY, DUP, HALF_DL, RPD_TYPE, RPD_PASS) %>%
  ggplot(aes(PRIMARY, DUP, color=RPD_PASS)) +
  geom_abline(linetype=2) +
  geom_point(aes(size=RPD_PASS)) +
  scale_x_log10(breaks=fluxr::log_breaks(c(1, 2, 3, 4, 5, 7), 10^seq(-3, 1))) +
  scale_y_log10(breaks=fluxr::log_breaks(c(1, 2, 3, 4, 5, 7), 10^seq(-3, 1))) +
  labs(x='Primary Sample (ppm)', y='Duplicate Sample (ppm)', title='Comparison of Primary and Duplicate Concentrations (All Sites)') +
  scale_color_manual('Pass RPD?', values=c('TRUE'='grey50', 'FALSE'='orangered')) +
  scale_size_manual('Pass RPD?', values=c('TRUE'=1, 'FALSE'=2)) +
  facet_wrap(~VAR, scales='free') +
  theme(panel.grid.minor=element_blank(),
        axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
dev.off()

# add rpd results
cat('Add RPD results to main data frame\n')
df <- left_join(df, select(df.rpd, SITE, DATE, VAR, RPD_PASS), by=c('SITE', 'DATE', 'VAR')) %>%
  mutate(QAQC=ifelse(!is.na(RPD_PASS) & RPD_PASS==FALSE, 'RPD', QAQC))
stopifnot(sum(!df$RPD_PASS, na.rm=TRUE)==sum(df$QAQC=='RPD'))

# assign BLANK for SF on 2008-07-16
cat('Assigning BLANK QAQC to SF on 2008-07-16\n')
stopifnot(sum(df[which(df$SITE=='SR0050' & df$DATE==ymd('2008-07-16') & df$VAR %in% c('TP', 'PO4', 'TN', 'NH4', 'NO23')), "VALUE"]) == 0.018)
df[which(df$SITE=='SR0050' & df$DATE==ymd('2008-07-16') & df$VAR %in% c('TP', 'PO4', 'TN', 'NH4', 'NO23')), 'QAQC'] <- 'BLANK'

# assign BLANK for abnormally low TP, TN and NH4 values
idx.low_tp <- which(df$VAR=="TP" & df$VALUE <= 0.009 & df$QAQC == 'PASS')
cat("Assigning LOW QAQC to", length(idx.low_tp), "TP results\n")
df[idx.low_tp, 'QAQC'] <- 'LOW'

idx.low_tn <- which(df$VAR=="TN" & df$VALUE < 0.02 & df$QAQC == 'PASS')
cat("Assigning LOW QAQC to", length(idx.low_tn), "TN results\n")
df[idx.low_tn, 'QAQC'] <- 'LOW'

idx.low_nh4 <- which(df$VAR=='NH4' & df$VALUE <= 0.001)
cat("Assigning LOW QAQC to", length(idx.low_nh4), "NH4 results\n")
df[idx.low_nh4, 'QAQC'] <- 'LOW'

# assign NEGATIVE for negative TSS results
idx.negative_tss <- which(df$VAR=='TSS' & df$VALUE <= 0)
cat("Assigning NEGATIVE QAQC to", length(idx.negative_tss), "TSS results\n")
df[idx.negative_tss, 'QAQC'] <- 'NEGATIVE'

# add outliers (via load_outliers.R)
outliers <- readRDS('outliers.Rdata')
cat("Results identified as outliers\n")
select(outliers, SITE_NAME, VAR, DATE) %>%
  arrange(SITE_NAME, VAR, DATE) %>%
  print

df <- left_join(df, outliers, by=c('DATE', 'SITE_NAME', 'VAR')) %>%
  mutate(FLAGGED=ifelse(is.na(FLAGGED), FALSE, FLAGGED))
stopifnot(sum(df$FLAGGED)==nrow(outliers))

# assign OUTLIER QAQC flag
idx.outlier <- which(df$FLAGGED)
cat("Assigning OUTLIER QAQC to", length(idx.outlier), "results\n")
df[idx.outlier, 'QAQC'] <- 'OUTLIER'

# convert QAQC to ordered factor
df <- mutate(df, QAQC=ordered(QAQC, levels=c('PASS', 'RPD', 'BLANK', 'LOW', 'NEGATIVE', 'OUTLIER')))
stopifnot(all(!is.na(df$QAQC)))

# add detection limits
df <- left_join(df, detection_limits, by='VAR') %>%
  mutate(DL_PERIOD=ifelse(DATE < ymd("2008-04-01"), 'UPPER', 'LOWER'),
         DL=ifelse(DL_PERIOD=='UPPER', UPPERDL, LOWERDL),
         HALF_DL=DL/2)

# RAW dataset ----
df.raw <- df %>%
  mutate(SITE=ordered(SITE, levels=levels(stn$SITE)),
         SITE_NAME=ordered(SITE_NAME, levels=levels(stn$SITE_NAME)))
cat("Summary of QAQC flags for RAW dataset")
table(df.raw$VAR, df.raw$QAQC)

# CLEAN dataset ----
df.clean <- df.raw

# keep only PASS and RPD samples
df.clean <- filter(df.clean, QAQC %in% c('PASS', 'RPD'))

# extract flow and nutrient samples
df.clean <- filter(df.clean, VAR %in% c('FLOW', 'TP', 'PO4', 'TN', 'NH4', 'NO23', 'TSS')) %>%
  mutate(VAR=ordered(VAR, levels=c('FLOW', 'TP', 'PO4', 'TN', 'NH4', 'NO23', 'TSS')))
df.clean <- droplevels(df.clean)
cat("Summary of QAQC flags for CLEAN dataset")
table(df.clean$VAR, df.clean$QAQC)

# POR dataset ----
df.por <- df.clean

# remove Ivory stations and TSS
df.por <- df.por %>%
  filter(!(SITE_NAME %in% c('SF_Ivory', 'NF_Ivory')),
         !(VAR %in% 'TSS')) %>%
  droplevels %>%
  mutate(LIMITED = ifelse(is.na(UPPERDL), FALSE, VALUE < UPPERDL),
         VALUE = ifelse(LIMITED, UPPERDL, VALUE))
cat("Summary of variables set to detection limit for POR dataset\n")
table(df.por$VAR, df.por$LIMITED)
cat("Summary of sites set to detection limit for POR dataset\n")
table(df.por$SITE_NAME, df.por$LIMITED)

# check that all values are at or above upper dl
df.por_check <- filter(df.por, VAR %in% detection_limits$VAR) %>%
  group_by(VAR) %>%
  summarise(MIN_VALUE=min(VALUE, na.rm=TRUE)) %>%
  left_join(detection_limits, by='VAR') %>%
  mutate(PASS=MIN_VALUE>=UPPERDL)
stopifnot(all(df.por_check$PASS))

# RECENT dataset ----
df.recent <- df.clean

# limit to lower detection limit
df.recent <- df.recent %>%
  mutate(LIMITED = ifelse(is.na(LOWERDL), FALSE, VALUE < LOWERDL),
         VALUE = ifelse(LIMITED, LOWERDL, VALUE))

# start WY2009
df.recent <- filter(df.recent, fluxr::wyear(DATETIME)>=2009)
cat("Summary of variables set to detection limit for RECENT dataset\n")
table(df.recent$VAR, df.recent$LIMITED)
cat("Summary of sites set to detection limit for RECENT dataset\n")
table(df.recent$SITE_NAME, df.recent$LIMITED)

# check all values >= 1/2 lower DL
df.recent_check <- filter(df.recent, VAR %in% detection_limits$VAR) %>%
  group_by(VAR) %>%
  summarise(MIN_VALUE=min(VALUE, na.rm=TRUE)) %>%
  left_join(detection_limits, by='VAR') %>%
  mutate(PASS=MIN_VALUE>=LOWERDL)
stopifnot(all(df.recent_check$PASS))

# rename objects
wq.kt_sprague.orig <- df.orig
wq.kt_sprague <- list(RAW=df.raw,
                      CLEAN=df.clean,
                      POR=df.por,
                      RECENT=df.recent)
stn.kt_sprague <- stn

# save to csv
write.csv(stn.kt_sprague, file=file.path('csv', 'stn_kt_sprague.csv'), row.names=FALSE)

# save to rdata
save(wq.kt_sprague.orig, wq.kt_sprague, stn.kt_sprague, file='kt_sprague.Rdata')
