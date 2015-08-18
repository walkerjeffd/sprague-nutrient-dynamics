library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(maptools)
library(ggplot2)
theme_set(theme_bw())

DATA_DIR <- getOption('UKL_DATA')

# load data ----
# df <- read.csv(file.path(DATA_DIR, 'sprague', 'kt', 'Sprague River--Water Quality Dataset 2001_2013_imp_20141008.csv'), stringsAsFactors=FALSE)
df <- read.csv(file.path(DATA_DIR, 'sprague', 'kt', 'Sprague River--Water Quality Dataset 2001_2013_imp_synoptic_20141008.csv'), stringsAsFactors=FALSE)

df <- plyr::rename(df, c("INDEX"="INDEX",
                         "DATE"="DATE",
                         "GPS"="LAT",
                         "GPS.1"="LON",
                         "Site.ID"="SITE_DESCRIPTION",
                         "SITE"="SITE",
                         "JD"="JD",
                         "CJD"="CJD",
                         "MO"="MONTH",
                         "DAY"="DAY",
                         "YR"="YEAR",
                         "TIME"="TIME",
                         "Q_CFS"="FLOW_cfs",
                         "STAFF_GAGE_FT"="STAGE_ft",
                         "TEMP_C"="TEMP_degC",
                         "TEMP_CALC"="TEMP_CALC",
                         "TEMP_ACT"="TEMP_ACT",
                         "TEMP_DIFF"="TEMP_DIFF",
                         "COND_MS_CM"="COND_mScm",
                         "COND_US_CM"="COND_uScm",
                         "COND_CALC"="COND_CALC",
                         "COND_ACT"="COND_ACT",
                         "COND_DIFF"="COND_DIFF" ,
                         "DO_MGL"="DO_ppm",
                         "DO_CALC"="DO_CALC",
                         "DO_ACT"="DO_ACT",
                         "DO_DIFF"="DO_DIFF",
                         "PH"="PH_su",
                         "PH_CALC"="PH_CALC" ,
                         "PH_ACT"="PH_ACT",
                         "PH_DIFF"="PH_DIFF",
                         "PSAT_PERC"="PSAT_pct",
                         "PSAT_CALC"="PSAT_CALC",
                         "PSAT_ACT"="PSAT_ACT",
                         "PSAT_DIFF"="PSAT_DIFF" ,
                         "TP_MGL"="TP_ppm",
                         "TP_DUP"="TP_DUP",
                         "TP_DIFF"="TP_DIFF",
                         "TP_RPD"="TP_RPD",
                         "TP_PASS_FAIL"="TP_PASS_FAIL", 
                         "PO4_MGL"="PO4_ppm",
                         "PO4_DUP"="PO4_DUP",
                         "PO4_DIFF"="PO4_DIFF",
                         "PO4_RPD"="PO4_RPD",
                         "PO4_PASS_FAIL"="PO4_PASS_FAIL",
                         "NH4_MGL"="NH4_ppm",
                         "NH4_DUP"="NH4_DUP",
                         "NH4_DIFF"="NH4_DIFF",
                         "NH4_RPD"="NH4_RPD",
                         "NH4_PASS_FAIL"="NH4_PASS_FAIL",
                         "NO3NO2_MGL"="NO23_ppm",
                         "NO3NO2_DUP"="NO3NO2_DUP",
                         "NO3NO2_DIFF"="NO3NO2_DIFF",
                         "NO3NO2_RPD"="NO3NO2_RPD",
                         "NO3NO2_PASS_FAIL"="NO3NO2_PASS_FAIL",
                         "TN_MGL"="TN_ppm",
                         "TN_DUP"="TN_DUP",
                         "TN_DIFF"="TN_DIFF",
                         "TN_RPD"="TN_RPD",
                         "TN_PASS_FAIL"="TN_PASS_FAIL", 
                         "CL_MGL"="CL_ppm",
                         "CL_DUP"="CL_DUP",
                         "CL_DIFF"="CL_DIFF",
                         "CL_RPD"="CL_RPD",
                         "CL_PASS_FAIL"="CL_PASS_FAIL", 
                         "TSS_MGL"="TSS_ppm",
                         "TSS_DUP"="TSS_DUP",
                         "TSS_DIFF"="TSS_DIFF",
                         "TSS_RPD"="TSS_RPD",
                         "TSS_PASS_FAIL"="TSS_PASS_FAIL",
                         "CHLA_UGL"="CHLA_ppb",
                         "CHLA_DUP"="CHLA_DUP",
                         "CHLA_DIFF"="CHLA_DIFF",
                         "CHLA_RPD"="CHLA_RPD",
                         "CHLA_PASS_FAIL"="CHLA_PASS_FAIL",
                         "PHAE_UGL"="PHAE_ppb",
                         "PHAE_DUP"="PHAE_DUP",
                         "PHAE_DIFF"="PHAE_DIFF",
                         "PHAE_RPD"="PHAE_RPD",
                         "PHAE_PASS_FAIL"="PHAE_PASS_FAIL",
                         "BOD5_MGL"="BOD5_ppm",
                         "BOD5_DUP"="BOD5_DUP",
                         "BOD5_DIFF"="BOD5_DIFF",
                         "BOD5_QAQC"="BOD5_QAQC",
                         "BOD5_PASS_FAIL"="BOD5_PASS_FAIL",
                         "NOTES"="NOTES"))

df <- select(df, DATE, TIME, LAT, LON, SITE_DESCRIPTION, SITE,
             FLOW_cfs, STAGE_ft, TEMP_degC, COND_uScm, DO_ppm, PH_su, PSAT_pct,
             TP_ppm, PO4_ppm, NH4_ppm, NO23_ppm, TN_ppm,
             TSS_ppm, NOTES) %>%
  mutate(STAGE_ft=as.double(STAGE_ft)) # 'n/a' converted to NA

# fix times
df[which(df$TIME==17.25), "TIME"] <- 1725

# parse dates and times
df <- mutate(df, DATE=mdy(DATE))
df <- mutate(df, HOUR=floor(TIME/100),
             MINUTE=TIME-HOUR*100,
             TIME=ifelse(is.na(HOUR), '12:00', paste0(sprintf('%02d', HOUR), ':', sprintf('%02d', MINUTE))),
             DATETIME=paste0(format(DATE, '%Y-%m-%d'), ' ', TIME) %>% ymd_hm())

# remove rows without date
df <- filter(df, !is.na(DATETIME))

# stations ----
stn.raw <- select(df, SITE, SITE_DESCRIPTION, LAT, LON) %>% unique %>% arrange(SITE)
stn.raw <- mutate(stn.raw, 
                  LAT_DMS=as.character(gsub("[^0-9]+", "", as.character(LAT))), 
                  LON_DMS=format(as.character(gsub("[^0-9]+", "", as.character(LON))), digits=8),
                  LAT_D=substr(LAT_DMS, 1, 2),
                  LAT_M=substr(LAT_DMS, 3, 4),
                  LAT_S=substr(LAT_DMS, 5, 8),
                  LON_D=substr(LON_DMS, 1, 3),
                  LON_M=substr(LON_DMS, 4, 5),
                  LON_S=substr(LON_DMS, 6, 9),
                  LAT_DD=as.numeric(LAT_D) + (as.numeric(LAT_M) + as.numeric(LAT_S)/10/60)/60,
                  LON_DD=-(as.numeric(LON_D) + (as.numeric(LON_M) + as.numeric(LON_S)/10/60)/60))
stn.kt_synoptic <- select(stn.raw, SITE, SITE_DESCRIPTION, LATITUDE=LAT_DD, LONGITUDE=LON_DD)
write.csv(stn.kt_synoptic, file='csv/stn_kt_synoptic_raw.csv', row.names=FALSE)

# reshape data ----
df <- gather(df, VAR.UNITS, VALUE, FLOW_cfs:TSS_ppm) %>%
  separate(VAR.UNITS, c('VAR', 'UNITS'), sep='[_]') %>%
  select(DATE, DATETIME, SITE, SITE_DESCRIPTION, VAR, UNITS, VALUE, NOTES) %>%
  mutate(VAR=factor(VAR),
         UNITS=factor(UNITS)) %>%
  filter(!is.na(VALUE))

# plots ----
ggplot(df, aes(DATETIME, VALUE)) +
  geom_point() +
  facet_grid(VAR~SITE, scales='free_y')

filter(df, SITE=='SR0200') %>%
  ggplot(aes(DATETIME, VALUE)) +
  geom_point() +
  facet_wrap(~VAR, scales='free_y')

# save ----
wq.kt_synoptic <- df

save(wq.kt_synoptic, stn.kt_synoptic, file="kt_synoptic.Rdata")
