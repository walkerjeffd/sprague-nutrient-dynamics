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
snotel_dir <- file.path(DATA_DIR, 'sprague', 'snotel')
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
                 WDAY_LABEL=as.Date("2001-10-01")+days(WDAY))

snotel <- snotel %>%
  filter(WYEAR <= 2014)

# stations ----
stn.snotel <- read.csv(file.path(snotel_dir, 'snotel_stations.csv'), stringsAsFactors=FALSE)
stn.snotel <- filter(stn.snotel, SITE_NAME %in% unique(snotel$SITE_NAME))

# aggregate by wyr ----
snotel.wyr <- snotel %>%
  dplyr::group_by(SITE_NAME, WYEAR) %>%
  dplyr::summarise(N=sum(!is.na(SWE_cm)),
            SWE_MEAN_cm=mean(SWE_cm, na.rm=TRUE),
            SWE_MAX_cm=max(SWE_cm, na.rm=TRUE)) %>%
  ungroup %>%
  filter(N>=300)

snotel.wyr <- snotel %>%
  filter(month(DATE)==4, day(DATE)==1) %>%
  select(SITE_NAME, WYEAR, SWE_APR_cm=SWE_cm) %>%
  left_join(snotel.wyr, by=c('SITE_NAME', 'WYEAR')) %>%
  select(SITE_NAME, WYEAR, N, SWE_APR_cm, SWE_MEAN_cm, SWE_MAX_cm)

filename <- "report/snotel-daily-ts.png"
cat('\nSaving daily timeseries plot to:', filename, '\n')
png(filename=filename, res=200, width=10, height=6, units = 'in')
p <- snotel %>%
  mutate(DATE = ymd(DATE)) %>%
  ggplot(aes(DATE, SWE_cm)) +
  geom_line() +
  labs(x="Date", y="Snow Water Equivalent (cm)") +
  facet_wrap(~SITE_NAME, ncol=1, scales='free_y') +
  scale_x_date( date_breaks ="5 years",
                date_labels = "%Y",
                limits=c(as.Date("1981-12-31"), as.Date("2014-09-30")))+
  theme(strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()

filename <- "report/snotel-daily-water-day.png"
cat('\nSaving water day plot plot to:', filename, '\n')
png(filename=filename, res=200, width=10, height=4, units = 'in')
p <- snotel %>%
  filter( WYEAR >= 2002) %>%
  mutate(WYEAR_GRP=ifelse(WYEAR>=2013, paste0('WY', WYEAR), "WY2002-2012")) %>%
  filter(WDAY<=280) %>%
  mutate(WDAY_LABEL = ymd(WDAY_LABEL)) %>%
  ggplot(aes(WDAY_LABEL, SWE_cm, #size=WYEAR_GRP,
             linetype=WYEAR_GRP, color=WYEAR_GRP, group=WYEAR)) +
  geom_line(aes(x = WDAY_LABEL, y = SWE_cm)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  facet_wrap(~SITE_NAME, scales='free', nrow=1) +
  scale_color_manual('', values=c('WY2002-2012'='grey50',
                                  'WY2013'='deepskyblue',
                                  'WY2014'='orangered')) +
  scale_size_manual('', values=c('WY2002-2012'=0.25,
                                  'WY2013'=0.5,
                                  'WY2014'=0.5)) +
  scale_linetype_manual('', values=c('WY2002-2012'='solid',
                                     'WY2013'='dashed',
                                     'WY2014'='dashed')) +
  labs(x="Water Year Date", y="Snow Water Equivalent (in)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
        strip.background=element_blank(),
        strip.text=element_text(face="bold"))
print(p)
dev.off()



# save ----
filename <- 'csv/stn_snotel.csv'
cat('\nSaving SNOTEL dataset to:', filename, '\n')
write.csv(stn.snotel, file=filename, row.names=FALSE)

filename <- 'snotel.Rdata'
cat('Saving SNOTEL dataset to:', filename, '\n')
save(snotel, snotel.wyr, stn.snotel, file=filename)

cat('\n\n')
