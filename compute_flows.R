library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(scales)
library(fluxr)
theme_set(theme_bw())

source("functions.R")
load('gis.Rdata')

log_x <- scale_x_log10(breaks=log_breaks(seq(1, 9), 10^seq(-3, 3)),
                       labels=log_labels(c(1, 5), 10^seq(-3, 3)))
log_y <- scale_y_log10(breaks=log_breaks(seq(1, 9), 10^seq(-3, 3)),
                       labels=log_labels(c(1, 5), 10^seq(-3, 3)))

start_date <- as.Date('2000-10-01')
end_date <- as.Date('2014-09-30')
start_date_ivory <- as.Date('2008-10-01')
end_date_ivory <- as.Date('2014-09-30')

# load usgs ----
load('usgs.Rdata')
stn.usgs <- mutate(stn.usgs, 
                   SOURCE="USGS",
                   SITE=as.character(STATION_ID),
                   SITE_NAME=as.character(SITE_NAME)) %>%
  select(SOURCE, SITE_NAME, SITE, DESCRIPTION, LAT, LON)
q.usgs <- select(q.usgs, SOURCE, SITE_NAME, SITE=STATION_ID, DATE, FLOW, FLAG)
stn.usgs <- filter(stn.usgs, SITE_NAME %in% unique(q.usgs$SITE_NAME))

# load owrd ----
load('owrd.Rdata')
stn.owrd <- mutate(stn.owrd,
                   SOURCE="OWRD",
                   SITE=STATION_ID) %>%
  select(SOURCE, SITE_NAME, SITE, DESCRIPTION, LAT, LON)
q.owrd <- rename(q.owrd, SITE=STATION_ID) %>%
  select(SOURCE, SITE_NAME, SITE, DATE, FLOW)

# load kt ----
load('kt_sprague.Rdata')
# use RAW dataset
stn_order <- levels(stn.kt_sprague$SITE_NAME)
q.kt_sprague <- filter(wq.kt_sprague[['RAW']], VAR=='FLOW') %>%
  select(DATE, SITE, SITE_NAME, FLOW=VALUE) %>%
  mutate(SOURCE="KT") %>% 
  select(SOURCE, SITE_NAME, SITE, DATE, FLOW)
stn.area <- select(stn.kt_sprague, SITE_NAME, DRAINAGE_AREA_SQKM)
stn.kt_sprague <- stn.kt_sprague %>%
  mutate(SOURCE="KT", SITE=as.character(SITE), SITE_NAME=as.character(SITE_NAME)) %>%
  select(SITE_NAME, SITE, DESCRIPTION, LAT, LON, SOURCE) %>%
  arrange(SITE_NAME)

# stations ----
stn <- rbind(stn.usgs, stn.kt_sprague, stn.owrd)
stn.ref <- filter(stn, (SOURCE=="USGS" & SITE_NAME=="Power") | (SOURCE=="OWRD" & SITE_NAME=="Sycan") | (SOURCE=="OWRD" & SITE_NAME=="Beatty")) %>%
  mutate(REF_LABEL=paste(SOURCE, SITE, sep='-'))
stn.kt_sprague <- stn.kt_sprague %>%
  left_join(data.frame(SITE_NAME=stn.kt_sprague$SITE_NAME, 
                       REF_LABEL=plyr::revalue(stn.kt_sprague$SITE_NAME, 
                                               c('Power'='USGS-11501000',
                                                 'Lone_Pine'='USGS-11501000',
                                                 'Godowa'='OWRD-11497500',
                                                 'Sycan'='OWRD-11499100',
                                                 'SF'='OWRD-11497500',
                                                 'NF'='OWRD-11497500',
                                                 'SF_Ivory'='OWRD-11497500',
                                                 'NF_Ivory'='OWRD-11497500'))),
            by='SITE_NAME')

# dataset por ----
por.usgs <- group_by(q.usgs, SITE_NAME) %>%
  summarise(START=min(DATE),
            END=max(DATE))
por.owrd <- group_by(q.owrd, SITE_NAME) %>%
  summarise(START=min(DATE),
            END=max(DATE))
por.kt <- group_by(q.kt_sprague, SITE_NAME) %>%
  summarise(START=min(DATE),
            END=max(DATE))

# power dataset ----
q.kt.power <- select(q.kt_sprague, -SITE, -SOURCE) %>%
  filter(SITE_NAME %in% c("Power", "Lone_Pine")) %>%
  spread(SITE_NAME, FLOW)
q.power <- filter(q.usgs, as.Date(DATE)>=start_date, as.Date(DATE)<=end_date, SITE_NAME=="Power") %>% 
  mutate(REF_SITE_NAME="Power",
         REF_SOURCE="USGS") %>%
  select(DATE, REF_FLOW=FLOW, REF_SITE_NAME, REF_SOURCE) %>%
  left_join(q.kt.power, by='DATE') %>%
  gather(SITE_NAME, FLOW, Power:Lone_Pine) %>%
  mutate(DATE=with_tz(DATE, tzone="UTC"),
         RATIO=FLOW/REF_FLOW)

# ggplot(q.power, aes(DATE)) +
#   geom_line(aes(y=REF_FLOW)) +
#   geom_point(aes(y=FLOW), color='red') +
#   facet_wrap(~SITE_NAME) +
#   scale_y_log10()

# filter(q.power, wyear(DATE)==2006) %>%
#   ggplot(aes(DATE)) +
#   geom_line(aes(y=REF_FLOW)) +
#   geom_point(aes(y=FLOW), color='red') +
#   facet_wrap(~SITE_NAME) +
#   scale_y_log10()

# sycan dataset ----
q.kt.sycan <- select(q.kt_sprague, -SITE, -SOURCE) %>% 
  filter(SITE_NAME=="Sycan") %>%
  spread(SITE_NAME, FLOW)
q.sycan <- filter(q.owrd, as.Date(DATE)>=start_date, as.Date(DATE)<=end_date, SITE_NAME=="Sycan") %>% 
  mutate(REF_SITE_NAME="Sycan",
         REF_SOURCE="OWRD") %>%
  select(REF_SITE_NAME, REF_SOURCE, DATE, REF_FLOW=FLOW) %>%
  left_join(q.kt.sycan, by='DATE') %>%
  gather(SITE_NAME, FLOW, Sycan) %>%
  mutate(DATE=with_tz(DATE, tzone="UTC"),
         RATIO=FLOW/REF_FLOW)
 
# ggplot(q.sycan, aes(DATE)) +
#   geom_line(aes(y=REF_FLOW)) +
#   geom_point(aes(y=FLOW), color='red') +
#   facet_wrap(~SITE_NAME) +
#   scale_y_log10()
# 
# beatty dataset ----
q.kt.beatty <- select(q.kt_sprague, -SITE, -SOURCE) %>%
  filter(SITE_NAME %in% c("Godowa", "SF_Ivory",
                          "SF", "NF_Ivory", "NF")) %>%
  spread(SITE_NAME, FLOW)
q.beatty <- filter(q.owrd, as.Date(DATE)>=start_date, as.Date(DATE)<=end_date, SITE_NAME=="Beatty") %>% 
  mutate(REF_SITE_NAME="Beatty",
         REF_SOURCE="OWRD") %>%
  select(REF_SITE_NAME, REF_SOURCE, DATE, REF_FLOW=FLOW) %>%
  left_join(q.kt.beatty, by='DATE') %>%
  gather(SITE_NAME, FLOW, Godowa:NF) %>%
  mutate(DATE=with_tz(DATE, tzone="UTC"),
         RATIO=FLOW/REF_FLOW)

# drop reference flows before 10/2008 for Ivory sites
q.beatty <- filter(q.beatty, SITE_NAME %in% c("Godowa", "SF", "NF") | as.Date(DATE) >= start_date_ivory)

# ggplot(q.beatty, aes(DATE)) +
#   geom_line(aes(y=REF_FLOW)) +
#   geom_point(aes(y=FLOW), color='red') +
#   facet_wrap(~SITE_NAME) +
#   scale_y_log10()

# combine reference datasets
q.ref <- rbind(q.power, q.sycan, q.beatty) %>%
  mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=stn_order))

ggplot(q.ref, aes(REF_FLOW, FLOW)) +
  geom_point(size=1) +
  geom_smooth(method='loess') +
  facet_wrap(~SITE_NAME, scales='free')

ggplot(q.ref, aes(REF_FLOW, FLOW)) +
  geom_point(size=1) +
  geom_smooth(method='loess') +
  log_y +
  log_x +
  facet_wrap(~SITE_NAME, scales='free')

mutate(q.ref, WDAY=water_day(DATE)) %>%
  ggplot(aes(WDAY, RATIO)) +
  geom_point(size=1) +
  facet_wrap(~SITE_NAME, scales='free')

mutate(q.ref, WDAY=water_day(DATE)) %>%
  ggplot(aes(WDAY, RATIO/REF_FLOW)) +
  geom_point(size=1) +
  facet_wrap(~SITE_NAME, scales='free')

mutate(q.ref, WDAY=water_day(DATE)) %>%
  ggplot(aes(REF_FLOW, RATIO)) +
  geom_point(size=1) +
  facet_wrap(~SITE_NAME, scales='free')

# compute ----
ratios <- q.ref %>%
  filter(!is.na(FLOW), !is.na(REF_FLOW)) %>%
  group_by(SITE_NAME, REF_SOURCE, REF_SITE_NAME, MONTH=month(DATE)) %>%
  summarise(N=n(),
            RATIO=mean(FLOW)/mean(REF_FLOW)) %>%
  ungroup

ratios %>%
  select(MONTH, SITE_NAME, RATIO) %>%
  spread(SITE_NAME, RATIO)

mutate(ratios, MONTH=ordered(MONTH, levels=c(10:12, 1:9))) %>%
  ggplot(aes(MONTH, RATIO)) +
  geom_bar(stat='identity') +
  facet_wrap(~SITE_NAME)

# merge flows with ratios
q <- mutate(q.ref, MONTH=month(DATE)) %>%
  left_join(select(ratios, SITE_NAME, MONTH, RATIO_MONTH=RATIO), by=c('SITE_NAME', 'MONTH'))

# compute predicted flows and interpolate residuals
interpolate <- function(x) {
  x$LN_RESID_INTERP <- approx(x$DATE, x$LN_RESID, xout=x$DATE, rule = 2)$y
  return(x)
}

q.model <- mutate(q, 
            PRED=REF_FLOW*RATIO_MONTH,
            LN_RESID=log(FLOW/PRED)) %>%
  arrange(SITE_NAME, DATE) %>%
  group_by(SITE_NAME) %>%
  mutate(CUMISNA=cumsum(!is.na(LN_RESID)),
         LN_RESID=ifelse(CUMISNA==0, 0, LN_RESID)) %>%
  group_by(SITE_NAME) %>%
  do(interpolate(.)) %>%
  mutate(PRED_RESID=PRED*exp(LN_RESID_INTERP)) %>%
  ungroup

q.out <- select(q.model, SITE_NAME, DATE, Q=PRED_RESID)

# explore ----
filter(q.model, CUMISNA>0) %>%
  ggplot(aes(DATE, LN_RESID)) +
  geom_point(size=1) +
  facet_wrap(~SITE_NAME)

filter(q.model, CUMISNA>0) %>%
  ggplot(aes(LN_RESID)) +
  geom_histogram() +
  facet_wrap(~SITE_NAME)

library(manipulate)

plot_flow <- function(site, start_year, end_year, log_trans=FALSE) {
  p <- filter(q.model, SITE_NAME==site) %>%
    filter(wyear(DATE)>=start_year, wyear(DATE)<=end_year) %>%
    ggplot(aes(DATE)) +
    geom_line(aes(y=PRED), size=0.5, color='deepskyblue') +
    geom_line(aes(y=PRED_RESID), size=0.5) +
    geom_point(aes(y=FLOW), color='red', size=2)
  if (log_trans) {
    p <- p + scale_y_log10()
  } else {
    p <- p + ylim(0, NA)
  }
  p
}

manipulate(plot_flow(site, start_year, end_year, log_trans),
           site=do.call(picker, as.list(unique(as.character(q$SITE_NAME)))),
           start_year=slider(2001, 2014),
           end_year=slider(2001, 2014, initial=2014),
           log_trans=checkbox())

# pdf ----
stn.map <- rbind(select(stn.kt_sprague, SITE_NAME, REF_LABEL, LAT, LON) %>% mutate(GROUP='KT'),
                 select(stn.ref, REF_LABEL, LAT, LON) %>% mutate(SITE_NAME=REF_LABEL, GROUP='Reference')) %>%
  arrange(desc(GROUP))

pdf(file.path('pdf', 'flow-model.pdf'), width=11, height=8.5)
ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
  geom_polygon(aes(x = long, y = lat, group = group), data = incbasin_ivory,
               color = 'grey50', fill = NA, size = 0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), data = basin,
               color = 'black', fill = NA, size = 0.2) +
  geom_point(aes(x = LON, y = LAT, fill = REF_LABEL, shape = GROUP, size = GROUP), 
             data = stn.map) +
  geom_text(aes(x = LON, y = LAT, label = REF_LABEL, vjust=ifelse(SITE=='11499100', -1, 1)), 
            data = stn.ref, fontface='bold', hjust=1.1, size=4) +
  geom_text(aes(x = LON-0.02, y = LAT, label = SITE_NAME), 
            data = filter(stn.kt_sprague, SITE_NAME=="Godowa"), size=4, hjust=1) +
  geom_text(aes(x = LON+0.02, y = LAT, label = SITE_NAME), 
            data = filter(stn.kt_sprague, SITE_NAME!="Godowa"), size=4, hjust=0) +
  scale_shape_manual('Station Type', values=c('KT'=21, 'Reference'=24)) +
  scale_size_manual('Station Type', values=c('KT'=4, 'Reference'=5)) +
  scale_fill_manual('Reference Station', values=c('USGS-11501000'='orangered', 
                                                  'OWRD-11497500'='deepskyblue', 
                                                  'OWRD-11499100'='chartreuse3')) +
  guides(fill=guide_legend(override.aes=list(shape=21))) +
  ggtitle('KT and Reference Flow Stations')
makeFootnote('Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.')
  
# ggplot(q.model, aes(DATE)) +
#   geom_line(aes(y=REF_FLOW), color='gray50') +
#   geom_point(aes(y=FLOW), size=1.5, color='orangered') +
#   labs(x='', y='Flow (cfs)', title='KT Biweekly (red) and Daily Reference (gray) Flows') +
#   facet_wrap(~SITE_NAME, scales='free_y', nrow=4)
ggplot(q.model, aes(DATE)) +
  geom_line(aes(y=PRED), color='gray50') +
  geom_point(aes(y=FLOW), size=1.5, color='orangered') +
  labs(x='', y='Flow (cfs)', title='KT Measured Flows (red) and Daily Predicted Flows from Reference Station (gray)') +
  facet_wrap(~SITE_NAME, scales='free_y', nrow=4)
ggplot(q.model, aes(DATE)) +
  geom_line(aes(y=PRED), color='gray50') +
  geom_point(aes(y=FLOW), size=1.5, color='orangered') +
  labs(x='', y='Flow (cfs)', title='KT Measured Flows (red) and Daily Predicted Flows from Reference Station (gray)\nLog Scale') +
  log_y +
  facet_wrap(~SITE_NAME, scales='free_y', nrow=4)
ggplot(q.model, aes(DATE)) +
  geom_line(aes(y=PRED_RESID), color='gray50') +
  geom_point(aes(y=FLOW), size=1.5, color='orangered') +
  labs(x='', y='Flow (cfs)', title='KT Measured Flows (red) and Interpolated Daily Flows from Reference Station (gray)') +
  facet_wrap(~SITE_NAME, scales='free_y', nrow=4)
ggplot(q.model, aes(DATE)) +
  geom_line(aes(y=PRED_RESID), color='gray50') +
  geom_point(aes(y=FLOW), size=1.5, color='orangered') +
  labs(x='', y='Flow (cfs)', title='KT Measured Flows (red) and Interpolated Daily Flows from Reference Station (gray)\nLog Scale') +
  log_y +
  facet_wrap(~SITE_NAME, scales='free_y', nrow=4) +
  theme(panel.grid.minor.y=element_blank())

filter(q.model, !is.na(FLOW)) %>%
  ggplot(aes(FLOW, LN_RESID)) + 
  geom_hline(yint=0) +
  geom_point(size=1) + 
  labs(x='log[ Measured KT Flow (cfs) ]', y='log[ Flow Residual (cfs) ]\nMeasured KT - Scaled Reference', title='Flow Residuals vs KT Measured') +
  facet_wrap(~SITE_NAME, scales='free', nrow=2) +
  log_x +
  theme(panel.grid.minor.x=element_blank(),
        axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) 

filter(q.model, !is.na(FLOW)) %>%
  ggplot(aes(DATE, LN_RESID)) + 
  geom_hline(yint=0) +
  geom_point(size=1) + 
  labs(x='', y='log[ Flow Residual ]', title='Timeseries of Flow Residuals') +
  facet_wrap(~SITE_NAME, scales='free_y', nrow=4)

filter(q.model, !is.na(FLOW)) %>%
  mutate(WDAY=water_day(DATE),
         WDATE=ymd('2001-10-01') + days(WDAY)) %>%
  ggplot(aes(WDATE, LN_RESID)) + 
  geom_hline(yint=0) +
  geom_point(size=1) + 
  labs(x='Water Year Day', y='log[ Flow Residual ]', title='Seasonality of Flow Residuals') +
  scale_x_datetime(labels=scales::date_format('%b %d')) +
  facet_wrap(~SITE_NAME, scales='free_y', nrow=4)

filter(q.model) %>%
  mutate(WDAY=water_day(DATE),
         WDATE=ymd('2001-10-01') + days(WDAY)) %>%
  select(SITE_NAME, WDATE, PRED, FLOW) %>%
  gather(TERM, VALUE, PRED, FLOW) %>%
  ggplot(aes(WDATE, VALUE, color=TERM)) + 
  geom_point(size=1) +
  scale_color_manual('', values=c('PRED'='deepskyblue', 'FLOW'='orangered'),
                     labels=c(PRED='Scaled Reference', FLOW='Measured')) +
  labs(x='Water Year Day', y='Flow (cfs)', title='Seasonality of Estimated and Measured Flows') +
  facet_wrap(~SITE_NAME, scales='free_y', nrow=4) +
  log_y +
  scale_x_datetime(labels=scales::date_format('%b %d')) +
  guides(colour=guide_legend(override.aes = list(size=2))) +
  theme(panel.grid.minor.y=element_blank(),
        legend.position='top')

filter(q.model, !is.na(FLOW)) %>%
  mutate(MONTH=ordered(MONTH, levels=c(seq(10, 12), seq(1, 9)))) %>%
  ggplot(aes(MONTH, RATIO)) +
  geom_hline(yint=1, color='gray50') +
  geom_boxplot(outlier.size=1.5) +
  ylim(0, NA) +
  labs(x="Month", y="Flow Ratio [Measured/Reference]",
       title="Distributions of Flow Ratios used to Interpolate Measured Flows") +
  facet_wrap(~SITE_NAME, nrow=2)
# 
# filter(q.model, !is.na(FLOW)) %>%
#   mutate(WDAY=water_day(DATE)) %>%
#   (function(x) {
#     x2 <- mutate(x, WDAY=WDAY+365)
#     x3 <- mutate(x, WDAY=WDAY+365*2)
#     x <- rbind(x, x2, x3)
#     x <- mutate(x, WDAY-365)
#   }) %>%
#   ggplot(aes(WDAY, RATIO)) +
#   geom_hline(yint=1, color='gray50') +
#   geom_point(size=1) +
#   geom_smooth(method="loess", se=FALSE, span=0.2) +
#   ylim(0, NA) +
#   geom_vline(xint=365) +
#   geom_vline(xint=365*2) +
#   labs(x="Month", y="Flow Ratio [Q_site/Q_ref]",
#        title="Seasonal Patterns in Flow Ratios") +
#   facet_wrap(~SITE_NAME, nrow=2)

ratios %>%
  mutate(MONTH=ordered(MONTH, levels=c(seq(10, 12), seq(1, 9)))) %>%
  ggplot(aes(MONTH, RATIO)) +
  geom_hline(yint=1, color='gray50') +
  geom_bar(stat='identity') +
  facet_wrap(~SITE_NAME, nrow=2) +
  labs(x="Month", y="Mean Flow Ratio [Measured/Reference]",
       title="Mean Monthly Flow Ratio used to Interpolate Measured Flows") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
# 
# ggplot(q.model, aes(DATE, RATIO_MONTH)) + 
#   geom_line() + 
#   geom_hline(yint=0, alpha=0) +
#   labs(x='', y='Monthly Flow Ratio', title='Monthly Ratios of KT Flow to Reference Flow') +
#   facet_wrap(~SITE_NAME, scales='free_y', nrow=4)

dev.off()

# save ----
list(ratios=ratios,
     model=q.model,
     df=q.out) %>%
  saveRDS(file='flows.Rdata')
