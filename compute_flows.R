library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(scales)
library(fluxr)
theme_set(theme_bw())

rm(list=ls())

cat(paste0(rep('=', 80), collapse=''), '\n')
cat("Running flow model...\n\n")

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

ref_sites <- c('Power'='USGS-11501000',
               'Lone_Pine'='USGS-11501000',
               'Godowa'='OWRD-11497500',
               'Sycan'='OWRD-11499100',
               'SF'='OWRD-11497500',
               'NF'='OWRD-11497500',
               'SF_Ivory'='OWRD-11497500',
               'NF_Ivory'='OWRD-11497500')

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
                                               ref_sites),
                       stringsAsFactors=FALSE),
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
q.ref.power <- filter(q.usgs, as.Date(DATE)>=start_date, as.Date(DATE)<=end_date, SITE_NAME=="Power")
q.power <- mutate(q.ref.power, REF_SITE_NAME="Power",
                  REF_SOURCE="USGS") %>%
  select(DATE, REF_FLOW=FLOW, REF_SITE_NAME, REF_SOURCE) %>%
  left_join(q.kt.power, by='DATE') %>%
  gather(SITE_NAME, FLOW, Power:Lone_Pine) %>%
  mutate(DATE=with_tz(DATE, tzone="UTC"),
         RATIO=FLOW/REF_FLOW)

# sycan dataset ----
q.kt.sycan <- select(q.kt_sprague, -SITE, -SOURCE) %>%
  filter(SITE_NAME=="Sycan") %>%
  spread(SITE_NAME, FLOW)
q.ref.sycan <- filter(q.owrd, as.Date(DATE)>=start_date, as.Date(DATE)<=end_date, SITE_NAME=="Sycan")
q.sycan <- mutate(q.ref.sycan,
                  REF_SITE_NAME="Sycan",
                  REF_SOURCE="OWRD") %>%
  select(REF_SITE_NAME, REF_SOURCE, DATE, REF_FLOW=FLOW) %>%
  left_join(q.kt.sycan, by='DATE') %>%
  gather(SITE_NAME, FLOW, Sycan) %>%
  mutate(DATE=with_tz(DATE, tzone="UTC"),
         RATIO=FLOW/REF_FLOW)

# beatty dataset ----
q.kt.beatty <- select(q.kt_sprague, -SITE, -SOURCE) %>%
  filter(SITE_NAME %in% c("Godowa", "SF_Ivory",
                          "SF", "NF_Ivory", "NF")) %>%
  spread(SITE_NAME, FLOW)
q.ref.beatty <- filter(q.owrd, as.Date(DATE)>=start_date, as.Date(DATE)<=end_date, SITE_NAME=="Beatty")
q.beatty <- mutate(q.ref.beatty,
                   REF_SITE_NAME="Beatty",
                   REF_SOURCE="OWRD") %>%
  select(REF_SITE_NAME, REF_SOURCE, DATE, REF_FLOW=FLOW) %>%
  left_join(q.kt.beatty, by='DATE') %>%
  gather(SITE_NAME, FLOW, Godowa:NF) %>%
  mutate(DATE=with_tz(DATE, tzone="UTC"),
         RATIO=FLOW/REF_FLOW)

# drop reference flows before 10/2008 for Ivory sites
q.beatty <- filter(q.beatty, SITE_NAME %in% c("Godowa", "SF", "NF") | as.Date(DATE) >= start_date_ivory)

# combine reference datasets
q.ref <- rbind(select(q.ref.power, -FLAG), q.ref.sycan, q.ref.beatty)
q <- rbind(q.power, q.sycan, q.beatty) %>%
  mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=stn_order))

# compute ----
ratios <- q %>%
  filter(!is.na(FLOW), !is.na(REF_FLOW)) %>%
  group_by(SITE_NAME, REF_SOURCE, REF_SITE_NAME, MONTH=month(DATE)) %>%
  summarise(N=n(),
            RATIO=mean(FLOW)/mean(REF_FLOW)) %>%
  ungroup

# merge flows with ratios
q <- mutate(q, MONTH=month(DATE)) %>%
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

# validation ----
q.valid <- filter(q.owrd, SITE_NAME %in% c('SF', 'NF', 'Godowa', 'Lone_Pine')) %>%
  rename(VALID_Q=FLOW) %>%
  mutate(SITE=paste0('OWRD:', SITE)) %>%
  select(-SOURCE)
q.valid <- left_join(q.valid, select(q.out, SITE_NAME, DATE, Q),
                     by=c('SITE_NAME', 'DATE')) %>%
  filter(!is.na(Q)) %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=c('Lone_Pine', 'Godowa', 'SF', 'NF'))) %>%
  arrange(SITE_NAME, DATE)
q.valid.mon <- q.valid %>%
  mutate(DATE=floor_date(DATE, unit = "month")) %>%
  group_by(SITE_NAME, SITE, DATE) %>%
  summarise(VALID_Q=mean(VALID_Q, na.rm=TRUE),
            Q=mean(Q, na.rm=TRUE)) %>%
  ungroup

q.valid.rmse <- group_by(q.valid, SITE_NAME, SITE) %>%
  summarise(START_DATE=min(DATE),
            END_DATE=max(DATE),
            N=n(),
            X=min(VALID_Q),
            Y=max(Q),
            R2=cor(Q, VALID_Q, use="complete.obs")^2,
            RMSE=sqrt(sum((VALID_Q-Q)^2)/N),
            VALID_Q=mean(VALID_Q),
            Q=mean(Q),
            REL_RMSE=RMSE/Q)
q.valid.mon.rmse <- group_by(q.valid.mon, SITE_NAME, SITE) %>%
  summarise(START_DATE=min(DATE),
            END_DATE=max(DATE),
            N=n(),
            X=min(VALID_Q),
            Y=max(Q),
            R2=cor(Q, VALID_Q, use="complete.obs")^2,
            RMSE=sqrt(sum((VALID_Q-Q)^2)/N),
            VALID_Q=mean(VALID_Q),
            Q=mean(Q),
            REL_RMSE=RMSE/Q)

# explore ----

ratios %>%
  select(MONTH, SITE_NAME, RATIO) %>%
  spread(SITE_NAME, RATIO)

ggplot(q, aes(REF_FLOW, FLOW)) +
  geom_point(size=1) +
  geom_smooth(method='loess') +
  facet_wrap(~SITE_NAME, scales='free')

mutate(q, WDAY=water_day(DATE)) %>%
  filter(!is.na(RATIO)) %>%
  ggplot(aes(WDAY, RATIO)) +
  geom_point(size=1) +
  facet_wrap(~SITE_NAME, scales='free')

mutate(q, WDAY=water_day(DATE)) %>%
  filter(!is.na(RATIO)) %>%
  ggplot(aes(WDAY, RATIO/REF_FLOW)) +
  geom_point(size=1) +
  facet_wrap(~SITE_NAME, scales='free')

mutate(q, WDAY=water_day(DATE)) %>%
  filter(!is.na(RATIO)) %>%
  ggplot(aes(REF_FLOW, RATIO)) +
  geom_point(size=1) +
  facet_wrap(~SITE_NAME, scales='free')

mutate(ratios, MONTH=ordered(MONTH, levels=c(10:12, 1:9))) %>%
  ggplot(aes(MONTH, RATIO)) +
  geom_bar(stat='identity') +
  facet_wrap(~SITE_NAME)

filter(q.model, CUMISNA>0, !is.na(LN_RESID)) %>%
  ggplot(aes(DATE, LN_RESID)) +
  geom_point(size=1) +
  facet_wrap(~SITE_NAME)

filter(q.model, CUMISNA>0, !is.na(LN_RESID)) %>%
  ggplot(aes(LN_RESID)) +
  geom_histogram(binwidth=0.1) +
  facet_wrap(~SITE_NAME)

# library(manipulate)
#
# plot_flow <- function(site, start_year, end_year, log_trans=FALSE) {
#   p <- filter(q.model, SITE_NAME==site) %>%
#     filter(wyear(DATE)>=start_year, wyear(DATE)<=end_year) %>%
#     ggplot(aes(DATE)) +
#     geom_line(aes(y=PRED), size=0.5, color='deepskyblue') +
#     geom_line(aes(y=PRED_RESID), size=0.5) +
#     geom_point(aes(y=FLOW), color='red', size=2)
#   if (log_trans) {
#     p <- p + scale_y_log10()
#   } else {
#     p <- p + ylim(0, NA)
#   }
#   p
# }
#
# manipulate(plot_flow(site, start_year, end_year, log_trans),
#            site=do.call(picker, as.list(unique(as.character(q$SITE_NAME)))),
#            start_year=slider(2001, 2014),
#            end_year=slider(2001, 2014, initial=2014),
#            log_trans=checkbox())

# pdf ----
stn.map <- rbind(select(stn.kt_sprague, SITE_NAME, REF_LABEL, LAT, LON) %>% mutate(GROUP='KT'),
                 select(stn.ref, REF_LABEL, LAT, LON) %>% mutate(SITE_NAME=REF_LABEL, GROUP='Reference')) %>%
  arrange(desc(GROUP))

# flow data ----
filename <- file.path('pdf', 'flow-data.pdf')
cat('Printing:', filename, '\n')
pdf(filename, width=11, height=8.5)
p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = filter(incbasin, INC_SITE_NAME != "Godowa-SF-NF"),
               color = 'grey50', fill = NA, size = 0.2) +
  geom_path(aes(x = long, y = lat, group = group), data = flowline,
            color='deepskyblue', size=0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), data = basin,
               color = 'black', fill = NA, size = 0.2) +
  geom_point(aes(x = LON, y = LAT),
             data = stn.ref, shape = 17, size = 3, color='red') +
  geom_text(aes(x = LON, y = LAT, label = REF_LABEL, vjust=ifelse(SITE=='11499100', -1, 1)),
            data = mutate(stn.ref, REF_LABEL=paste0(REF_LABEL, "-", SITE_NAME)), fontface='bold', hjust=-0.1, size=4)
print(p)
makeFootnote('Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.   ')

p <- q.ref %>%
  mutate(DATE=as.Date(DATE)) %>%
  ggplot(aes(DATE, FLOW)) +
  geom_line() +
  labs(x="", y="Flow (cfs)\n ") +
  scale_x_date(breaks=scales::date_breaks(width = "1 year"), labels=scales::date_format('%Y')) +
  facet_wrap(~SOURCE+SITE+SITE_NAME, scales='free_y', ncol=1)
print(p)

dev.off()

# flow model ----
if (!file.exists(file.path('pdf', 'flow-model'))) {
  dir.create(file.path('pdf', 'flow-model'))
}

site <- 'Power'
pdf(file.path('pdf', 'flow-model', 'flow-model-summary.pdf'), width=11, height=8.5)
for (site in levels(q$SITE_NAME)) {
  cat('..', site, '\n')
  p.ts <- filter(q, SITE_NAME==site) %>%
    ggplot(aes(DATE, REF_FLOW)) +
    geom_line(aes(color='Reference'), size=0.25, show_guide = TRUE) +
    geom_line(aes(x=DATE, y=Q, color='Interpolated'),
              data=filter(q.out, SITE_NAME==site),
              size=0.1,
              show_guide = TRUE) +
    geom_point(aes(y=FLOW, color='Biweekly'), size=1.5,
               show_guide = TRUE) +
    log_y +
    scale_color_manual('',
                       values=c('Reference'='grey50',
                                'Biweekly'='red',
                                'Interpolated'='red')) +
    labs(x="Date", y="Flow (cfs)") +
    guides(colour=guide_legend(override.aes = list(linetype=c('blank', 'solid', 'solid'),
                                                   shape=c(16, NA, NA)))) +
    theme(legend.position='bottom')

  p.scatter <- filter(q, SITE_NAME==site) %>%
    ggplot(aes(REF_FLOW, FLOW)) +
    geom_point(size=1) +
    log_y +
    log_x +
    geom_abline(aes(color="1:1 Line"), linetype=2, show_guide=TRUE) +
    scale_color_manual('', values=c('red')) +
    labs(x="Flow @ Reference Site (cfs)", y=paste0("Flow @ ", site, " (cfs)")) +
    theme(legend.position='bottom')

  p.resid <- filter(q.model, SITE_NAME==site) %>%
    filter(!is.na(FLOW)) %>%
    ggplot(aes(DATE, LN_RESID)) +
    geom_hline(yint=0) +
    geom_point(size=1) +
    labs(x="Date", y="Log Flow Residual\nln[Measured/Scaled Reference]")

  p.resid.flow <- filter(q.model, SITE_NAME==site) %>%
    filter(!is.na(FLOW)) %>%
    ggplot(aes(FLOW, LN_RESID)) +
    geom_hline(yint=0) +
    geom_point(size=1) +
    labs(x="Measured Biweekly Flow (cfs)",
         y="Log Flow Residual\nln[Measured/Scaled Reference]") +
    theme(strip.background=element_blank(),
          strip.text=element_text(face='bold', size=12))

  p.ratio <- filter(q.model, !is.na(FLOW), SITE_NAME==site) %>%
    mutate(MONTH=ordered(MONTH, levels=c(seq(10, 12), seq(1, 9)))) %>%
    ggplot(aes(MONTH, RATIO)) +
    geom_hline(yint=1, color='gray50') +
    geom_boxplot(outlier.size=1.5) +
    geom_point(aes(MONTH, RATIO, color="Mean"),
               data=filter(ratios, SITE_NAME==site) %>%
                 mutate(MONTH=ordered(MONTH, levels=c(seq(10, 12), seq(1, 9)))),
               show_guide=TRUE) +
    scale_color_manual('', values='orangered') +
    labs(x="Month", y="Flow Ratio [Measured/Reference]") +
    theme(legend.position=c(0, 1),
          legend.justification=c(0, 1),
          legend.background=element_blank())

  grid.arrange(grobs=list(p.ts, p.scatter, p.ratio, p.resid, p.resid.flow),
               layout_matrix=rbind(c(1, 1, 2), c(3, 4, 5)),
               top=paste0('\nStreamflow Model Diagnostics\nSite: ', site,
                          '  |  Reference Site: ', ref_sites[[site]]))
}
dev.off()

# filename <- file.path('pdf', 'flow-model.pdf')
# cat('Printing:', filename, '\n')
# pdf(filename, width=11, height=8.5)
# p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
#   geom_polygon(aes(x = long, y = lat, group = group),
#                data = filter(incbasin, INC_SITE_NAME != "Godowa-SF-NF"),
#                color = 'grey50', fill = NA, size = 0.2) +
#   geom_polygon(aes(x = long, y = lat, group = group), data = basin,
#                color = 'black', fill = NA, size = 0.2) +
#   geom_point(aes(x = LON, y = LAT, fill = REF_LABEL, shape = GROUP, size = GROUP),
#              data = stn.map) +
#   geom_text(aes(x = LON, y = LAT, label = REF_LABEL, vjust=ifelse(SITE=='11499100', -1, 1)),
#             data = stn.ref, fontface='bold', hjust=1.1, size=4) +
#   geom_text(aes(x = LON-0.02, y = LAT, label = SITE_NAME),
#             data = filter(stn.kt_sprague, SITE_NAME=="Godowa"), size=4, hjust=1) +
#   geom_text(aes(x = LON+0.02, y = LAT, label = SITE_NAME),
#             data = filter(stn.kt_sprague, SITE_NAME!="Godowa"), size=4, hjust=0) +
#   scale_shape_manual('Station Type', values=c('KT'=21, 'Reference'=24)) +
#   scale_size_manual('Station Type', values=c('KT'=4, 'Reference'=5)) +
#   scale_fill_manual('Reference Station', values=c('USGS-11501000'='orangered',
#                                                   'OWRD-11497500'='deepskyblue',
#                                                   'OWRD-11499100'='chartreuse3')) +
#   guides(fill=guide_legend(override.aes=list(shape=21))) +
#   ggtitle('KT and Reference Flow Stations')
# print(p)
# makeFootnote('Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.')
#
# p <- ggplot(q.model, aes(DATE)) +
#   geom_line(aes(y=PRED), color='gray50') +
#   geom_point(aes(y=FLOW), size=1.5, color='orangered') +
#   labs(x='', y='Flow (cfs)', title='KT Measured Flows (red) and Daily Predicted Flows from Reference Station (gray)') +
#   facet_wrap(~SITE_NAME, scales='free_y', nrow=4)
# print(p)
#
# p <- ggplot(q.model, aes(DATE)) +
#   geom_line(aes(y=PRED), color='gray50') +
#   geom_point(aes(y=FLOW), size=1.5, color='orangered') +
#   labs(x='', y='Flow (cfs)', title='KT Measured Flows (red) and Daily Predicted Flows from Reference Station (gray)\nLog Scale') +
#   log_y +
#   facet_wrap(~SITE_NAME, scales='free_y', nrow=4)
# print(p)
#
# p <- ggplot(q.model, aes(DATE)) +
#   geom_line(aes(y=PRED_RESID), color='gray50') +
#   geom_point(aes(y=FLOW), size=1.5, color='orangered') +
#   labs(x='', y='Flow (cfs)', title='KT Measured Flows (red) and Interpolated Daily Flows from Reference Station (gray)') +
#   facet_wrap(~SITE_NAME, scales='free_y', nrow=4)
# print(p)
#
# p <- ggplot(q.model, aes(DATE)) +
#   geom_line(aes(y=PRED_RESID), color='gray50') +
#   geom_point(aes(y=FLOW), size=1.5, color='orangered') +
#   labs(x='', y='Flow (cfs)', title='KT Measured Flows (red) and Interpolated Daily Flows from Reference Station (gray)\nLog Scale') +
#   log_y +
#   facet_wrap(~SITE_NAME, scales='free_y', nrow=4) +
#   theme(panel.grid.minor.y=element_blank())
# print(p)
#
# p <- filter(q.model, !is.na(FLOW)) %>%
#   ggplot(aes(FLOW, LN_RESID)) +
#   geom_hline(yint=0) +
#   geom_point(size=1) +
#   labs(x='log[ Measured KT Flow (cfs) ]', y='log[ Flow Residual (cfs) ]\nMeasured KT - Scaled Reference', title='Flow Residuals vs KT Measured') +
#   facet_wrap(~SITE_NAME, scales='free', nrow=2) +
#   log_x +
#   theme(panel.grid.minor.x=element_blank(),
#         axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
# print(p)
#
# p <- filter(q.model, !is.na(FLOW)) %>%
#   ggplot(aes(DATE, LN_RESID)) +
#   geom_hline(yint=0) +
#   geom_point(size=1) +
#   labs(x='', y='log[ Flow Residual ]', title='Timeseries of Flow Residuals') +
#   facet_wrap(~SITE_NAME, scales='free_y', nrow=4)
# print(p)
#
# p <- filter(q.model, !is.na(FLOW)) %>%
#   mutate(WDAY=water_day(DATE),
#          WDATE=ymd('2001-10-01') + days(WDAY)) %>%
#   ggplot(aes(WDATE, LN_RESID)) +
#   geom_hline(yint=0) +
#   geom_point(size=1) +
#   labs(x='Water Year Day', y='log[ Flow Residual ]', title='Seasonality of Flow Residuals') +
#   scale_x_datetime(labels=scales::date_format('%b %d')) +
#   facet_wrap(~SITE_NAME, scales='free_y', nrow=4)
# print(p)
#
# p <- filter(q.model) %>%
#   mutate(WDAY=water_day(DATE),
#          WDATE=ymd('2001-10-01') + days(WDAY)) %>%
#   select(SITE_NAME, WDATE, PRED, FLOW) %>%
#   gather(TERM, VALUE, PRED, FLOW) %>%
#   ggplot(aes(WDATE, VALUE, color=TERM)) +
#   geom_point(size=1) +
#   scale_color_manual('', values=c('PRED'='deepskyblue', 'FLOW'='orangered'),
#                      labels=c(PRED='Scaled Reference', FLOW='Measured')) +
#   labs(x='Water Year Day', y='Flow (cfs)', title='Seasonality of Estimated and Measured Flows') +
#   facet_wrap(~SITE_NAME, scales='free_y', nrow=4) +
#   log_y +
#   scale_x_datetime(labels=scales::date_format('%b %d')) +
#   guides(colour=guide_legend(override.aes = list(size=2))) +
#   theme(panel.grid.minor.y=element_blank(),
#         legend.position='top')
# print(p)
#
# p <- filter(q.model, !is.na(FLOW)) %>%
#   mutate(MONTH=ordered(MONTH, levels=c(seq(10, 12), seq(1, 9)))) %>%
#   ggplot(aes(MONTH, RATIO)) +
#   geom_hline(yint=1, color='gray50') +
#   geom_boxplot(outlier.size=1.5) +
#   ylim(0, NA) +
#   labs(x="Month", y="Flow Ratio [Measured/Reference]",
#        title="Distributions of Flow Ratios used to Interpolate Measured Flows") +
#   facet_wrap(~SITE_NAME, nrow=2)
# print(p)
#
# p <- ratios %>%
#   mutate(MONTH=ordered(MONTH, levels=c(seq(10, 12), seq(1, 9)))) %>%
#   ggplot(aes(MONTH, RATIO)) +
#   geom_hline(yint=1, color='gray50') +
#   geom_bar(stat='identity') +
#   facet_wrap(~SITE_NAME, nrow=2) +
#   labs(x="Month", y="Mean Flow Ratio [Measured/Reference]",
#        title="Mean Monthly Flow Ratio used to Interpolate Measured Flows") +
#   theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
# print(p)
#
# dev.off()


# filename <- file.path('pdf', 'flow-model', 'flow-scatter-ref-vs-site.pdf')
# cat('Printing:', filename, '\n')
# pdf(filename, width=11, height=8.5)
#
# p <- filter(q, !is.na(FLOW)) %>%
#   ggplot(aes(REF_FLOW, FLOW)) +
#   geom_point(size=1) +
#   log_y +
#   log_x +
#   geom_abline(aes(color="1:1 Line"), linetype=2, show_guide=TRUE) +
#   scale_color_manual('', values=c('red')) +
#   facet_wrap(~SITE_NAME, scales='free') +
#   labs(x="Flow @ Reference Site (cfs)", y="Flow @ WQ Station (cfs)",
#        title="Measured Biweekly Flow vs. Reference Flow") +
#   theme(strip.background=element_blank(),
#         strip.text=element_text(face='bold'))
# print(p)
#
# dev.off()
#
# filename <- file.path('pdf', 'flow-model', 'flow-scatter-ref-vs-site.pdf')
# cat('Printing:', filename, '\n')
# pdf(filename, width=11, height=8.5)
#
# p <- filter(q, !is.na(FLOW)) %>%
#   ggplot(aes(REF_FLOW, FLOW)) +
#   geom_point(size=1) +
#   log_y +
#   log_x +
#   geom_abline(aes(color="1:1 Line"), linetype=2, show_guide=TRUE) +
#   scale_color_manual('', values=c('red')) +
#   facet_wrap(~SITE_NAME, scales='free') +
#   labs(x="Flow @ Reference Site (cfs)", y="Flow @ WQ Station (cfs)",
#        title="Measured Biweekly Flow vs. Reference Flow") +
#   theme(strip.background=element_blank(),
#         strip.text=element_text(face='bold'))
# print(p)
#
# dev.off()
#
# filename <- file.path('pdf', 'flow-model', 'flow-timeseries.pdf')
# cat('Printing:', filename, '\n')
# pdf(filename, width=11, height=8.5)
#
# p1 <- filter(q, SITE_NAME %in% c('Power', 'Lone_Pine', 'Godowa', 'Sycan')) %>%
#   ggplot(aes(DATE, REF_FLOW)) +
#   geom_line(aes(color='Reference'), show_guide = TRUE) +
#   geom_point(aes(y=FLOW, color='Biweekly'), size=1.5,
#              show_guide = TRUE) +
#   log_y +
#   scale_color_manual('', values=c('Reference'='grey50', 'Biweekly'='red')) +
#   facet_wrap(~SITE_NAME, scales='free') +
#   labs(x="Date", y="Flow (cfs)",
#        title="Biweekly Measured and Daily Reference Flows") +
#   guides(colour=guide_legend(override.aes = list(linetype=c('blank', 'solid'),
#                                                  shape=c(16, NA)))) +
#   theme(strip.background=element_blank(),
#         strip.text=element_text(face='bold', size=12))
# p2 <- filter(q, !(SITE_NAME %in% c('Power', 'Lone_Pine', 'Godowa', 'Sycan'))) %>%
#   ggplot(aes(DATE, REF_FLOW)) +
#   geom_line(aes(color='Reference'), show_guide = TRUE) +
#   geom_point(aes(y=FLOW, color='Biweekly'), size=1.5,
#              show_guide = TRUE) +
#   log_y +
#   scale_color_manual('', values=c('Reference'='grey50', 'Biweekly'='red')) +
#   facet_wrap(~SITE_NAME, scales='free') +
#   labs(x="Date", y="Flow (cfs)",
#        title="Biweekly Measured and Daily Reference Flows") +
#   guides(colour=guide_legend(override.aes = list(linetype=c('blank', 'solid'),
#                                                  shape=c(16, NA)))) +
#   theme(strip.background=element_blank(),
#         strip.text=element_text(face='bold', size=12))
#
# print(p1)
# print(p2)
#
# dev.off()
#
#
# filename <- file.path('pdf', 'flow-model', 'flow-residual-timeseries.pdf')
# cat('Printing:', filename, '\n')
# pdf(filename, width=11, height=8.5)
# p <- q.model %>%
#   filter(!is.na(FLOW)) %>%
#   ggplot(aes(DATE, LN_RESID)) +
#   geom_hline(yint=0) +
#   geom_point(size=1) +
#   facet_wrap(~SITE_NAME, ncol=2) +
#   labs(x="Date", y="Log Flow Residual\nln[Measured/Scaled Reference]",
#        title="Timeseries of Flow Model Residuals") +
#   theme(strip.background=element_blank(),
#         strip.text=element_text(face='bold', size=12))
# print(p)
# dev.off()
#
# filename <- file.path('pdf', 'flow-model', 'flow-residual-scatter.pdf')
# cat('Printing:', filename, '\n')
# pdf(filename, width=11, height=8.5)
# p <- q.model %>%
#   filter(!is.na(FLOW)) %>%
#   ggplot(aes(FLOW, LN_RESID)) +
#   geom_hline(yint=0) +
#   geom_point(size=1) +
#   facet_wrap(~SITE_NAME, ncol=4, scale='free_x') +
#   labs(x="Measured Biweekly Flow (cfs)",
#        y="Log Flow Residual\nln[Measured/Scaled Reference]",
#        title="Comparison of Flow Model Residuals to Measured Flow") +
#   theme(strip.background=element_blank(),
#         strip.text=element_text(face='bold', size=12))
# print(p)
# dev.off()

# report ----
filename <- 'report/map-flow-station.png'
cat("\nSaving reference station map to:", filename, '\n')
png(filename, width=8, height=5, res=200, units='in')
p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
  geom_path(aes(x = long, y = lat, group = group), data = flowline,
            color='deepskyblue', size=0.2) +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = filter(incbasin, INC_SITE_NAME != "Godowa-SF-NF"),
               color = 'grey50', fill = NA, size = 0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), data = basin,
               color = 'black', fill = NA, size = 0.2) +
  geom_point(aes(x = LON, y = LAT, fill = REF_LABEL, shape = GROUP, size = GROUP),
             data = stn.map) +
  geom_text(aes(x = LON-ifelse(SITE!='11497500', 0, 0.02),
                y = LAT, label = REF_LABEL,
                hjust = ifelse(SITE!='11497500', 0.5, 1),
                vjust = ifelse(SITE!='11497500', -1, 1)),
            data = stn.ref, fontface='bold', size=4) +
  geom_text(aes(x = LON-0.02, y = LAT, label = SITE_NAME),
            data = filter(stn.kt_sprague, SITE_NAME=="Godowa"), size=3, hjust=1) +
  geom_text(aes(x = LON+0.02, y = LAT, label = SITE_NAME),
            data = filter(stn.kt_sprague, SITE_NAME!="Godowa"), size=3, hjust=0) +
  scale_shape_manual('Station Type', values=c('KT'=21, 'Reference'=24)) +
  scale_size_manual('Station Type', values=c('KT'=4, 'Reference'=5)) +
  scale_fill_manual('Reference Station', values=c('USGS-11501000'='orangered',
                                                  'OWRD-11497500'='deepskyblue',
                                                  'OWRD-11499100'='chartreuse3')) +
  guides(fill=guide_legend(override.aes=list(shape=21))) +
  geom_text(x=-120.61, y=42.17,
            label='Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.',
            size=3,
            hjust=1,
            color='grey50')
print(p)
dev.off()

filename <- 'report/results-flow-daily-ts.png'
cat("Saving daily flows to:", filename, '\n')
png(filename, width=8, height=6, res=200, units='in')
p <- ggplot(q.model, aes(DATE)) +
  geom_line(aes(y=PRED_RESID, color="Estimated Daily\nFlows")) +
  geom_point(aes(y=FLOW, color='Measured Biweekly\nFlow'), size=1) +
  scale_color_manual('', values=c('grey50', 'orangered')) +
  labs(x='Date', y='Flow (cfs)') +
  facet_wrap(~SITE_NAME, scales='free_y', nrow=4) +
  guides(colour = guide_legend(override.aes=list(linetype=c('solid', 'blank'),
                                                 shape=c(NA, 16)))) +
  theme(legend.position='top')
print(p)
dev.off()

filename <- 'report/results-flow-annual-ts.png'
cat("Saving annual flows to:", filename, '\n')
png(filename, width=8, height=6, res=200, units='in')
p <- mutate(q.out, WYEAR=wyear(DATE)) %>%
  group_by(SITE_NAME, WYEAR) %>%
  summarise(N=n(),
            Q=mean(Q)) %>%
  ungroup %>%
  filter(N>=365) %>%
  ggplot(aes(WYEAR, Q)) +
  geom_bar(stat='identity') +
  labs(x='Water Year', y='Annual Mean Flow (cfs)') +
  facet_wrap(~SITE_NAME, scales='free_y', nrow=4)
print(p)
dev.off()


filename <- file.path('report', 'flow-validation-timeseries.png')
cat('Printing:', filename, '\n')
png(filename, width=10, height=6, res=200, units='in')

p <- q.valid %>%
  ggplot(aes(DATE, VALUE, color=TERM)) +
  geom_line(aes(color="OWRD Station", y=VALID_Q), size=0.3) +
  # geom_line(aes(color="WQ Station", y=Q), size=0.3) +
  geom_point(aes(color="WQ Station", y=Q), size=1) +
  log_y +
  labs(x="Date", y="Flow (cfs)") +
  scale_color_manual('', values=c('OWRD Station'='grey50', 'WQ Station'='orangered')) +
  facet_wrap(~SITE_NAME+SITE, scales='free_y') +
  guides(colour=guide_legend(override.aes = list(linetype=c("solid", "blank"),
                                                 shape=c(NA, 16)))) +
  theme(strip.background=element_blank(),
        strip.text=element_text(face='bold', size=12),
        panel.grid.minor.y=element_blank())
print(p)

dev.off()


filename <- file.path('report', 'flow-validation-timeseries-mon.png')
cat('Printing:', filename, '\n')
png(filename, width=10, height=6, res=200, units='in')

p <- q.valid.mon %>%
  ggplot(aes(DATE, VALUE, color=TERM)) +
  geom_line(aes(color="OWRD Station", y=VALID_Q)) +
  geom_point(aes(color="WQ Station", y=Q), size=1.5) +
  log_y +
  labs(x="Date", y="Flow (cfs)") +
  scale_color_manual('', values=c('OWRD Station'='grey50', 'WQ Station'='orangered')) +
  facet_wrap(~SITE_NAME+SITE, scales='free_y') +
  guides(colour=guide_legend(override.aes = list(linetype=c("blank", "solid"),
                                                 shape=c(16, NA)))) +
  theme(strip.background=element_blank(),
        strip.text=element_text(face='bold', size=12),
        panel.grid.minor.y=element_blank())
print(p)

dev.off()

filename <- file.path('report', 'flow-validation-scatter.png')
cat('Printing:', filename, '\n')
png(filename, width=10, height=8, res=200, units='in')

p <- q.valid %>%
  ggplot(aes(VALID_Q, Q)) +
  geom_point(size=1, alpha=0.7) +
  geom_abline(aes(color='1:1 Line'), linetype=2, show_guide=TRUE) +
  geom_text(aes(x=X, y=Y, label=R2), data=mutate(q.valid.rmse, R2=paste0("R2 = ", scales::percent(R2))),
            hjust=0, vjust=1) +
  log_x +
  log_y +
  scale_color_manual('', values='red') +
  labs(x="Daily Flow @ OWRD Station (cfs)", y="Daily Flow @ WQ Station (cfs)") +
  facet_wrap(~SITE_NAME+SITE, scales='free') +
  theme(strip.background=element_blank(),
        strip.text=element_text(face='bold', size=12),
        panel.grid.minor=element_blank())
print(p)

dev.off()

filename <- file.path('report', 'flow-validation-scatter-mon.png')
cat('Printing:', filename, '\n')
png(filename, width=10, height=8, res=200, units='in')

p <- q.valid.mon %>%
  ggplot(aes(VALID_Q, Q)) +
  geom_point() +
  geom_abline(aes(color='1:1 Line'), linetype=2, show_guide=TRUE) +
  geom_text(aes(x=X, y=Y, label=R2), data=mutate(q.valid.mon.rmse, R2=paste0("R2 = ", scales::percent(R2))),
            hjust=0, vjust=1) +
  log_x +
  log_y +
  scale_color_manual('', values='red') +
  labs(x="Monthly Flow @ OWRD Station (cfs)", y="Monthly Flow @ WQ Station (cfs)") +
  facet_wrap(~SITE_NAME+SITE, scales='free') +
  theme(strip.background=element_blank(),
        strip.text=element_text(face='bold', size=12),
        panel.grid.minor=element_blank())
print(p)

dev.off()


# save ----
cat('Saving flows to flows.Rdata...\n')

# units in cfs
list(ratios=ratios,
     model=q.model,
     df=q.out) %>%
  saveRDS(file='flows.Rdata')

cat('\n\n')




