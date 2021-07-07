library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())
library(fluxr)
library(gridExtra)

rm(list=ls())

cat(paste0(rep('=', 80), collapse=''), '\n')
cat("Running flow-precip comparison...\n\n")

# load ----
load('gis.Rdata')

flows <- readRDS('flows.Rdata')

q.day <- flows$df %>%
  dplyr::rename(Q_cfs=Q) %>%
  mutate(Q_cms=Q_cfs*0.0283168, # ft3/s -> m3/s
         DATE=as.Date(DATE),
         MONTH=month(DATE))

q.day[['WYEAR']] <- wyear(q.day[['DATE']])

q.day <- left_join(q.day, subbasin_area, by='SITE_NAME') %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=levels(subbasin_area$SITE_NAME)),
         Q_mm_d=Q_cms*86400/(AREA_KM2*1e6)*1000) # mm/day
q.mon <- dplyr::group_by(q.day, SITE_NAME, WYEAR, MONTH) %>%
  dplyr::summarise(Q_mm_mon=sum(Q_mm_d)) %>% # mm/mon
  ungroup
q.wyr <- dplyr::group_by(q.mon, SITE_NAME, WYEAR) %>%
  dplyr::summarise(Q_cm_yr=sum(Q_mm_mon)/10) %>% # cm/yr
  ungroup

q.wyr.site <- dplyr::group_by(q.wyr, SITE_NAME) %>%
  dplyr::summarise(Q_cm_yr=mean(Q_cm_yr))

load('prism.Rdata')
prism.mon <- mutate(prism_subbasin, MONTH=month(MONTHYEAR)) %>%
  select(SITE_NAME, MONTHYEAR, WYEAR, MONTH, PRCP_mm_mon=PRCP)

prism.wyr <- dplyr::group_by(prism.mon, SITE_NAME, WYEAR) %>%
  dplyr::summarise(N=n(),
            PRCP_cm_yr=sum(PRCP_mm_mon)/10) %>% # cm/yr
  select(-N)

# prism.mon <- filter(prism.mon, WYEAR >= 2001)
# prism.wyr <- filter(prism.wyr, WYEAR >= 2001)

load('snotel.Rdata')
snotel <- filter(snotel, SITE_NAME %in% c('SUMMER RIM', 'TAYLOR BUTTE', 'CRAZYMAN FLAT')) %>%
  droplevels

snotel.wyr <- filter(snotel.wyr, SITE_NAME %in% c('SUMMER RIM', 'TAYLOR BUTTE', 'CRAZYMAN FLAT')) %>%
  droplevels


# beatty station
load('owrd.Rdata')
df.beatty <- filter(q.owrd, STATION_ID=='11497500') %>%
  select(DATE, FLOW) %>%
  mutate(Q_cms=FLOW*0.0283168,
         Q_mm_d=Q_cms*86400/(1470.1266*1e6)*1000) %>%
  mutate(MONTHYEAR=as.Date(floor_date(DATE, 'month'))) %>%
  dplyr::group_by(MONTHYEAR) %>%
  dplyr::summarise(N=sum(!is.na(Q_mm_d)),
            Q_mm_mon=sum(Q_mm_d, na.rm=TRUE)) %>%
  mutate(WYEAR=wyear(MONTHYEAR),
         MONTH=month(MONTHYEAR)) %>%
  filter(WYEAR <= 2014) %>%
  left_join(filter(prism.mon, SITE_NAME=='Godowa') %>%
              select(MONTHYEAR, PRCP_mm_mon),
            by=c('MONTHYEAR')) %>%
  filter(WYEAR >= 1982)

df.beatty.wyr <- dplyr::group_by(df.beatty, WYEAR) %>%
  dplyr::summarise(N=sum(N),
            Q_cm_yr=sum(Q_mm_mon)/10,
            PRCP_cm_yr=sum(PRCP_mm_mon)/10)

df.beatty %>%
  ggplot(aes(PRCP_mm_mon, Q_mm_mon, color=factor(MONTH))) +
  geom_point() +
  geom_abline() +
  facet_wrap(~MONTH)

df.beatty %>%
  dplyr::group_by(WYEAR) %>%
  dplyr::summarise(Q_mm_yr=sum(Q_mm_mon),
            PRCP_mm_yr=sum(PRCP_mm_mon)) %>%
  ggplot(aes(PRCP_mm_yr, Q_mm_yr, color=WYEAR>=2013)) +
  geom_point()

# power station
load('usgs.Rdata')
df.power <- filter(q.usgs, SITE_NAME=='Power') %>%
  select(DATE, FLOW) %>%
  mutate(Q_cms=FLOW*0.0283168,
         Q_mm_d=Q_cms*86400/(1470.1266*1e6)*1000) %>%
  mutate(MONTHYEAR=as.Date(floor_date(DATE, 'month'))) %>%
  dplyr::group_by(MONTHYEAR) %>%
  dplyr::summarise(N=sum(!is.na(Q_mm_d)),
            Q_mm_mon=sum(Q_mm_d, na.rm=TRUE)) %>%
  mutate(WYEAR=wyear(MONTHYEAR),
         MONTH=month(MONTHYEAR)) %>%
  filter(WYEAR <= 2014) %>%
  left_join(filter(prism.mon, SITE_NAME=='Power') %>%
              select(MONTHYEAR, PRCP_mm_mon),
            by=c('MONTHYEAR')) %>%
  filter(WYEAR >= 1982)
df.power.wyr <- dplyr::group_by(df.power, WYEAR) %>%
  dplyr::summarise(N=sum(N),
            Q_cm_yr=sum(Q_mm_mon)/10,
            PRCP_cm_yr=sum(PRCP_mm_mon)/10)

df.power %>%
  ggplot(aes(PRCP_mm_mon, Q_mm_mon, color=factor(MONTH))) +
  geom_point() +
  geom_abline() +
  facet_wrap(~MONTH)


# merge ----
df.wyr <- left_join(ungroup(q.wyr), ungroup(prism.wyr), by=c('SITE_NAME', 'WYEAR')) %>%
  filter(WYEAR %in% seq(2001, 2014)) %>%
  mutate(Q_PRCP=Q_cm_yr/PRCP_cm_yr) %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=levels(subbasin_area$SITE_NAME)))

# plots ----
filename <- file.path('pdf', 'flow-precip-relationships.pdf')
cat('Printing:', filename, '\n')
pdf(filename, width=11, height=8.5)

p.beatty <- df.beatty.wyr %>%
  mutate(GROUP=ifelse(WYEAR <= 2012, '1982-2012',
                      ifelse(WYEAR==2013, '2013', '2014'))) %>%
  ggplot(aes(PRCP_cm_yr, Q_cm_yr)) +
  geom_point(aes(color=GROUP), size=2) +
  scale_color_manual('', values=c('grey50', 'orangered', 'deepskyblue3')) +
  ggtitle('Station: Godowa/Beatty') +
  labs(x='Annual Precip (cm/yr)', y='Observed Annual Flow per Area (cm/yr)') +
  xlim(0, NA) +
  ylim(0, NA)
p.power <- df.power.wyr %>%
  mutate(GROUP=ifelse(WYEAR <= 2012, '1982-2012',
                      ifelse(WYEAR==2013, '2013', '2014'))) %>%
  ggplot(aes(PRCP_cm_yr, Q_cm_yr)) +
  geom_point(aes(color=GROUP), size=2) +
  scale_color_manual('', values=c('grey50', 'orangered', 'deepskyblue3')) +
  ggtitle('Station: Power') +
  labs(x='Annual Precip (cm/yr)', y='Observed Annual Flow per Area (cm/yr)') +
  xlim(0, NA) +
  ylim(0, NA)

grid.arrange(grobs=list(p.beatty, p.power),
             ncol=2, nrow=2,
             top='Long Term Annual Precip vs Flow, 1982-2012')

p <- mutate(df.wyr, GROUP=ifelse(WYEAR <= 2012, '2001-2012',
                        ifelse(WYEAR==2013, '2013', '2014'))) %>%
  ggplot(aes(PRCP_cm_yr, Q_cm_yr)) +
  geom_point(aes(color=GROUP), size=2) +
  scale_color_manual('', values=c('grey50', 'orangered', 'deepskyblue3')) +
  facet_wrap(~SITE_NAME, scales='free') +
  ggtitle('Annual Precip vs Flow (by Water Year)') +
  labs(x='Annual Precip (cm/yr)', y='Annual Flow per Area (cm/yr)')
print(p)

p.max <- full_join(df.wyr, select(snotel.wyr, SNOTEL_NAME=SITE_NAME, WYEAR, SWE_MAX_cm), by='WYEAR') %>%
  filter(!is.na(SITE_NAME)) %>%
  mutate(GROUP=ifelse(WYEAR <= 2012, '2001-2012',
                      ifelse(WYEAR==2013, '2013', '2014'))) %>%
  ggplot(aes(SWE_MAX_cm, Q_cm_yr, color=GROUP)) +
  geom_point(size=2) +
  scale_color_manual('', values=c('grey50', 'orangered', 'deepskyblue3')) +
  facet_grid(SITE_NAME~SNOTEL_NAME, scales='free') +
  ggtitle('Annual Max Snow Water Equiv.') +
  theme(legend.position='top') +
  labs(x='Max Snow Water Equiv (cm)', y='Flow per Area (cm/yr)')

p.apr <- full_join(df.wyr, select(snotel.wyr, SNOTEL_NAME=SITE_NAME, WYEAR, SWE_APR_cm), by='WYEAR') %>%
  filter(!is.na(SITE_NAME)) %>%
  mutate(GROUP=ifelse(WYEAR <= 2012, '2001-2012',
                      ifelse(WYEAR==2013, '2013', '2014'))) %>%
  ggplot(aes(SWE_APR_cm, Q_cm_yr, color=GROUP)) +
  geom_point(size=2) +
  scale_color_manual('', values=c('grey50', 'orangered', 'deepskyblue3')) +
  facet_grid(SITE_NAME~SNOTEL_NAME, scales='free') +
  ggtitle('April 1 Snow Water Equiv.') +
  theme(legend.position='top') +
  labs(x='Snow Water Equiv on Apr 1 (cm)', y='Flow per Area (cm/yr)')

grid.arrange(grobs=list(p.max, p.apr),
             ncol=2,
             top='Annual Flow per Area vs Snow Water Equivalent')

p <- ggplot(df.wyr, aes(WYEAR, Q_PRCP)) +
  geom_line() +
  ylim(0, NA) +
  labs(x='', y='Annual Flow:Precip Ratio') +
  ggtitle('Annual Flow:Precip Ratio by Site') +
  scale_x_continuous(breaks=scales::pretty_breaks(n=10)) +
  facet_wrap(~SITE_NAME)
print(p)

p <- ggplot(df.wyr, aes(SITE_NAME, Q_PRCP)) +
  geom_boxplot() +
  ylim(0, NA) +
  labs(x='', y='Annual Flow:Precip Ratio') +
  ggtitle('Distribution of Annual Flow:Precip Ratio by Site, 2001-2014')
print(p)

dev.off()

# double mass curves ----
# df_dmc <- arrange(df.wyr, SITE_NAME, WYEAR) %>%
#   filter(SITE_NAME %in% c('Power', 'Lone', 'Sycan', 'Godowa', 'SF', 'NF')) %>%
#   group_by(SITE_NAME) %>%
#   mutate(CUM_Q_cm_yr=cumsum(Q_cm_yr),
#          CUM_PRCP_cm_yr=cumsum(PRCP_cm_yr))
# df_dmc_ratio <- group_by(df_dmc, SITE_NAME) %>%
#   summarise(CUM_RATIO=max(CUM_Q_cm_yr)/max(CUM_PRCP_cm_yr))
# ggplot(df_dmc, aes(CUM_PRCP_cm_yr, CUM_Q_cm_yr)) +
#   geom_point() +
#   geom_abline(aes(slope=CUM_RATIO), intercept=0, data=df_dmc_ratio) +
#   xlim(0, NA) +
#   ylim(0, NA) +
#   facet_wrap(~SITE_NAME, scales='free') +
#   labs(x='Cumulative Annual Precip (cm/yr)', y='Cumulative Annual Flow (cm/yr)')

cat('\n\n')

