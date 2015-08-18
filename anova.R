library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
theme_set(theme_bw())
library(GGally)

load('kt_sprague.Rdata')

wq.raw <- wq.kt_sprague[['CLEAN']] %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=levels(stn.kt_sprague$SITE_NAME)))

wq <- filter(wq.raw, !FLAGGED, QAQC %in% c('PASS', 'RPD')) %>%
  select(DATE, SITE, SITE_NAME, VAR, VALUE) %>%
  filter(VAR %in% c('FLOW', 'TP', 'PO4', 'TN', 'NO23', 'NH4', 'TSS')) %>%
  mutate(VAR=ordered(VAR, levels=c('FLOW', 'TP', 'PO4', 'TN', 'NH4', 'NO23', 'TSS')))

variable <- 'TP'
site <- 'Power'


x <- wq %>%
  filter(VAR==variable) %>%
  mutate(YEAR=year(DATE),
         WEEK=floor(week(DATE)/2)) %>%
  group_by(SITE_NAME, YEAR, WEEK) %>%
  summarise(MED=median(VALUE),
            N=n()) %>%
  ungroup %>%
  mutate(DATE=as.Date(paste(YEAR, 1, 1, sep='-'))+(WEEK-1)*14)

x %>%
  gather(STAT, VALUE, MED:N) %>%
  unite(SITE_STAT, SITE_NAME, STAT) %>%
  spread(SITE_STAT, VALUE) %>%
  ggplot(aes(Power_MED, Lone_Pine_MED, color=factor(Power_N+Lone_Pine_N))) +
  geom_point()

y <- filter(x, SITE_NAME %in% c('Power', 'Lone_Pine')) %>%
  select(DATE, SITE_NAME, MED) %>%
  spread(SITE_NAME, MED) %>%
  mutate(Power_Lone_Pine=Power-Lone_Pine) %>%
  gather(SITE_NAME, MED, Power:Power_Lone_Pine, na.rm=TRUE)
filter(y, SITE_NAME %in% c('Power', 'Lone_Pine')) %>%
  ggplot(aes(DATE, MED, color=SITE_NAME)) +
  geom_point()


y %>%
  ggplot(aes(MED)) +
  geom_histogram() +
  facet_wrap(~SITE_NAME, scales='free')

filter(y, SITE_NAME=='Power_Lone_Pine') %>%
  mutate(SEASON=factor(floor((month(DATE)-1)/3))) %>%
  ggplot(aes(MED)) +
  geom_histogram() +
  facet_wrap(~SEASON, scales='free')

# density by site
filter(y, SITE_NAME %in% c('Power', 'Lone_Pine')) %>%
  ggplot(aes(MED, color=SITE_NAME)) +
  geom_density()

# boxplot by site
filter(wq, SITE_NAME %in% c('Power', 'Lone_Pine'), VAR=='TP') %>%
  ggplot(aes(x=1, y=log10(VALUE), fill=SITE_NAME)) +
  geom_boxplot(position='dodge')

# lm of MED ~ SITE_NAME
filter(y, SITE_NAME %in% c('Power', 'Lone_Pine')) %>%
  lm(MED~SITE_NAME, data=.) %>%
  summary

# box plot by season/site
filter(wq, SITE_NAME %in% c('Power', 'Lone_Pine'), VAR=='TP') %>%
  mutate(SEASON=factor(floor((month(DATE)-1)/3))) %>%
  ggplot(aes(SEASON, log10(VALUE), fill=SITE_NAME)) +
  geom_boxplot(position='dodge')

# lm of MED ~ SITE_NAME + SEASON
lm1 <- filter(wq, SITE_NAME %in% c('Power', 'Lone_Pine'), VAR=='TP') %>%
  mutate(SEASON=factor(floor((month(DATE)-1)/3))) %>%
  lm(log10(VALUE)~SITE_NAME+SEASON, data=.)
aov(lm1) %>% summary


lm2 <- filter(wq, SITE_NAME %in% c('Power', 'Lone_Pine'), VAR=='TP') %>%
  mutate(VALUE=log10(VALUE)) %>%
  lm(VALUE~SITE_NAME, data=.)

filter(wq, SITE_NAME %in% c('Power', 'Lone_Pine'), VAR=='TP') %>%
  mutate(VALUE=log10(VALUE)) %>%
  group_by(SITE_NAME) %>%
  summarise(MEAN=mean(VALUE))

filter(wq, SITE_NAME %in% c('Power', 'Lone_Pine'), VAR=='TP') %>%
  mutate(SEASON=factor(floor((month(DATE)-1)/3))) %>%
  aov(log10(VALUE)~SITE_NAME, data=.) %>%
  plot

filter(x, SITE_NAME %in% c('Power', 'Lone_Pine')) %>%
  select(DATE, SITE_NAME, MED) %>%
  spread(SITE_NAME, MED) %>%
  ggplot(aes(Power, Lone_Pine)) +
  geom_point() +
  geom_abline() +
  geom_smooth(method='lm')

filter(x, SITE_NAME %in% c('Power', 'Lone_Pine')) %>%
  select(DATE, SITE_NAME, MED) %>%
  spread(SITE_NAME, MED) %>%
  mutate(SEASON=factor(floor((month(DATE)-1)/3))) %>%
  ggplot(aes(Power, Lone_Pine)) +
  geom_point() +
  geom_abline() +
  geom_smooth(method='lm') +
  facet_wrap(~SEASON, scales='free')


filter(y, SITE_NAME %in% c('Power_Lone_Pine')) %>%
  ggplot(aes(DATE, MED)) +
  geom_point()

ggplot(aes(Power_MED, Lone_Pine_MED, color=factor(Power_N+Lone_Pine_N))) +
  geom_point()


pdf(file.path('pdf', 'dataset-variable-scatterplot-matrix-variable.pdf'), width=11, height=8.5)
for (site in levels(wq$SITE_NAME)) {
  cat(site, '\n')
  p <- wq %>%
    spread(VAR, VALUE) %>%
    filter(SITE_NAME==site) %>%
    select(FLOW:TSS) %>%
    #   ggpairs(upper=list(continuous = "points", combo = "dot")) +
    ggpairs(upper=list(continuous="points", params=c(size=1)),
            lower=list(continuous="points", params=c(size=1)),
            title=paste0('Site: ', site)) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
  print(p, left=0.5, bottom=0.5)
}
dev.off()

pdf(file.path('pdf', 'dataset-variable-scatterplot-matrix-variable-log10.pdf'), width=11, height=8.5)
for (site in levels(wq$SITE_NAME)) {
  cat(site, '\n')
  p <- wq %>%
    mutate(VALUE=log10(VALUE)) %>%
    spread(VAR, VALUE) %>%
    filter(SITE_NAME==site) %>%
    select(FLOW:TSS) %>%
    #   ggpairs(upper=list(continuous = "points", combo = "dot")) +
    ggpairs(upper=list(continuous="points", params=c(size=1)),
            lower=list(continuous="points", params=c(size=1)),
            title=paste0('Site: ', site)) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
  print(p, left=0.5, bottom=0.5)
}
dev.off()


pdf(file.path('pdf', 'dataset-variable-scatterplot-matrix-site.pdf'), width=11, height=8.5)
for (variable in levels(wq$VAR)) {
  cat(variable, '\n')
  p <- wq %>%
    filter(VAR==variable) %>%
    mutate(YEAR=year(DATE),
           WEEK=floor(week(DATE)/2)) %>%
    group_by(SITE_NAME, YEAR, WEEK) %>%
    summarise(VALUE=median(VALUE, na.rm=TRUE)) %>%
    spread(SITE_NAME, VALUE) %>%
    select(-YEAR, -WEEK) %>%
    #   ggpairs(upper=list(continuous = "points", combo = "dot")) +
    ggpairs(upper=list(continuous="points", params=c(size=1)),
            lower=list(continuous="points", params=c(size=1)),
            title=paste0('Variable: ', variable)) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
  print(p, left=0.5, bottom=0.5)
}
dev.off()

pdf(file.path('pdf', 'dataset-variable-scatterplot-matrix-site-log10.pdf'), width=11, height=8.5)
for (variable in levels(wq$VAR)) {
  cat(variable, '\n')
  p <- wq %>%
    filter(VAR==variable) %>%
    mutate(YEAR=year(DATE),
           WEEK=floor(week(DATE)/2),
           VALUE=log10(VALUE)) %>%
    group_by(SITE_NAME, YEAR, WEEK) %>%
    summarise(VALUE=median(VALUE)) %>%
    spread(SITE_NAME, VALUE) %>%
    select(-YEAR, -WEEK) %>%
    #   ggpairs(upper=list(continuous = "points", combo = "dot")) +
    ggpairs(upper=list(continuous="points", params=c(size=1)),
            lower=list(continuous="points", params=c(size=1)),
            title=paste0('Variable: ', variable)) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
  print(p, left=0.5, bottom=0.5)
}
dev.off()
