library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())
library(scales)
library(ggmap)
library(gridExtra)

load('prism.Rdata')
load('ghcnd.Rdata')
load('gis.Rdata')

# plots ----
pdf(file.path('pdf', 'prism.pdf'), width=11, height=8.5)
ggplot(prism_subbasin, aes(MONTHYEAR, PRCP/25.4, color=SITE_NAME)) +
  geom_line() +
  facet_wrap(~SITE_NAME) +
  scale_color_manual('Site', values = RColorBrewer::brewer.pal(n=8, name='Dark2')) +
  labs(x='Month/Year', y='Monthly Precip (in/mon)') +
  ggtitle('Monthly Precipitation by Drainage Subbasin')

mutate(prism_subbasin, MONTH=ordered(month(MONTHYEAR), levels=c(10:12, 1:9))) %>%
  ggplot(aes(MONTH, PRCP/25.4, fill=SITE_NAME)) +
  geom_boxplot() +
  facet_wrap(~SITE_NAME) +
  labs(x="Month", y="Monthly Precip (in/mon)") +
  scale_fill_manual('Site', values = RColorBrewer::brewer.pal(n=8, name='Dark2')) +
  ggtitle('Distribution of Monthly Precipitation by Drainage Subbasin and Month')

mutate(prism_subbasin, MONTH=ordered(month(MONTHYEAR), levels=c(10:12, 1:9))) %>%
  group_by(SITE_NAME, MONTH) %>%
  summarise(PRCP=median(PRCP)/25.4) %>%
  ggplot(aes(MONTH, PRCP, color=SITE_NAME, group=SITE_NAME)) +
  stat_summary(fun.y=sum, geom="line") +
  scale_color_manual('Site', values = RColorBrewer::brewer.pal(n=8, name='Dark2')) +
  ggtitle('Median Monthly Precipitation by Drainage Subbasin and Month') +
  labs(x='Month', y='Monthly Precip (in/mon)')

group_by(prism_subbasin, SITE_NAME, WYEAR) %>%
  summarise(N_MONTH=n(),
            PRCP=sum(PRCP)/25.4) %>%
  filter(N_MONTH == 12) %>%
  ggplot(aes(factor(WYEAR), PRCP, fill=SITE_NAME)) +
  geom_bar(stat='identity') +
  scale_fill_manual('Site', values = RColorBrewer::brewer.pal(n=8, name='Dark2')) +
  facet_wrap(~SITE_NAME) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=6)) +
  labs(x='Water Year', y='Annual Precip (in/yr)') +
  ggtitle('Annual Precipitation by Drainage Subbasin')

group_by(prism_subbasin, SITE_NAME, WYEAR) %>%
  summarise(N_MONTH=n(),
            PRCP=sum(PRCP)/25.4) %>%
  filter(N_MONTH == 12) %>%
  ggplot(aes(WYEAR, PRCP, color=SITE_NAME)) +
  geom_line() +
  scale_color_manual('Site', values = RColorBrewer::brewer.pal(n=8, name='Dark2')) +
  scale_x_continuous(breaks=pretty_breaks(25)) +
  labs(x='Water Year', y='Annual Precip (in/yr)') +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8)) +
  ylim(0, NA) +
  ggtitle('Annual Precipitation by Drainage Subbasin')

dev.off()

# plot prism vs ghcnd ----
prcp <- filter(ghcnd, STATION=='GHCND:USW00094236') %>%
  select(DATE, PRCP) %>%
  mutate(MONTHYEAR=floor_date(DATE, 'month')) %>%
  group_by(MONTHYEAR) %>%
  summarise(PRCP=sum(PRCP, na.rm=TRUE)/25.4) %>%
  mutate(SOURCE='GHCND') %>%
  rbind(filter(prism_subbasin, SITE_NAME=='Power') %>% select(MONTHYEAR, PRCP) %>% mutate(PRCP=PRCP/25.4, SOURCE='PRISM')) %>%
  mutate(WYEAR=fluxr::wyear(MONTHYEAR)) %>%
  filter(WYEAR>=2001)

pdf(file.path('pdf', 'prism-ghcnd-compare.pdf'), width=11, height=8.5)

p.mon <- prcp %>%
  ggplot(aes(MONTHYEAR, PRCP, fill=SOURCE)) +
  geom_bar(stat='identity', position='dodge') +
  labs(x='Month/Year', y='Monthly Precip (in/mon)') +
  scale_fill_manual('', values=c('GHCND'='orangered', 'PRISM'='deepskyblue3')) +
  ggtitle('Monthly Precipitation Timeseries\nGHCND @ Klamath Falls vs. PRISM @ Sprague Basin')
p.wyr <- prcp %>%
  group_by(WYEAR, SOURCE) %>%
  summarise(PRCP=sum(PRCP)) %>%
  ggplot(aes(factor(WYEAR), PRCP, fill=SOURCE)) +
  geom_bar(stat='identity', position='dodge') +
  scale_fill_manual('', values=c('GHCND'='orangered', 'PRISM'='deepskyblue3')) +
  labs(x='Water Year', y='Annual Precip (in/yr)') +
  ggtitle('Annual Precipitation Timeseries\nGHCND @ Klamath Falls vs. PRISM @ Sprague Basin')
grid.arrange(p.mon, p.wyr, ncol=1)

p.tile <- prcp %>%
  mutate(MONTH=month(MONTHYEAR), WYEAR=fluxr::wyear(MONTHYEAR),
         MONTH=ordered(MONTH, levels=rev(c(10:12, 1:9)))) %>%
  ggplot(aes(factor(WYEAR), MONTH, fill=PRCP)) +
  geom_tile() +
  facet_wrap(~SOURCE) +
  scale_fill_gradientn('Precip (in/mon)', 
                       colours=rev(scales::brewer_pal(type = "seq", palette = 'GnBu')(9)), limits=c(0, NA)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  labs(x='Water Year', y='Month')
p.mon <- prcp %>%
  mutate(MONTH=month(MONTHYEAR),
         MONTH=ordered(MONTH, levels=c(10:12, 1:9))) %>%
  spread(SOURCE, PRCP) %>%
  ggplot(aes(PRISM, GHCND, color=MONTH)) +
  geom_point() +
  geom_abline(linetype=2) +
  labs(x="PRISM @ Sprague Basin (in/mon)", y="GHCND @ Klamath Falls (in/mon)") +
  scale_color_discrete('Month') +
  ggtitle('Comparison of Monthly Precip')
p.wyr <- prcp %>%
  group_by(WYEAR, SOURCE) %>%
  summarise(PRCP=sum(PRCP)) %>%
  spread(SOURCE, PRCP) %>%
  ggplot(aes(PRISM, GHCND)) +
  geom_point() +
  geom_smooth(method='lm') +
  geom_abline(linetype=2) +
  xlim(0, NA) +
  ylim(0, NA) +
  labs(x="PRISM @ Sprague Basin (in/yr)", y="GHCND @ Klamath Falls (in/yr)") +
  ggtitle('Comparison of Annual Precip')
grid.arrange(p.tile, arrangeGrob(p.mon, p.wyr, ncol=2), nrow=2)

dev.off()