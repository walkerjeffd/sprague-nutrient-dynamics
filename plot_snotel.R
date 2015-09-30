library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())
library(scales)
library(ggmap)
library(gridExtra)

rm(list=ls())

load('snotel.Rdata')
load('gis.Rdata')
source('functions.R')

# plots ----
filename <- file.path('pdf', 'snotel.pdf')
cat('Printing:', filename, '\n')
pdf(filename, width=11, height=8.5)

p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
  geom_polygon(aes(x = long, y = lat, group = group), data = basin,
               color = 'black', alpha = 0, size = 0.5) +
  geom_path(aes(x = long, y = lat, group = group), data = flowline,
            color='deepskyblue', size=0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), data = incbasin,
               color = 'orangered', fill = 'grey50', alpha = 0.2, size = 0.2) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE), data=stn.snotel,
             fill='deepskyblue', size=4, pch=21) +
  geom_text(aes(x = LONGITUDE, y = LATITUDE, label=SITE_NAME), data=stn.snotel,
            color='black', size=4, hjust=1.1) +
  ggtitle('SNOTEL Stations')
print(p)
makeFootnote()

p <- snotel %>%
  filter(WYEAR >= 2000) %>%
  ggplot(aes(DATE, SWE_in)) +
  geom_line() +
  geom_point(aes(DATE, SWE_in),
             data=filter(snotel, month(DATE)==4, day(DATE)==1, WYEAR>=2000),
             color='red', size=3) +
  geom_point(aes(DATE, SWE_in),
             data=filter(snotel, WYEAR >= 2000) %>%
               group_by(SITE_NAME, WYEAR) %>%
               mutate(N=sum(!is.na(SWE_in)),
                      RANK=row_number(SWE_in)) %>%
               filter(RANK==max(RANK), N>300),
             color='deepskyblue', size=3) +
  facet_wrap(~SITE_NAME, ncol=1, scales='free_y') +
  labs(x='Date', y='SWE (in)',
       title='Daily Snow Water Equivalent\nRed Point=April 1, Blue Point=Maximum')
print(p)

# annual values
p.mean <- snotel.wyr %>%
  filter(WYEAR >= 2000) %>%
  ggplot(aes(factor(WYEAR), SWE_MEAN)) +
  geom_bar(stat='identity', fill='grey50') +
  facet_wrap(~SITE_NAME, ncol=1, scales='free_y') +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  labs(x='Water Year', y='Mean Annual SWE (in)',
       title='Mean Annual SWE')
p.max <- snotel.wyr %>%
  filter(WYEAR >= 2000) %>%
  ggplot(aes(factor(WYEAR), SWE_MAX)) +
  geom_bar(stat='identity', fill='grey50') +
  facet_wrap(~SITE_NAME, ncol=1, scales='free_y') +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  labs(x='Water Year', y='Max Annual SWE (in)',
       title='Max Annual SWE')
p.apr <- snotel.wyr %>%
  filter(WYEAR >= 2000) %>%
  ggplot(aes(factor(WYEAR), SWE_APR)) +
  geom_bar(stat='identity', fill='grey50') +
  facet_wrap(~SITE_NAME, ncol=1, scales='free_y') +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  labs(x='Water Year', y='April 1 SWE (in)',
       title='April 1 SWE')
grid.arrange(grobs=list(p.apr, p.mean, p.max),
             nrow=1)

# snowpack trends
p.apr <- filter(snotel.wyr, SITE_NAME %in% c("SUMMER RIM", "TAYLOR BUTTE", "CRAZYMAN FLAT")) %>%
  filter(WYEAR >= 2000) %>%
  ggplot(aes(WYEAR, SWE_APR)) +
  geom_point() +
  geom_smooth(method='lm') +
  ylim(0, NA) +
  facet_wrap(~SITE_NAME, ncol=1, scales='free_y') +
  labs(x='Water Year', y='April 1 SWE (in)') +
  ggtitle('April 1 SWE')
p.mean <- filter(snotel.wyr, SITE_NAME %in% c("SUMMER RIM", "TAYLOR BUTTE", "CRAZYMAN FLAT")) %>%
  filter(WYEAR >= 2000) %>%
  ggplot(aes(WYEAR, SWE_MEAN)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~SITE_NAME, ncol=1, scales='free_y') +
  ylim(0, NA) +
  labs(x='Water Year', y='Max Annual SWE (in)') +
  ggtitle('Peak SWE by Water Year')
p.max <- filter(snotel.wyr, SITE_NAME %in% c("SUMMER RIM", "TAYLOR BUTTE", "CRAZYMAN FLAT")) %>%
  filter(WYEAR >= 2000) %>%
  ggplot(aes(WYEAR, SWE_MAX)) +
  geom_point() +
  geom_smooth(method='lm') +
  facet_wrap(~SITE_NAME, ncol=1, scales='free_y') +
  ylim(0, NA) +
  labs(x='Water Year', y='Max Annual SWE (in)') +
  ggtitle('Peak SWE by Water Year')
grid.arrange(grobs=list(p.apr, p.mean, p.max),
             ncol=3,
             top='Trends in Snowpack since WY2000')

dev.off()
