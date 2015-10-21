library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(fluxr)
library(ggplot2)
theme_set(theme_bw())
library(gridExtra)

rm(list=ls())

# load data ----
load('kt_sprague.Rdata')
load('loads.Rdata')
load('gis.Rdata')
network <- readRDS('network.Rdata')

units <- c(Q='hm3/d', L='kg/d', C='ppb')
term_labels <- c(Q='Flow (hm3/d)', L='Load (kg/d)', C='FWM Conc (ppb)')
period_labels <- c(P2002='WY2002-2014', P2010='WY2010-2014')

# compute total subbasin areas of junction stations
subbasin_area <- select(subbasin_area, SITE_NAME, AREA_KM2) %>%
  mutate(GROUP=1) %>%
  spread(SITE_NAME, AREA_KM2) %>%
  mutate('Godowa+Sycan'=Godowa+Sycan,
         'SF+NF'=SF+NF,
         'SF_Ivory+NF_Ivory'=SF_Ivory+NF_Ivory) %>%
  gather(SITE_NAME, TOTAL_AREA_KM2, -GROUP) %>%
  select(-GROUP)

# load results ----
df_site <- loads_df[['site']] %>%
  filter(DATASET=="POR",
         SITE_NAME %in% subbasin_area$SITE_NAME) %>%
  select(-DATASET, -START_DATE, -END_DATE, -N_DAY, -N_YEAR) %>%
  droplevels() %>%
  mutate(PERIOD=plyr::revalue(PERIOD, c("2002-2014"="P2002",
                                        "2010-2014"="P2010",
                                        "2011-2014"="P2010")),
         PERIOD=as.character(PERIOD)) %>%
  filter(!(SITE_NAME=="SF+NF" & PERIOD=="P2010"))

df_wyr <- loads_df[['wyr']] %>%
  filter(DATASET=="POR",
         SITE_NAME %in% subbasin_area$SITE_NAME) %>%
  mutate(PERIOD=ifelse(WYEAR<2010, "P2002", "P2010")) %>%
  filter(!(SITE_NAME=="SF+NF" & WYEAR >= 2010)) %>%
  select(-DATASET, -DATE, -N_DAY) %>%
  droplevels()

network <- mutate(network,
                  PERIOD=plyr::revalue(DATASET, c("POR"="P2002",
                                                  "RECENT"="P2010")),
                  PERIOD=as.character(PERIOD),
                  FROM=as.character(FROM),
                  TO=as.character(TO)) %>%
  select(-DATASET)

df_segments <- left_join(network, mutate(df_site, SITE_NAME=as.character(SITE_NAME)),
                         by=c(FROM='SITE_NAME', PERIOD='PERIOD')) %>%
  left_join(mutate(df_site, SITE_NAME=as.character(SITE_NAME)),
            by=c(TO='SITE_NAME', 'PERIOD', 'SEASON', 'VAR', 'TERM')) %>%
#   mutate(TO=ordered(TO, levels=levels(df_site$SITE_NAME)),
#          FROM=ordered(FROM, levels=levels(df_site$SITE_NAME))) %>%
  rename(VALUE.FROM=VALUE.x,
         AREA_KM2.FROM=AREA_KM2.x,
         VALUE.TO=VALUE.y,
         AREA_KM2.TO=AREA_KM2.y) %>%
  droplevels

df_segments_wyr <- left_join(network, df_wyr, by=c(FROM='SITE_NAME', PERIOD='PERIOD')) %>%
  left_join(df_wyr, by=c(TO='SITE_NAME', 'SEASON', 'VAR', 'TERM', 'WYEAR', 'PERIOD')) %>%
  rename(VALUE.FROM=VALUE.x,
         AREA_KM2.FROM=AREA_KM2.x,
         VALUE.TO=VALUE.y,
         AREA_KM2.TO=AREA_KM2.y) %>%
  droplevels

# cumul-area loads detail pdfs ----
dataset <- 'POR'
season <- 'Annual'

if (!file.exists(file.path('pdf', tolower(dataset), 'loads-network'))) {
  dir.create(file.path('pdf', tolower(dataset), 'loads-network'))
}

for (season in unique(as.character(df_wyr$SEASON))) {
  season_label <- str_split(season, "[ ]")[[1]][1]
  filename <- file.path('pdf', tolower(dataset), 'loads-network',
                        paste0('loads-network-term-', tolower(season_label), '.pdf'))
  cat('Printing:', filename, '\n')
  pdf(filename, width=17, height=11)

  for (term in c('Q', 'L', 'C')) {
    cat('..', term, '\n')
    p <- filter(df_wyr, TERM==term, SEASON==season) %>%
      ggplot() +
      geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
      geom_segment(aes(x=AREA_KM2.FROM, xend=AREA_KM2.TO, y=VALUE.FROM, yend=VALUE.TO, size=MAINSTEM),
                   data=filter(df_segments_wyr, TERM==term, SEASON==season),
                   alpha=0.5) +
      geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME), size=3) +
      facet_grid(VAR~WYEAR, scales='free_y') +
      scale_color_discrete('') +
      scale_size_manual(guide=FALSE, values=c('FALSE'=0.5, 'TRUE'=1)) +
      scale_x_continuous(label=scales::comma) +
      scale_y_continuous(label=scales::comma) +
      labs(x='Cumulative Drainage Area (km2)', y=term_labels[[term]],
           title=paste0('Annual Mean vs Cumulative Drainage Area\n',
                        'Season: ', season,
                        '  |  Term: ', term_labels[[term]])) +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
            aspect.ratio=1,
            strip.background=element_blank(),
            strip.text=element_text(face='bold'))
    print(p)
  }
  dev.off()
}


dataset <- 'POR'
for (period in c("P2002", "P2010")) {
  filename <- file.path('pdf', tolower(dataset), 'loads-network',
                        paste0('loads-network-season-', period_labels[[period]],'.pdf'))
  cat('Printing:', filename, '\n')

  pdf(filename, height=11, width=8.5)

  for (term in c('Q', 'L', 'C')) {
    p <- filter(df_site, PERIOD==period, TERM==term) %>%
      ggplot() +
      geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
      geom_segment(aes(x=AREA_KM2.FROM, xend=AREA_KM2.TO, y=VALUE.FROM, yend=VALUE.TO, size=MAINSTEM),
                   data=filter(df_segments, PERIOD==period, TERM==term),
                   alpha=0.5) +
      geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME), size=3) +
      facet_grid(VAR~SEASON, scales='free_y') +
      scale_color_discrete('') +
      scale_size_manual(guide=FALSE, values=c('FALSE'=0.5, 'TRUE'=1)) +
      scale_x_continuous(label=scales::comma) +
      scale_y_continuous(label=scales::comma) +
      labs(x='Cumulative Drainage Area (km2)', y=term_labels[[term]],
           title=paste0('Annual and Seasonal Value vs Cumulative Drainage Area\n',
                        'Period: ', period_labels[[period]],
                        '  |  Term: ', term_labels[[term]])) +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
            aspect.ratio=1,
            strip.background=element_blank(),
            strip.text=element_text(face='bold'),
            strip.text.x=element_text(size=8))
    print(p)
  }
  dev.off()
}

# report ----
filename <- 'report/results-load-network-tp-WY2010-2014.png'
cat('Saving report figure to:', filename, '\n')
png(filename, width=10, height=5, res=200, units='in')
p <- filter(df_site, PERIOD=='P2010', VAR %in% c('TP', 'FLOW'),
            TERM %in% c('C', 'L', 'Q'),
            SITE_NAME != 'SF+NF') %>%
  ggplot() +
  geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
  geom_segment(aes(x=AREA_KM2.FROM, xend=AREA_KM2.TO,
                   y=VALUE.FROM, yend=VALUE.TO,
                   size=MAINSTEM),
               data=filter(df_segments, PERIOD=='P2010', VAR %in% c('TP', 'FLOW'),
                           TERM %in% c('C', 'L', 'Q')),
               alpha=0.5) +
  geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME), size=3) +
  facet_grid(TERM~SEASON, scales='free_y') +
  scale_color_discrete('') +
  scale_size_manual(guide=FALSE, values=c('FALSE'=0.5, 'TRUE'=1)) +
  scale_x_continuous(labels=scales::comma) +
  labs(x='Cumulative Drainage Area (km2)',
       y=paste(c('TP Conc (ppb)', 'TP Load (kg/d)', 'Flow (hm3/d)'),
               collapse='     ')) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        aspect.ratio=1,
        strip.background=element_blank(),
        strip.text.y=element_blank(),
        strip.text.x=element_text(face="bold"))
print(p)
dev.off()

filename <- 'report/results-load-network-all-conc-WY2010-2014.png'
cat('Saving report figure to:', filename, '\n')
png(filename, height=10, width=10, res=200, units='in')
p <- filter(df_site, PERIOD=='P2010', TERM=='C',
            SITE_NAME != 'SF+NF') %>%
  ggplot() +
  geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
  geom_segment(aes(x=AREA_KM2.FROM, xend=AREA_KM2.TO, y=VALUE.FROM, yend=VALUE.TO, size=MAINSTEM),
               data=filter(df_segments, PERIOD=='P2010', TERM=='C'),
               alpha=0.5) +
  geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME), size=3) +
  facet_grid(VAR~SEASON, scales='free_y') +
  scale_color_discrete('') +
  scale_size_manual(guide=FALSE, values=c('FALSE'=0.5, 'TRUE'=1)) +
  scale_x_continuous(labels=scales::comma) +
  scale_y_continuous(labels=scales::comma) +
  labs(x='Cumulative Drainage Area (km2)', y='Concentration (ppb)') +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        aspect.ratio=1,
        strip.background=element_blank(),
        strip.text=element_text(face="bold"))
print(p)
dev.off()