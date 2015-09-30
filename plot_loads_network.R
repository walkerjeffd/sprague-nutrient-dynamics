library(dplyr)
library(tidyr)
library(lubridate)
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
term_labels <- c(Q='Flow (hm3/d)', L='Load (kg/d)', C='Conc (ppb)')
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
  # convert load/flow to daily
  mutate(VALUE=ifelse(TERM != 'C', VALUE*N_YEAR/N_DAY, VALUE)) %>%
  select(-DATASET, -START_DATE, -END_DATE, -N_DAY, -N_YEAR) %>%
  droplevels() %>%
  mutate(PERIOD=plyr::revalue(PERIOD, c("2002-2014"="P2002",
                                        "2010-2014"="P2010",
                                        "2011-2014"="P2010")),
         PERIOD=as.character(PERIOD))

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

# df_segments_wyr <- left_join(network, df_wyr_area, by=c(FROM='SITE_NAME', DATASET='DATASET')) %>%
#   left_join(df_wyr_area, by=c(TO='SITE_NAME', 'EXTENT', 'DATASET', 'SEASON', 'VAR', 'TERM', 'WYEAR', 'N.MONTH', 'N.DAY', 'LANDUSE')) %>%
#   mutate(TO=ordered(TO, levels=levels(df_site$SITE_NAME)),
#          FROM=ordered(FROM, levels=levels(df_site$SITE_NAME))) %>%
#   rename(VALUE.FROM=VALUE.x,
#          AREA_KM2.FROM=AREA_KM2.x,
#          AREA_KM2_BASIN.FROM=AREA_KM2_BASIN.x,
#          TOTAL_AREA_KM2.FROM=TOTAL_AREA_KM2.x,
#          VALUE.TO=VALUE.y,
#          AREA_KM2.TO=AREA_KM2.y,
#          AREA_KM2_BASIN.TO=AREA_KM2_BASIN.y,
#          TOTAL_AREA_KM2.TO=TOTAL_AREA_KM2.y) %>%
#   droplevels


# cumul-area loads detail pdfs ----
# dataset <- 'RECENT'
# variable <- 'TP'
# term <- 'C'
# season <- 'Annual'
# for (dataset in c("POR", "RECENT")) {
#   cat(dataset, '\n')
#   variables <- filter(df_mon, DATASET==dataset) %>% (function(x) unique(x$VAR))
#   for (variable in variables) {
#     filename <- file.path('pdf', tolower(dataset), 'loads-cumul-area',
#                           paste0('loads-cumul-area-', tolower(variable), '.pdf'))
#     cat('Printing:', filename, '\n')
#     cat('..', variable, '\n')
#     pdf(filename, width=17, height=11)
#
#     # TERM vs WYEAR by WYEAR
#     p1 <- filter(df_wyr_area, DATASET==dataset, VAR==variable, SEASON==season, EXTENT=='basin', LANDUSE=='Total') %>%
#       filter(SITE_NAME %in% stn_primary[[dataset]]) %>%
#       ggplot() +
#       geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
#       geom_segment(aes(x=AREA_KM2.FROM, xend=AREA_KM2.TO, y=VALUE.FROM, yend=VALUE.TO, size=MAINSTEM),
#                    data=filter(df_segments_wyr, DATASET==dataset, VAR==variable, SEASON==season, LANDUSE=='Total', EXTENT=='basin'),
#                    alpha=0.5) +
#       geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME), size=3) +
#       facet_grid(TERM~WYEAR, scales='free_y') +
#       scale_size_manual(guide=FALSE, values=c('FALSE'=0.5, 'TRUE'=1)) +
#       scale_color_discrete('') +
#       labs(x='Total Cumulative Drainage Area (km2)', y=paste0('Flow (hm3/d) / Load (kg/d) / Conc (ppb)'),
#            title=paste0('Annual Mean by Water Year\nDataset: ', dataset, ' | Period: ', periods[[dataset]], ' | Variable: ', variable)) +
#       theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
#             aspect.ratio=1)
#     # print(p1)
#
#     # SEASON vs TERM by SITE
#     p2 <- filter(df_site_area, DATASET==dataset, VAR==variable, EXTENT=='basin', LANDUSE=='Total') %>%
#       filter(SITE_NAME %in% stn_primary[[dataset]]) %>%
#       ggplot() +
#       geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
#       geom_segment(aes(x=AREA_KM2.FROM, xend=AREA_KM2.TO, y=VALUE.FROM, yend=VALUE.TO, size=MAINSTEM),
#                    data=filter(df_segments_site, DATASET==dataset, VAR==variable, EXTENT=='basin', LANDUSE=='Total'),
#                    alpha=0.5) +
#       geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME), size=3) +
#       facet_grid(TERM~SEASON, scales='free_y') +
#       scale_color_discrete('') +
#       scale_size_manual(guide=FALSE, values=c('FALSE'=0.5, 'TRUE'=1)) +
#       labs(x='Cumulative Drainage Area (km2)', y=paste0('Flow (hm3/d) / Load (kg/d) / Conc (ppb)'),
#            title=paste0('Seasonal Mean by Site\nDataset: ', dataset,' | Period: ', periods[[dataset]],  ' | Variable: ', variable)) +
#       theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
#             aspect.ratio=1)
#     # print(p2)
#     grid.arrange(grobs=list(p1, p2), nrow=2)
#
#     # SEASON vs WYEAR by WYEAR
#     for (term2 in c('C', 'L', 'Q')) {
#       if (term2 == "Q") {
#         ylabel <- 'Flow (hm3/d)'
#       } else {
#         ylabel <- paste0(variable, " ", term_labels[[term2]])
#       }
#       p3 <- filter(df_wyr_area, DATASET==dataset, VAR==variable, TERM==term2, EXTENT=='basin', LANDUSE=='Total') %>%
#         filter(SITE_NAME %in% stn_primary[[dataset]]) %>%
#         ggplot() +
#         geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
#         geom_segment(aes(x=AREA_KM2.FROM, xend=AREA_KM2.TO, y=VALUE.FROM, yend=VALUE.TO, size=MAINSTEM),
#                      data=filter(df_segments_wyr, DATASET==dataset, VAR==variable, TERM==term2, EXTENT=='basin', LANDUSE=='Total'),
#                      alpha=0.5) +
#         geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME), size=3) +
#         facet_grid(SEASON~WYEAR, scales='free_y') +
#         scale_size_manual(guide=FALSE, values=c('FALSE'=0.5, 'TRUE'=1)) +
#         scale_color_discrete('') +
#         labs(x='Cumulative Drainage Area (km2)', y=ylabel,
#              title=paste0('Seasonal Mean by Water Year\nDataset: ', dataset,' | Period: ', periods[[dataset]],  ' | Variable: ', ylabel)) +
#         theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
#               aspect.ratio=1)
#
#       if (dataset == "POR") {
#         p3 <- p3 + theme(strip.text.y=element_text(size=8))
#       }
#
#       print(p3)
#     }
#
#     dev.off()
#   }
# }

# cumul-area summary pdfs ----
dataset <- 'POR'
for (period in c("P2002", "P2010")) {
  filename <- file.path('pdf', tolower(dataset), paste0('loads-network-', period_labels[[period]],'.pdf'))
  cat('Printing:', filename, '\n')

  pdf(filename, width=11, height=8.5)
  for (term in c('C', 'L')) {
    p <- filter(df_site, PERIOD==period, TERM==term,
                VAR %in% c('TP', 'PO4', 'PP', 'TN', 'TSS')) %>%
      ggplot() +
      geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
      geom_segment(aes(x=AREA_KM2.FROM, xend=AREA_KM2.TO, y=VALUE.FROM, yend=VALUE.TO, size=MAINSTEM),
                   data=filter(df_segments, PERIOD==period, TERM==term,
                               VAR %in% c('TP', 'PO4', 'PP', 'TN', 'TSS')),
                   alpha=0.5) +
      geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME), size=3) +
      facet_grid(VAR~SEASON, scales='free_y') +
      scale_color_discrete('') +
      scale_size_manual(guide=FALSE, values=c('FALSE'=0.5, 'TRUE'=1)) +
      labs(x='Cumulative Drainage Area (km2)', y=term_labels[[term]],
           title=paste0('Annual and Seasonal ', term_labels[[term]], ' vs Cumulative Drainage Area\nPeriod: ', period_labels[[period]])) +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
            aspect.ratio=1)
    print(p)
  }
  term <- 'Q'
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
    labs(x='Cumulative Drainage Area (km2)', y=term_labels[[term]],
         title=paste0('Annual and Seasonal ', term_labels[[term]], ' vs Cumulative Drainage Area\nPeriod: ', period_labels[[period]])) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
          aspect.ratio=1)
  print(p)
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