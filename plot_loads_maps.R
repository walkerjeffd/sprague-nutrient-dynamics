library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(scales)
library(fluxr)
library(maptools)
library(gpclib)
library(sp)
gpclibPermit()
theme_set(theme_bw())

rm(list=ls())

source('functions.R')

# load data ----
load('kt_sprague.Rdata')
load('loads.Rdata')
load('gis.Rdata')

df_wyr <- loads_df$wyr %>%
  mutate(VALUE=ifelse(TERM %in% c('Q', 'Q_AREA', 'L', 'L_AREA'),
                      VALUE*365.25, VALUE))
df_site <- loads_df$site %>%
  mutate(PERIOD=plyr::revalue(PERIOD, c('2011-2014'='P2010',
                                        '2010-2014'='P2010',
                                        '2002-2014'='P2002'))) %>%
  mutate(VALUE=ifelse(TERM %in% c('Q', 'Q_AREA', 'L', 'L_AREA'),
                      VALUE*365.25, VALUE))

stn <- left_join(subbasin_area, select(stn.kt_sprague, SITE_NAME, LAT, LON), by="SITE_NAME")

dataset_levels <- "POR"
subbasin_levels <- list(P2010=levels(subbasin_area$SITE_NAME),
                        P2002=setdiff(levels(subbasin_area$SITE_NAME), c("NF_Ivory", "SF_Ivory")))
incbasin_levels <- list(P2010=setdiff(levels(incbasin_area$INC_SITE_NAME), c("Godowa-SF-NF")),
                        P2002=setdiff(levels(incbasin_area$INC_SITE_NAME),
                                      c("Godowa-SF_Ivory-NF_Ivory", "SF_Ivory-SF", "NF_Ivory-NF")))
wyears_levels <- list(P2010=c(2010, 2014),
                      P2002=c(2002, 2014))

# separate gis by dataset
stn <- list(P2010=filter(stn, SITE_NAME %in% subbasin_levels[['P2010']]),
            P2002=filter(stn, SITE_NAME %in% subbasin_levels[['P2002']]))
subbasin <- list(P2010=subbasin,
                 P2002=filter(subbasin, !(SITE_NAME %in% c('SF_Ivory', 'NF_Ivory'))))
incbasin <- list(P2010=filter(incbasin,
                              INC_SITE_NAME %in% incbasin_levels[['P2010']]),
                 P2002=filter(incbasin,
                              INC_SITE_NAME %in% incbasin_levels[['P2002']]))

scale_fill_term <- list(Q=scale_fill_gradientn('Flow\n(hm3/yr)', colours=RColorBrewer::brewer.pal(n=6, name="Blues"), limits=c(0,NA)),
                        L=scale_fill_gradientn('Load\n(kg/yr)', colours=RColorBrewer::brewer.pal(n=6, name="Greens"), limits=c(0,NA)),
                        Q_AREA=scale_fill_gradientn('Flow per Area\n(cm/yr)', colours=RColorBrewer::brewer.pal(n=6, name="Blues"), limits=c(0,NA)),
                        L_AREA=scale_fill_gradientn('Load per Area\n(kg/km2/yr)', colours=RColorBrewer::brewer.pal(n=6, name="Greens"), limits=c(0,NA)),
                        C=scale_fill_gradientn('FWM Conc\n(ppb)', colours=RColorBrewer::brewer.pal(n=6, name="YlOrRd"), limits=c(0,NA)))

scale_fill_term_inc <- list(
  Q=scale_fill_gradient2('Net Flow (hm3/yr)',
                         high="#08519C", mid='white', low='black',
                         space='Lab'),
  L=scale_fill_gradient2('Net Load\n(kg/yr)',
                         high="#006D2C", mid='white', low='black',
                         space='Lab'),
  Q_AREA=scale_fill_gradient2('Net Flow per\nArea (cm/yr)',
                              high="#08519C", mid='white', low='black',
                              space='Lab'),
  L_AREA=scale_fill_gradient2('Net Load per\nArea (kg/m2/yr)',
                              high="#006D2C", mid='white', low='black',
                              space='Lab'),
  C=scale_fill_gradient2('Change in\nFWM Conc (ppb)',
                         high='orangered', mid='white', low='black',
                         space='Lab')
)

# subbasin functions ----
map_subbasin <- function(dataset, period, season, variable, term, title=NULL) {
  ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
    geom_polygon(aes(x = long, y = lat, group = group),
                 data = incbasin[[period]],
                 color = 'grey50', fill = NA, size = 0.2) +
    geom_polygon(aes(x = long, y = lat, fill = VALUE),
                 data = subbasin[[period]] %>%
                   left_join(filter(df_site, PERIOD==period, SEASON==season, DATASET==dataset, VAR==variable, TERM==term, SITE_NAME %in% subbasin_levels[[period]]),
                             by='SITE_NAME') %>%
                   mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=subbasin_levels[[period]])),
                 colour = 'black', size = 0.2) +
    geom_polygon(aes(x = long, y = lat, group = group), data = basin,
                 color = 'black', fill = NA, size = 0.2) +
    geom_point(aes(x = LON, y = LAT), data = stn[[period]], fill = 'deepskyblue', pch = 21, color = 'black', size = 3) +
    scale_fill_term[[term]] +
    facet_wrap(~SITE_NAME, nrow=2) +
    ggtitle(title) +
    theme(strip.background=element_blank(),
          strip.text.x=element_text(face='bold'))
}
# map_subbasin(dataset='POR', period='P2002', season='Annual', variable='FLOW', term='Q_AREA')
# map_subbasin(dataset='POR', period='P2002', season='Annual', variable='TP', term='C')
# map_subbasin(dataset='POR', period='P2002', season='Summer (Jul-Sep)', variable='TP', term='C')

bar_subbasin <- function(dataset, period, season, variable, term, title=NULL) {
  p <- filter(df_site, PERIOD==period, SEASON==season, DATASET==dataset, VAR==variable, TERM==term, SITE_NAME %in% subbasin_levels[[period]]) %>%
    mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=rev(levels(SITE_NAME)))) %>%
    ggplot(aes(x=SITE_NAME, y=VALUE, fill=VALUE)) +
    geom_bar(stat='identity', color='grey50') +
    labs(x='', y=scale_fill_term[[term]]$name) +
    scale_fill_term[[term]] +
    coord_flip() +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
    ggtitle(title)
  p
}
# bar_subbasin(dataset='POR', period='P2002', season='Annual', variable='TP', term='L_AREA')
# bar_subbasin(dataset='POR', period='P2010', season='Annual', variable='TP', term='C')

tile_subbasin <- function(dataset, period, season, variable, term, title=NULL) {
  x <- filter(df_wyr,
              DATASET==dataset, VAR==variable, TERM==term,
              WYEAR >= wyears_levels[[period]][1],
              WYEAR <= wyears_levels[[period]][2],
              SEASON == season,
              SITE_NAME %in% subbasin_levels[[period]]) %>%
    mutate(SITE_NAME=ordered(as.character(SITE_NAME),
                             levels=rev(levels(SITE_NAME))))
  p <- x %>%
    ggplot(aes(x=factor(WYEAR), y=SITE_NAME)) +
    geom_tile(aes(fill=VALUE)) +
    scale_fill_term[[term]] +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    labs(x='Water Year', y='') +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
    ggtitle(title)
  p
}
# tile_subbasin(dataset='POR', period='P2002', season='Annual', variable='TP', term='L_AREA')
# tile_subbasin(dataset='POR', period='P2010', season='Summer (Jul-Sep)', variable='TP', term='C')

dash_subbasin <- function(dataset, period, season, variable, term, title=NULL) {
  p.map <- map_subbasin(dataset=dataset, period=period, season=season, variable=variable, term=term, title=title)
  p.bar <- bar_subbasin(dataset=dataset, period=period, season=season, variable=variable, term=term)
  p.tile <- tile_subbasin(dataset=dataset, period=period, season=season, variable=variable, term=term)
  grid.arrange(p.map, arrangeGrob(p.bar, p.tile, ncol=2), heights=c(2/3, 1/3), ncol=1)
}
# dash_subbasin('POR', 'P2002', 'Annual', 'FLOW', 'Q_AREA',
#               paste0('Mean Annual Flow per Unit Area', '   |   ', 'Dataset: POR   |   Variable: Flow\n'))
# dash_subbasin('POR', 'P2002', 'Annual', 'TP', 'L',
#               paste0('Mean Annual Load', '   |   ', 'Dataset: POR   |   Variable: TP\n'))
# dash_subbasin('POR', 'P2002', 'Annual', 'TP', 'L_AREA',
#               paste0('Mean Annual Load per Unit Area', '   |   ', 'Dataset: POR   |   Variable: TP\n'))
# dash_subbasin('POR', 'P2002', 'Annual', 'TP', 'C',
#               paste0('Mean Annual FWM Concentration', '   |   ', 'Dataset: POR   |   Variable: TP\n'))
# dash_subbasin('POR', 'P2010', 'Annual', 'TP', 'C',
#               paste0('Mean Annual FWM Concentration', '   |   ', 'Dataset: POR   |   Variable: TP\n'))

map_incbasin <- function(dataset, period, season, variable, term, title=NULL) {
  if (term == 'C') {
    p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
      geom_polygon(aes(x = long, y = lat, group = group),
                   data = incbasin[[period]],
                   color = 'grey50', fill = NA, size = 0.2) +
      geom_polygon(aes(x = long, y = lat, fill = VALUE, group=id),
                   data = filter(incbasin[[period]], !(INC_SITE_NAME %in% c('Sycan', 'NF', 'SF'))) %>%
                     left_join(filter(df_site, DATASET==dataset,
                                      PERIOD==period, SEASON==season,
                                      VAR==variable, TERM==term,
                                      SITE_NAME %in% incbasin_levels[[period]]),
                               by=c('INC_SITE_NAME'='SITE_NAME')),
                   colour = 'black', size = 0.2) +
      geom_polygon(aes(x = long, y = lat, group = group), data = basin,
                   color = 'black', fill = NA, size = 0.2) +
      geom_point(aes(x = LON, y = LAT), data = stn[[period]], fill = 'deepskyblue', pch = 21, color = 'black', size = 3) +
      geom_text(aes(x = long, y = lat, label = INC_SITE_NAME),
                data=incbasin[[period]] %>%
                  mutate(INC_SITE_NAME=plyr::revalue(INC_SITE_NAME, incbasin_names)) %>%
                  group_by(INC_SITE_NAME) %>%
                  summarise(long=mean(c(min(long), max(long))),
                            lat=mean(c(min(lat), max(lat)))),
                fontface='bold', size=3) +
      scale_fill_term_inc[[term]] +
      ggtitle(title)
  } else {
    p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
      geom_polygon(aes(x = long, y = lat, group = group), data = incbasin[[period]],
                   color = 'grey50', fill = NA, size = 0.2) +
      geom_polygon(aes(x = long, y = lat, fill = VALUE, group=id),
                   data = incbasin[[period]] %>%
                     left_join(filter(df_site,
                                      DATASET==dataset, VAR==variable, TERM==term,
                                      PERIOD==period, SEASON==season,
                                      SITE_NAME %in% incbasin_levels[[period]]),
                               by=c('INC_SITE_NAME'='SITE_NAME')),
                   colour = 'black', size = 0.2) +
      geom_polygon(aes(x = long, y = lat, group = group), data = basin,
                   color = 'black', fill = NA, size = 0.2) +
      geom_point(aes(x = LON, y = LAT), data = stn[[period]], fill = 'deepskyblue', pch = 21, color = 'black', size = 3) +
      geom_text(aes(x = long, y = lat, label = INC_SITE_NAME),
                data=incbasin[[period]] %>%
                  mutate(INC_SITE_NAME=plyr::revalue(INC_SITE_NAME, incbasin_names)) %>%
                  group_by(INC_SITE_NAME) %>%
                  summarise(long=mean(c(min(long), max(long))),
                            lat=mean(c(min(lat), max(lat)))),
                fontface='bold', size=3) +
      scale_fill_term_inc[[term]] +
      ggtitle(title)
  }
  p <- p + theme(strip.background=element_blank(),
                 strip.text.x=element_text(face='bold'))
  p <- p + geom_text(x=-120.61, y=42.17,
                     label='Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.',
                     size=3,
                     hjust=1,
                     color='grey50')
  p
}
# map_incbasin(dataset='POR', period='P2002', season='Annual',
#              variable='FLOW', term='Q_AREA',
#              title=paste0('Mean Annual Flow per Unit Area', '   |   ',
#                           'Dataset: POR   |   Variable: Flow\n'))
# map_incbasin(dataset='POR', period='P2010', season='Annual',
#              variable='FLOW', term='Q_AREA',
#              title=paste0('Mean Annual Flow per Unit Area', '   |   ',
#                           'Dataset: POR   |   Variable: Flow\n'))
# map_incbasin(dataset='POR', period='P2010', season='Annual',
#              variable='TP', term='C')

bar_incbasin <- function(dataset, period, season, variable, term, title=NULL) {
  x <- filter(df_site, DATASET==dataset, PERIOD==period, SEASON==season,
              VAR==variable, TERM==term, SITE_NAME %in% incbasin_levels[[period]]) %>%
    mutate(SITE_NAME=plyr::revalue(SITE_NAME, incbasin_names)) %>%
    mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=rev(levels(SITE_NAME))))
  if (term == 'C') {
    x <- filter(x, !(SITE_NAME %in% c('Upper SF', 'Upper NF', 'Sycan')))
  }
  p <- x %>%
    ggplot(aes(x=SITE_NAME, y=VALUE, fill=VALUE)) +
    geom_bar(stat='identity', color='grey50') +
    geom_hline(yint=0, color='grey50') +
    labs(x='', y=scale_fill_term[[term]]$name) +
    scale_fill_term_inc[[term]] +
    coord_flip() +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
    ggtitle(title)
  p
}
# bar_incbasin(dataset='POR', period='P2002', season='Annual', variable='TP', term='L_AREA')
# bar_incbasin(dataset='POR', period='P2010', season='Annual', variable='TP', term='C')

tile_incbasin <- function(dataset, period, season, variable, term, title=NULL) {
  x <- filter(df_wyr, DATASET==dataset, VAR==variable, SEASON==season,
              WYEAR >= wyears_levels[[period]][1],
              WYEAR <= wyears_levels[[period]][2],
              TERM==term, SITE_NAME %in% incbasin_levels[[period]]) %>%
    mutate(SITE_NAME=plyr::revalue(SITE_NAME, incbasin_names)) %>%
    mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=rev(levels(SITE_NAME))))

  if (term == 'C') {
    x <- filter(x, !(SITE_NAME %in% c('Upper SF', 'Upper NF', 'Sycan')))
  }

  p <- ggplot(x, aes(x=factor(WYEAR), y=SITE_NAME)) +
    geom_tile(aes(fill=VALUE)) +
    scale_fill_term_inc[[term]] +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_discrete(expand=c(0,0)) +
    labs(x='Water Year', y='') +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
    ggtitle(title)
  p
}
# tile_incbasin(dataset='POR', period='P2002', season='Annual', variable='TP', term='L_AREA')

dash_incbasin <- function(dataset, period, season, variable, term, title=NULL) {
  p.map <- map_incbasin(dataset=dataset, period=period, season=season, variable=variable, term=term, title=title)
  p.bar <- bar_incbasin(dataset=dataset, period=period, season=season, variable=variable, term=term)
  p.tile <- tile_incbasin(dataset=dataset, period=period, season=season, variable=variable, term=term)
    # theme(plot.margin = grid::unit(c(1,0,1,0), "cm"))
  grid.arrange(grobs=list(p.map, arrangeGrob(p.bar, p.tile, ncol=2)),
               heights=c(2/3, 1/3), ncol=1)
  # makeFootnote('Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.')
}
# dash_incbasin('POR', 'P2002', 'Annual', 'FLOW', 'Q_AREA',
#               paste0('Mean Annual Flow per Unit Area', '   |   ', 'Dataset: POR   |   Variable: Flow\n'))
# dash_incbasin('POR', 'P2010', 'Annual', 'FLOW', 'Q_AREA',
#               paste0('Mean Annual Flow per Unit Area', '   |   ', 'Dataset: POR   |   Variable: Flow\n'))
# dash_incbasin('POR', 'P2010', 'Annual', 'TP', 'C',
#               paste0('Mean Annual FWM Concentration', '   |   ', 'Dataset: POR   |   Variable: TP\n'))
# dash_incbasin('POR', 'P2002', 'Annual', 'TP', 'C',
#               paste0('Mean Annual FWM Concentration', '   |   ', 'Dataset: POR   |   Variable: TP\n'))


# pdf ----
dataset <- 'POR'
if (!file.exists(file.path('pdf', tolower(dataset), 'loads-maps-cumul-basin'))) {
  dir.create(file.path('pdf', tolower(dataset), 'loads-maps-cumul-basin'))
}
if (!file.exists(file.path('pdf', tolower(dataset), 'loads-maps-incr-basin'))) {
  dir.create(file.path('pdf', tolower(dataset), 'loads-maps-incr-basin'))
}

dataset <- 'POR'
season <- 'Annual'
for (period in c('P2002', 'P2010')) {
  if (!file.exists(file.path('pdf', tolower(dataset), 'loads-maps-cumul-basin', period))) {
    dir.create(file.path('pdf', tolower(dataset), 'loads-maps-cumul-basin', period))
  }
  if (!file.exists(file.path('pdf', tolower(dataset), 'loads-maps-incr-basin', period))) {
    dir.create(file.path('pdf', tolower(dataset), 'loads-maps-incr-basin', period))
  }

  cat(period, '\n')
  cat('.. SUBBASINS', '\n')
  variables <- setdiff(filter(df_site, PERIOD==period)$VAR %>% as.character %>% unique, 'FLOW')

#   for (variable in variables) {
#     if (variable == 'TSS') {
#       period_label <- 'WY2011-2014'
#     } else {
#       period_label <- paste0('WY', paste0(wyears_levels[[period]], collapse='-'))
#     }
#
#     filename <- file.path('pdf', tolower(dataset), 'loads-maps-cumul-basin', period,
#                           paste0('loads-cumul-basin-', period_label, '-', tolower(variable), '.pdf'))
#     cat('Printing:', filename, '\n')
#     pdf(filename, width=11, height=8.5)
#     dash_subbasin(dataset, period, season, variable, 'L_AREA',
#                   paste0('Mean Annual Load per Unit Area\n',
#                          'Period: ', period_label,
#                          '   |   Season: ', season,
#                          '   |   Variable: ', variable,
#                          '   |   Term: ', 'Load per Unit Area', '\n'))
#     dash_subbasin(dataset, period, season, variable, 'C',
#                   paste0('Mean Annual FWM Concentration\n',
#                          'Period: ', period_label,
#                          '   |   Season: ', season,
#                          '   |   Variable: ', variable,
#                          '   |   Term: ', 'FWM Concentration', '\n'))
#     dev.off()
#     Sys.sleep(2)
#   }
#
#   variable <- 'Flow'
#   filename <- file.path('pdf', tolower(dataset), 'loads-maps-cumul-basin', period,
#                         paste0('loads-cumul-basin-', period_label, '-', tolower(variable), '.pdf'))
#   cat('Printing:', filename, '\n')
#   pdf(filename, width=11, height=8.5)
#   dash_subbasin(dataset, period, season, 'FLOW', 'Q_AREA',
#                 paste0('Mean Annual Flow per Unit Area\n',
#                        'Period: ', period_label,
#                        '   |   Season: ', season,
#                        '   |   Variable: ', variable,
#                        '   |   Term: ', 'Flow per Unit Area', '\n'))
#   dev.off()

  cat('.. INCBASINS', '\n')
  for (variable in variables) {
    if (variable == 'TSS') {
      period_label <- 'WY2011-2014'
    } else {
      period_label <- paste0('WY', paste0(wyears_levels[[period]], collapse='-'))
    }
    filename <- file.path('pdf', tolower(dataset),
                          'loads-maps-incr-basin', period,
                          paste0('loads-incr-basin-', period_label, '-', tolower(variable), '.pdf'))
    cat('Printing:', filename, '\n')
    pdf(filename, width=11, height=8.5)
    dash_incbasin(dataset, period, season, variable, 'L_AREA',
                  paste0('Mean Annual Load per Unit Area\n',
                         'Period: ', period_label,
                         '   |   Season: ', season,
                         '   |   Variable: ', variable,
                         '   |   Term: ', 'Load per Unit Area', '\n'))
    dash_incbasin(dataset, period, season, variable, 'C',
                  paste0('Change in Annual FWM Concentration\n',
                         'Period: ', period_label,
                         '   |   Season: ', season,
                         '   |   Variable: ', variable,
                         '   |   Term: ', 'FWM Concentration', '\n'))
    dev.off()
    Sys.sleep(2)
  }

  variable <- 'Flow'
  filename <- file.path('pdf', tolower(dataset),
                        'loads-maps-incr-basin', period,
                        paste0('loads-incr-basin-', period_label, '-', tolower(variable), '.pdf'))
  cat('Printing:', filename, '\n')
  pdf(filename, width=11, height=8.5)
  dash_incbasin(dataset, period, season, 'FLOW', 'Q_AREA',
                paste0('Mean Annual Flow per Unit Area\n',
                       'Period: ', period_label,
                       '   |   Season: ', season,
                       '   |   Variable: ', variable,
                       '   |   Term: ', 'Flow per Unit Area', '\n'))
  dev.off()
}

# summary maps
dataset <- 'POR'
season <- 'Annual'
for (period in c('P2002', 'P2010')) {
  term <- 'C'
  variables <- setdiff(droplevels(filter(df_site, PERIOD==period))$VAR %>% levels, 'FLOW')
  period_label <- paste0('WY', paste0(wyears_levels[[period]], collapse='-'))
  maps.c <- lapply(variables, function(variable) {
    p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
      geom_polygon(aes(x = long, y = lat, group = group),
                   data = incbasin[[period]],
                   color = 'grey50', fill = NA, size = 0.2) +
      geom_polygon(aes(x = long, y = lat, fill = VALUE, group=id),
                   data = filter(incbasin[[period]], !(INC_SITE_NAME %in% c("Sycan", "NF", "SF"))) %>%
                     left_join(filter(df_site, DATASET==dataset, PERIOD==period,
                                      SEASON==season, VAR==variable, TERM==term,
                                      SITE_NAME %in% incbasin_levels[[period]],
                                      !(SITE_NAME %in% c('NF', 'SF', 'Sycan'))),
                               by=c('INC_SITE_NAME'='SITE_NAME')),
                   colour = 'black', size = 0.2) +
      geom_polygon(aes(x = long, y = lat, group = group), data = basin,
                   color = 'black', fill = NA, size = 0.2) +
      geom_point(aes(x = LON, y = LAT), data = stn[[period]], fill = 'deepskyblue', pch = 21, color = 'black', size = 3) +
      scale_fill_gradient2('Change in\nFWM Conc (ppb)', high='orangered', mid='white', low='black', space='Lab') +
      ggtitle(variable) +
      theme(legend.text=element_text(size=8),
            legend.position='bottom',
            plot.margin=grid::unit(c(0.5, 0, 0.5, 0), 'lines'))
            # strip.text=element_text(size=12, face='bold'),
            # strip.background=element_blank())
    ggplotGrob(p)
  })

  term <- 'L_AREA'
  maps.l <- lapply(variables, function(variable) {
    x <- filter(df_site, DATASET==dataset,
                PERIOD==period, SEASON==season,
                VAR==variable, TERM==term,
                SITE_NAME %in% incbasin_levels[[period]])
    if (min(x$VALUE)<0) {
      color_limits <- c(NA, NA)
    } else {
      color_limits <- c(0, NA)
    }

    p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
      geom_polygon(aes(x = long, y = lat, group = group),
                   data = incbasin[[period]],
                   color = 'grey50', fill = NA, size = 0.2) +
      geom_polygon(aes(x = long, y = lat, fill = VALUE, group=id),
                   data = incbasin[[period]] %>%
                     left_join(x, by=c('INC_SITE_NAME'='SITE_NAME')) %>%
                     mutate(VAR=ordered(as.character(VAR), levels=levels(df_site$VAR)),
                            INC_SITE_NAME=ordered(as.character(INC_SITE_NAME),
                                                  levels=incbasin_levels[[period]])),
                   colour = 'black', size = 0.2) +
      geom_polygon(aes(x = long, y = lat, group = group), data = basin,
                   color = 'black', fill = NA, size = 0.2) +
      geom_point(aes(x = LON, y = LAT), data = stn[[period]],
                 fill = 'deepskyblue', pch = 21, color = 'black', size = 2) +
      scale_fill_gradient2('Net Load per\nArea (kg/m2/yr)',
                           high="#006D2C", mid='white', low='black',
                           space='Lab',
                           lim=color_limits)+
      ggtitle(variable) +
      theme(legend.text=element_text(size=8),
            legend.position='bottom',
            plot.margin=grid::unit(c(0.5, 0, 0.5, 0), 'lines'))
    ggplotGrob(p)
  })

  x.flow <- filter(df_site, DATASET==dataset, PERIOD==period,
                   SEASON==season, VAR=="FLOW", TERM=='Q_AREA',
                   SITE_NAME %in% incbasin_levels[[period]])
  if (min(x.flow$VALUE)<0) {
    color_limits <- c(NA, NA)
  } else {
    color_limits <- c(0, NA)
  }

  maps.q <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
      geom_polygon(aes(x = long, y = lat, group = group),
                   data = incbasin[[period]],
                   color = 'grey50', fill = NA, size = 0.2) +
      geom_polygon(aes(x = long, y = lat, fill = VALUE, group=id),
                   data = incbasin[[period]] %>%
                     left_join(x.flow, by=c('INC_SITE_NAME'='SITE_NAME')) %>%
                     mutate(INC_SITE_NAME=ordered(as.character(INC_SITE_NAME),
                                              levels=incbasin_levels[[period]])),
                   colour = 'black', size = 0.2) +
      geom_polygon(aes(x = long, y = lat, group = group), data = basin,
                   color = 'black', fill = NA, size = 0.2) +
      geom_point(aes(x = LON, y = LAT), data = stn[[period]],
                 fill = 'deepskyblue', pch = 21, color = 'black', size = 2) +
      scale_fill_gradient2('Net Flow per\nArea (cm/yr)',
                           high="#08519C", mid='white', low='black',
                           space='Lab', lim=color_limits) +
      facet_wrap(~VAR) +
      theme(legend.text=element_text(size=8),
            legend.position='bottom',
            plot.margin=grid::unit(c(0.5, 0, 0.5, 0), 'lines'),
            strip.text=element_text(size=12, face='bold'),
            strip.background=element_blank())

  filename <- file.path('pdf', tolower(dataset), paste0('loads-summary-maps-', period_label, '.pdf'))
  cat('Printing:', filename, '\n')
  pdf(filename, width=10, height=8)
  grid.arrange(grobs=maps.c, nrow=2,
               top=paste0('\nNet Change in FWM Concentration by Incremental Subbasin\n',
                          'Period: ', period_label))
  makeFootnote('Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.')
  grid.arrange(grobs=maps.l, nrow=2,
               top=paste0('\nNet Load per Unit Area by Incremental Subbasin\n',
                          'Period: ', period_label))
  makeFootnote('Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.')
  grid.arrange(grobs=arrangeGrob(maps.q, nrow=2, ncol=3),
               top=paste0('\nNet Flow per Unit Area by Incremental Subbasin\n',
                          'Period: ', period_label))
  makeFootnote('Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.')
  dev.off()
}

# report ----

# subbasin
png('report/results-load-map-subbasin-tp-conc.png', width=10, height=8, res=200, units='in')
dash_subbasin('POR', 'P2010', 'Annual', 'TP', 'C', '')
dev.off()

png('report/results-load-map-subbasin-tp-load.png', width=10, height=8, res=200, units='in')
dash_subbasin('POR', 'P2010', 'Annual', 'TP', 'L_AREA', '')
dev.off()

png('report/results-load-map-subbasin-flow.png', width=10, height=8, res=200, units='in')
dash_subbasin('POR', 'P2010', 'Annual', 'FLOW', 'Q_AREA', '')
dev.off()

# incbasin
png('report/results-load-map-incbasin-tp-conc.png', width=10, height=8, res=200, units='in')
dash_incbasin('POR', 'P2010', 'Annual', 'TP', 'C', '')
dev.off()

png('report/results-load-map-incbasin-tp-load.png', width=10, height=8, res=200, units='in')
dash_incbasin('POR', 'P2010', 'Annual', 'TP', 'L_AREA', '')
dev.off()

png('report/results-load-map-incbasin-flow.png', width=10, height=8, res=200, units='in')
dash_incbasin('POR', 'P2010', 'Annual', 'FLOW', 'Q_AREA', '')
dev.off()

