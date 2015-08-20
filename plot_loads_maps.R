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

df_wyr <- loads_df$wyr
df_site <- loads_df$site

stn <- select(stn.kt_sprague, SITE, SITE_NAME, LAT, LON) %>%
  arrange(SITE) %>%
  #   rbind(data.frame(SITE='WR1000', SITE_NAME='Sprague_Kirchers', LAT=42.567806, LON=-121.864472), .) %>%
  mutate(SITE=ordered(SITE, levels=SITE),
         SITE_NAME=ordered(SITE_NAME, levels=SITE_NAME))
stn <- left_join(stn, select(subbasin_area, SITE, AREA_KM2))

dataset_levels <- names(loads)
subbasin_levels <- levels(subbasin_area$SITE_NAME)
incbasin_levels <- list(RECENT=unique(incbasin_ivory_area$INC_SITE_NAME),
                        POR=unique(incbasin_area$INC_SITE_NAME))
#
# # compute junction flows and loads
# df <- filter(df_site, SITE_NAME %in% stn.kt_sprague$SITE_NAME, TERM %in% c('Q', 'L')) %>%
#   select(DATASET, VAR, SITE_NAME, TERM, VALUE) %>%
#   spread(SITE_NAME, VALUE) %>%
#   mutate(`Godowa+Sycan`=Godowa+Sycan,
#          `SF_Ivory+NF_Ivory`=SF_Ivory+NF_Ivory,
#          `SF+NF`=SF+NF) %>%
#   gather(SITE_NAME, VALUE, Power:`SF+NF`)
#
# # compute concentration
# df_flow <- filter(df, VAR=="FLOW") %>% select(-VAR) %>% spread(TERM, VALUE)
# df <- filter(df, VAR!="FLOW") %>%
#   spread(TERM, VALUE) %>%
#   left_join(df_flow, by=c("DATASET", "SITE_NAME")) %>%
#   mutate(C=L/Q) %>%
#   gather(TERM, VALUE, L:C)
#
# # compute net change
# df <- spread(df, SITE_NAME, VALUE) %>%
#   mutate(`Power-Lone_Pine`=Power-Lone_Pine,
#          `Lone_Pine-Godowa-Sycan`=Lone_Pine-`Godowa+Sycan`,
#          `Godowa-SF-NF`=Godowa-`SF+NF`,
#          `Godowa-SF_Ivory-NF_Ivory`=Godowa-`SF_Ivory+NF_Ivory`,
#          `SF_Ivory-SF`=SF_Ivory-SF,
#          `NF_Ivory-NF`=NF_Ivory-NF) %>%
#   gather(SITE_NAME, VALUE, Power:`NF_Ivory-NF`)
#
# # add areas
# areas <- rbind(select(subbasin_area, SITE_NAME, AREA_KM2),
#                select(incbasin_area, SITE_NAME=INC_SITE_NAME, AREA_KM2),
#                select(incbasin_ivory_area, SITE_NAME=INC_SITE_NAME, AREA_KM2)) %>%
#   unique %>%
#   mutate(GROUP=1) %>%
#   spread(SITE_NAME, AREA_KM2) %>%
#   mutate(`Godowa+Sycan`=Godowa+Sycan,
#          `SF_Ivory+NF_Ivory`=SF_Ivory+NF_Ivory,
#          `SF+NF`=SF+NF) %>%
#   gather(SITE_NAME, AREA_KM2, -GROUP) %>%
#   select(-GROUP)
# stopifnot(any(!duplicated(areas$SITE_NAME)))
# stopifnot(all(unique(df$SITE_NAME) %in% areas$SITE_NAME))
# stopifnot(all(areas$SITE_NAME %in% unique(df$SITE_NAME)))
#
# # add areas and compute flow/load per area
# df <- left_join(df, areas, by="SITE_NAME") %>%
#   spread(TERM, VALUE) %>%
#   mutate(L_AREA=L/AREA_KM2,
#          Q_AREA=Q/AREA_KM2*100) %>%
#   gather(TERM, VALUE, Q, Q_AREA, L, L_AREA, C)
#
# # move flow to variable
# df.flow <- filter(df, TERM %in% c("Q", "Q_AREA")) %>%
#   mutate(VAR="FLOW")
# df <- filter(df, TERM %in% c("C", "L", "L_AREA")) %>%
#   rbind(df.flow)
# table(df$VAR, df$TERM)

# separate gis by dataset
stn <- list(RECENT=stn,
            POR=filter(stn, !(SITE_NAME %in% c('SF_Ivory', 'NF_Ivory'))))
subbasin <- list(RECENT=subbasin,
                 POR=filter(subbasin, !(SITE_NAME %in% c('SF_Ivory', 'NF_Ivory'))))
incbasin <- list(RECENT=incbasin_ivory,
                 POR=incbasin)

scale_fill_term <- list(Q=scale_fill_gradientn('Flow\n(hm3/yr)', colours=RColorBrewer::brewer.pal(n=6, name="Blues"), limits=c(0,NA)),
                        L=scale_fill_gradientn('Load\n(kg/yr)', colours=RColorBrewer::brewer.pal(n=6, name="Greens"), limits=c(0,NA)),
                        Q_AREA=scale_fill_gradientn('Flow per Area\n(cm/yr)', colours=RColorBrewer::brewer.pal(n=6, name="Blues"), limits=c(0,NA)),
                        L_AREA=scale_fill_gradientn('Load per Area\n(kg/km2/yr)', colours=RColorBrewer::brewer.pal(n=6, name="Greens"), limits=c(0,NA)),
                        C=scale_fill_gradientn('FWM Conc\n(ppb)', colours=RColorBrewer::brewer.pal(n=6, name="YlOrRd"), limits=c(0,NA)))

scale_fill_term_inc <- list(
  Q=scale_fill_gradient2('Net Flow (cm/yr)',
                         high="#08519C", mid='white', low='black',
                         space='rgb'),
  L=scale_fill_gradient2('Net Load\n(kg/yr)',
                         high="#006D2C", mid='white', low='black',
                         space='rgb'),
  Q_AREA=scale_fill_gradient2('Net Flow per\nArea (cm/yr)',
                              high="#08519C", mid='white', low='black',
                              space='rgb'),
  L_AREA=scale_fill_gradient2('Net Load per\nArea (kg/m2/yr)',
                              high="#006D2C", mid='white', low='black',
                              space='rgb'),
  C=scale_fill_gradient2('Change in\nFWM Conc (ppb)',
                         high='orangered', mid='white', low='black',
                         space='rgb')
)

# variable <- 'TP'
# term <- 'Q_AREA'
# dataset <- 'RECENT'
# ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
#   geom_polygon(aes(x = long, y = lat, group = group),
#                data = incbasin[[dataset]] %>%
#                  select(-SITE_NAME),
#                color = 'grey50', fill = NA, size = 0.2) +
#   geom_polygon(aes(x = long, y = lat, fill = VALUE, group=id),
#                data = filter(incbasin[[dataset]], !(SITE_NAME %in% c("Sycan", "NF", "SF"))) %>%
#                  left_join(filter(df, DATASET==dataset, VAR==variable, TERM==term,
#                                   SITE_NAME %in% incbasin_levels[[dataset]]) %>%
#                              mutate(VALUE=ifelse(SITE_NAME %in% c("Sycan", "NF", "SF"), 0, VALUE)),
#                            by=c('INC_SITE_NAME'='SITE_NAME')),
#                colour = 'black', size = 0.2) +
#   geom_polygon(aes(x = long, y = lat, group = group), data = basin,
#                color = 'black', fill = NA, size = 0.2) +
#   geom_point(aes(x = LON, y = LAT), data = stn[[dataset]], fill = 'deepskyblue', pch = 21, color = 'black', size = 3) +
#   geom_text(aes(x = long, y = lat, label = INC_SITE_NAME),
#             data=incbasin[[dataset]] %>%
#               group_by(INC_SITE_NAME) %>%
#               summarise(long=mean(c(min(long), max(long))),
#                         lat=mean(c(min(lat), max(lat)))),
#             fontface='bold', size=3) +
#   scale_fill_gradient2(high='orangered', mid='white', low='deepskyblue', space='rgb')


# subbasin functions ----
map_subbasin <- function(dataset, variable, term, title=NULL) {
  ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
    geom_polygon(aes(x = long, y = lat, group = group), data = select(incbasin[[dataset]], -SITE_NAME),
                 color = 'grey50', fill = NA, size = 0.2) +
    geom_polygon(aes(x = long, y = lat, fill = VALUE),
                 data = subbasin[[dataset]] %>%
                   left_join(filter(df_site, DATASET==dataset, VAR==variable, TERM==term, SITE_NAME %in% subbasin_levels), by='SITE_NAME') %>%
                   mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=subbasin_levels)),
                 colour = 'black', size = 0.2) +
    geom_polygon(aes(x = long, y = lat, group = group), data = basin,
                 color = 'black', fill = NA, size = 0.2) +
    geom_point(aes(x = LON, y = LAT), data = stn[[dataset]], fill = 'deepskyblue', pch = 21, color = 'black', size = 3) +
    scale_fill_term[[term]] +
    facet_wrap(~SITE_NAME, nrow=2) +
    ggtitle(title)
}
# map_subbasin(dataset='POR', variable='FLOW', term='Q_AREA')
# map_subbasin(dataset='POR', variable='TP', term='C')

bar_subbasin <- function(dataset, variable, term, title=NULL) {
  p <- filter(df_site, DATASET==dataset, VAR==variable, TERM==term, SITE_NAME %in% subbasin_levels) %>%
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
# bar_subbasin(dataset='RECENT', variable='TP', term='L_AREA')

tile_subbasin <- function(dataset, variable, term, title=NULL) {
  p <- filter(df_wyr, DATASET==dataset, VAR==variable, TERM==term, SITE_NAME %in% subbasin_levels) %>%
    mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=rev(levels(SITE_NAME)))) %>%
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
# tile_subbasin(dataset='POR', variable='TP', term='L_AREA')
# tile_subbasin(dataset='RECENT', variable='FLOW', term='Q_AREA')

dash_subbasin <- function(dataset, variable, term, title=NULL) {
  p.map <- map_subbasin(dataset=dataset, variable=variable, term=term, title=title)
  p.bar <- bar_subbasin(dataset=dataset, variable=variable, term=term) +
    theme(aspect.ratio=1)
  p.tile <- tile_subbasin(dataset=dataset, variable=variable, term=term) +
    theme(plot.margin = grid::unit(c(1,0,1,0), "cm")) +
    theme(aspect.ratio=1)
  grid.arrange(p.map, arrangeGrob(p.bar, p.tile, ncol=2), heights=c(2/3, 1/3), ncol=1)
  makeFootnote('Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.')
}
# dash_subbasin('POR', 'FLOW', 'Q_AREA',
#               paste0('Mean Annual Flow per Unit Area', '   |   ', 'Dataset: POR   |   Variable: Flow\n'))
# dash_subbasin('RECENT', 'TP', 'L',
#               paste0('Mean Annual Load', '   |   ', 'Dataset: POR   |   Variable: TP\n'))
# dash_subbasin('POR', 'TP', 'L_AREA',
#               paste0('Mean Annual Load per Unit Area', '   |   ', 'Dataset: POR   |   Variable: TP\n'))
# dash_subbasin('RECENT', 'TP', 'C',
#               paste0('Mean Annual FWM Concentration', '   |   ', 'Dataset: POR   |   Variable: TP\n'))

map_incbasin <- function(dataset, variable, term, title=NULL) {
  if (term == 'C') {
    p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
      geom_polygon(aes(x = long, y = lat, group = group),
                   data = select(incbasin[[dataset]], -SITE_NAME),
                   color = 'grey50', fill = NA, size = 0.2) +
      geom_polygon(aes(x = long, y = lat, fill = VALUE, group=id),
                   data = filter(incbasin[[dataset]], !(SITE_NAME %in% c('Sycan', 'NF', 'SF'))) %>%
                     left_join(filter(df_site, DATASET==dataset,
                                      VAR==variable, TERM==term,
                                      SITE_NAME %in% incbasin_levels[[dataset]]),
                               by=c('INC_SITE_NAME'='SITE_NAME')),
                   colour = 'black', size = 0.2) +
      geom_polygon(aes(x = long, y = lat, group = group), data = basin,
                   color = 'black', fill = NA, size = 0.2) +
      geom_point(aes(x = LON, y = LAT), data = stn[[dataset]], fill = 'deepskyblue', pch = 21, color = 'black', size = 3) +
      geom_text(aes(x = long, y = lat, label = INC_SITE_NAME),
                data=incbasin[[dataset]] %>%
                  group_by(INC_SITE_NAME) %>%
                  summarise(long=mean(c(min(long), max(long))),
                            lat=mean(c(min(lat), max(lat)))),
                fontface='bold', size=3) +
      scale_fill_term_inc[[term]] +
      ggtitle(title)
  } else {
    p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
      geom_polygon(aes(x = long, y = lat, group = group), data = select(incbasin[[dataset]], -SITE_NAME),
                   color = 'grey50', fill = NA, size = 0.2) +
      geom_polygon(aes(x = long, y = lat, fill = VALUE, group=id),
                   data = incbasin[[dataset]] %>%
                     left_join(filter(df_site, DATASET==dataset, VAR==variable, TERM==term, SITE_NAME %in% incbasin_levels[[dataset]]), by=c('INC_SITE_NAME'='SITE_NAME')),
                   colour = 'black', size = 0.2) +
      geom_polygon(aes(x = long, y = lat, group = group), data = basin,
                   color = 'black', fill = NA, size = 0.2) +
      geom_point(aes(x = LON, y = LAT), data = stn[[dataset]], fill = 'deepskyblue', pch = 21, color = 'black', size = 3) +
      geom_text(aes(x = long, y = lat, label = INC_SITE_NAME),
                data=incbasin[[dataset]] %>%
                  group_by(INC_SITE_NAME) %>%
                  summarise(long=mean(c(min(long), max(long))),
                            lat=mean(c(min(lat), max(lat)))),
                fontface='bold', size=3) +
      scale_fill_term_inc[[term]] +
      ggtitle(title)
  }
  p
}
# map_incbasin(dataset='POR', variable='FLOW', term='Q_AREA',
#              title=paste0('Mean Annual Flow per Unit Area', '   |   ',
#                           'Dataset: POR   |   Variable: Flow\n'))
# map_incbasin(dataset='RECENT', variable='FLOW', term='Q_AREA',
#              title=paste0('Mean Annual Flow per Unit Area', '   |   ',
#                           'Dataset: RECENT   |   Variable: Flow\n'))
# map_incbasin(dataset='RECENT', variable='TP', term='C')
# map_incbasin(dataset='RECENT', variable='TP', term='C')
# map_incbasin(dataset='POR', variable='TP', term='L_AREA')
# map_incbasin(dataset='RECENT', variable='NO23', term='C')

bar_incbasin <- function(dataset, variable, term, title=NULL) {
  x <- filter(df_site, DATASET==dataset, VAR==variable, TERM==term, SITE_NAME %in% incbasin_levels[[dataset]]) %>%
    mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=rev(levels(SITE_NAME))))
  if (term == 'C') {
    x <- filter(x, !(SITE_NAME %in% c('SF', 'NF', 'Sycan')))
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
# bar_incbasin(dataset='POR', variable='TP', term='L_AREA')
# bar_incbasin(dataset='RECENT', variable='TP', term='C')
# bar_incbasin(dataset='RECENT', variable='FLOW', term='Q_AREA')

tile_incbasin <- function(dataset, variable, term, title=NULL) {
  x <- filter(df_wyr, DATASET==dataset, VAR==variable, TERM==term, SITE_NAME %in% incbasin_levels[[dataset]]) %>%
    mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=rev(levels(SITE_NAME))))

  if (term == 'C') {
    x <- filter(x, !(SITE_NAME %in% c('SF', 'NF', 'Sycan')))
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
# tile_incbasin(dataset='POR', variable='TP', term='L_AREA')
# tile_incbasin(dataset='RECENT', variable='FLOW', term='Q_AREA')

dash_incbasin <- function(dataset, variable, term, title=NULL) {
  p.map <- map_incbasin(dataset=dataset, variable=variable, term=term, title=title)
  p.bar <- bar_incbasin(dataset=dataset, variable=variable, term=term)
  p.tile <- tile_incbasin(dataset=dataset, variable=variable, term=term) +
    theme(plot.margin = grid::unit(c(1,0,1,0), "cm"))
  grid.arrange(grobs=list(p.map, arrangeGrob(p.bar, p.tile, ncol=2)),
               heights=c(2/3, 1/3), ncol=1)
  makeFootnote('Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.')
}
# dash_incbasin('POR', 'FLOW', 'Q_AREA',
#               paste0('Mean Annual Flow per Unit Area', '   |   ', 'Dataset: POR   |   Variable: Flow\n'))
# dash_incbasin('RECENT', 'FLOW', 'Q_AREA',
#               paste0('Mean Annual Flow per Unit Area', '   |   ', 'Dataset: RECENT   |   Variable: Flow\n'))
# dash_incbasin('RECENT', 'TP', 'C',
#               paste0('Mean Annual FWM Concentration', '   |   ', 'Dataset: RECENT   |   Variable: TP\n'))
# dash_incbasin('RECENT', 'NO23', 'C',
#               paste0('Mean Annual FWM Concentration', '   |   ', 'Dataset: RECENT   |   Variable: NO23\n'))


# pdf ----
for (dataset in c('POR', 'RECENT')) {
  cat(dataset, '\n')
  cat('.. SUBBASINS', '\n')
  variables <- setdiff(filter(df_site, DATASET==dataset)$VAR %>% as.character %>% unique, 'FLOW')
  for (variable in variables) {
    filename <- file.path('pdf', tolower(dataset), 'loads-maps-subbasin',
                          paste0('loads-subbasin-', tolower(variable), '.pdf'))
    cat('Printing:', filename, '\n')
    pdf(filename, width=11, height=8.5)
    cat('....', variable, '\n')
    dash_subbasin(dataset, variable, 'C',
                  paste0('Mean Annual FWM Concentration', '   |   ', 'Dataset: ', dataset,'   |   Variable: ', variable, '\n'))
    dash_subbasin(dataset, variable, 'L_AREA',
                  paste0('Mean Annual Load per Unit Area', '   |   ', 'Dataset: ', dataset,'   |   Variable: ', variable, '\n'))
#     dash_subbasin(dataset, 'FLOW', 'Q_AREA',
#                   paste0('Mean Annual Flow per Unit Area', '   |   ', 'Dataset: ', dataset, '   |   Variable: Flow\n'))
#     dash_subbasin(dataset, variable, 'L',
#                   paste0('Mean Annual Load', '   |   ', 'Dataset: ', dataset,'   |   Variable: ', variable, '\n'))
#     dash_subbasin(dataset, 'FLOW', 'Q',
#                   paste0('Mean Annual Flow', '   |   ', 'Dataset: ', dataset, '   |   Variable: Flow\n'))
    dev.off()
    Sys.sleep(2)
  }

  cat('.. INCBASINS', '\n')
  for (variable in variables) {
    filename <- file.path('pdf', tolower(dataset),
                          'loads-maps-incremental-basin',
                          paste0('loads-incbasin-', tolower(variable), '.pdf'))
    cat('Printing:', filename, '\n')
    pdf(filename, width=11, height=8.5)
    cat('....', variable, '\n')
    dash_incbasin(dataset, variable, 'C',
                  paste0('Change in Annual FWM Concentration', '   |   ', 'Dataset: ', dataset, '   |   Variable: ', variable, '\n'))
    dash_incbasin(dataset, variable, 'L_AREA',
                  paste0('Mean Annual Load per Unit Area', '   |   ', 'Dataset: ', dataset, '   |   Variable: ', variable, '\n'))
#     dash_incbasin(dataset, 'FLOW', 'Q_AREA',
#                   paste0('Mean Annual Flow per Unit Area', '   |   ', 'Dataset: ', dataset, '   |   Variable: Flow\n'))
#     dash_incbasin(dataset, variable, 'L',
#                   paste0('Mean Annual Load', '   |   ', 'Dataset: ', dataset, '   |   Variable: ', variable, '\n'))
#     dash_incbasin(dataset, 'FLOW', 'Q',
#                   paste0('Mean Annual Flow', '   |   ', 'Dataset: ', dataset, '   |   Variable: Flow\n'))
    dev.off()
    Sys.sleep(2)
  }
}

# summary maps
for (dataset in c('POR', 'RECENT')) {
  term <- 'C'
  maps.c <- lapply(c('TP', 'PO4', 'PP', 'TN', 'NH4', 'NO23'), function(variable) {
    p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
      geom_polygon(aes(x = long, y = lat, group = group),
                   data = incbasin[[dataset]] %>%
                     select(-SITE_NAME),
                   color = 'grey50', fill = NA, size = 0.2) +
      geom_polygon(aes(x = long, y = lat, fill = VALUE, group=id),
                   data = filter(incbasin[[dataset]], !(SITE_NAME %in% c("Sycan", "NF", "SF"))) %>%
                     left_join(filter(df_site, DATASET==dataset, VAR==variable, TERM==term,
                                      SITE_NAME %in% incbasin_levels[[dataset]],
                                      !(SITE_NAME %in% c('NF', 'SF', 'Sycan'))),
                               by=c('INC_SITE_NAME'='SITE_NAME')),
                   colour = 'black', size = 0.2) +
      geom_polygon(aes(x = long, y = lat, group = group), data = basin,
                   color = 'black', fill = NA, size = 0.2) +
      geom_point(aes(x = LON, y = LAT), data = stn[[dataset]], fill = 'deepskyblue', pch = 21, color = 'black', size = 3) +
      scale_fill_gradient2('Change in\nFWM Conc (ppb)', high='orangered', mid='white', low='black', space='rgb') +
      ggtitle(variable) +
      theme(legend.text=element_text(size=8),
            legend.position='bottom',
            plot.margin=grid::unit(c(0.5, 0, 0.5, 0), 'lines'))
            # strip.text=element_text(size=12, face='bold'),
            # strip.background=element_blank())
    ggplotGrob(p)
  })

  term <- 'L_AREA'
  maps.l <- lapply(c('TP', 'PO4', 'PP', 'TN', 'NH4', 'NO23'), function(variable) {
    x <- filter(df_site, DATASET==dataset, VAR==variable, TERM==term,
                SITE_NAME %in% incbasin_levels[[dataset]])
    if (min(x$VALUE)<0) {
      color_limits <- c(NA, NA)
    } else {
      color_limits <- c(0, NA)
    }

    p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
      geom_polygon(aes(x = long, y = lat, group = group),
                   data = select(incbasin[[dataset]], -SITE_NAME),
                   color = 'grey50', fill = NA, size = 0.2) +
      geom_polygon(aes(x = long, y = lat, fill = VALUE, group=id),
                   data = incbasin[[dataset]] %>%
                     left_join(x, by=c('INC_SITE_NAME'='SITE_NAME')) %>%
                     mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=incbasin_levels)),
                   colour = 'black', size = 0.2) +
      geom_polygon(aes(x = long, y = lat, group = group), data = basin,
                   color = 'black', fill = NA, size = 0.2) +
      geom_point(aes(x = LON, y = LAT), data = stn[[dataset]],
                 fill = 'deepskyblue', pch = 21, color = 'black', size = 2) +
      scale_fill_gradient2('Net Load per\nArea (kg/m2/yr)',
                           high="#006D2C", mid='white', low='black',
                           space='rgb',
                           lim=color_limits)+
      ggtitle(variable) +
      theme(legend.text=element_text(size=8),
            legend.position='bottom',
            plot.margin=grid::unit(c(0.5, 0, 0.5, 0), 'lines'))
    ggplotGrob(p)
  })

  x.flow <- filter(df_site, DATASET==dataset, VAR=="FLOW", TERM=='Q_AREA',
                   SITE_NAME %in% incbasin_levels[[dataset]])
  if (min(x.flow$VALUE)<0) {
    color_limits <- c(NA, NA)
  } else {
    color_limits <- c(0, NA)
  }

  maps.q <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
      geom_polygon(aes(x = long, y = lat, group = group),
                   data = select(incbasin[[dataset]], -SITE_NAME),
                   color = 'grey50', fill = NA, size = 0.2) +
      geom_polygon(aes(x = long, y = lat, fill = VALUE, group=id),
                   data = incbasin[[dataset]] %>%
                     left_join(x.flow, by=c('INC_SITE_NAME'='SITE_NAME')) %>%
                     mutate(SITE_NAME=ordered(as.character(SITE_NAME),
                                              levels=incbasin_levels)),
                   colour = 'black', size = 0.2) +
      geom_polygon(aes(x = long, y = lat, group = group), data = basin,
                   color = 'black', fill = NA, size = 0.2) +
      geom_point(aes(x = LON, y = LAT), data = stn[[dataset]],
                 fill = 'deepskyblue', pch = 21, color = 'black', size = 2) +
      scale_fill_gradient2('Net Flow per\nArea (cm/yr)',
                           high="#08519C", mid='white', low='black',
                           space='rgb', lim=color_limits) +
      facet_wrap(~VAR) +
      theme(legend.text=element_text(size=8),
            legend.position='bottom',
            plot.margin=grid::unit(c(0.5, 0, 0.5, 0), 'lines'),
            strip.text=element_text(size=12, face='bold'),
            strip.background=element_blank())

  filename <- file.path('pdf', tolower(dataset), paste0('loads-summary-maps.pdf'))
  cat('Printing:', filename, '\n')
  pdf(filename, width=10, height=8)
  grid.arrange(grobs=maps.c, nrow=2,
               top=paste0('\nNet Change in FWM Concentration by Incremental Subbasin\nDataset: ', dataset))
  makeFootnote('Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.')
  grid.arrange(grobs=maps.l, nrow=2,
               top=paste0('\nNet Load per Unit Area by Incremental Subbasin\nDataset: ', dataset))
  makeFootnote('Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.')
  grid.arrange(arrangeGrob(maps.q, nrow=2, ncol=3),
               top=paste0('\nNet Flow per Unit Area by Incremental Subbasin\nDataset: ', dataset))
  makeFootnote('Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.')
  dev.off()
}