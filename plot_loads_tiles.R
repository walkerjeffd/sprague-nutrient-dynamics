library(dplyr)
library(tidyr)
library(lubridate)
library(fluxr)
library(ggplot2)
theme_set(theme_bw())
library(gridExtra)

# load data ----
load('kt_sprague.Rdata')
load('loads.Rdata')

stn_primary <- list(POR=c('Power', 'Lone_Pine', 'Sycan', 'Godowa', 'SF', 'NF'),
                    RECENT=c('Power', 'Lone_Pine', 'Sycan', 'Godowa', 'SF_Ivory', 'SF', 'NF_Ivory', 'NF'))
stn_basins <- list(POR=c('Power-Lone_Pine', 'Lone_Pine-Godowa-Sycan', 'Sycan', 'Godowa-SF-NF', 'SF', 'NF'),
                   RECENT=c('Power-Lone_Pine', 'Lone_Pine-Godowa-Sycan', 'Sycan', 'Godowa-SF_Ivory-NF_Ivory', 'SF_Ivory-SF', 'SF', 'NF_Ivory-NF', 'NF'))
variables <- list(POR=c('TP', 'PO4', 'PP', 'TN', 'NH4', 'NO23'),
                  RECENT=c('TP', 'PO4', 'PP', 'TN', 'NH4', 'NO23', 'TSS'))
term_names <- c('Q'='Flow', 'Q_AREA'='Flow per Area', 'C'='FWM Concentration', 'L'='Load', 'L_AREA'='Load per Area')

rm(loads)
df_mon <- loads_df[['mon']]

plot_tiles <- function(dataset, variable, term, sites=stn_primary) {
  x <- filter(df_mon, DATASET==dataset, VAR==variable, TERM==term, SITE_NAME %in% sites[[dataset]]) %>%
    mutate(MONTH=ordered(MONTH, levels=rev(c(10:12, 1:9))))
#   if (term != 'C') {
  x <- mutate(x, VALUE=log10(VALUE))
#   }
  p <- x %>%
#     group_by(SITE_NAME) %>%
#     mutate(MEAN=mean(VALUE),
#            VALUE2=VALUE/MEAN) %>%
#     ggplot(aes(factor(WYEAR), MONTH, fill=log10(VALUE))) +
    ggplot(aes(factor(WYEAR), MONTH, fill=VALUE)) +
    geom_tile() +
    facet_wrap(~SITE_NAME, nrow=2)
#   if (term == 'C') {
#     p <- p + scale_fill_gradientn(paste0('log10(', term, ')'), colours=rev(scales::brewer_pal(type = "seq", palette = 'GnBu')(9)), limits=c(0, NA))
#   } else {
#     p <- p + scale_fill_gradientn(paste0('log10(', term, ')'), colours=rev(scales::brewer_pal(type = "seq", palette = 'GnBu')(9)))
#   }
  p <- p + scale_fill_gradientn(paste0('log10(', term, ')'), colours=rev(scales::brewer_pal(type = "seq", palette = 'GnBu')(9)))
  p <- p +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
    labs(x='Water Year', y='Month') +
    ggtitle(paste0('Dataset: ', dataset, ' | Variable: ', variable, ' | Term: ', term_names[[term]]))
  print(p)
}
# plot_tiles('POR', 'TP', 'C')

for (dataset in c('POR', 'RECENT')) {
  cat(dataset, '\n')
  pdf(file.path('pdf', tolower(dataset), paste0('loads-tiles-sites.pdf')), width=11, height=8.5)
  cat('..', 'FLOW', '\n')
  plot_tiles(dataset, 'FLOW', 'Q')
  plot_tiles(dataset, 'FLOW', 'Q_AREA')
  for (variable in variables[[dataset]]) {
    cat('..', variable, '\n')
    plot_tiles(dataset, variable, 'C')
    plot_tiles(dataset, variable, 'L_AREA')
    plot_tiles(dataset, variable, 'L')
    
  }
  dev.off()
}

for (dataset in c('POR', 'RECENT')) {
  cat(dataset, '\n')
  pdf(file.path('pdf', tolower(dataset), paste0('loads-tiles-incremental-basins.pdf')), width=11, height=8.5)
  cat('..', 'FLOW', '\n')
  plot_tiles(dataset, 'FLOW', 'Q', sites=stn_basins)
  plot_tiles(dataset, 'FLOW', 'Q_AREA', sites=stn_basins)
  for (variable in variables[[dataset]]) {
    cat('..', variable, '\n')
    plot_tiles(dataset, variable, 'C', sites=stn_basins)
    plot_tiles(dataset, variable, 'L_AREA', sites=stn_basins)
    plot_tiles(dataset, variable, 'L', sites=stn_basins)
  }
  dev.off()
}












dataset <- 'POR'
term <- 'C'
plot_tiles_large <- function(dataset, term, by_site=FALSE) {
  if (by_site) {
    x <- filter(df_mon, DATASET==dataset, TERM==term, SITE_NAME %in% stn_primary[[dataset]], VAR != 'PP') %>%
      mutate(MONTH=ordered(MONTH, levels=rev(c(10:12, 1:9))), 
             VALUE=log10(VALUE)) %>%
      group_by(VAR, SITE_NAME) %>%
      mutate(VALUE=scale(VALUE)) 
    title <- paste0('Dataset: ', dataset, ' | Term: ', term_names[[term]], '\nStandardized log10[Value] by Variable and Site')
  } else {
    x <- filter(df_mon, DATASET==dataset, TERM==term, SITE_NAME %in% stn_primary[[dataset]], VAR != 'PP') %>%
      mutate(MONTH=ordered(MONTH, levels=rev(c(10:12, 1:9))), 
             VALUE=log10(VALUE)) %>%
      group_by(VAR) %>%
      mutate(VALUE=scale(VALUE)) 
    title <- paste0('Dataset: ', dataset, ' | Term: ', term_names[[term]], '\nStandardized log10[Value] by Variable')
  }
  if (term %in% c('Q', 'Q_AREA')) {
    p <- x %>%
      ggplot(aes(factor(WYEAR), MONTH, fill=VALUE)) +
      geom_tile() +
      facet_grid(VAR~SITE_NAME) + 
      scale_fill_gradientn(term, colours=rev(scales::brewer_pal(type = "seq", palette = 'GnBu')(9))) +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
            axis.text.y=element_text(size=8)) +
      labs(x='Water Year', y='Month') +
      ggtitle(title)
    grid.arrange(p, nrow=2, heights=c(1/3, 2/3))
  } else {
    p <- x %>%
      ggplot(aes(factor(WYEAR), MONTH, fill=VALUE)) +
      geom_tile() +
      facet_grid(VAR~SITE_NAME) + 
      scale_fill_gradientn(term, colours=rev(scales::brewer_pal(type = "seq", palette = 'GnBu')(9))) +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
            axis.text.y=element_text(size=8)) +
      labs(x='Water Year', y='Month') +
      ggtitle(title)
    print(p)
  }
  
}

for (dataset in c('POR', 'RECENT')) {
  cat(dataset, '\n')
  pdf(file.path('pdf', tolower(dataset), 'loads-tiles-overview.pdf'), width=11, height=8.5)
  plot_tiles_large(dataset, 'C')
  plot_tiles_large(dataset, 'C', by_site=TRUE)
  plot_tiles_large(dataset, 'L_AREA')
  plot_tiles_large(dataset, 'L_AREA', by_site=TRUE)
  plot_tiles_large(dataset, 'Q_AREA')
  plot_tiles_large(dataset, 'Q_AREA', by_site=TRUE)
  dev.off()
}
