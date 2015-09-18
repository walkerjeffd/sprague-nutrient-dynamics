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

stn_primary <- list(POR=c('Power', 'Lone_Pine', 'Sycan', 'Godowa', 'SF', 'NF'),
                    RECENT=c('Power', 'Lone_Pine', 'Sycan', 'Godowa', 'SF_Ivory', 'SF', 'NF_Ivory', 'NF'))
stn_basins <- list(POR=c('Power-Lone_Pine', 'Lone_Pine-Godowa-Sycan', 'Godowa-SF-NF'),
                   RECENT=c('Power-Lone_Pine', 'Lone_Pine-Godowa-Sycan', 'Godowa-SF_Ivory-NF_Ivory', 'SF_Ivory-SF', 'NF_Ivory-NF'))
variables <- list(POR=c('TP', 'PO4', 'PP', 'TN', 'NH4', 'NO23'),
                  RECENT=c('TP', 'PO4', 'PP', 'TN', 'NH4', 'NO23', 'TSS'))
term_names <- c('Q'='Flow', 'Q_AREA'='Flow per Area', 'C'='FWM Concentration', 'L'='Load', 'L_AREA'='Load per Area')

rm(loads)
df_mon <- loads_df[['mon']]

plot_tiles <- function(dataset, variable, term, sites=stn_primary, log.trans=TRUE) {
  x <- filter(df_mon, DATASET==dataset, VAR==variable, TERM==term, SITE_NAME %in% sites[[dataset]]) %>%
    mutate(MONTH=ordered(MONTH, levels=rev(c(10:12, 1:9))))
  lbl <- term
  if (log.trans) {
    x <- mutate(x, VALUE=log10(VALUE))
    lbl <- paste0('log10(', term, ')')
  }
  p <- x %>%
    ggplot(aes(factor(WYEAR), MONTH, fill=VALUE)) +
    geom_tile() +
    facet_wrap(~SITE_NAME, nrow=2)
  p <- p + scale_fill_gradientn(lbl, colours=rev(scales::brewer_pal(type = "seq", palette = 'GnBu')(9)))
  p <- p +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
    labs(x='Water Year', y='Month') +
    ggtitle(paste0('Dataset: ', dataset, ' | Variable: ', variable, ' | Term: ', term_names[[term]]))
  print(p)
}
# plot_tiles('POR', 'TP', 'C')

for (dataset in c('POR', 'RECENT')) {
  filename <- file.path('pdf', tolower(dataset), paste0('loads-tiles-sites.pdf'))
  cat('Printing:', filename, '\n')
  cat(dataset, '\n')
  pdf(filename, width=11, height=8.5)
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
  filename <- file.path('pdf', tolower(dataset), paste0('loads-tiles-incremental-basins.pdf'))
  cat('Printing:', filename, '\n')
  cat(dataset, '\n')
  pdf(filename, width=11, height=8.5)
  cat('..', 'FLOW', '\n')
  plot_tiles(dataset, 'FLOW', 'Q', sites=stn_basins)
  plot_tiles(dataset, 'FLOW', 'Q_AREA', sites=stn_basins)
  for (variable in variables[[dataset]]) {
    cat('..', variable, '\n')
    plot_tiles(dataset, variable, 'C', sites=stn_basins, log.trans=FALSE)
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
  filename <- file.path('pdf', tolower(dataset), 'loads-tiles-overview.pdf')
  cat('Printing:', filename, '\n')
  cat(dataset, '\n')
  pdf(filename, width=11, height=8.5)
  plot_tiles_large(dataset, 'C')
  plot_tiles_large(dataset, 'C', by_site=TRUE)
  plot_tiles_large(dataset, 'L_AREA')
  plot_tiles_large(dataset, 'L_AREA', by_site=TRUE)
  plot_tiles_large(dataset, 'Q_AREA')
  plot_tiles_large(dataset, 'Q_AREA', by_site=TRUE)
  dev.off()
}

# report ----
df_mon_report <- filter(loads_df[['mon']],
                        DATASET=="POR",
                        SITE_NAME %in% c('Power', 'Lone_Pine', 'Sycan', 'Godowa',
                                         'SF', 'NF')) %>%
  filter(TERM %in% c("Q", "C")) %>%
  # filter(VAR != "PP") %>%
  arrange(VAR, SITE_NAME) %>%
  mutate(SITE_NAME=ordered(as.character(SITE_NAME),
                           levels=c("Power", "Lone_Pine", "Godowa", "Sycan",
                                    "SF", "NF")))
df_mon_report$VAR_LABEL <- ifelse(df_mon_report$VAR=="FLOW", "Flow", paste0(df_mon_report$VAR, " Conc"))
df_mon_report$VAR_LABEL <- ordered(df_mon_report$VAR_LABEL, levels=unique(df_mon_report$VAR_LABEL))

df_mon_report.tile <- mutate(df_mon_report,
                      MONTH=ordered(MONTH, levels=rev(c(10:12, 1:9))),
                      VALUE=log10(VALUE)) %>%
  group_by(VAR_LABEL) %>%
  mutate(VALUE=scale(VALUE)) %>%
  ungroup

wyr_labels <- seq(2002, 2014)
wyr_labels[as.logical(wyr_labels %% 2)] <- ""
mon_labels <- c(10:12, 1:9)
mon_labels[as.logical(mon_labels %% 2)] <- ""
mon_labels <- rev(mon_labels)

p <- df_mon_report.tile %>%
  ggplot(aes(factor(WYEAR), MONTH, fill=VALUE)) +
  geom_tile() +
  facet_grid(VAR_LABEL~SITE_NAME) +
  scale_fill_gradientn('Standardized\nLog(Value)',
                       colours=rev(scales::brewer_pal(type = "seq",
                                                      palette = 'GnBu')(9))) +
  scale_x_discrete(labels=wyr_labels) +
  scale_y_discrete(labels=mon_labels) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=10),
        axis.text.y=element_text(size=8),
        panel.grid=element_blank(),
        strip.background=element_blank()) +
  labs(x='Water Year', y='Month')

png('report/results-load-tile-mon.png', width=10, height=8, res=200, units='in')
print(p)
dev.off()

# annual tile plot
seasons <- list(Annual=1:12,
                'Fall (Oct-Dec)'=c(10:12),
                'Winter (Jan-Mar)'=c(1:3),
                'Spring (Apr-Jun)'=c(4:6),
                'Summer (Jul-Sep)'=c(7:9))
df_seasons <- lapply(names(seasons), function(s) {
  data.frame(MONTH=seasons[[s]], SEASON=s, stringsAsFactors=FALSE)
}) %>%
  rbind_all

df_wyr_report <- filter(loads_df[['mon']],
                 DATASET=="POR",
                 SITE_NAME %in% c('Power', 'Lone_Pine', 'Sycan', 'Godowa',
                                  'SF', 'NF')) %>%
  filter(TERM %in% c("Q", "L")) %>%
  droplevels %>%
  spread(TERM, VALUE)
df_wyr_report.flow <- filter(df_wyr_report, VAR == "FLOW") %>%
  select(-VAR ,-L)
df_wyr_report.load <- filter(df_wyr_report, VAR != "FLOW") %>%
  select(-Q)
df_wyr_report <- left_join(df_wyr_report.load, df_wyr_report.flow)

df_wyr_report <- left_join(df_seasons, df_wyr_report, by='MONTH') %>%
  mutate(SEASON=ordered(SEASON, levels=names(seasons)))

# compute annual flows/loads/concs
df_wyr_report <- group_by(df_wyr_report, DATASET, VAR, SITE_NAME, SEASON, WYEAR) %>%
  summarise(N.MONTH=n(),
            N.DAY=sum(days_in_month(MONTHYEAR)),
            L=sum(L)/N.DAY,
            Q=sum(Q)/N.DAY,
            C=L/Q) %>%
  ungroup

df_wyr_report.flow <- select(df_wyr_report, -VAR, -L, -C) %>%
  unique %>%
  mutate(VAR="FLOW", TERM="Q") %>%
  rename(VALUE=Q)
df_wyr_report.wq <- select(df_wyr_report, -Q) %>%
  gather(TERM, VALUE, L, C) %>%
  filter(TERM != "L")
df_wyr_report <- rbind(df_wyr_report.flow, df_wyr_report.wq) %>%
  mutate(SITE_NAME=ordered(as.character(SITE_NAME),
                           levels=c("Power", "Lone_Pine", "Godowa", "Sycan",
                                    "SF", "NF")),
         VAR_LABEL=ifelse(VAR=="FLOW", "Flow", paste0(VAR, " Conc")),
         VAR_LABEL=ordered(VAR_LABEL, levels=unique(VAR_LABEL)))

df_wyr_report.tile <- mutate(df_wyr_report,
                             VALUE_LOG=log10(VALUE)) %>%
  group_by(VAR_LABEL) %>%
  mutate(VALUE_SCALE=as.numeric(scale(VALUE_LOG))) %>%
  ungroup

stopifnot(
  group_by(df_wyr_report.tile, SEASON, WYEAR, VAR_LABEL, SITE_NAME) %>%
    summarise(N=n()) %>%
    filter(N>1) %>%
    nrow == 0
)

# limit low values to -3
df_wyr_report.tile$VALUE_SCALE <- with(df_wyr_report.tile, ifelse(VALUE_SCALE < -3, -3, VALUE_SCALE))

p <- df_wyr_report.tile %>%
  ggplot(aes(factor(WYEAR), SITE_NAME, fill=VALUE_SCALE)) +
  geom_tile() +
  facet_grid(VAR_LABEL~SEASON) +
  scale_fill_gradientn('Std. Value',
                       colours=rev(scales::brewer_pal(type = "seq",
                                                      palette = 'GnBu')(9))) +
  # scale_x_discrete(labels=wyr_labels) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=10),
        axis.text.y=element_text(size=10),
        panel.grid=element_blank()) +
  labs(x='Water Year', y='Station')

png('report/results-load-tile-wyr.png', width=10, height=8, res=200, units='in')
print(p)
dev.off()


# line chart variations
# df_wyr.tile %>%
#   ggplot(aes(WYEAR, VALUE, color=SEASON)) +
#   geom_line() +
#   facet_grid(VAR_LABEL~SITE_NAME, scales='free_y') +
#   scale_y_log10() +
#   labs(x='Water Year', y='Value')
# df_wyr.tile %>%
#   ggplot(aes(WYEAR, VALUE, color=SITE_NAME)) +
#   geom_line() +
#   facet_grid(VAR_LABEL~SEASON, scales='free_y') +
#   scale_x_continuous(breaks=seq(2002, 2014, by=2)) +
#   scale_y_log10() +
#   labs(x='Water Year', y='Value')


