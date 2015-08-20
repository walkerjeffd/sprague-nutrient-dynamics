library(dplyr)
library(tidyr)
library(lubridate)
library(fluxr)
library(ggplot2)
theme_set(theme_bw())
library(gridExtra)
library(manipulate)

rm(list=ls())

# load data ----
load('kt_sprague.Rdata')
load('loads.Rdata')

df_site <- loads_df[['site']]

df_mon <- loads_df[['mon']]

df_wyr <- loads_df[['wyr']]

df_day <- loads_df[['day']]

df_obs <- loads_input %>%
  filter(SAMPLED) %>%
  select(DATASET, SITE_NAME, VAR, DATE, Q=Qs, C=C, L=L) %>%
  gather(TERM, VALUE, Q, C, L, na.rm=TRUE)

df_obs_flow <- filter(df_obs, TERM=='Q') %>%
  mutate(VAR='FLOW') %>%
  unique
df_obs_wq <- filter(df_obs, !(TERM %in% c('Q'))) %>%
  spread(VAR, VALUE) %>%
  mutate(PP = TP - PO4) %>%
  gather(VAR, VALUE, NH4:PP, na.rm=TRUE)

df_obs <- rbind(df_obs_wq, df_obs_flow) %>%
  mutate(TERM=ordered(as.character(TERM), levels=c('Q','L','C')),
         VAR=ordered(as.character(VAR), levels=c('FLOW','TP','PO4','PP','TN','NH4','NO23','TSS')))

rm(loads, loads_df)

# plots ----
site_network <- list('Sprague_Power'='Sprague_Lone',
                     'Sprague_Lone'=c('Sycan', 'Sprague_Godowa'),
                     'Sprague_Godowa'=c('Sprague_NF', 'Sprague_SF'),
                     'Sprague_Godowa_Ivory'=c('Sprague_NF_Ivory', 'Sprague_SF_Ivory'))

dataset <- 'POR'
variable <- 'TP'
plot.type <- 'wyr'
site_ds <- 'Sprague_Power'


plot_loads <- function(dataset, variable, plot.type, site_ds, log.trans=FALSE) {
  if (site_ds == 'Sprague_Godowa_Ivory') {
    sites_us <- site_network[['Sprague_Godowa_Ivory']]
    site_ds <- 'Sprague_Godowa'
  } else {
    sites_us <- site_network[[site_ds]]
  }

  if (plot.type == 'wyr') {
    p <- filter(df_wyr, DATASET==dataset, VAR %in% c('FLOW', variable), SITE_NAME %in% c(site_ds, sites_us), TERM %in% c('Q', 'L', 'C')) %>%
      mutate(DATE=ymd(paste(WYEAR-1, 10, 1, sep='-'))) %>%
      ggplot(aes(DATE, VALUE, color=SITE_NAME)) +
      geom_step() +
      geom_point(data=filter(df_obs, DATASET==dataset, VAR %in% c('FLOW', variable), SITE_NAME %in% c(site_ds, sites_us), TERM %in% c('C'))) +
      facet_wrap(~TERM, scales='free_y', ncol=1) +
      labs(x='', y='')
  } else if (plot.type == 'monyr') {
    p <- filter(df_mon, DATASET==dataset, VAR %in% c('FLOW', variable), SITE_NAME %in% c(site_ds, sites_us), TERM %in% c('Q', 'L', 'C')) %>%
      mutate(DATE=MONTHYEAR) %>%
      ggplot(aes(DATE, VALUE, color=SITE_NAME)) +
      geom_line() +
      geom_point(data=filter(df_obs, DATASET==dataset, VAR %in% c('FLOW', variable), SITE_NAME %in% c(site_ds, sites_us), TERM %in% c('C'))) +
      facet_wrap(~TERM, scales='free_y', ncol=1) +
      labs(x='', y='')
  } else if (plot.type == 'day') {
    p <- filter(df_day, DATASET==dataset, VAR %in% c('FLOW', variable), SITE_NAME %in% c(site_ds, sites_us), TERM %in% c('Q', 'L', 'C')) %>%
      ggplot(aes(DATE, VALUE, color=SITE_NAME)) +
      geom_line() +
      geom_point(data=filter(df_obs, DATASET==dataset, VAR %in% c('FLOW', variable), SITE_NAME %in% c(site_ds, sites_us), TERM %in% c('C'))) +
      facet_wrap(~TERM, scales='free_y', ncol=1) +
      labs(x='', y='')
  } else if (plot.type == 'mon') {
    p <- filter(df_mon, DATASET==dataset, VAR %in% c('FLOW', variable), SITE_NAME %in% c(site_ds, sites_us), TERM %in% c('Q', 'L', 'C')) %>%
      mutate(MONTH=ordered(MONTH, levels=c(10:12, 1:9))) %>%
      ggplot(aes(MONTH, VALUE, fill=SITE_NAME)) +
      geom_boxplot(position='dodge') +
      geom_jitter(mapping=aes(color=SITE_NAME),
                  data=filter(df_obs, DATASET==dataset, VAR %in% c('FLOW', variable), SITE_NAME %in% c(site_ds, sites_us), TERM %in% c('C')) %>%
                       mutate(MONTH=ordered(month(DATE), levels=c(10:12, 1:9))),
                  alpha=0.5) +
      facet_wrap(~TERM, scales='free_y', ncol=1) +
      labs(x='', y='')
  } else if (plot.type == 'wday') {
    p <- filter(df_day, DATASET==dataset, VAR %in% c('FLOW', variable), SITE_NAME %in% c(site_ds, sites_us), TERM %in% c('Q', 'L', 'C')) %>%
      mutate(WDAY=ifelse(yday(DATE)-leap_year(DATE)>=274,
                         yday(DATE)-leap_year(DATE)-274,
                         yday(DATE)+(365-274)),
             SITE_YEAR=paste(SITE_NAME, WYEAR)) %>%
      ggplot(aes(WDAY, VALUE, color=SITE_NAME, group=SITE_YEAR)) +
      geom_line(size=1, alpha=0.5) +
      geom_point(data=filter(df_obs, DATASET==dataset, VAR %in% c('FLOW', variable), SITE_NAME %in% c(site_ds, sites_us), TERM %in% c('C')) %>%
                   mutate(WDAY=ifelse(yday(DATE)-leap_year(DATE)>=274,
                                      yday(DATE)-leap_year(DATE)-274,
                                      yday(DATE)+(365-274)),
                          SITE_YEAR=paste(SITE_NAME, wyear(DATE))),
                 alpha=0.5) +
      facet_wrap(~TERM, scales='free_y', ncol=1) +
      labs(x='', y='')
  }
  if (log.trans) {
    p <- p + scale_y_log10()
  }
  p
}

plot_loads(dataset='POR', variable='TP', plot.type='wyr', site_ds='Sprague_Power', log.trans=TRUE)


manipulate(plot_loads(dataset=dataset, variable=variable, plot.type=plot.type, site_ds=site_ds, log.trans=log.trans),
           dataset=picker('POR', 'RECENT'),
           variable=picker('TP', 'PO4'),
           plot.type=picker('day', 'monyr', 'wyr', 'mon', 'wday'),
           site_ds=picker('Sprague_Power', 'Sprague_Lone', 'Sprague_Godowa', 'Sprague_Godowa_Ivory'),
           log.trans=checkbox(FALSE, 'Log y'))




df_wyr_c <- arrange(df_wyr, DATASET, VAR, SITE_NAME, TERM, WYEAR) %>%
#   filter(TERM!='C') %>%
  group_by(DATASET, VAR, SITE_NAME, TERM) %>%
  mutate(CUMSUM=cumsum(VALUE))


dataset <- 'POR'
variable <- 'FLOW'
term <- 'Q'
site1 <- 'Sprague_Power'
site2 <- 'Sprague_Lone'
plot_dmc <- function(dataset, variable, term, site1, site2) {
  x <- select(df_wyr_c, -AREA_KM2, -VALUE) %>%
    filter(DATASET==dataset, VAR==variable, TERM==term)
  x.max <- group_by(x, DATASET, VAR, TERM, SITE_NAME) %>%
    summarise(MAX=max(CUMSUM)) %>%
    spread(SITE_NAME, MAX)
  x.wide <- x %>%
    spread(SITE_NAME, CUMSUM) %>%
    as.data.frame

  slope <- x.max[[site2]]/x.max[[site1]]

  x.wide$DIFF <- x.wide[, site2] - x.wide[, site1]
  x.wide$RESID <- x.wide[, site2] - x.wide[, site1]*slope
  p1 <- ggplot(x.wide, aes_string(site1, site2)) +
    geom_point() +
    geom_text(aes(label=WYEAR), hjust=-1) +
    geom_abline(intercept=0, slope=slope, linetype=2) +
    ylim(0, NA) +
    xlim(0, NA) +
    ggtitle(paste(dataset, variable, term, site1, site2, sep=' | '))
  p2 <- ggplot(x.wide, aes(WYEAR, RESID)) +
    geom_line() +
    geom_hline(yint=0) +
    ylab(paste(site2, site1, sep=' - '))
  grid.arrange(p1, p2, ncol=1)
}

plot_dmc('POR', 'FLOW', 'Q', 'Sprague_Power', 'Sprague_Lone')
plot_dmc('POR', 'TP', 'L', 'Sprague_Power', 'Sprague_Lone')
plot_dmc('POR', 'TP', 'C', 'Sprague_Power', 'Sprague_Lone')
plot_dmc('POR', 'TN', 'L', 'Sprague_Power', 'Sprague_Lone')
plot_dmc('POR', 'TN', 'C', 'Sprague_Power', 'Sprague_Lone')

plot_dmc('POR', 'FLOW', 'Q', 'Sprague_Lone', 'Sprague_Godowa')
plot_dmc('POR', 'TP', 'L', 'Sprague_Lone', 'Sprague_Godowa')
plot_dmc('POR', 'TP', 'C', 'Sprague_Lone', 'Sprague_Godowa')
plot_dmc('POR', 'TN', 'L', 'Sprague_Lone', 'Sprague_Godowa')
plot_dmc('POR', 'TN', 'C', 'Sprague_Lone', 'Sprague_Godowa')

plot_dmc('POR', 'FLOW', 'Q', 'Sprague_Lone', 'Sycan')
plot_dmc('POR', 'TP', 'L', 'Sprague_Lone', 'Sycan')
plot_dmc('POR', 'TP', 'C', 'Sprague_Lone', 'Sycan')
plot_dmc('POR', 'TN', 'L', 'Sprague_Lone', 'Sycan')
plot_dmc('POR', 'TN', 'C', 'Sprague_Lone', 'Sycan')
