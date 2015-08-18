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

df_obs <- lapply(names(loads), function(dataset) {
  lapply(names(loads[[dataset]]), function(variable) {
    lapply(names(loads[[dataset]][[variable]]), function(site) {
      x <- loads[[dataset]][[variable]][[site]][['obs']]
      x$DATASET <- dataset
      x$VAR <- variable
      x$SITE_NAME <- site
      x
    }) %>%
      rbind_all
  }) %>%
    rbind_all
}) %>%
  rbind_all %>%
  filter(!is.na(C))
dataset_levels <- levels(loads_df$site$DATASET)
var_levels <- levels(loads_df$site$VAR)
site_levels <- levels(loads_df$site$SITE_NAME)
df_obs <- mutate(df_obs,
                 DATASET=ordered(as.character(DATASET), levels=dataset_levels),
                 VAR=ordered(VAR, levels=var_levels),
                 SITE_NAME=ordered(SITE_NAME, levels=site_levels))

df_obs_wyr <- mutate(df_obs, L=Q*C, WYEAR=fluxr::wyear(DATE)) %>%
  group_by(DATASET, VAR, SITE_NAME, WYEAR) %>%
  summarise(N_SAMPLE=n(),
            Q=sum(Q),
            L=sum(L),
            C=L/Q)

df_obs_site <- summarise(df_obs_wyr,
  N_SAMPLE=sum(N_SAMPLE),
  N_WYEAR=n(),
  Q=sum(Q),
  L=sum(L),
  C=L/Q) %>%
  ungroup

df_obs_wyr <- ungroup(df_obs_wyr)

df_day <- filter(loads_df[['day']]) %>%
  spread(TERM, VALUE) %>%
  select(DATASET, VAR, SITE_NAME, DATE, Q, C) %>%
  mutate(SOURCE='MODEL') %>%
  rbind(select(df_obs, DATASET, VAR, SITE_NAME, DATE, Q, C) %>% mutate(SOURCE='OBS')) %>%
  filter(SITE_NAME %in% stn.kt_sprague$SITE_NAME,
         VAR != 'PP') %>%
  droplevels

df_site <- filter(loads_df[['site']], VAR != 'FLOW') %>%
  spread(TERM, VALUE) %>%
  select(DATASET, VAR, SITE_NAME, C) %>%
  mutate(SOURCE='MODEL') %>%
  rbind(select(df_obs_site, DATASET, VAR, SITE_NAME, C) %>% mutate(SOURCE='OBS')) %>%
  filter(SITE_NAME %in% stn.kt_sprague$SITE_NAME,
         VAR != 'PP') %>%
  droplevels

df_wyr <- filter(loads_df[['wyr']], VAR != 'FLOW') %>%
  spread(TERM, VALUE) %>%
  select(DATASET, VAR, SITE_NAME, WYEAR, C) %>%
  mutate(SOURCE='MODEL') %>%
  rbind(select(df_obs_wyr, DATASET, VAR, SITE_NAME, WYEAR, C) %>% mutate(SOURCE='OBS')) %>%
  filter(SITE_NAME %in% stn.kt_sprague$SITE_NAME,
         VAR != 'PP') %>%
  droplevels

pdf(file.path('pdf', 'loads-model-vs-obs.pdf'), width=11, height=8.5)

ggplot(df_site, aes(SITE_NAME, C, fill=SOURCE)) +
  geom_bar(position='dodge', stat='identity') +
  facet_grid(VAR~DATASET, scales='free_y') +
  labs(x='', y='FWM Concentration (ppb)', title='FWM Concentrations by Site based on Load Model and Direct Observations') +
  scale_fill_manual('', values=c('MODEL'='gray20', 'OBS'='deepskyblue')) +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))

spread(df_site, SOURCE, C) %>%
  filter(DATASET=='POR') %>%
  ggplot(aes(OBS, MODEL)) +
  geom_point() +
  geom_abline() +
  geom_hline(yint=0, alpha=0) +
  geom_vline(xint=0, alpha=0) +
  labs(x='FWM Concentration from Direct Observations (ppb)', 
       y='FWM Concentration from Daily Load Model (ppb)', 
       title='Comparison of FWM Concentrations between Daily Load Model and Direct Observations\nDataset: POR') +
  facet_wrap(~VAR, scales='free')

spread(df_site, SOURCE, C) %>%
  filter(DATASET=='RECENT') %>%
  ggplot(aes(OBS, MODEL)) +
  geom_point() +
  geom_abline() +
  geom_hline(yint=0, alpha=0) +
  geom_vline(xint=0, alpha=0) +
  labs(x='FWM Concentration from Direct Observations (ppb)', 
       y='FWM Concentration from Daily Load Model (ppb)', 
       title='Comparison of FWM Concentrations between Daily Load Model and Direct Observations\nDataset: RECENT') +
  facet_wrap(~VAR, scales='free')

for (variable in c('TP', 'PO4', 'TN', 'NO23', 'NH4')) {
  p <- filter(df_wyr, VAR==variable, WYEAR>=2002) %>%
    ggplot(aes(factor(WYEAR), C, fill=SOURCE)) +
    geom_bar(position='dodge', stat='identity') +
    facet_grid(SITE_NAME~DATASET, scales='free_y') +
    labs(x='Water Year', y='FWM Concentration (ppb)', 
         title=paste0('Annual FWM Concentrations by Site and Water Year\nbased on Load Model and Direct Observations\nVariable: ', variable)) +
    scale_fill_manual('', values=c('MODEL'='gray20', 'OBS'='deepskyblue')) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
          strip.text.y=element_text(size=6))
  print(p)
}

dev.off()