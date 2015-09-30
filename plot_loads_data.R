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

df_obs <- lapply("POR", function(dataset) {
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
            C=L/Q) %>%
  ungroup

df_obs_site <- summarise(df_obs_wyr,
  N_SAMPLE=sum(N_SAMPLE),
  N_WYEAR=n(),
  Q=sum(Q),
  L=sum(L),
  C=L/Q) %>%
  ungroup

df_obs_day <- select(df_obs, VAR, SITE_NAME, DATE, Q, C) %>%
  mutate(SOURCE='OBS',
         L=Q*C)

df_mod_day <- filter(loads_df[['day']], DATASET=="POR") %>%
  select(VAR, SITE_NAME, DATE, TERM, VALUE)
df_mod_day_flow <- filter(df_mod_day, VAR=="FLOW") %>%
  spread(TERM, VALUE) %>%
  select(-VAR)
df_mod_day <- filter(df_mod_day, VAR!="FLOW") %>%
  spread(TERM, VALUE) %>%
  left_join(df_mod_day_flow, by=c('SITE_NAME', 'DATE')) %>%
  filter(SITE_NAME %in% stn.kt_sprague$SITE_NAME,
         VAR != "PP") %>%
  mutate(SOURCE='MODEL') %>%
  droplevels

df_day <- rbind(df_obs_day, df_mod_day) %>%
  mutate(WYEAR=wyear(DATE))

filter(df_day, VAR=="TP") %>%
  ggplot(aes(factor(WYEAR), log10(C), fill=SOURCE)) +
  geom_boxplot(position='dodge', outlier.size=0) +
  scale_fill_manual(values=c('MODEL'='grey50', 'OBS'='orangered')) +
  facet_wrap(~SITE_NAME)

df_day %>%
  ggplot(aes(SITE_NAME, log10(C), fill=SOURCE)) +
  geom_boxplot(position='dodge', outlier.size=0) +
  scale_fill_manual(values=c('MODEL'='grey50', 'OBS'='orangered')) +
  facet_wrap(~VAR, scales='free_y')

filter(df_day, VAR=="TP") %>%
  arrange(SOURCE) %>%
  ggplot(aes(log10(Q), log10(C), color=SOURCE, size=SOURCE)) +
  geom_point(alpha=0.8) +
  scale_color_manual(values=c('MODEL'='black', 'OBS'='orangered')) +
  scale_size_manual(values=c('MODEL'=1, 'OBS'=2)) +
  facet_wrap(~SITE_NAME)



# df_site <- filter(loads_df[['site']], SEASON=="Annual", VAR != 'FLOW') %>%
#   spread(TERM, VALUE) %>%
#   select(DATASET, VAR, SITE_NAME, C) %>%
#   mutate(SOURCE='MODEL') %>%
#   rbind(select(df_obs_site, DATASET, VAR, SITE_NAME, C) %>% mutate(SOURCE='OBS')) %>%
#   filter(SITE_NAME %in% stn.kt_sprague$SITE_NAME,
#          VAR != 'PP') %>%
#   droplevels
#
# df_wyr <- filter(loads_df[['wyr']], SEASON=="Annual", VAR != 'FLOW') %>%
#   spread(TERM, VALUE) %>%
#   select(DATASET, VAR, SITE_NAME, WYEAR, C) %>%
#   mutate(SOURCE='MODEL') %>%
#   rbind(select(df_obs_wyr, DATASET, VAR, SITE_NAME, WYEAR, C) %>% mutate(SOURCE='OBS')) %>%
#   filter(SITE_NAME %in% stn.kt_sprague$SITE_NAME,
#          VAR != 'PP') %>%
#   droplevels
#
# filename <- file.path('pdf', 'loads-model-vs-obs.pdf')
# cat('Printing:', filename, '\n')
# pdf(filename, width=11, height=8.5)
#
# p <- ggplot(df_site, aes(SITE_NAME, C, fill=SOURCE)) +
#   geom_bar(position='dodge', stat='identity') +
#   facet_grid(VAR~DATASET, scales='free_y') +
#   labs(x='', y='FWM Concentration (ppb)', title='FWM Concentrations by Site based on Load Model and Direct Observations') +
#   scale_fill_manual('', values=c('MODEL'='gray20', 'OBS'='deepskyblue')) +
#   theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
# print(p)
#
# p <- spread(df_site, SOURCE, C) %>%
#   filter(DATASET=='POR') %>%
#   ggplot(aes(OBS, MODEL)) +
#   geom_point() +
#   geom_abline() +
#   geom_hline(yint=0, alpha=0) +
#   geom_vline(xint=0, alpha=0) +
#   labs(x='FWM Concentration from Direct Observations (ppb)',
#        y='FWM Concentration from Daily Load Model (ppb)',
#        title='Comparison of FWM Concentrations between Daily Load Model and Direct Observations\nDataset: POR') +
#   facet_wrap(~VAR, scales='free')
# print(p)
#
# p <- spread(df_site, SOURCE, C) %>%
#   filter(DATASET=='RECENT') %>%
#   ggplot(aes(OBS, MODEL)) +
#   geom_point() +
#   geom_abline() +
#   geom_hline(yint=0, alpha=0) +
#   geom_vline(xint=0, alpha=0) +
#   labs(x='FWM Concentration from Direct Observations (ppb)',
#        y='FWM Concentration from Daily Load Model (ppb)',
#        title='Comparison of FWM Concentrations between Daily Load Model and Direct Observations\nDataset: RECENT') +
#   facet_wrap(~VAR, scales='free')
# print(p)
#
# for (variable in c('TP', 'PO4', 'TN', 'NO23', 'NH4')) {
#   p <- filter(df_wyr, VAR==variable, WYEAR>=2002) %>%
#     ggplot(aes(factor(WYEAR), C, fill=SOURCE)) +
#     geom_bar(position='dodge', stat='identity') +
#     facet_grid(SITE_NAME~DATASET, scales='free_y') +
#     labs(x='Water Year', y='FWM Concentration (ppb)',
#          title=paste0('Annual FWM Concentrations by Site and Water Year\nbased on Load Model and Direct Observations\nVariable: ', variable)) +
#     scale_fill_manual('', values=c('MODEL'='gray20', 'OBS'='deepskyblue')) +
#     theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
#           strip.text.y=element_text(size=6))
#   print(p)
# }
#
# dev.off()