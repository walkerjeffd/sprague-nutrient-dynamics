library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(knitr)
library(ggmap)
library(gridExtra)
library(scales)
library(fluxr)
library(maptools)
library(gpclib)
library(sp)
gpclibPermit()
theme_set(theme_bw())
opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)

load('kt_sprague.Rdata')
load('loads.Rdata')
load('gis.Rdata')

df_wyr <- loads_df$wyr
df_mon <- loads_df$mon
df_site <- loads_df$site

filter(df_mon, DATASET=='POR', SITE_NAME %in% c('Power', 'Lone_Pine'), VAR %in% c('FLOW', 'TP'), TERM %in% c('C','L','Q')) %>%
  ggplot(aes(MONTHYEAR, VALUE, color=SITE_NAME)) +
  geom_line() +
  facet_wrap(~TERM, ncol=1, scales='free_y')
filter(loads_df$day, DATASET=='POR', SITE_NAME %in% c('Power', 'Lone_Pine'), VAR %in% c('FLOW', 'TP'), TERM %in% c('C','L','Q')) %>%
  ggplot(aes(DATE, VALUE, color=SITE_NAME)) +
  geom_line() +
  facet_wrap(~TERM, ncol=1, scales='free_y')


# problem pdfs ----
df_day <- lapply(names(loads), function(dataset) {
  lapply(names(loads[[dataset]]), function(variable) {
    lapply(names(loads[[dataset]][[variable]]), function(site) {
      x <- loads[[dataset]][[variable]][[site]][['out']][['day']]
      x$DATASET <- dataset
      x$VAR <- variable
      x$SITE_NAME <- site
      x
    }) %>%
      rbind_all
  }) %>%
    rbind_all
}) %>%
  rbind_all

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
  rbind_all


dataset <- 'POR'
variable <- 'TP'
p1 <- filter(df_wyr, DATASET==dataset, VAR %in% c('FLOW', variable), SITE_NAME %in% c('Power', 'Lone_Pine')) %>%
  ggplot(aes(factor(WYEAR), VALUE, fill=SITE_NAME)) +
  geom_bar(stat='identity', position='dodge') +
  facet_wrap(~TERM, scales='free_y', nrow=1) +
  theme(legend.position='top', axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  labs(x='', y='Value') +
  scale_fill_manual(values=c('Power'='black', 'Lone_Pine'='orangered')) 
p2 <- filter(df_wyr, DATASET==dataset, VAR %in% c('FLOW', variable), SITE_NAME %in% c('Power-Lone_Pine')) %>%
  ggplot(aes(factor(WYEAR), VALUE)) +
  geom_bar(stat='identity', position='dodge') +
  facet_wrap(~TERM, scales='free_y', nrow=1) +
  labs(x='', y='Power - Lone_Pine') +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
p3 <- filter(df_day, DATASET==dataset, VAR==variable, SITE_NAME %in% c('Power', 'Lone_Pine')) %>%
  select(SITE_NAME, DATE, Q, C, L) %>%
  gather(TERM, VALUE, Q:L) %>%
  ggplot(aes(DATE, VALUE, color=SITE_NAME)) +
  geom_line() +
  geom_point(data=filter(df_obs, DATASET==dataset, VAR==variable, SITE_NAME %in% c('Power', 'Lone_Pine')) %>%
               select(SITE_NAME, DATE, Q, C) %>% 
               mutate(L=Q*C) %>% 
               gather(TERM, VALUE, Q, C, L) %>% 
               filter(TERM != 'Q'),
             size=1.2) +
  scale_color_manual(values=c('Power'='black', 'Lone_Pine'='orangered'), guide=FALSE) +
  facet_wrap(~TERM, scales='free_y', ncol=1) +
  labs(x='', y='Value')
p4 <- filter(df_mon, DATASET=='POR', SITE_NAME %in% c('Power', 'Lone_Pine'), VAR %in% c('FLOW', 'TP'), TERM %in% c('C','L','Q')) %>%
  ggplot(aes(MONTHYEAR, VALUE, color=SITE_NAME)) +
  geom_line() +
  facet_wrap(~TERM, ncol=1, scales='free_y') +
  scale_color_manual(values=c('Power'='black', 'Lone_Pine'='orangered'), guide=FALSE) +
  labs(x='', y='Value')


pdf(file.path('pdf', 'issues', paste0(tolower(dataset), '-', tolower(variable), '-power-lone.pdf')), height=11, width=8.5)
grid.arrange(arrangeGrob(p1, p2, ncol=1), arrangeGrob(p3, p4, ncol=2), ncol=1, heights=c(1/2, 1/2), 
             main=paste0('\nAnnual and Daily Flow/Load/Conc between Power and Lone\nDataset: ', dataset, ' | Variable: ', variable))
dev.off()

dataset <- 'POR'
variable <- 'TP'
p1 <- filter(df_wyr, DATASET==dataset, VAR %in% c('FLOW', variable), SITE_NAME %in% c('Lone_Pine', 'Sycan', 'Godowa')) %>%
  ggplot(aes(factor(WYEAR), VALUE, fill=SITE_NAME)) +
  geom_bar(stat='identity', position='dodge') +
  facet_wrap(~TERM, scales='free_y', nrow=1) +
  theme(legend.position='top', axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  labs(x='', y='Value') +
  scale_fill_manual(values=c('Lone_Pine'='black', 'Sycan'='deepskyblue', 'Godowa'='orangered')) 
p2 <- filter(df_wyr, DATASET==dataset, VAR %in% c('FLOW', variable), SITE_NAME %in% c('Lone_Pine-Godowa-Sycan')) %>%
  ggplot(aes(factor(WYEAR), VALUE)) +
  geom_bar(stat='identity', position='dodge') +
  facet_wrap(~TERM, scales='free_y', nrow=1) +
  labs(x='', y='Lone_Pine-(Sycan+Godowa)') +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
p3 <- filter(df_day, DATASET==dataset, VAR==variable, SITE_NAME %in% c('Lone_Pine', 'Sycan', 'Godowa')) %>%
  select(SITE_NAME, DATE, Q, C, L) %>%
  gather(TERM, VALUE, Q:L) %>%
  ggplot(aes(DATE, VALUE, color=SITE_NAME)) +
  geom_line() +
  geom_point(data=filter(df_obs, DATASET==dataset, VAR==variable, SITE_NAME %in% c('Lone_Pine', 'Sycan', 'Godowa')) %>% 
               select(SITE_NAME, DATE, Q, C) %>% 
               mutate(L=Q*C) %>% 
               gather(TERM, VALUE, Q, C, L) %>% 
               filter(TERM != 'Q')) +
  scale_color_manual(values=c('Lone_Pine'='black', 'Sycan'='deepskyblue', 'Godowa'='orangered'), guide=FALSE) +
  facet_wrap(~TERM, scales='free_y', ncol=1) +
  scale_y_log10() +
  labs(x='', y='Value')

pdf(file.path('pdf', 'issues', paste0(tolower(dataset), '-', tolower(variable), '-lone_pine-godowa-sycan.pdf')), height=11, width=8.5)
grid.arrange(arrangeGrob(p1, p2, ncol=1), p3, ncol=1, heights=c(1/2, 1/2),
             main=paste0('\nAnnual and Daily Flow/Load/Conc between Lone_Pine and Godowa+Sycan\nDataset: ', dataset, ' | Variable: ', variable))
dev.off()



p <- filter(df_day, DATASET=='POR', wyear(DATE)==2003, VAR=='TP', SITE_NAME %in% c('Lone_Pine', 'Sycan', 'Godowa')) %>%
  select(SITE_NAME, DATE, Q, C, L) %>%
  gather(TERM, VALUE, Q:L) %>%
  filter(TERM %in% c('Q', 'L')) %>%
  spread(SITE_NAME, VALUE) %>% 
  mutate('Lone_Pine-Godowa-Sycan' = Lone_Pine - Godowa - Sycan) %>%
  gather(SITE_NAME, VALUE, -DATE, -TERM) %>%
  #   spread(TERM, VALUE) %>%
  #   mutate(C=L/Q) %>%
  #   gather(TERM, VALUE, Q:C) %>%
  filter(SITE_NAME %in% c('Lone_Pine-Godowa-Sycan')) %>%
  ggplot(aes(DATE, VALUE)) +
  geom_line() +
  #   geom_point(data=filter(df_obs, wyear(DATE)==2003, DATASET=='POR', VAR=="TP", SITE_NAME %in% c('Lone_Pine', 'Sycan', 'Godowa')) %>% 
  #                select(SITE_NAME, DATE, Q, C) %>% 
  #                mutate(L=Q*C) %>% 
  #                gather(TERM, VALUE, Q, C, L) %>% 
  #                filter(TERM != 'Q')) +
  #   scale_color_manual(values=c('Lone_Pine'='black', 'Sycan'='deepskyblue', 'Godowa'='orangered')) +
  facet_wrap(~TERM, scales='free_y', ncol=1) +
  labs(x='', y='Value')

p.cumsum <- filter(df_day, DATASET=='POR', wyear(DATE)==2003, VAR=='TP', SITE_NAME %in% c('Lone_Pine', 'Sycan', 'Godowa')) %>%
  select(SITE_NAME, DATE, Q, C, L) %>%
  gather(TERM, VALUE, Q:L) %>%
  filter(TERM %in% c('Q', 'L')) %>%
  spread(SITE_NAME, VALUE) %>% 
  mutate('Lone_Pine-Godowa-Sycan' = Lone_Pine - Godowa - Sycan) %>%
  gather(SITE_NAME, VALUE, -DATE, -TERM) %>%
  filter(SITE_NAME %in% c('Lone_Pine-Godowa-Sycan')) %>%
  group_by(TERM) %>%
  mutate(CUMVALUE=cumsum(VALUE)) %>%
  ggplot(aes(DATE, CUMVALUE)) +
  geom_line() +
  #   geom_point(data=filter(df_obs, wyear(DATE)==2003, DATASET=='POR', VAR=="TP", SITE_NAME %in% c('Lone_Pine', 'Sycan', 'Godowa')) %>% 
  #                select(SITE_NAME, DATE, Q, C) %>% 
  #                mutate(L=Q*C) %>% 
  #                gather(TERM, VALUE, Q, C, L) %>% 
  #                filter(TERM != 'Q')) +
  #   scale_color_manual(values=c('Lone_Pine'='black', 'Sycan'='deepskyblue', 'Godowa'='orangered')) +
  facet_wrap(~TERM, scales='free_y', ncol=1) +
  labs(x='', y='Value')

grid.arrange(p, p.cumsum, ncol=2)

filter(df_day, DATASET=='POR', wyear(DATE)==2003, VAR=='TP', SITE_NAME %in% c('Lone_Pine', 'Sycan', 'Godowa')) %>%
  select(SITE_NAME, DATE, Q, C, L) %>%
  gather(TERM, VALUE, Q:L) %>%
  filter(TERM %in% c('Q', 'L')) %>%
  spread(SITE_NAME, VALUE) %>% 
  mutate('Lone_Pine-Godowa-Sycan' = Lone_Pine - Godowa - Sycan,
         'Godowa+Sycan' = Godowa + Sycan) %>%
  gather(SITE_NAME, VALUE, -DATE, -TERM) %>%
  spread(TERM, VALUE) %>%
  mutate(C=L/Q) %>%
  gather(TERM, VALUE, Q:C) %>%
  filter(SITE_NAME %in% c('Lone_Pine', 'Godowa+Sycan')) %>%
  ggplot(aes(DATE, VALUE, color=SITE_NAME)) +
  geom_line() +
  #   geom_point(data=filter(df_obs, wyear(DATE)==2003, DATASET=='POR', VAR=="TP", SITE_NAME %in% c('Lone_Pine', 'Sycan', 'Godowa')) %>% 
  #                select(SITE_NAME, DATE, Q, C) %>% 
  #                mutate(L=Q*C) %>% 
  #                gather(TERM, VALUE, Q, C, L) %>% 
  #                filter(TERM != 'Q')) +
  #   scale_color_manual(values=c('Lone_Pine'='black', 'Sycan'='deepskyblue', 'Godowa'='orangered')) +
  facet_wrap(~TERM, scales='free_y', ncol=1) +
  labs(x='', y='Value')

