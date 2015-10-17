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

df_wyr <- loads_df$wyr %>%
  mutate(VALUE=ifelse(TERM=="C", VALUE, VALUE/N_DAY))
df_wyr_q <- filter(df_wyr, VAR=="FLOW") %>%
  spread(TERM, VALUE) %>%
  select(-VAR)
df_wyr <- filter(df_wyr, VAR!="FLOW") %>%
  left_join(df_wyr_q) %>%
  spread(TERM, VALUE)

filter(df_wyr,
       SEASON == 'Annual',
       SITE_NAME %in% stn.kt_sprague$SITE_NAME) %>%
  ggplot(aes(Q, C, color=WYEAR)) +
  geom_point() +
  scale_color_gradientn(colours=RColorBrewer::brewer.pal(n=6, name="GnBu")) +
  facet_grid(VAR~SITE_NAME, scales='free')