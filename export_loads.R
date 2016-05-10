library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())

load('loads.Rdata')

df_day <- loads_df$day %>%
  filter(DATASET=="POR") %>%
  filter(SITE_NAME %in% c("Power", "Lone_Pine", "Godowa", "Sycan", "SF_Ivory", "SF", "NF_Ivory", "NF")) %>%
  select(SITE_NAME, DATE, MONTH, WYEAR, VAR, TERM, VALUE)

df_day_flow <- filter(df_day, VAR=="FLOW", TERM=="Q") %>%
  select(SITE_NAME, DATE, TERM, VALUE) %>%
  spread(TERM, VALUE)

df_day_wq <- filter(df_day, VAR!="FLOW", TERM!="Q") %>%
  select(SITE_NAME, DATE, VAR, TERM, VALUE) %>%
  spread(TERM, VALUE)

df_day <- left_join(df_day_wq, df_day_flow, by=c("SITE_NAME", "DATE")) %>%
  select(SITE_NAME, DATE, VAR, Q, L, C)

df_day %>%
  mutate(DATE = format(DATE, "%Y-%m-%d")) %>%
  write.csv("~/sprague_loads_daily_20160510.csv", row.names=FALSE)

df_mon <- loads_df$mon %>%
  filter(DATASET=="POR") %>%
  filter(SITE_NAME %in% c("Power", "Lone_Pine", "Godowa", "Sycan", "SF_Ivory", "SF", "NF_Ivory", "NF")) %>%
  select(SITE_NAME, MONTHYEAR, MONTH, WYEAR, VAR, TERM, VALUE)

df_mon_flow <- filter(df_mon, VAR=="FLOW", TERM=="Q") %>%
  select(SITE_NAME, MONTHYEAR, TERM, VALUE) %>%
  spread(TERM, VALUE)

df_mon_wq <- filter(df_mon, VAR!="FLOW", TERM!="Q") %>%
  select(SITE_NAME, MONTHYEAR, VAR, TERM, VALUE) %>%
  spread(TERM, VALUE)

df_mon <- left_join(df_mon_wq, df_mon_flow, by=c("SITE_NAME", "MONTHYEAR")) %>%
  select(SITE_NAME, MONTHYEAR, VAR, Q, L, C)

df_mon %>%
  mutate(MONTHYEAR = format(MONTHYEAR, "%Y-%m-%d")) %>%
  write.csv("~/sprague_loads_monthly_20160510.csv", row.names=FALSE)

df_mon_stats <- df_mon %>%
  mutate(MONTH = month(MONTHYEAR),
         MONTH = ordered(MONTH, levels=c(10:12, 1:9))) %>%
  group_by(SITE_NAME, VAR, MONTH) %>%
  summarise(Q=mean(Q),
            L=mean(L),
            C=L/Q) %>%
  ungroup

df_mon_stats %>%
  write.csv("~/sprague_loads_monthly_stats_20160510.csv", row.names=FALSE)

