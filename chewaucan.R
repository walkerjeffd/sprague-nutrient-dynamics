library(tidyverse)
library(lubridate)

rm(list = ls())

theme_set(theme_bw())

source("functions.R")

load("loads.Rdata")

flows_day <- filter(loads_df[['day']],
                    DATASET=="POR",
                    TERM %in% c("Q", "Q_AREA")) %>%
  droplevels %>%
  spread(TERM, VALUE) %>%
  mutate(Q=hm3d_cfs(Q))

# fetch from owrd
owrd_fetch <- read_tsv(url(paste0(
  "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx?station_nbr=",
  "10384000", # site number
  "&start_date=1/1/1912%2012:00:00%20AM", # set start date
  "&end_date=7/21/2021%2012:00:00%20AM&dataset=MDF&format=tsv"))) # set end date
chew <- owrd_fetch %>%
  transmute(site = "chewacuan", date = mdy(record_date), flow_cfs = mean_daily_flow_cfs)
chew %>%
  ggplot(aes(date, flow_cfs)) +
  geom_line()

chew %>%
  mutate(wyear = fluxr::wyear(date), wday = water_day(date)) %>%
  mutate(decade = factor(round(wyear / 10) * 10)) %>%
  ggplot(aes(wday, log10(flow_cfs))) +
  geom_line(aes(group = wyear, color = decade)) +
  facet_wrap(vars(decade))

chew %>%
  group_by(date = floor_date(date, "month")) %>%
  summarise(flow_cfs = mean(flow_cfs)) %>%
  mutate(wyear = fluxr::wyear(date), wday = water_day(date)) %>%
  mutate(decade = factor(round(wyear / 10) * 10)) %>%
  mutate(month = factor(as.character(month(date)), levels = as.character(c(10:12, 1:9)))) %>%
  ggplot(aes(month, (flow_cfs), color = decade)) +
  stat_summary(geom = "line", aes(group = decade)) +
  stat_summary(geom = "pointrange")

df_day <- flows_day %>%
  mutate(DATE = as_date(DATE)) %>%
  select(SITE_NAME, date = DATE, Q) %>%
  spread(SITE_NAME, Q) %>%
  as_tibble() %>%
  inner_join(
    chew %>%
      select(date, Chewaucan = flow_cfs),
    by = "date"
  ) %>%
  pivot_longer(-c(date, Chewaucan)) %>%
  mutate(name = factor(name, levels = names(loads$POR$TP)))

df_day %>%
  ggplot(aes(Chewaucan, value)) +
  geom_point(size = 0.5, alpha = 0.5) +
  facet_wrap(vars(name), scales = "free", strip.position = "left") +
  labs(y = NULL) +
  theme(strip.background = element_blank(), strip.placement = "outside")

df_mon <- df_day %>%
  filter(!is.na(value)) %>%
  group_by(name, date = floor_date(date, unit = "month")) %>%
  summarise(
    value = mean(value),
    Chewaucan = mean(Chewaucan)
  )

df_mon %>%
  filter(month(date) %in% 8:9) %>%
  ggplot(aes(Chewaucan, value)) +
  geom_point(aes(color = fluxr::wyear(date) >= 2013)) +
  # geom_smooth(aes(color = fluxr::wyear(date) >= 2013), se = FALSE) +
  facet_grid(vars(name), vars(month(date)), scales = "free") +
  labs(y = NULL) +
  theme(strip.background = element_blank(), strip.placement = "outside")



# beatty ------------------------------------------------------------------

load("owrd.Rdata")
beatty <- q.owrd %>%
  filter(SITE_NAME == "Beatty") %>%
  select(date = DATE, flow_cfs = FLOW) %>%
  as_tibble() %>%
  mutate(site = "beatty")
chew
df2_day <- bind_rows(beatty, chew) %>%
  filter(date >= min(beatty$date))
df2_mon <- df2_day %>%
  group_by(site, date = floor_date(date, unit = "month")) %>%
  summarise(
    flow_cfs = mean(flow_cfs)
  )

df2_mon %>%
  ggplot(aes(date, log10(flow_cfs))) +
  geom_line(aes(color = site))

df2_mon %>%
  spread(site, flow_cfs) %>%
  ggplot(aes(chewacuan, beatty)) +
  geom_point(aes(color = year(date) >= 2013)) +
  scale_x_log10() +
  scale_y_log10()

df2_mon %>%
  mutate(month = factor(month(date, label = TRUE), levels = month.abb[c(10:12, 1:9)])) %>%
  ggplot(aes(month, flow_cfs)) +
  geom_line(aes(color = site, group = interaction(site, fluxr::wyear(date)))) +
  scale_y_log10()


df2_mon %>%
  mutate(month = factor(month(date, label = TRUE), levels = month.abb[c(10:12, 1:9)])) %>%
  mutate(period = fluxr::wyear(date) >= 2013) %>%
  ggplot(aes(month, flow_cfs)) +
  geom_line(aes(color = site, group = interaction(site, fluxr::wyear(date)))) +
  scale_y_log10() +
  facet_wrap(vars(period))

df2_mon %>%
  mutate(month = factor(month(date, label = TRUE), levels = month.abb[c(10:12, 1:9)])) %>%
  mutate(period = fluxr::wyear(date) >= 2013) %>%
  ggplot(aes(month, flow_cfs)) +
  geom_line(aes(color = period, alpha = period, group = interaction(fluxr::wyear(date)))) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_color_manual(values = c("gray50", "orangered")) +
  scale_y_log10() +
  facet_wrap(vars(site))



df2_mon %>%
  filter(year(date) >= 1980) %>%
  mutate(month = factor(month(date, label = TRUE), levels = month.abb[c(10:12, 1:9)])) %>%
  mutate(period = fluxr::wyear(date) >= 2013) %>%
  ggplot(aes(month, flow_cfs)) +
  geom_line(aes(color = period, alpha = period, group = interaction(fluxr::wyear(date)))) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_color_manual(values = c("gray50", "orangered")) +
  # scale_y_log10() +
  coord_cartesian(ylim = c(0, 200)) +
  facet_wrap(vars(site))



df2_mon %>%
  filter(year(date) >= 1980, month(date) %in% c(7:10)) %>%
  mutate(month = factor(month(date, label = TRUE), levels = month.abb)) %>%
  mutate(period = fluxr::wyear(date) >= 2013) %>%
  spread(site, flow_cfs) %>%
  ggplot(aes(chewacuan, beatty)) +
  geom_point(aes(color = period)) +
  geom_smooth(method = "lm", se = FALSE, aes(color = period)) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_color_manual(values = c("gray50", "orangered")) +
  facet_wrap(vars(month), scales = "free")


