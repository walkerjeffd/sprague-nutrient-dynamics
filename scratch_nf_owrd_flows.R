library(dplyr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())

rm(list=ls())

load('kt_sprague.Rdata')
load('gis.Rdata')

q <- filter(wq.kt_sprague$RAW, VAR=="FLOW", SITE_NAME %in% c("NF", "NF_Ivory")) %>%
  select(DATE, SITE_NAME, VALUE) %>%
  filter(year(DATE)>=2009) %>%
  mutate(MONTH=ordered(month(DATE), levels=c(10:12, 1:9)))

nat <- data.frame(SITE=c(rep("UPPER", 12), rep("LOWER", 12)),
                  MONTH=rep(seq(1:12), times=2),
                  VALUE=c(c(61.1, 72, 91.4, 168, 247, 127, 56.7, 47.2, 49.2, 54.1, 71.6, 71),
                          c(126, 146, 170, 270, 379, 234, 126, 102, 110, 120, 136, 132))) %>%
  mutate(MONTH=ordered(MONTH, levels=c(10:12, 1:9)))

ggplot(q, aes(MONTH, VALUE)) + geom_boxplot() + geom_point(aes(color=SITE), data=nat) + facet_wrap(~SITE_NAME)
