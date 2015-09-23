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
load('gis.Rdata')

p1 <- loads_df$wyr %>%
  filter(VAR %in% c('TN', 'NH4', 'NO23'), TERM=='C',
         WYEAR >= 2009,
         SITE_NAME %in% stn.kt_sprague$SITE_NAME) %>%
  filter(!(SITE_NAME %in% c('SF_Ivory', 'NF_Ivory'))) %>%
  ggplot(aes(factor(WYEAR), VALUE, fill=DATASET)) +
  geom_bar(stat='identity', position='dodge') +
  facet_grid(VAR~SITE_NAME, scales='free_y') +
  theme(legend.position='top',
        axis.text.x=element_text(angle=90, hjust=0, vjust=0.5)) +
  labs(x="Water Year", y="FWM Concentration (ppb)")

p2 <- loads_df$wyr %>%
  filter(VAR %in% c('TN', 'NH4', 'NO23'), TERM=='C',
         WYEAR >= 2009,
         SITE_NAME %in% stn.kt_sprague$SITE_NAME) %>%
  filter(!(SITE_NAME %in% c('SF_Ivory', 'NF_Ivory'))) %>%
  spread(DATASET, VALUE) %>%
  mutate(DIFF=POR-RECENT,
         REL_DIFF=DIFF/((POR+RECENT)/2)) %>%
  ggplot(aes(factor(WYEAR), REL_DIFF)) +
  geom_bar(stat='identity', position='dodge') +
  scale_y_continuous(labels=scales::percent) +
  facet_grid(VAR~SITE_NAME, scales='free_y') +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5)) +
  labs(x="Water Year", y="Percent Difference\n[POR-RECENT] / [(POR+RECENT)/2]")

pdf('pdf/nitrogen-dataset-comparison.pdf', width=11, height=8.5)
grid.arrange(grobs=list(p1, p2), nrow=2)
dev.off()