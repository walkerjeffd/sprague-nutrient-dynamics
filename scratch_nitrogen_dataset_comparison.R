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

dl <- readRDS('detection_limits.Rdata')

dl <- gather(dl, TERM, VALUE, -VAR) %>%
  filter(VAR %in% c('TN', 'NH4', 'NO23')) %>%
  mutate(VALUE=VALUE*1000,
         TERM=ordered(TERM, levels=c('UPPERDL', 'LOWERDL')))

p.wyr <- loads_df$wyr %>%
  filter(VAR %in% c('TN', 'NH4', 'NO23'), TERM=='C',
         SEASON == "Annual",
         WYEAR >= 2010,
         SITE_NAME %in% stn.kt_sprague$SITE_NAME) %>%
  # filter(!(SITE_NAME %in% c('SF_Ivory', 'NF_Ivory'))) %>%
  ggplot(aes(factor(WYEAR), VALUE)) +
  geom_bar(aes(fill=DATASET), stat='identity', position='dodge') +
  geom_hline(aes(yintercept=VALUE, x=NULL, y=NULL, linetype=TERM), data=dl, show_guide=TRUE) +
  scale_linetype_discrete('Detection Limit',
                          labels=c('LOWERDL'='Lower', 'UPPERDL'='Higher')) +
  scale_fill_manual('Detection Limit', values=c('grey50', 'deepskyblue'),
                    labels=c('RECENT'='Lower', 'POR'='Higher')) +
  facet_grid(VAR~SITE_NAME, scales='free_y') +
  labs(x="Water Year", y="FWM Concentration (ppb)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        strip.background=element_blank(),
        strip.text=element_text(face='bold', size=12),
        aspect.ratio=1)

# p2 <- loads_df$wyr %>%
#   filter(SEASON == "Annual",
#          VAR %in% c('TN', 'NH4', 'NO23'), TERM=='C',
#          WYEAR >= 2010,
#          SITE_NAME %in% stn.kt_sprague$SITE_NAME) %>%
#   filter(!(SITE_NAME %in% c('SF_Ivory', 'NF_Ivory'))) %>%
#   spread(DATASET, VALUE) %>%
#   mutate(DIFF=POR-RECENT,
#          REL_DIFF=DIFF/((POR+RECENT)/2)) %>%
#   ggplot(aes(factor(WYEAR), REL_DIFF)) +
#   geom_bar(stat='identity', position='dodge') +
#   scale_y_continuous(labels=scales::percent) +
#   facet_grid(VAR~SITE_NAME, scales='free_y') +
#   theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5)) +
#   labs(x="Water Year", y="Percent Difference\n[POR-RECENT] / [(POR+RECENT)/2]")

p.site <- loads_df$site %>%
  filter(VAR %in% c('TN', 'NH4', 'NO23'), TERM=='C',
         SEASON == "Annual",
         PERIOD == "2010-2014",
         SITE_NAME %in% stn.kt_sprague$SITE_NAME) %>%
  # filter(!(SITE_NAME %in% c('SF_Ivory', 'NF_Ivory'))) %>%
  ggplot(aes(SITE_NAME, VALUE)) +
  geom_bar(aes(fill=DATASET), stat='identity', position='dodge') +
  geom_hline(aes(yintercept=VALUE, x=NULL, y=NULL, linetype=TERM), data=dl, show_guide=TRUE) +
  scale_linetype_discrete('Detection Limit',
                          labels=c('LOWERDL'='Lower', 'UPPERDL'='Higher')) +
  scale_fill_manual('Detection Limit', values=c('grey50', 'deepskyblue'),
                    labels=c('RECENT'='Lower', 'POR'='Higher')) +
  facet_wrap(~VAR, scales='free_y') +
  labs(x="Station", y="FWM Concentration (ppb)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        strip.background=element_blank(),
        strip.text=element_text(face='bold', size=12),
        aspect.ratio=1)


pdf('pdf/nitrogen-dataset-comparison.pdf', width=11, height=8.5)
print(p.site)
print(p.wyr)
dev.off()


