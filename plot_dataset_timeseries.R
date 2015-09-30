library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(maptools)
library(ggplot2)
theme_set(theme_bw())

rm(list=ls())

source('functions.R')
load('kt_sprague.Rdata')

detection_limits <- readRDS('detection_limits.Rdata')

# scales
log_y <- scale_y_log10(breaks=log_breaks(seq(1, 9), 10^seq(-3, 3)),
                       labels=log_labels(c(1, 5), 10^seq(-3, 3)))
log_x <- scale_x_log10(breaks=log_breaks(seq(1, 9), 10^seq(-3, 3)),
                       labels=log_labels(c(1, 5), 10^seq(-3, 3)))

# timeseries plots ----
filename <- file.path('pdf', 'dataset-timeseries.pdf')
cat('Printing:', filename, '\n')
pdf(filename, width=11, height=8.5)
p <- filter(wq.kt_sprague$RAW, VAR %in% detection_limits$VAR) %>%
  filter(QAQC != 'NEGATIVE') %>%
  mutate(VAR = ordered(VAR, levels=c('FLOW', 'TP', 'PO4', 'TN', 'NH4', 'NO23', 'TSS'))) %>%
  arrange(QAQC) %>%
  gather(DL_GRP, DL_VALUE, DL, HALF_DL) %>%
  ggplot(aes(DATE)) +
  geom_line(aes(y=DL_VALUE, linetype=DL_GRP, group=DL_GRP), color='black') +
  # geom_line(aes(y=HALF_DL), color='black', linetype=2) +
  geom_point(aes(y=VALUE, color=QAQC, size=QAQC)) +
  log_y +
  scale_color_manual('QAQC', values=c('BLANK'='deepskyblue', 'LOW'='chartreuse3', 'PASS'='gray50', 'RPD'='black', 'OUTLIER'='orangered')) +
  scale_size_manual('QAQC', values=c('BLANK'=2, 'LOW'=2, 'PASS'=1, 'RPD'=2, 'OUTLIER'=2)) +
  scale_linetype_manual("Detection Limit", values=c("DL"=1, "HALF_DL"=2), labels=c("DL"="DL", "HALF_DL"="1/2 DL")) +
  labs(x='', y='Concentration (ppm)', title="Timeseries by Variable, Dataset: RAW") +
  facet_wrap(~VAR, scales='free_y', ncol=2) +
  theme(panel.grid.minor.y=element_blank())
print(p)

p <- filter(wq.kt_sprague$RAW, VAR %in% detection_limits$VAR) %>%
  filter(QAQC != 'NEGATIVE') %>%
  mutate(VAR = ordered(VAR, levels=c('FLOW', 'TP', 'PO4', 'TN', 'NH4', 'NO23', 'TSS'))) %>%
  arrange(QAQC) %>%
  gather(DL_GRP, DL_VALUE, DL, HALF_DL) %>%
  ggplot(aes(DATE)) +
  geom_line(aes(y=DL_VALUE, linetype=DL_GRP, group=DL_GRP), color='black') +
  geom_point(aes(y=VALUE, color=QAQC, size=QAQC)) +
  log_y +
  scale_color_manual('QAQC', values=c('BLANK'='deepskyblue', 'LOW'='chartreuse3', 'PASS'='gray50', 'RPD'='black', 'OUTLIER'='orangered')) +
  scale_size_manual('QAQC', values=c('BLANK'=2, 'LOW'=2, 'PASS'=1, 'RPD'=2, 'OUTLIER'=2)) +
  scale_linetype_manual("Detection Limit", values=c("DL"=1, "HALF_DL"=2), labels=c("DL"="DL", "HALF_DL"="1/2 DL")) +
  labs(x='', y='Concentration (ppm)', title="Timeseries by Variable and Site, Dataset: RAW") +
  facet_grid(VAR~SITE_NAME, scales='free_y') +
  theme(panel.grid.minor.y=element_blank(),
        axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8))
print(p)

# filter(wq.kt_sprague$CLEAN, VAR %in% detection_limits$VAR) %>%
#   gather(DL_GRP, DL_VALUE, DL, HALF_DL) %>%
#   ggplot(aes(DATE)) +
#   geom_line(aes(y=DL_VALUE, linetype=DL_GRP, group=DL_GRP), color='black') +
#   geom_point(aes(DATE, VALUE), size=1) +
#   scale_y_log10(breaks=fluxr::log_breaks(c(1, 2, 3, 5), 10^seq(-3, 3))) +
#   scale_linetype_manual("Detection Limit", values=c("DL"=1, "HALF_DL"=2), labels=c("DL"="DL", "HALF_DL"="1/2 DL")) +
#   labs(x='', y='Concentration (ppm)', title="CLEAN Dataset") +
#   facet_wrap(~VAR, scales='free_y', ncol=2) +
#   theme(panel.grid.minor.y=element_blank())

# filter(wq.kt_sprague$CLEAN, VAR %in% detection_limits$VAR) %>%
#   ggplot(aes(DATE)) +
#   geom_line(aes(y=DL), color='black') +
#   geom_line(aes(y=HALF_DL), color='black', linetype=2) +
#   geom_point(aes(DATE, VALUE), size=1) +
#   scale_y_log10(breaks=fluxr::log_breaks(c(1, 2, 3, 5), 10^seq(-3, 3))) +
#   labs(x='', y='Concentration (ppm)', title="CLEAN Dataset") +
#   facet_wrap(~VAR, scales='free_y', ncol=2) +
#   theme(panel.grid.minor.y=element_blank())

# filter(wq.kt_sprague$CLEAN, VAR %in% detection_limits$VAR) %>%
#   gather(DL_GRP, DL_VALUE, DL, HALF_DL) %>%
#   ggplot(aes(DATE)) +
#   geom_line(aes(y=DL_VALUE, linetype=DL_GRP, group=DL_GRP), color='black') +
#   geom_point(aes(DATE, VALUE), size=1) +
#   scale_y_log10(breaks=fluxr::log_breaks(c(1, 2, 3, 5), 10^seq(-3, 3))) +
#   scale_linetype_manual("Detection Limit", values=c("DL"=1, "HALF_DL"=2), labels=c("DL"="DL", "HALF_DL"="1/2 DL")) +
#   labs(x='', y='Concentration (ppm)', title="CLEAN Dataset") +
#   facet_grid(VAR~SITE_NAME, scales='free_y') +
#   theme(panel.grid.minor.y=element_blank(),
#         axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

p <- filter(wq.kt_sprague$POR, VAR %in% detection_limits$VAR) %>%
  ggplot(aes(DATE)) +
  geom_line(aes(y=UPPERDL, linetype='Detection\nLimit'), color='black') +
  geom_point(aes(y=VALUE, color=LIMITED), size=1) +
  scale_color_manual('Set to DL', values=c('TRUE'='orangered', 'FALSE'='black')) +
  scale_linetype('', drop=FALSE) +
  log_y +
  labs(x='', y='Concentration (ppm)', title="Timeseries by Variable, Dataset: POR") +
  facet_wrap(~VAR, scales='free_y', ncol=2) +
  theme(panel.grid.minor.y=element_blank())
print(p)

p <- filter(wq.kt_sprague$POR, VAR %in% detection_limits$VAR) %>%
  ggplot(aes(DATE)) +
  geom_line(aes(y=UPPERDL, linetype='Detection\nLimit'), color='black') +
  geom_point(aes(y=VALUE, color=LIMITED), size=1) +
  scale_color_manual('Set to DL', values=c('TRUE'='orangered', 'FALSE'='black')) +
  scale_linetype('', drop=FALSE) +
  log_y +
  labs(x='', y='Concentration (ppm)', title="Timeseries by Variable and Site, Dataset: POR") +
  facet_grid(VAR~SITE_NAME, scales='free_y') +
  theme(panel.grid.minor.y=element_blank(),
        axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8))
print(p)

p <- filter(wq.kt_sprague$RECENT, VAR %in% detection_limits$VAR) %>%
  ggplot(aes(DATE)) +
  geom_line(aes(y=LOWERDL, linetype='Detection\nLimit'), color='black') +
  geom_point(aes(y=VALUE), size=1) +
  scale_color_manual('Set to DL', values=c('TRUE'='orangered', 'FALSE'='black')) +
  scale_linetype('', drop=FALSE) +
  log_y +
  labs(x='', y='Concentration (ppm)', title="Timeseries by Variable, Dataset: RECENT") +
  facet_wrap(~VAR, scales='free_y', ncol=2) +
  theme(panel.grid.minor.y=element_blank())
print(p)

p <- filter(wq.kt_sprague$RECENT, VAR %in% detection_limits$VAR) %>%
  ggplot(aes(DATE)) +
  geom_line(aes(y=LOWERDL, linetype='Detection\nLimit'), color='black') +
  geom_point(aes(y=VALUE, color=LIMITED), size=1) +
  scale_color_manual('Set to DL', values=c('TRUE'='orangered', 'FALSE'='black')) +
  scale_linetype('', drop=FALSE) +
  log_y +
  labs(x='', y='Concentration (ppm)', title="Timeseries by Variable and Site, Dataset: RECENT") +
  facet_grid(VAR~SITE_NAME, scales='free_y') +
  theme(panel.grid.minor.y=element_blank(),
        axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8))
print(p)

dev.off()
