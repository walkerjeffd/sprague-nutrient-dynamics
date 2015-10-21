library(dplyr)
library(tidyr)
library(lubridate)
library(fluxr)
library(ggplot2)
theme_set(theme_bw())
library(gridExtra)

rm(list=ls())

source('functions.R')

# load data ----
load('kt_sprague.Rdata')
load('loads.Rdata')

seasons <- list(Annual=1:12,
                'Fall (Oct-Dec)'=c(10:12),
                'Winter (Jan-Mar)'=c(1:3),
                'Spring (Apr-Jun)'=c(4:6),
                'Summer (Jul-Sep)'=c(7:9))
df_seasons <- lapply(names(seasons), function(s) {
  data.frame(MONTH=seasons[[s]], SEASON=s, stringsAsFactors=FALSE)
}) %>%
  rbind_all %>%
  mutate(SEASON=ordered(SEASON, levels=names(seasons)))


df_day <- loads_df[['day']]

df_day <- left_join(df_seasons, df_day, by="MONTH")

df_day_q <- filter(df_day, DATASET=="POR", TERM=="Q") %>%
  select(SEASON, SITE_NAME, DATE, Q=VALUE)

df_day <- left_join(df_day, df_day_q, by=c("SEASON", "SITE_NAME", "DATE"))

filter(df_day, SEASON=="Annual", DATASET=='POR', TERM=='C', VAR %in% c('TP', 'PO4', 'PP')) %>%
  spread(VAR, VALUE) %>%
  mutate(PO4_TP=PO4/TP) %>%
  ggplot(aes(DATE, PO4_TP)) +
  geom_line() +
  facet_wrap(~SITE_NAME)

filter(df_day, SEASON=="Annual", DATASET=='POR', TERM=='C', VAR %in% c('TP', 'PO4', 'PP')) %>%
  spread(VAR, VALUE) %>%
  mutate(PO4_TP=PO4/TP,
         WDAY=water_day(DATE)) %>%
  ggplot(aes(WDAY, PO4_TP)) +
  geom_point(size=1) +
  facet_wrap(~SITE_NAME)

filter(df_day, SEASON=="Annual", DATASET=='POR', TERM=='C', VAR %in% c('TP', 'PO4', 'PP')) %>%
  spread(VAR, VALUE) %>%
  mutate(PO4_TP=PO4/TP,
         WDAY=water_day(DATE)) %>%
  ggplot(aes(WDAY, PO4_TP)) +
  geom_point(size=1) +
  facet_wrap(~SITE_NAME)

filter(df_day, DATASET=='POR', TERM=='C', VAR %in% c('TP', 'PO4', 'PP')) %>%
  spread(VAR, VALUE) %>%
  mutate(PO4_TP=PO4/TP,
         WDAY=water_day(DATE)) %>%
  ggplot(aes(Q, PO4_TP)) +
  geom_point(size=1) +
  facet_grid(SITE_NAME~SEASON, scales='free')



x <- left_join(df_seasons, mutate(wq.kt_sprague$POR, MONTH=month(DATE))) %>%
  select(SEASON, MONTH, DATE, SITE_NAME, VAR, VALUE) %>%
  spread(VAR, VALUE) %>%
  mutate(WYEAR=wyear(DATE),
         WDAY=water_day(DATE),
         WDAY_LABEL=as.Date("2000-10-01") + days(WDAY),
         PO4_TP=ifelse(PO4>TP, 1, PO4/TP))

x %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(WDAY_LABEL, PO4_TP)) +
  geom_point(size=1) +
  geom_smooth() +
  facet_wrap(~SITE_NAME, nrow=2) +
  scale_x_date(labels=scales::date_format('%b')) +
  theme(aspect.ratio=1)

x %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(WDAY_LABEL, 1-PO4_TP)) +
  geom_point(size=1) +
  geom_smooth() +
  facet_wrap(~SITE_NAME, nrow=2) +
  scale_x_date(labels=scales::date_format('%b')) +
  theme(aspect.ratio=1)

x %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(WDAY_LABEL, log10(FLOW))) +
  geom_point(size=1) +
  geom_smooth() +
  facet_wrap(~SITE_NAME, nrow=2) +
  scale_x_date(labels=scales::date_format('%b')) +
  theme(aspect.ratio=1)


x %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(WDAY, 1-PO4_TP, color=SITE_NAME)) +
  geom_smooth(se=FALSE) +
  theme(aspect.ratio=1)


x %>%
  filter(SEASON=="Annual", !is.na(TSS), !is.na(TP)) %>%
  ggplot(aes(TSS, TP)) +
  geom_point(size=1) +
  scale_y_log10() +
  scale_x_log10() +
  facet_wrap(~SITE_NAME, scales='free', nrow=2) +
  theme(aspect.ratio=1)
  # facet_grid(SITE_NAME~SEASON, scales="free")

x %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(FLOW, TSS)) +
  geom_point(size=1) +
  geom_smooth(method='lm', se=FALSE) +
  scale_y_log10() +
  scale_x_log10() +
  facet_wrap(~SITE_NAME, scales='free', nrow=2) +
  theme(aspect.ratio=1)
# facet_grid(SITE_NAME~SEASON, scales="free")

x %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(FLOW, TP)) +
  geom_point(size=1) +
  scale_y_log10() +
  scale_x_log10() +
  facet_wrap(~SITE_NAME, scales='free', nrow=2) +
  theme(aspect.ratio=1)
# facet_grid(SITE_NAME~SEASON, scales="free")


x %>%
  # filter(SEASON=="Annual") %>%
  ggplot(aes(FLOW, 1-PO4_TP)) +
  geom_point(size=1) +
  geom_smooth(se=FALSE) +
  scale_x_log10() +
  # facet_wrap(~SITE_NAME, scales='free', nrow=2) +
  theme(aspect.ratio=1) +
  facet_grid(SITE_NAME~SEASON, scales="free")

log_x <- scale_x_log10(breaks=log_breaks(seq(1, 9), 10^seq(-3, 3)),
                       labels=log_labels(c(1, 5), 10^seq(-3, 3)))

log_y <- scale_y_log10(breaks=log_breaks(seq(1, 9), 10^seq(-3, 3)),
                       labels=log_labels(c(1, 5), 10^seq(-3, 3)))


filename <- file.path('report', 'erosion-tss-flow.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- x %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(FLOW, TSS)) +
  geom_point(size=1) +
  geom_smooth(se=FALSE) +
  log_x +
  log_y +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="Flow (cfs)", y="TSS (mg/L)") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()


filename <- file.path('report', 'erosion-tss-tp.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- x %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(TSS, TP)) +
  geom_point(size=1) +
  geom_smooth(se=FALSE) +
  log_x +
  log_y +
  facet_wrap(~SITE_NAME, scales='free', nrow=2) +
  labs(x="TSS (mg/L)", y="TP (mg/L)") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()

filename <- file.path('report', 'erosion-tss-tp-particulate-fraction.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- x %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(TSS, 1-PO4_TP)) +
  geom_point(size=1) +
  geom_smooth(se=FALSE) +
  log_x +
  scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="TSS (mg/L)", y="Fraction TP as Particulate") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()


filename <- file.path('report', 'erosion-tp-particulate-fraction-flow.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- x %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(FLOW, 1-PO4_TP)) +
  geom_point(size=1) +
  geom_smooth(se=FALSE) +
  log_x +
  scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="Flow (cfs)", y="Fraction TP as Particulate") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()


filename <- file.path('report', 'erosion-tp-particulate-fraction-boxplot.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=6, height=4, res=200, units="in")
p <- x %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(SITE_NAME, 1-PO4_TP)) +
  geom_boxplot(fill='grey80') +
  scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
  labs(x="Station", y="Fraction TP as Particulate")
print(p)
dev.off()