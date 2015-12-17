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

log_x <- scale_x_log10(breaks=log_breaks(seq(1, 9), 10^seq(-3, 3)),
                       labels=log_labels(c(1, 5), 10^seq(-3, 3)))

log_y <- scale_y_log10(breaks=log_breaks(seq(1, 9), 10^seq(-3, 3)),
                       labels=log_labels(c(1, 5), 10^seq(-3, 3)))

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

# observed data ----
df <- left_join(df_seasons, mutate(wq.kt_sprague$POR, MONTH=month(DATE)),
                by="MONTH") %>%
  select(SEASON, MONTH, DATE, SITE_NAME, VAR, VALUE) %>%
  spread(VAR, VALUE) %>%
  mutate(WYEAR=wyear(DATE),
         WDAY=water_day(DATE),
         WDAY_LABEL=as.Date("2000-10-01") + days(WDAY),
         PO4_TP=ifelse(PO4>TP, 1, PO4/TP),
         PP=ifelse(TP-PO4 < 0.001, 0.001, TP-PO4),
         INORGN=NH4+NO23,
         ORGN=TN-INORGN,
         ORGN=ifelse(ORGN<0.03, 0.03, ORGN))


# plots ----
df %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(WDAY_LABEL, PO4_TP)) +
  geom_point(size=1) +
  geom_smooth() +
  facet_wrap(~SITE_NAME, nrow=2) +
  scale_x_date(labels=scales::date_format('%b')) +
  theme(aspect.ratio=1)

df %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(WDAY_LABEL, 1-PO4_TP)) +
  geom_point(size=1) +
  geom_smooth() +
  facet_wrap(~SITE_NAME, nrow=2) +
  scale_x_date(labels=scales::date_format('%b')) +
  theme(aspect.ratio=1)

df %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(WDAY_LABEL, log10(FLOW))) +
  geom_point(size=1) +
  geom_smooth() +
  facet_wrap(~SITE_NAME, nrow=2) +
  scale_x_date(labels=scales::date_format('%b')) +
  theme(aspect.ratio=1)

df %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(WDAY, 1-PO4_TP, color=SITE_NAME)) +
  geom_smooth(se=FALSE) +
  theme(aspect.ratio=1)

df %>%
  filter(SEASON=="Annual", !is.na(TSS), !is.na(TP)) %>%
  ggplot(aes(TSS, TP)) +
  geom_point(size=1) +
  scale_y_log10() +
  scale_x_log10() +
  facet_wrap(~SITE_NAME, scales='free', nrow=2) +
  theme(aspect.ratio=1)

df %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(FLOW, TSS)) +
  geom_point(size=1) +
  geom_smooth(method='lm', se=FALSE) +
  scale_y_log10() +
  scale_x_log10() +
  facet_wrap(~SITE_NAME, scales='free', nrow=2) +
  theme(aspect.ratio=1)

df %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(FLOW, TP)) +
  geom_point(size=1) +
  scale_y_log10() +
  scale_x_log10() +
  facet_wrap(~SITE_NAME, scales='free', nrow=2) +
  theme(aspect.ratio=1)

df %>%
  ggplot(aes(FLOW, 1-PO4_TP)) +
  geom_point(size=1) +
  geom_smooth(se=FALSE) +
  scale_x_log10() +
  theme(aspect.ratio=1) +
  facet_grid(SITE_NAME~SEASON, scales="free")

# report figures ----
filename <- file.path('report', 'erosion-tss-flow.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- df %>%
  filter(SEASON!="Annual") %>%
  ggplot(aes(FLOW, TSS)) +
  geom_point(aes(color=SEASON), size=1.5) +
  geom_smooth(se=FALSE, color='black') +
  log_x +
  log_y +
  scale_color_manual('Season',
                     values=c('grey50', 'orangered', 'deepskyblue', 'chartreuse3')) +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="Flow (cfs)", y="TSS (mg/L)") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()


filename <- file.path('report', 'erosion-tp-tss.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- df %>%
  filter(SEASON!="Annual") %>%
  ggplot(aes(TSS, TP)) +
  geom_point(aes(color=SEASON), size=1.6) +
  geom_smooth(se=FALSE, color='black') +
  log_x +
  log_y +
  scale_color_manual('Season',
                     values=c('grey50', 'orangered', 'deepskyblue', 'chartreuse3')) +
  facet_wrap(~SITE_NAME, scales='free', nrow=2) +
  labs(x="TSS (mg/L)", y="TP (mg/L)") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()

filename <- file.path('report', 'erosion-ppfrac-tss.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- df %>%
  filter(SEASON!="Annual") %>%
  ggplot(aes(TSS, 1-PO4_TP)) +
  geom_point(aes(color=SEASON), size=1.5) +
  geom_smooth(se=FALSE, color='black') +
  log_x +
  scale_color_manual('Season',
                     values=c('grey50', 'orangered', 'deepskyblue', 'chartreuse3')) +
  scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="TSS (mg/L)", y="% Particulate P") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()


filename <- file.path('report', 'erosion-pp-tss.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- df %>%
  filter(SEASON!="Annual") %>%
  ggplot(aes(TSS, PP)) +
  geom_point(aes(color=SEASON), size=1.5) +
  geom_smooth(se=FALSE, color='black') +
  log_x +
  log_y +
  scale_color_manual('Season',
                     values=c('grey50', 'orangered', 'deepskyblue', 'chartreuse3')) +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="TSS (mg/L)", y="Particulate Phosphorus (mg/L)") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()

filename <- file.path('report', 'erosion-pp-flow.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- df %>%
  filter(SEASON!="Annual") %>%
  ggplot(aes(FLOW, PP)) +
  geom_point(aes(color=SEASON), size=1.5) +
  geom_smooth(se=FALSE, color='black') +
  scale_color_manual('Season',
                     values=c('grey50', 'orangered', 'deepskyblue', 'chartreuse3')) +
  log_x +
  log_y +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="Flow (cfs)", y="Particulate Phosphorus (mg/L)") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()

filename <- file.path('report', 'erosion-ppfrac-flow.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- df %>%
  filter(SEASON!="Annual") %>%
  ggplot(aes(FLOW, 1-PO4_TP)) +
  geom_point(aes(color=SEASON), size=1.5) +
  geom_smooth(se=FALSE, color='black') +
  log_x +
  scale_color_manual('Season',
                     values=c('grey50', 'orangered', 'deepskyblue', 'chartreuse3')) +
  scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="Flow (cfs)", y="% Particulate P") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()

filename <- file.path('report', 'erosion-ppfrac-boxplot.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=6, height=4, res=200, units="in")
p <- df %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(SITE_NAME, 1-PO4_TP)) +
  geom_boxplot(fill='grey80') +
  scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
  labs(x="Station", y="% Particulate P")
print(p)
dev.off()




filename <- file.path('report', 'erosion-tn-flow.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- df %>%
  filter(SEASON!="Annual") %>%
  ggplot(aes(FLOW, TN)) +
  geom_point(aes(color=SEASON), size=1.5) +
  geom_smooth(se=FALSE, color='black') +
  scale_color_manual('Season',
                     values=c('grey50', 'orangered', 'deepskyblue', 'chartreuse3')) +
  log_x +
  log_y +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="Flow (cfs)", y="Total Nitrogen (mg/L)") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()

filename <- file.path('report', 'erosion-nh4-flow.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- df %>%
  filter(SEASON!="Annual") %>%
  ggplot(aes(FLOW, NH4)) +
  geom_point(aes(color=SEASON), size=1.5) +
  geom_smooth(se=FALSE, color='black') +
  scale_color_manual('Season',
                     values=c('grey50', 'orangered', 'deepskyblue', 'chartreuse3')) +
  log_x +
  log_y +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="Flow (cfs)", y="Ammonia-N (mg/L)") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()

filename <- file.path('report', 'erosion-no23-flow.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- df %>%
  filter(SEASON!="Annual") %>%
  ggplot(aes(FLOW, NO23)) +
  geom_point(aes(color=SEASON), size=1.5) +
  geom_smooth(se=FALSE, color='black') +
  scale_color_manual('Season',
                     values=c('grey50', 'orangered', 'deepskyblue', 'chartreuse3')) +
  log_x +
  log_y +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="Flow (cfs)", y="Nitrate-Nitrite-N (mg/L)") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()

filename <- file.path('report', 'erosion-orgn-flow.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- df %>%
  filter(SEASON!="Annual") %>%
  ggplot(aes(FLOW, ORGN)) +
  geom_point(aes(color=SEASON), size=1.5) +
  geom_smooth(se=FALSE, color='black') +
  scale_color_manual('Season',
                     values=c('grey50', 'orangered', 'deepskyblue', 'chartreuse3')) +
  log_x +
  log_y +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="Flow (cfs)", y="Organic-N (mg/L)") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()

filename <- file.path('report', 'erosion-inorgn-flow.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- df %>%
  filter(SEASON!="Annual") %>%
  ggplot(aes(FLOW, INORGN)) +
  geom_point(aes(color=SEASON), size=1.5) +
  geom_smooth(se=FALSE, color='black') +
  scale_color_manual('Season',
                     values=c('grey50', 'orangered', 'deepskyblue', 'chartreuse3')) +
  log_x +
  log_y +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="Flow (cfs)", y="Inorganic-N (mg/L)") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()

filename <- file.path('report', 'erosion-pct-orgn-flow.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- df %>%
  filter(SEASON!="Annual") %>%
  ggplot(aes(FLOW, ORGN/TN)) +
  geom_point(aes(color=SEASON), size=1.5) +
  geom_smooth(se=FALSE, color='black') +
  scale_color_manual('Season',
                     values=c('grey50', 'orangered', 'deepskyblue', 'chartreuse3')) +
  log_x +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="Flow (cfs)", y="% TN as Organic-N") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()

filename <- file.path('report', 'erosion-orgn-inorgn-ratio-flow.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- df %>%
  filter(SEASON!="Annual") %>%
  ggplot(aes(FLOW, ORGN/INORGN)) +
  geom_point(aes(color=SEASON), size=1.5) +
  geom_smooth(se=FALSE, color='black') +
  scale_color_manual('Season',
                     values=c('grey50', 'orangered', 'deepskyblue', 'chartreuse3')) +
  log_x +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="Flow (cfs)", y="Organic-N : Inorganic-N Ratio") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()


filename <- file.path('report', 'erosion-tn-tss.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- df %>%
  filter(SEASON!="Annual") %>%
  ggplot(aes(TSS, TN)) +
  geom_point(aes(color=SEASON), size=1.5) +
  geom_smooth(se=FALSE, color='black') +
  scale_color_manual('Season',
                     values=c('grey50', 'orangered', 'deepskyblue', 'chartreuse3')) +
  log_x +
  log_y +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="TSS (mg/L)", y="Total Nitrogen (mg/L)") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()

filename <- file.path('report', 'erosion-orgn-tss.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- df %>%
  filter(SEASON!="Annual") %>%
  ggplot(aes(TSS, ORGN)) +
  geom_point(aes(color=SEASON), size=1.5) +
  geom_smooth(se=FALSE, color='black') +
  scale_color_manual('Season',
                     values=c('grey50', 'orangered', 'deepskyblue', 'chartreuse3')) +
  log_x +
  log_y +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="TSS (mg/L)", y="Organic-N (mg/L)") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()


filename <- file.path('report', 'erosion-pct-orgn-tss.png')
cat('Saving Figure:', filename, '\n')
png(filename, width=10, height=6, res=200, units="in")
p <- df %>%
  filter(SEASON!="Annual") %>%
  ggplot(aes(TSS, ORGN/TN)) +
  geom_point(aes(color=SEASON), size=1.5) +
  geom_smooth(se=FALSE, color='black') +
  scale_color_manual('Season',
                     values=c('grey50', 'orangered', 'deepskyblue', 'chartreuse3')) +
  log_x +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="TSS (mg/L)", y="% TN as Organic-N") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()







# median values
df %>%
  filter(SEASON=="Annual") %>%
  group_by(SITE_NAME) %>%
  summarise(MEDIAN=median(1-PO4_TP, na.rm=TRUE))



pdf(file.path('pdf', 'erosion.pdf'), width=11, height=8.5)
p <- df %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(WDAY_LABEL, FLOW)) +
  geom_point(size=1) +
  geom_smooth(se=FALSE) +
  scale_x_date(labels=scales::date_format('%b')) +
  log_y +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="", y="Flow (cfs)",
       title="Flow - Seasonality") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)

p <- df %>%
  filter(SEASON=="Annual") %>%
  mutate(PP=ifelse(TP>PO4, TP-PO4, 0.001)) %>%
  select(SITE_NAME, WDAY_LABEL, PP, TP, PO4) %>%
  gather(VAR, VALUE, PP:PO4) %>%
  mutate(VAR=ordered(VAR, levels=c('TP', 'PO4', 'PP'))) %>%
  ggplot(aes(WDAY_LABEL, VALUE, color=VAR)) +
  geom_point(size=1, alpha=0.5) +
  geom_smooth(se=FALSE) +
  scale_x_date(labels=scales::date_format('%b')) +
  log_y +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="", y="Concentration (mg/L)",
       title="TP, PO4, PP - Seasonality") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)

p <- df %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(WDAY_LABEL, 1-PO4_TP)) +
  geom_point(size=1) +
  geom_smooth(se=FALSE) +
  scale_x_date(labels=scales::date_format('%b')) +
  scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="", y="% Particulate P",
       title="% Particulate P - Seasonality") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)


p <- df %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(FLOW, 1-PO4_TP)) +
  geom_point(size=1) +
  geom_smooth(se=FALSE) +
  log_x +
  scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="Flow (cfs)", y="% Particulate P",
       title="% Particulate P vs Flow") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)

p <- df %>%
  filter(SEASON=="Annual") %>%
  mutate(PP=ifelse(TP>PO4, TP-PO4, 0.001)) %>%
  select(SITE_NAME, WDAY_LABEL, FLOW, PP, TP, PO4) %>%
  gather(VAR, VALUE, PP:PO4) %>%
  mutate(VAR=ordered(VAR, levels=c('TP', 'PO4', 'PP'))) %>%
  ggplot(aes(FLOW, VALUE, color=VAR)) +
  geom_point(size=1, alpha=0.5) +
  geom_smooth(se=FALSE) +
  log_x +
  log_y +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="Flow (cfs)", y="Concentration (mg/L)",
       title="TP, PO4, PP vs Flow") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)

p <- df %>%
  filter(SEASON=="Annual") %>%
  mutate(PP=ifelse(TP>PO4, TP-PO4, 0.001)) %>%
  select(SITE_NAME, WDAY_LABEL, TSS, PP, TP, PO4) %>%
  gather(VAR, VALUE, PP:PO4) %>%
  mutate(VAR=ordered(VAR, levels=c('TP', 'PO4', 'PP'))) %>%
  ggplot(aes(TSS, VALUE, color=VAR)) +
  geom_point(size=1, alpha=0.5) +
  geom_smooth(se=FALSE) +
  log_x +
  log_y +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="TSS (mg/L)", y="Concentration (mg/L)",
       title="TP, PO4, PP vs TSS") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)


p <- df %>%
  filter(SEASON=="Annual") %>%
  ggplot(aes(TSS, 1-PO4_TP)) +
  geom_point(size=1) +
  geom_smooth(se=FALSE) +
  log_x +
  scale_y_continuous(labels=scales::percent, limits=c(0, 1)) +
  facet_wrap(~SITE_NAME, scales='free_x', nrow=2) +
  labs(x="TSS (mg/L)", y="% Particulate P",
       title="% Particulate P vs TSS") +
  theme(aspect.ratio=1,
        panel.grid.minor.x = element_blank(),
        axis.text.x=element_text(size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)

dev.off()