library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggmap)
theme_set(theme_bw())

rm(list=ls())

source('functions.R')

load("kt_synoptic.Rdata")
load("gis.Rdata")

variables <- c("TP", "PO4", "TN", "NH4", "NO23")

wq.kt_synoptic <- filter(wq.kt_synoptic,
                         VAR %in% variables) %>%
  mutate(VAR = ordered(as.character(VAR),
                       levels=variables))

synoptic_stn <- c("SR0100", "SR0120", "SR0200")

pdf(file.path("pdf", "synoptic-data.pdf"), width=11, height=8.5)

p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
  geom_polygon(aes(x = long, y = lat, group = group), data = basin,
               color = 'black', alpha = 0, size = 0.5) +
  geom_path(aes(x = long, y = lat, group = group), data = flowline,
            color='deepskyblue', size=0.2) +
  geom_polygon(aes(x = long, y = lat, group = group), data = incbasin,
               color = 'orangered', fill = 'grey50', alpha = 0.2, size = 0.2) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE), data = filter(stn.kt_synoptic, SITE %in% synoptic_stn),
             shape=21, fill='deepskyblue', size=3) +
  geom_text(aes(x = LONGITUDE+0.02, y = LATITUDE, label = SITE_LABEL),
            data = (filter(stn.kt_synoptic, SITE %in% synoptic_stn) %>%
                      mutate(SITE_LABEL=paste0(SITE_DESCRIPTION, " (", SITE, ")"))),
            size=4, hjust=0) +
  ggtitle('Synoptic Stations')
print(p)
makeFootnote()

p <- filter(wq.kt_synoptic, SITE %in% synoptic_stn) %>%
  filter(VAR %in% c("TP", "PO4", "TN", "NO23", "NH4", "TSS")) %>%
  ggplot(aes(DATETIME, VALUE)) +
  geom_point() +
  facet_grid(VAR~SITE_DESCRIPTION, scales="free_y") +
  scale_x_datetime(labels=scales::date_format("%m/%Y")) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  labs(x="Date", y="Concentration (ppm)", title="Synoptic WQ Data")
print(p)

p <- filter(wq.kt_synoptic, SITE %in% synoptic_stn) %>%
  filter(VAR %in% c("TP", "PO4", "TN", "NO23", "NH4", "TSS")) %>%
  ggplot(aes(SITE_DESCRIPTION, VALUE)) +
  geom_boxplot() +
  facet_wrap(~VAR, scales="free_y") +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) +
  ylim(0, NA) +
  labs(x="", y="Concentration (ppm)", title="Distributions of Synoptic WQ Data")
print(p)

dev.off()

filter(wq.kt_synoptic, SITE %in% synoptic_stn,
       VAR == "TP") %>%
  group_by(SITE, VAR) %>%
  summarise(N=n())

p <- filter(wq.kt_synoptic, SITE %in% synoptic_stn) %>%
  filter(VAR %in% c("TP")) %>%
  ggplot(aes(SITE_DESCRIPTION, VALUE)) +
  geom_boxplot() +
  geom_point(color='orangered') +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) +
  ylim(0, NA) +
  labs(x="", y="Concentration (ppm)", title="Distributions of Synoptic WQ Data")
print(p)

p <- filter(wq.kt_synoptic, SITE %in% synoptic_stn) %>%
  filter(VAR %in% c("TP")) %>%
  ggplot(aes(SITE_DESCRIPTION, VALUE)) +
  geom_boxplot() +
  geom_point(color='orangered') +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) +
  ylim(0, NA) +
  labs(x="", y="Concentration (ppm)", title="Distributions of Synoptic WQ Data")
print(p)

load('kt_sprague.Rdata')



tp_medians <- filter(wq.kt_synoptic,
                     SITE %in% synoptic_stn,
                     VAR == "TP") %>%
  group_by(SITE, SITE_DESCRIPTION) %>%
  summarise(MEDIAN=median(VALUE*1000),
            N=n())

filename <- 'report/synoptic-tp.png'
cat('Saving report figure to:', filename, '\n')
png(filename, width=8, height=3, res=200, units='in')

p <- filter(wq.kt_synoptic, SITE %in% synoptic_stn) %>%
  filter(VAR %in% c("TP")) %>%
  ggplot(aes(DATETIME, VALUE*1000)) +
  geom_point() +
  geom_hline(aes(yintercept=MEDIAN, color="Median"), data=tp_medians, linetype=2,
             show_guide=TRUE) +
  scale_color_manual('', values='red') +
  facet_grid(~SITE_DESCRIPTION, scales="free_y") +
  scale_x_datetime(labels=scales::date_format("%m/%Y")) +
  scale_y_continuous(limits=c(0, NA)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        strip.background=element_blank(),
        strip.text=element_text(face='bold')) +
  labs(x="Date", y="TP Concentration (ppb)")
print(p)

dev.off()

write.csv(tp_medians, file='csv/synoptic_median_tp.csv', row.names=FALSE)