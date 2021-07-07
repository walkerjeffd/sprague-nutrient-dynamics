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

stn.kt_synoptic <- mutate(stn.kt_synoptic, SITE_LABEL=paste0(SITE_DESCRIPTION, " (", SITE, ")"))

# synoptic_stn <- c("SR0100", "SR0120", "SR0200")

pdf(file.path("pdf", "synoptic-data.pdf"), width=11, height=8.5)

p <- ggmap(map, extent = 'device', darken = c(0.2, 'white')) +
  geom_sf(inherit.aes=FALSE, data = basin,
              color = 'black', alpha = 0, size = 0.5) +
  geom_sf(inherit.aes=FALSE, data = flowline,
           color='deepskyblue', size=0.2) +
  geom_sf(inherit.aes=FALSE, data = incbasin,
              color = 'orangered', fill = 'grey50', alpha = 0.2, size = 0.2) +
  #geom_polygon(aes(x = long, y = lat, group = group), data = basin,
   #            color = 'black', alpha = 0, size = 0.5) +
  #geom_path(aes(x = long, y = lat, group = group), data = flowline,
   #         color='deepskyblue', size=0.2) +
  #geom_polygon(aes(x = long, y = lat, group = group), data = incbasin,
   #            color = 'orangered', fill = 'grey50', alpha = 0.2, size = 0.2) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE), data = stn.kt_synoptic,
             shape=21, fill='deepskyblue', size=3) +
  geom_text(aes(x = LONGITUDE+0.02, y = LATITUDE, label = SITE_LABEL),
            data = filter(stn.kt_synoptic, SITE %in% c('SR0100', 'SR0190', 'SR0180', 'SR0120')),
            size=4, hjust=0) +
  geom_text(aes(x = LONGITUDE-0.02, y = LATITUDE, label = SITE_LABEL),
            data = filter(stn.kt_synoptic, SITE %in% c('SR0200')),
            size=4, hjust=1) +
  geom_text(aes(x = LONGITUDE-0.02, y = LATITUDE+0.01, label = SITE_LABEL),
            data = filter(stn.kt_synoptic, SITE %in% c('SR0160')),
            size=4, hjust=1) +
  geom_text(aes(x = LONGITUDE-0.02, y = LATITUDE, label = SITE_LABEL),
            data = filter(stn.kt_synoptic, SITE %in% c('SR0600')),
            size=4, hjust=1) +
  geom_text(aes(x = LONGITUDE+0.02, y = LATITUDE, label = SITE_LABEL),
            data = filter(stn.kt_synoptic, SITE %in% c('SR0170')),
            size=4, hjust=0, vjust=0) +
  geom_text(aes(x = LONGITUDE+0.02, y = LATITUDE-0.01, label = SITE_LABEL),
            data = filter(stn.kt_synoptic, SITE %in% c('SR0500')),
            size=4, hjust=0, vjust=1) +
  ggtitle('Synoptic Stations')
print(p)
makeFootnote()

p <- wq.kt_synoptic %>%
  ggplot(aes(DATETIME, VALUE)) +
  geom_point() +
  facet_grid(VAR~SITE_DESCRIPTION, scales="free_y") +
  scale_x_datetime(date_labels="%m/%Y") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  labs(x="Date", y="Concentration (ppm)", title="Synoptic/Springs WQ Data")
print(p)

p <- wq.kt_synoptic %>%
  ggplot(aes(SITE_DESCRIPTION, VALUE)) +
  geom_boxplot() +
  geom_point(color='red') +
  facet_grid(VAR~., scales="free_y") +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) +
  ylim(0, NA) +
  labs(x="", y="Concentration (ppm)", title="Distributions of Synoptic/Springs WQ Data")
print(p)

p <- wq.kt_synoptic %>%
  ggplot(aes(VAR, VALUE)) +
  geom_boxplot() +
  geom_point(color='orangered') +
  facet_wrap(~VAR, nrow=1, scales='free') +
  ylim(0, NA) +
  labs(x="", y="Concentration (ppm)", title="Distributions of Synoptic/Springs WQ Data\nAll Individual Samples and Stations Combined")
print(p)

p <- wq.kt_synoptic %>%
  dplyr::group_by(SITE, SITE_DESCRIPTION, VAR) %>%
  dplyr::summarise(VALUE=median(VALUE)) %>%
  ggplot(aes(SITE_DESCRIPTION, VALUE)) +
  geom_bar(stat='identity') +
  facet_grid(VAR~., scales='free_y') +
  ylim(0, NA) +
  labs(x="", y="Concentration (ppm)", title="Median Value by Station, Synoptic/Springs WQ Data")
print(p)

p <- wq.kt_synoptic %>%
  dplyr::group_by(SITE, SITE_DESCRIPTION, VAR) %>%
  dplyr::summarise(VALUE=median(VALUE)) %>%
  ggplot(aes(VAR, VALUE)) +
  geom_boxplot() +
  geom_point(color='orangered') +
  facet_wrap(~VAR, nrow=1, scales='free') +
  ylim(0, NA) +
  labs(x="", y="Concentration (ppm)", title="Distributions of Median by Station, Synoptic/Springs WQ Data")
print(p)

p <- select(wq.kt_synoptic, DATE, DATETIME, SITE, SITE_DESCRIPTION, VAR, VALUE) %>%
  spread(VAR, VALUE) %>%
  mutate(PO4_TP_FRAC=PO4/TP) %>%
  filter(PO4_TP_FRAC < 1.1) %>%
  ggplot(aes(SITE_DESCRIPTION, PO4_TP_FRAC)) +
  geom_boxplot() +
  scale_y_continuous(labels=scales::percent) +
  labs(x='', y="Fraction TP as PO4")
print(p)

dev.off()


filter(wq.kt_synoptic, VAR == "TP") %>%
  dplyr::group_by(SITE, VAR) %>%
  dplyr::summarise(N=n())


wq.kt_synoptic %>%
  select(DATETIME, SITE, SITE_DESCRIPTION, VAR, VALUE) %>%
  spread(VAR, VALUE) %>%
  mutate(PO4_TP=PO4/TP) %>%
  ggplot(aes(SITE_DESCRIPTION, PO4_TP)) +
  geom_boxplot() +
  coord_flip() +
  ylim(0, 1)

select(wq.kt_synoptic, DATE, DATETIME, SITE, SITE_DESCRIPTION, VAR, VALUE) %>%
  spread(VAR, VALUE) %>%
  mutate(PO4_TP_FRAC=PO4/TP) %>%
  ggplot(aes(DATETIME, PO4_TP_FRAC, color=SITE_DESCRIPTION)) +
  geom_point() +
  ylim(NA, 1.1)

load('kt_sprague.Rdata')

tp_medians <- filter(wq.kt_synoptic,
                     VAR == "TP") %>%
  dplyr::group_by(SITE, SITE_DESCRIPTION) %>%
  dplyr::summarise(MEDIAN=median(VALUE*1000),
            N=n())

filename <- 'report/synoptic-tp.png'
cat('Saving report figure to:', filename, '\n')
png(filename, width=10, height=8, res=200, units='in')

p <- wq.kt_synoptic %>%
  filter(VAR %in% c("TP")) %>%
  ggplot(aes(DATETIME, VALUE*1000)) +
  geom_point() +
  geom_hline(aes(yintercept=MEDIAN, color="Median"), data=tp_medians, linetype=2,
             show.legend=TRUE) +
  scale_color_manual('', values='red') +
  facet_wrap(~SITE_DESCRIPTION) +
  scale_x_datetime(date_labels="%m-%Y") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        strip.background=element_blank(),
        strip.text=element_text(face='bold')) +
  labs(x="Date", y="TP Concentration (ppb)")
print(p)

dev.off()

write.csv(tp_medians, file='csv/synoptic_median_tp.csv', row.names=FALSE)
