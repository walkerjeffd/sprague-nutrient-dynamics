library(fluxr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())
library(gridExtra)

rm(list=ls())

source('functions.R')
load('loads.Rdata')

DATA_DIR <- getOption('UKL_DATA')

gannett <- read.csv(file=file.path(DATA_DIR, 'sprague', 'gannett', 'gannett.csv'),
                    stringsAsFactors=FALSE) %>%
  mutate(SOURCE="Gannett")

# season <- "Annual"
season <- 'Summer (Jul-Sep)'
study_wyr <- filter(loads_df[['wyr']],
                    SEASON==season,
                    DATASET=='POR',
                    WYEAR %in% 2013:2014,
                    # PERIOD=="2010-2014",
                    TERM=='Q') %>%
  mutate(SOURCE=as.character(WYEAR),
         FLOW_hm3d=VALUE,
         FLOW_cfs=hm3d_cfs(FLOW_hm3d)) %>%
  select(SOURCE, SITE_NAME, FLOW_cfs)

study_site <- filter(loads_df[['site']],
                    SEASON==season,
                    DATASET=='POR',
                    PERIOD=="2010-2014",
                    TERM=='Q') %>%
  mutate(SOURCE=PERIOD,
         FLOW_hm3d=VALUE,
         FLOW_cfs=hm3d_cfs(FLOW_hm3d)) %>%
  # filter(SITE_NAME %in% gannett$SITE_NAME) %>%
  select(SOURCE, SITE_NAME, FLOW_cfs)



flow <- rbind(gannett, study_wyr, study_site) %>%
  filter(SITE_NAME %in% gannett$SITE_NAME) %>%
  mutate(SITE_NAME=plyr::revalue(SITE_NAME, incbasin_names),
         SITE_NAME=ordered(SITE_NAME, levels=unname(incbasin_names)))
as.data.frame(flow)

# flow$SITE_NAME=ordered(flow$SITE_NAME, levels=levels(loads_df[['site']]$SITE_NAME))

spread(flow, SOURCE, FLOW_cfs)

spread(flow, SOURCE, FLOW_cfs) %>%
  write.csv(file=file.path('csv', 'gannett_compare.csv'), row.names=FALSE)

filename <- 'report/gannet.png'
cat('Saving report figure to:', filename, '\n')
png(filename, width=8, height=6, res=200, units='in')
p <- filter(flow, SOURCE %in% c("Gannett", "2010-2014")) %>%
  mutate(SOURCE=ifelse(SOURCE=="Gannett", "Gannett et al (2007)", paste0("This Study (WY", SOURCE, ")"))) %>%
  ggplot(aes(SITE_NAME, FLOW_cfs, fill=SOURCE)) +
  geom_bar(stat='identity', position='dodge') +
  geom_hline(yint=0, color='grey20') +
  scale_y_continuous(breaks=seq(-20, 100, by=10)) +
  labs(x="", y="Flow (cfs)") +
  scale_fill_manual("", values=c("Gannett et al (2007)"="grey50",
                                 "This Study (WY2010-2014)"="deepskyblue")) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
print(p)
dev.off()


flow %>%
  filter(SITE_NAME %in% gannett$SITE_NAME) %>%
  ggplot(aes(SITE_NAME, FLOW_cfs, fill=SOURCE)) +
  geom_bar(stat='identity', position='dodge') +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))





source('functions.R')
load('loads.Rdata')
load('owrd.Rdata')

sf.day <- filter(loads_df[['day']], SITE_NAME=="SF", TERM=="Q") %>%
  filter(WYEAR >= 2010) %>%
  mutate(VALUE=hm3d_cfs(VALUE))
sf.mon <- filter(loads_df[['mon']], SITE_NAME=="SF", TERM=="Q") %>%
  filter(WYEAR >= 2010) %>%
  mutate(VALUE=hm3d_cfs(VALUE))
sf.wyr <- filter(loads_df[['wyr']], SITE_NAME=="SF", TERM=="Q", SEASON=="Annual", DATASET=="POR") %>%
  filter(WYEAR >= 2010) %>%
  mutate(VALUE=hm3d_cfs(VALUE))
sf.site <- filter(loads_df[['site']], SITE_NAME=="SF", TERM=="Q", SEASON=="Annual", DATASET=="POR", PERIOD=="2010-2014") %>%
  mutate(VALUE=hm3d_cfs(VALUE))


sf.owrd <- filter(q.owrd, SITE_NAME=="SF")

ggplot(sf.day, aes(DATE, VALUE)) +
  geom_line() +
  geom_line(aes(y=FLOW), data=sf.owrd, color='red') +
  geom_step(aes(x=MONTHYEAR), data=sf.mon) +
  geom_step(aes(x=DATE), data=sf.wyr) +
  geom_hline(aes(yintercept=VALUE, x=NULL, y=NULL), data=sf.site)


filter(loads_df[['site']], TERM=="Q", SEASON=="Summer (Jul-Sep)", DATASET=="POR", PERIOD=="2010-2014") %>%
  mutate(VALUE=hm3d_cfs(VALUE)) %>%
  arrange(SITE_NAME) %>%
  select(-DATASET, -VAR, -START_DATE, -END_DATE)


filter(loads_df[['day']], SITE_NAME %in% c("NF", "NF_Ivory"), TERM=="Q", DATASET=="POR") %>%
  filter(WYEAR >= 2010,
         MONTH %in% 7:9) %>%
  mutate(VALUE=hm3d_cfs(VALUE)) %>%
  ggplot(aes(DATE, VALUE, color=SITE_NAME)) +
  geom_line() +
  facet_wrap(~WYEAR, scales='free_x')

filter(loads_df[['mon']], SITE_NAME %in% c("NF", "NF_Ivory"), TERM=="Q", DATASET=="POR") %>%
  filter(WYEAR >= 2010,
         MONTH %in% 7:9) %>%
  mutate(VALUE=hm3d_cfs(VALUE)) %>%
  ggplot(aes(MONTHYEAR, VALUE, color=SITE_NAME)) +
  geom_line() +
  facet_wrap(~WYEAR, scales='free_x')


filter(loads_df[['day']], SITE_NAME %in% c("Power", "Lone_Pine"), TERM=="Q", DATASET=="POR") %>%
  filter(WYEAR >= 2010,
         MONTH %in% 7:9) %>%
  mutate(VALUE=hm3d_cfs(VALUE)) %>%
  ggplot(aes(DATE, VALUE, color=SITE_NAME)) +
  geom_line() +
  facet_wrap(~WYEAR, scales='free_x')

p1 <- filter(loads_df[['day']], SITE_NAME %in% c("Godowa", "Lone_Pine"), TERM=="Q", DATASET=="POR") %>%
  filter(WYEAR >= 2010,
         MONTH %in% 7:9) %>%
  mutate(VALUE=hm3d_cfs(VALUE)) %>%
  ggplot(aes(DATE, VALUE, color=SITE_NAME)) +
  geom_line() +
  facet_wrap(~WYEAR, scales='free_x', nrow=1)
p2 <- filter(q.owrd, SITE_NAME %in% c("Godowa", "Lone_Pine")) %>%
  mutate(WYEAR=wyear(DATE)) %>%
  filter(wyear(DATE) >= 2010,
         month(DATE) %in% 7:9) %>%
  ggplot(aes(DATE, FLOW, color=SITE_NAME)) +
  geom_line() +
  facet_wrap(~WYEAR, scales='free_x', nrow=1)
grid.arrange(grobs=list(p1, p2), nrow=2)


sites <- c("Lone_Pine", "Power")
p1 <- filter(loads_df[['day']], SITE_NAME %in% sites, TERM=="Q", DATASET=="POR") %>%
  filter(WYEAR >= 2005,
         MONTH %in% 7:9) %>%
  mutate(VALUE=hm3d_cfs(VALUE)) %>%
  ggplot(aes(DATE, VALUE, color=SITE_NAME)) +
  geom_line() +
  geom_hline(aes(x=NULL, y=NULL, yintercept=VALUE, color=SITE_NAME),
             data=filter(loads_df[['wyr']], TERM=="Q", SEASON=="Summer (Jul-Sep)", DATASET=="POR", WYEAR %in% 2010:2014, SITE_NAME %in% sites) %>%
               mutate(VALUE=hm3d_cfs(VALUE))) +
  geom_hline(aes(x=NULL, y=NULL, yintercept=VALUE, color=SITE_NAME),
             data=filter(loads_df[['site']], TERM=="Q", SEASON=="Summer (Jul-Sep)", DATASET=="POR", PERIOD=="2010-2014", SITE_NAME %in% sites) %>%
               mutate(VALUE=hm3d_cfs(VALUE)),
             linetype='dashed') +
  facet_wrap(~WYEAR, scales='free_x', nrow=1) +
  ggtitle('study')
p2 <- filter(q.owrd, SITE_NAME %in% sites) %>%
  mutate(WYEAR=wyear(DATE)) %>%
  filter(wyear(DATE) >= 2005,
         month(DATE) %in% 7:9) %>%
  ggplot(aes(DATE, FLOW, color=SITE_NAME)) +
  geom_line() +
  facet_wrap(~WYEAR, scales='free_x', nrow=1) +
  ggtitle('owrd')
grid.arrange(grobs=list(p1, p2), nrow=2)



filter(loads_df[['day']], SITE_NAME %in% c("Lone_Pine", "Power"),
       TERM=="Q", DATASET=="POR") %>%
  filter(WYEAR >= 2005,
         MONTH %in% 7:9) %>%
  mutate(VALUE=hm3d_cfs(VALUE)) %>%
  select(DATE, WYEAR, SITE_NAME, FLOW=VALUE) %>%
  spread(SITE_NAME, FLOW) %>%
  mutate(DIFF=Power-Lone_Pine) %>%
  ggplot(aes(DATE, DIFF)) +
  geom_line() +
  facet_wrap(~WYEAR, scales='free_x')

filter(loads_df[['day']], SITE_NAME %in% c("Lone_Pine", "Power"),
       TERM=="Q", DATASET=="POR") %>%
  filter(MONTH %in% 7:9) %>%
  mutate(VALUE=hm3d_cfs(VALUE)) %>%
  select(DATE, WYEAR, SITE_NAME, FLOW=VALUE) %>%
  spread(SITE_NAME, FLOW) %>%
  mutate(DIFF=Power-Lone_Pine) %>%
  group_by(WYEAR) %>%
  summarise(DIFF=mean(DIFF)) %>%
  mutate(DIFF_WYR=mean(DIFF))




filter(loads_df[['day']],
       DATASET=="POR",
       VAR %in% c("FLOW", "TP"),
       TERM %in% c("Q", "L", "C")) %>%
  mutate(SITE_NAME = ordered(SITE_NAME, levels=names(loads[['POR']][['TP']]))) %>%
  ggplot(aes(DATE, log10(VALUE))) +
  geom_line() +
  facet_grid(TERM~SITE_NAME, scales='free_y')

filename <- 'report/loads-mon-tp.png'
cat('Saving report figure to:', filename, '\n')
png(filename, width=10, height=6, res=200, units='in')
p <- filter(loads_df[['mon']],
       DATASET=="POR",
       VAR %in% c("FLOW", "TP"),
       TERM %in% c("Q", "L", "C"),
       SITE_NAME %in% names(loads[['POR']][['TP']])) %>%
  mutate(SITE_NAME = ordered(SITE_NAME, levels=names(loads[['POR']][['TP']])),
         SITE_GRP = SITE_NAME %in% c('Power', 'Godowa', 'SF_Ivory', 'NF_Ivory'),
         VALUE = ifelse(TERM=="Q", hm3d_cfs(VALUE), VALUE),
         TERM = plyr::revalue(TERM, c(Q="Flow", L="TP Load", C="TP Conc"))) %>%
  ggplot(aes(MONTHYEAR, VALUE, color=SITE_NAME, linetype=SITE_GRP)) +
  geom_line() +
  labs(x="Date", y=paste(c("TP Conc (ppb)", "TP Load (kg/d)", "Flow (cfs)"),
                         collapse=paste(rep(" ", 20), collapse=""))) +
  scale_linetype_manual('', values=c(1, 6)) +
  scale_y_continuous(labels=scales::comma) +
  facet_grid(TERM~., scales='free_y') +
  guides(linetype="none",
         color=guide_legend('', override.aes = list(linetype=rep(c(5, 1), 4)))) +
  theme(strip.background=element_blank(),
        strip.text=element_blank())
print(p)
dev.off()