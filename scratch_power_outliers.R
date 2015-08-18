library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())
library(gridExtra)
library(fluxr)

load('kt_sprague.Rdata')

tp.qaqc <- wq.kt_sprague.orig %>%
  select(YEAR, MONTH, DAY, SITE, TP_ppm, TP_DUP) %>%
  filter(SITE %in% stn.kt_sprague$SITE) %>%
  mutate(DATE=ymd(paste(YEAR,MONTH,DAY,sep='-')),
         SITE=factor(SITE)) %>%
  select(SITE, DATE, TP_ppm, TP_DUP) %>%
  left_join(select(stn.kt_sprague, SITE, SITE_NAME)) %>%
  mutate(TP_DIFF=abs(TP_ppm-TP_DUP),
         TP_RPD=abs(TP_ppm-TP_DUP)/((TP_ppm+TP_DUP)/2),
         TP_HIGH=TP_ppm > 0.010 & TP_DUP > 0.010,
         TP_PASS=ifelse(TP_HIGH, TP_RPD<=0.2, TP_DIFF<=0.01))

wq <- wq.kt_sprague[['CLEAN']] %>%
  filter(SITE_NAME %in% c('Sprague_Power', 'Sprague_Lone'), VAR %in% c('TP', 'PO4', 'FLOW', 'STAGE')) %>%
  droplevels

# load flow data
# (saved from compute_flows.R)
q <- readRDS('flows.Rdata') %>%
  ungroup
q <- select(q, SITE_NAME, DATE, Q) %>%
  mutate(DATE=with_tz(DATE, "UTC"),
         Q=cfs_to_hm3d(Q))
q <- left_join(q, select(stn.kt_sprague, SITE, SITE_NAME)) %>%
  mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=levels(stn.kt_sprague$SITE_NAME)))
q <- filter(q, SITE_NAME %in% c('Sprague_Power', 'Sprague_Lone')) %>%
  droplevels

# plot daily flows
ggplot(q, aes(DATE, Q, color=SITE_NAME)) +
  geom_line() +
  labs(x="", y="Flow (hm3/d)")

# plot annual flows
mutate(q, WYEAR=wyear(DATE)) %>%
  group_by(SITE_NAME, WYEAR) %>%
  summarize(Q=mean(Q)) %>%
  ggplot(aes(factor(WYEAR), Q, fill=SITE_NAME)) +
  geom_bar(stat='identity', position='dodge') +
  labs(x="Water Year", y="Annual Mean Flow (hm3/d)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

# reshape wq data
wq <- select(wq, -DATETIME)
wq.flow <- select(wq, SITE, DATE, VAR, VALUE) %>%
  filter(VAR %in% c('FLOW', 'STAGE')) %>%
  spread(VAR, VALUE)
wq <- filter(wq, VAR %in% c("TP","PO4")) %>%
  droplevels %>%
  left_join(wq.flow) %>%
  select(SITE, SITE_NAME, DATE, FLOW, STAGE, VAR, UNITS, VALUE, QAQC) %>%
  arrange(SITE, DATE)

# convert units from ppm to ppb
wq <- wq %>%
  mutate(Qs=cfs_to_hm3d(FLOW),
         VALUE=ifelse(UNITS=="ppm", VALUE*1000, VALUE),
         UNITS=ifelse(UNITS=="ppm", "ppb", "ppm")) %>%
  select(-FLOW, -STAGE) %>%
  rename(C=VALUE)
stopifnot(all(wq$C > 0 | wq$DATASET=='RAW'))

# plot wq data
wq %>%
  ggplot(aes(DATE, C)) +
  geom_point(size=1) +
  facet_grid(VAR~SITE_NAME, scales='free_y') +
  labs(x="", y="Concentration (ppb)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

wq <- select(wq, SITE, SITE_NAME, DATE, VAR, Qs, C)
wq.wide <- spread(wq, VAR, C)
df1 <- left_join(q, wq.wide) %>%
  gather(VAR, C, PO4, TP) %>%
  arrange(SITE, VAR, DATE) %>%
  mutate(SAMPLED=!is.na(C),
         WYEAR=wyear(DATE),
         L=Q*C,
         SITE=ordered(as.character(SITE), levels=levels(stn.kt_sprague$SITE)),
         SITE_NAME=ordered(as.character(SITE_NAME), levels=levels(stn.kt_sprague$SITE_NAME))) %>%
  select(SITE, SITE_NAME, DATE, WYEAR, VAR, Q, Qs, C, L, SAMPLED) %>%
  mutate(DATASET='CLEAN')

df2 <- df1 %>%
  left_join(select(tp.qaqc, SITE_NAME, DATE, TP_PASS)) %>%
  mutate(DATASET='QAQC')
ggplot(df2, aes(DATE, C, color=TP_PASS)) +
  geom_point() +
  facet_grid(VAR~SITE_NAME, scales='free_y')
idx <- which(!is.na(df2$TP_PASS) & !df2$TP_PASS)
df2[idx,]
df2[idx, 'Qs'] <- NA
df2[idx, 'C'] <- NA
df2[idx, 'L'] <- NA
df2[idx, 'SAMPLED'] <- FALSE

df <- rbind(df1, select(df2, -TP_PASS))

# estimate loads
estimate_loads <- function(df) {
  # df must have columns DATE, Q, C, L
  dataset <- unique(df$DATASET)
  variable <- unique(df$VAR)
  site <- unique(df$SITE_NAME)
  
  stopifnot(length(dataset)==1)
  stopifnot(length(variable)==1)
  stopifnot(length(site)==1)
  
    loads <- flux_regression(select(df, DATE, Q, C), interp = TRUE, max_interval = 90, predict_wyear_range = c(2001,2014))
  
  loads
}
# x <- filter(df, DATASET=='CLEAN', SITE=="SR0090", VAR=="TP") %>% estimate_loads(.)


loads <- lapply(unique(df$DATASET), function(dataset) {
  cat(dataset, '\n')
  x <- lapply(unique(as.character(df$VAR)), function(variable) {
    cat('..', variable, '\n')
    x <- lapply(unique(as.character(df$SITE_NAME)), function(site) {
      cat('....', site, '\n')
      x <- filter(df, DATASET==dataset, VAR==variable, SITE_NAME==site)
      x <- estimate_loads(x)
    })
    names(x) <- unique(df$SITE_NAME)
    x
  })
  names(x) <- unique(df$VAR)
  x
})
names(loads) <- unique(as.character(df$DATASET))


df.mon <- lapply(names(loads), function(dataset) {
  lapply(names(loads[[dataset]]), function(variable) {
    lapply(names(loads[[dataset]][[variable]]), function(site) {
      x <- loads[[dataset]][[variable]][[site]][['out']][['mon']]
      x <- mutate(x,
                  DATASET=dataset,
                  VAR=variable,
                  SITE_NAME=site)
      x
    }) %>%
      rbind_all()
  }) %>%
    rbind_all()
}) %>%
  rbind_all()

df.wyr <- lapply(names(loads), function(dataset) {
  lapply(names(loads[[dataset]]), function(variable) {
    lapply(names(loads[[dataset]][[variable]]), function(site) {
      x <- loads[[dataset]][[variable]][[site]][['out']][['wyr']]
      x <- mutate(x,
                  DATASET=dataset,
                  VAR=variable,
                  SITE_NAME=site)
      x
    }) %>%
      rbind_all()
  }) %>%
    rbind_all()
}) %>%
  rbind_all()

df.day <- lapply(names(loads), function(dataset) {
  lapply(names(loads[[dataset]]), function(variable) {
    lapply(names(loads[[dataset]][[variable]]), function(site) {
      x <- loads[[dataset]][[variable]][[site]][['predict']]
      x <- mutate(x,
                  DATASET=dataset,
                  VAR=variable,
                  SITE_NAME=site)
      x
    }) %>%
      rbind_all()
  }) %>%
    rbind_all()
}) %>%
  rbind_all()


p.model <- filter(df.day, WYEAR==2013, VAR=='TP') %>%
  ggplot(aes(DATE)) +
  geom_point(aes(y=Cobs), color='red') +
  geom_line(aes(y=Cest)) +
  geom_line(aes(y=Cpred), linetype=2) +
  facet_grid(SITE_NAME~DATASET) +
  labs(x='', y='TP Conc (ppb)', title='Change TP Concentration Model between Datasets\nCLEAN = includes all samples, QAQC = removes samples failing duplicate QAQC\nSolid Line=Interpolated Residuals, Dash Line=Model Prediction')

p.c <- filter(df.day, WYEAR==2013, VAR=='TP') %>%
  ggplot(aes(DATE, color=SITE_NAME)) +
  geom_point(aes(y=Cobs)) +
  geom_line(aes(y=Cest)) +
  geom_hline(yint=0, alpha=0) +
  scale_color_manual(values=c('deepskyblue', 'orangered')) +
  facet_grid(.~DATASET) +
  labs(x='', y='Concentration (ppb)') +
  theme(legend.position='bottom')
p.l <- filter(df.day, WYEAR==2013, VAR=='TP') %>%
  ggplot(aes(DATE, color=SITE_NAME)) +
  geom_point(aes(y=Lobs)) +
  geom_line(aes(y=Lest)) +
  geom_hline(yint=0, alpha=0) +
  scale_color_manual(values=c('deepskyblue', 'orangered')) +
  facet_grid(.~DATASET) +
  labs(x='', y='Load (kg/d)') +
  theme(legend.position='bottom')
grid.arrange(p.c, p.l, ncol=1, main='\nChange in Predicted TP Concentrations and Loads\nCLEAN = includes all samples, QAQC = removes samples failing duplicate QAQC')

p.c <- filter(df.wyr, VAR=='TP') %>%
  ggplot(aes(WYEAR, C, fill=SITE_NAME)) +
  geom_bar(stat='identity', position='dodge') +
  scale_fill_manual(values=c('deepskyblue', 'orangered')) +  
  labs(x='Water Year', y='Concentration (ppb)') +
  facet_grid(VAR~DATASET, scales='free_y')
p.l <- filter(df.wyr, VAR=='TP') %>%
  ggplot(aes(WYEAR, L, fill=SITE_NAME)) +
  geom_bar(stat='identity', position='dodge') +
  scale_fill_manual(values=c('deepskyblue', 'orangered')) +  
  labs(x='Water Year', y='Load (kg/day)') +
  facet_grid(VAR~DATASET, scales='free_y')
grid.arrange(p.c, p.l, ncol=1, main='\nChange in Annual TP Concentrations and Loads\nCLEAN = includes all samples, QAQC = removes samples failing duplicate QAQC')

p.2013 <- filter(df.wyr, WYEAR %in% c(2013)) %>%
  select(DATASET, VAR, WYEAR, SITE_NAME, Q, L) %>%
  gather(TERM, VALUE, Q, L) %>%
  filter(VAR=='TP') %>%
  spread(SITE_NAME, VALUE) %>%
  mutate(Power_Lone=Sprague_Power-Sprague_Lone) %>%
  gather(SITE_NAME, VALUE, Sprague_Lone, Sprague_Power, Power_Lone) %>%
  spread(TERM, VALUE) %>%
  mutate(C=L/Q) %>%
  gather(TERM, VALUE, Q, L, C) %>%
  ggplot(aes(SITE_NAME, VALUE)) +
  geom_bar(stat='identity') +
  labs(x='Site', y='') +
  ggtitle('Change in 2013 Flow and TP Conc/Load\nCLEAN = includes all samples, QAQC = removes samples failing duplicate QAQC') +
  facet_grid(TERM~DATASET, scales='free_y')



# check raw data
tp.qaqc %>%
ggplot(aes(DATE, TP_RPD, color=TP_PASS)) +
  geom_point() +
  facet_wrap(~SITE_NAME)

tp.qaqc.plot <- tp.qaqc %>%
  select(SITE_NAME, DATE, TP_PASS, TP_ppm, TP_DUP) %>%
  gather(VAR, VALUE, TP_ppm, TP_DUP) %>%
  arrange(SITE_NAME, VAR, TP_PASS) %>%
  mutate(TP_PASS_STR=ifelse(is.na(TP_PASS), 'NA', ifelse(TP_PASS, 'TRUE', 'FALSE')),
         TP_PASS_STR=ordered(TP_PASS_STR, levels=c("NA", "TRUE", "FALSE"))) %>%
  arrange(desc(TP_PASS))

p.day <- ggplot() +
  aes(DATE, VALUE*1000, color=TP_PASS_STR) +
  geom_point(data=filter(tp.qaqc.plot, TP_PASS_STR=='NA')) +
  geom_point(data=filter(tp.qaqc.plot, TP_PASS_STR=='TRUE')) +
  geom_point(data=filter(tp.qaqc.plot, TP_PASS_STR=='FALSE')) +
  facet_wrap(~SITE_NAME) +
  scale_y_log10(breaks=fluxr::log_breaks(c(1,5), 10^(seq(0, 3)))) +
  scale_color_manual('Pass QAQC?', values=c('NA'='grey50', 'TRUE'='deepskyblue', 'FALSE'='orangered'),
                     labels=c('NA'='No Duplicate', 'FALSE'='No', 'TRUE'='Yes')) +
  labs(x='', y='TP Conc (ppb)', title='QAQC Samples for TP\nColored points') +
  theme(legend.position='top')


pdf('dataset-qaqc-comparison.pdf', width=11, height=8.5)
p.day
p.model
grid.arrange(p.c, p.l, ncol=1, main='\nChange in Annual TP Concentrations and Loads\nCLEAN = includes all samples, QAQC = removes samples failing duplicate QAQC')
p.2013
dev.off()

# NEXT: create alternate dataset without outliers