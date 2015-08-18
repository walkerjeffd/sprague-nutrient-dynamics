library(dplyr)
library(tidyr)
library(lubridate)
library(fluxr)
library(ggplot2)
theme_set(theme_bw())
library(gridExtra)

load('kt_sprague.Rdata')

dataset_levels <- names(wq.kt_sprague)
site_name_levels <- levels(stn.kt_sprague$SITE_NAME)
wyear_ranges <- list(CLEAN=c(2001, 2014),
                     RAW=c(2001, 2014),
                     POR=c(2001, 2014),
                     RECENT=c(2009, 2014),
                     TSS=c(2010, 2014))
predict_wyear_ranges <- list(CLEAN=c(2002, 2014),
                             RAW=c(2002, 2014),
                             POR=c(2002, 2014),
                             RECENT=c(2009, 2014),
                             TSS=c(2011, 2014))


# load flow data
# (saved from compute_flows.R)
q <- readRDS('flows.Rdata')[['df']] %>%
  ungroup
q <- select(q, SITE_NAME, DATE, Q) %>%
  mutate(DATE=as.Date(DATE),
         Q=cfs_to_hm3d(Q))
q <- left_join(q, select(stn.kt_sprague, SITE, SITE_NAME)) %>%
  mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=levels(stn.kt_sprague$SITE_NAME))) %>%
  select(SITE, SITE_NAME, DATE, Q)

# reshape wq data
wq <- lapply(c('POR', 'RECENT'), function(dataset) {
  x <- wq.kt_sprague[[dataset]]
  x$DATASET <- dataset
  x
}) %>%
  rbind_all(.)
wq <- select(wq, -DATETIME) %>%
  mutate(DATE=as.Date(DATE))
wq.flow <- select(wq, DATASET, SITE, DATE, VAR, VALUE) %>%
  filter(VAR %in% c('FLOW', 'STAGE')) %>%
  spread(VAR, VALUE)
wq <- filter(wq, VAR %in% c("TP","PO4","TN","NO23","NH4","TSS")) %>%
  droplevels %>%
  left_join(wq.flow) %>%
  select(DATASET, SITE, SITE_NAME, DATE, FLOW, STAGE, VAR, UNITS, VALUE, QAQC) %>%
  arrange(DATASET, SITE_NAME, DATE)

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
  filter(DATASET=='POR') %>%
  ggplot(aes(DATE, C)) +
  geom_point(size=1) +
  facet_grid(VAR~SITE_NAME, scales='free_y') +
  labs(x="", y="Concentration (ppb)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

# create list of sites and variables for each dataset
dataset_sites <- lapply(unique(wq$DATASET), function(dataset) {
  x.dataset <- filter(wq, DATASET==dataset)
  unique(as.character(x.dataset$SITE_NAME))
})
names(dataset_sites) <- unique(wq$DATASET)

dataset_vars <- lapply(unique(wq$DATASET), function(dataset) {
  x.dataset <- filter(wq, DATASET==dataset)
  unique(as.character(x.dataset$VAR))
})
names(dataset_vars) <- unique(wq$DATASET)

# merge continuous flow and wq data
q <- lapply(unique(wq$DATASET), function(dataset) {
  mutate(q,
         SITE_NAME=as.character(SITE_NAME),
         DATASET=dataset) %>% 
    filter(SITE_NAME %in% dataset_sites[[dataset]])
}) %>%
  rbind_all(.)
table(q$SITE_NAME, q$DATASET)

wq <- select(wq, DATASET, SITE, SITE_NAME, DATE, VAR, Qs, C)
wq.wide <- spread(wq, VAR, C)
df <- left_join(q, wq.wide) %>%
  gather(VAR, C, NH4, NO23, PO4, TN, TP, TSS) %>%
  arrange(DATASET, SITE, VAR, DATE) %>%
  mutate(SAMPLED=!is.na(C),
         WYEAR=wyear(DATE),
         L=Q*C,
         SITE=ordered(as.character(SITE), levels=levels(stn.kt_sprague$SITE)),
         SITE_NAME=ordered(as.character(SITE_NAME), levels=levels(stn.kt_sprague$SITE_NAME))) %>%
  select(DATASET, SITE, SITE_NAME, DATE, WYEAR, VAR, Q, Qs, C, L, SAMPLED)
table(df$SITE_NAME, df$DATASET)

# years for predictions
filter(df, SAMPLED) %>%
  group_by(DATASET, SITE_NAME, VAR, WYEAR) %>%
  summarise(N=n()) %>%
  filter(N>1) %>%
  ggplot() +
  geom_tile(aes(x=factor(WYEAR), SITE_NAME)) +
  facet_grid(VAR~DATASET) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

# estimate loads
estimate_loads <- function(df) {
  # df must have columns DATE, Q, C, L
  dataset <- unique(df$DATASET)
  variable <- unique(df$VAR)
  site <- unique(df$SITE_NAME)
  
  stopifnot(length(dataset)==1)
  stopifnot(length(variable)==1)
  stopifnot(length(site)==1)
  
  if (variable == 'TSS') {
    df <- filter(df, WYEAR %in% seq(wyear_ranges[['TSS']][1], wyear_ranges[['TSS']][2]))
    predict_wyear_range <- predict_wyear_ranges[['TSS']]
  } else {
    df <- filter(df, WYEAR %in% seq(wyear_ranges[[dataset]][1], wyear_ranges[[dataset]][2]))
    predict_wyear_range <- predict_wyear_ranges[[dataset]]
  }
  
  loads <- flux_regression(select(df, DATE, Q, C), interp = TRUE, max_interval = 90, predict_wyear_range = predict_wyear_range)
  
  loads
}
# x <- filter(df, DATASET=='RECENT', SITE=="SR0090", VAR=="TP") %>% estimate_loads(.)

loads <- lapply(unique(wq$DATASET), function(dataset) {
  cat(dataset, '\n')
  x <- lapply(dataset_vars[[dataset]], function(variable) {
    cat('..', variable, '\n')
    x <- lapply(dataset_sites[[dataset]], function(site) {
      cat('....', site, '\n')
      x <- filter(df, DATASET==dataset, VAR==variable, SITE_NAME==site)
      x.sampled <- filter(x, SAMPLED)
      
      x.loads <- estimate_loads(x)
      x.wyr <- x.loads[['out']][['wyr']]
      x.wyr$DROPPED <- NA
      
      x.wyr.drop <- lapply(seq(1, nrow(x.sampled)), function(i) {
        y.sampled <- x.sampled[i, ]
        idx.dropped <- which(x$DATE==y.sampled$DATE)
        y <- x
        y[idx.dropped, 'Qs'] <- NA
        y[idx.dropped, 'C'] <- NA
        y[idx.dropped, 'L'] <- NA
        y[idx.dropped, 'SAMPLED'] <- FALSE
        y.wyr <- estimate_loads(y)[['out']][['wyr']]
        
        x.orig_conc <- x.wyr[which(x.wyr$WYEAR == y.sampled$WYEAR),]$C
        if (length(x.orig_conc) == 0) {
          x.orig_conc <- NA
        }
        
        y.drop_conc <- y.wyr[which(y.wyr$WYEAR == y.sampled$WYEAR),]$C
        if (length(y.drop_conc) == 0) {
          y.drop_conc <- NA
        }
        
        data.frame(DATASET=dataset,
                   VAR=variable,
                   SITE_NAME=site,
                   DROPPED_DATE=y.sampled$DATE,
                   C_orig=x.orig_conc,
                   C_drop=y.drop_conc) %>%
          mutate(C_diff=abs(C_orig-C_drop),
                 C_reldiff=C_diff/C_orig)
      }) %>%
        rbind_all
      x.wyr.drop
    }) %>%
      rbind_all
    x
  }) %>%
    rbind_all
  x
}) %>%
  rbind_all

loads_na <- loads
loads <- filter(loads, !is.na(C_orig))

filter(loads, DATASET=='POR', VAR=='TP') %>%
  ggplot(aes(C_orig, C_drop)) +
  geom_point() +
  geom_abline() +
  facet_wrap(~SITE_NAME)

filter(loads, DATASET=='POR', VAR=='TP') %>%
  ggplot(aes(DROPPED_DATE, C_diff)) +
  geom_point() +
  facet_wrap(~SITE_NAME)

saveRDS(loads, file='loads_dropped.Rdata')
