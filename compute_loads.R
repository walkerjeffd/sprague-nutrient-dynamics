library(dplyr)
library(tidyr)
library(lubridate)
library(fluxr)
library(ggplot2)
theme_set(theme_bw())
library(gridExtra)

rm(list=ls())

cat(paste0(rep('=', 80), collapse=''), '\n')
cat("Running water quality model...\n\n")

load('kt_sprague.Rdata')
load('gis.Rdata')

dataset_levels <- c("POR", "RECENT")
site_name_levels <- levels(stn.kt_sprague$SITE_NAME)
wyear_ranges <- list(POR=c(2001, 2014),
                     RECENT=c(2009, 2014),
                     IVORY=c(2009, 2014),
                     TSS=c(2010, 2014))
predict_wyear_ranges <- list(POR=c(2002, 2014),
                             RECENT=c(2010, 2014),
                             IVORY=c(2010, 2014),
                             TSS=c(2011, 2014))

# load flow data ----
# (saved from compute_flows.R)
q <- readRDS('flows.Rdata')[['df']] %>%
  ungroup
q <- select(q, SITE_NAME, DATE, Q) %>%
  mutate(DATE=with_tz(DATE, "UTC"),
         Q=cfs_to_hm3d(Q))
q <- left_join(q, select(stn.kt_sprague, SITE, SITE_NAME), by='SITE_NAME') %>%
  mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=levels(stn.kt_sprague$SITE_NAME)))
stopifnot(all(!is.na(q$Q)))

# plot daily flows
ggplot(q, aes(DATE, Q)) +
  geom_line() +
  facet_wrap(~SITE_NAME) +
  labs(x="", y="Flow (hm3/d)")

# plot annual flows
mutate(q, WYEAR=wyear(DATE)) %>%
  dplyr::group_by(SITE_NAME, WYEAR) %>%
  dplyr::summarise(Q=mean(Q)) %>%
  ggplot(aes(factor(WYEAR), Q)) +
  geom_bar(stat='identity') +
  facet_wrap(~SITE_NAME) +
  labs(x="Water Year", y="Annual Mean Flow (hm3/d)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

# reshape wq data
wq <- lapply(dataset_levels, function(dataset) {
  x <- wq.kt_sprague[[dataset]]
  x$DATASET <- dataset
  x
}) %>%
  bind_rows(.)
wq <- select(wq, -DATETIME)
wq.flow <- select(wq, DATASET, SITE, DATE, VAR, VALUE) %>%
  filter(VAR %in% c('FLOW')) %>%
  spread(VAR, VALUE)
wq <- filter(wq, VAR %in% c("TP","PO4","TN","NO23","NH4","TSS")) %>%
  droplevels %>%
  left_join(wq.flow, by=c('SITE', 'DATASET', 'DATE')) %>%
  select(DATASET, SITE, SITE_NAME, DATE, FLOW, VAR, UNITS, VALUE, QAQC) %>%
  arrange(DATASET, SITE_NAME, DATE)

# convert flow units
wq[['Qs']] <- cfs_to_hm3d(wq[['FLOW']])

# convert units from ppm to ppb
wq <- wq %>%
  mutate(VALUE=ifelse(UNITS=="ppm", VALUE*1000, VALUE),
         UNITS=ifelse(UNITS=="ppm", "ppb", "ppm")) %>%
  select(-FLOW) %>%
  dplyr::rename(C=VALUE)
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
dataset_sites <- lapply(dataset_levels, function(dataset) {
  x.dataset <- filter(wq, DATASET==dataset)
  unique(as.character(x.dataset$SITE_NAME))
})
names(dataset_sites) <- dataset_levels

dataset_vars <- lapply(dataset_levels, function(dataset) {
  x.dataset <- filter(wq, DATASET==dataset)
  unique(as.character(x.dataset$VAR))
})
names(dataset_vars) <- dataset_levels

# merge continuous flow and wq data
q <- lapply(names(wq.kt_sprague), function(dataset) {
  mutate(q,
         SITE_NAME=as.character(SITE_NAME),
         DATASET=dataset) %>%
    filter(SITE_NAME %in% dataset_sites[[dataset]])
}) %>%
  bind_rows(.)
table(q$SITE_NAME, q$DATASET)

wq <- select(wq, DATASET, SITE, SITE_NAME, DATE, VAR, Qs, C)
wq.wide <- spread(wq, VAR, C)

df <- left_join(mutate(q, SITE=as.character(SITE)),
                mutate(wq.wide,
                       SITE=as.character(SITE),
                       SITE_NAME=as.character(SITE_NAME)),
                by=c('SITE_NAME', 'DATE', 'SITE', 'DATASET')) %>%
  gather(VAR, C, NH4, NO23, PO4, TN, TP, TSS) %>%
  arrange(DATASET, SITE, VAR, DATE) %>%
  mutate(WYEAR=wyear(DATE),
         SAMPLED=!is.na(C),
         L=Q*C,
         SITE=ordered(SITE, levels=levels(stn.kt_sprague$SITE)),
         SITE_NAME=ordered(SITE_NAME, levels=levels(stn.kt_sprague$SITE_NAME))) %>%
  select(DATASET, SITE, SITE_NAME, DATE, WYEAR, VAR, Q, Qs, C, L, SAMPLED)
str(df)

# table(df$SITE_NAME, df$DATASET)
# summary(df)

# years for predictions
filter(df, SAMPLED) %>%
  dplyr::group_by(DATASET, SITE_NAME, VAR, WYEAR) %>%
  dplyr::summarise(N=n()) %>%
  filter(N>1) %>%
  ggplot() +
  geom_tile(aes(x=factor(WYEAR), SITE_NAME)) +
  facet_grid(VAR~DATASET) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

# estimate loads
estimate_loads <- function(df, dataset, variable, site) {
  # df must have columns DATE, Q, C, L
  if (variable == 'TSS') {
    x <- filter(df, WYEAR %in% seq(wyear_ranges[['TSS']][1], wyear_ranges[['TSS']][2]))
    predict_wyear_range <- predict_wyear_ranges[['TSS']]
  } else if (site %in% c('SF_Ivory', 'NF_Ivory')) {
    x <- filter(df, WYEAR %in% seq(wyear_ranges[['IVORY']][1], wyear_ranges[['IVORY']][2]))
    predict_wyear_range <- predict_wyear_ranges[['IVORY']]
  } else {
    x <- filter(df, WYEAR %in% seq(wyear_ranges[[dataset]][1], wyear_ranges[[dataset]][2]))
    predict_wyear_range <- predict_wyear_ranges[[dataset]]
  }

  x <- ungroup(x)
  x <- x[, c('DATE', 'Q', 'C')]

  loads <- flux_regression(x, interp = TRUE, max_interval = 90, predict_wyear_range = predict_wyear_range)

  loads
}
# x <- filter(df, DATASET=='RECENT', SITE_NAME=="Power", VAR=="TP") %>% estimate_loads(.)
# x <- filter(df, DATASET=='POR', SITE_NAME=="Power", VAR=="TP") %>% estimate_loads(., 'POR', 'TP', 'Power')
# x <- filter(df, DATASET=='POR', SITE_NAME=="SF_Ivory", VAR=="TP") %>% estimate_loads(., 'POR', 'TP', 'SF_Ivory')

loads <- lapply(dataset_levels, function(dataset) {
  cat(dataset, '\n')
  x_dataset <- lapply(dataset_vars[[dataset]], function(variable) {
    cat('..', variable, '\n')
    x_variable <- lapply(dataset_sites[[dataset]], function(site) {
      cat('....', site, '\n')
      # print(str(df, max.level = 1))
      x_df <- filter(df, DATASET==dataset, VAR==variable, SITE_NAME==site)
      x_site <- estimate_loads(x_df, dataset=dataset, variable=variable, site=site)

      return(x_site)
    })
    names(x_variable) <- dataset_sites[[dataset]]
    return(x_variable)
  })
  names(x_dataset) <- dataset_vars[[dataset]]
  return(x_dataset)
})
names(loads) <- dataset_levels

areas <- rbind(select(subbasin_area, SITE_NAME, AREA_KM2),
               select(incbasin_area, SITE_NAME=INC_SITE_NAME, AREA_KM2)) %>%
  as.data.frame() %>%
  select(SITE_NAME, AREA_KM2) %>%
  unique %>%
 # unique  %>% # i converted to a df and removed the spatial information to get the unique function working and reduce the dataset appropriately
  mutate(SITE_NAME=as.character(SITE_NAME)) %>%
  arrange(SITE_NAME) %>%
  mutate(IDX=1) %>%
  pivot_wider(names_from=SITE_NAME,values_from=AREA_KM2) %>%  #spread(SITE_NAME, AREA_KM2) %>%
  mutate('Godowa+Sycan' = Godowa + Sycan,
         'SF+NF' = SF + NF,
         'SF_Ivory+NF_Ivory' = SF_Ivory + NF_Ivory) %>%
  pivot_longer(c(Godowa:`SF_Ivory+NF_Ivory`),names_to="SITE_NAME",values_to="AREA_KM2") %>%  #gather(SITE_NAME, AREA_KM2, -IDX) %>%
  mutate(SITE_NAME=as.character(SITE_NAME)) %>%
  select(-IDX)

# daily data frame ----
df_day <- lapply(names(loads), function(dataset) {
  lapply(names(loads[[dataset]]), function(variable) {
    lapply(names(loads[[dataset]][[variable]]), function(site) {
      x <- loads[[dataset]][[variable]][[site]][['out']][['day']]
      x$DATASET <- dataset
      x$VAR <- variable
      x$SITE_NAME <- site
      x
    }) %>%
      bind_rows
  }) %>%
    bind_rows
}) %>%
  bind_rows %>%
  mutate(DATASET=ordered(DATASET, levels=dataset_levels),
         SITE_NAME=ordered(SITE_NAME, levels=site_name_levels),
         VAR=factor(VAR)) %>%
  select(-SAMPLED)

df_day_p <- filter(df_day, VAR %in% c('TP', 'PO4')) %>%
  gather(TERM, VALUE, L, C) %>%
  spread(VAR, VALUE) %>%
  mutate(PP = TP - PO4) %>%
  gather(VAR, VALUE, TP, PO4, PP) %>%
  filter(VAR=='PP') %>%
  spread(TERM, VALUE)
df_day_p$C <- ifelse(df_day_p$C<=1, 1, df_day_p$C)
df_day_p$L <- df_day_p$Q*df_day_p$C

df_day <- rbind(df_day, df_day_p) %>%
  arrange(DATASET, VAR, SITE_NAME, DATE, MONTH, WYEAR) %>%
  mutate(FREQ="DAY",
         START_DATE=DATE,
         END_DATE=DATE) %>%
  select(FREQ, DATASET, VAR, SITE_NAME, DATE, MONTH, WYEAR, START_DATE, END_DATE, Q, L, C) %>%
  mutate(DATASET=as.character(DATASET),
         VAR=as.character(VAR),
         SITE_NAME=as.character(SITE_NAME))

# compute month
df_mon <- df_day %>%
  mutate(MONTHYEAR=floor_date(DATE, 'month')) %>%
  dplyr::group_by(DATASET, VAR, SITE_NAME, MONTHYEAR, MONTH, WYEAR) %>%
  dplyr::summarise(START_DATE=min(DATE),
            END_DATE=max(DATE),
            Q=mean(Q),
            L=mean(L),
            C=ifelse(L <= 0 | Q <= 0, NA, L/Q)) %>%
  ungroup %>%
  mutate(FREQ="MON") %>%
  select(FREQ, DATASET, VAR, SITE_NAME, DATE=MONTHYEAR, MONTH, WYEAR, START_DATE, END_DATE, Q, L, C)

# compute wyr
seasons <- list(Annual=1:12,
                'Fall (Oct-Dec)'=c(10:12),
                'Winter (Jan-Mar)'=c(1:3),
                'Spring (Apr-Jun)'=c(4:6),
                'Summer (Jul-Sep)'=c(7:9))
df_seasons <- lapply(names(seasons), function(s) {
  data.frame(MONTH=seasons[[s]], SEASON=s, stringsAsFactors=FALSE)
}) %>%
  bind_rows %>%
  mutate(SEASON=ordered(SEASON, levels=names(seasons)))

df_wyr <- left_join(df_seasons, df_day, by="MONTH") %>%
  mutate(MONTH=10,
         WYEARDATE=ymd(paste(WYEAR-1, MONTH, 1, sep='-'))) %>%
  dplyr::group_by(DATASET, VAR, SITE_NAME, WYEARDATE, MONTH, WYEAR, SEASON) %>%
  dplyr::summarise(N_DAY=n(),
            START_DATE=min(DATE),
            END_DATE=max(DATE),
            Q=mean(Q),
            L=mean(L),
            C=ifelse(L <= 0 | Q <= 0, NA, L/Q)) %>%
  ungroup %>%
  mutate(FREQ="WYR") %>%
  select(FREQ, DATASET, VAR, SITE_NAME, DATE=WYEARDATE, MONTH, WYEAR, SEASON, START_DATE, END_DATE, N_DAY, Q, L, C)

# compute site
df_site_tss <- filter(df_wyr, VAR=="TSS") %>%
  dplyr::group_by(DATASET, VAR, SITE_NAME, SEASON) %>%
  dplyr::summarise(N_YEAR=length(unique(WYEAR)),
            N_DAY=sum(N_DAY),
            PERIOD=paste0(min(WYEAR), '-', max(WYEAR)),
            START_DATE=min(DATE),
            END_DATE=max(DATE),
            DATE=min(DATE),
            Q=mean(Q),
            L=mean(L),
            C=ifelse(L <= 0 | Q <= 0, NA, L/Q)) %>%
  ungroup

df_site_por_2002 <- filter(df_wyr, DATASET=="POR", VAR!="TSS",
                           !(SITE_NAME %in% c('SF_Ivory', 'NF_Ivory'))) %>%
  dplyr::group_by(DATASET, VAR, SITE_NAME, SEASON) %>%
  dplyr::summarise(N_YEAR=length(unique(WYEAR)),
            N_DAY=sum(N_DAY),
            PERIOD=paste0(min(WYEAR), '-', max(WYEAR)),
            START_DATE=min(DATE),
            END_DATE=max(DATE),
            DATE=min(DATE),
            Q=mean(Q),
            L=mean(L),
            C=ifelse(L <= 0 | Q <= 0, NA, L/Q)) %>%
  ungroup
stopifnot(all(df_site_por_2002$PERIOD == '2002-2014'))

df_site_por_2010 <- filter(df_wyr, DATASET=="POR", VAR!="TSS",
                           WYEAR >= 2010) %>%
  dplyr::group_by(DATASET, VAR, SITE_NAME, SEASON) %>%
  dplyr::summarise(N_YEAR=length(unique(WYEAR)),
            N_DAY=sum(N_DAY),
            PERIOD=paste0(min(WYEAR), '-', max(WYEAR)),
            START_DATE=min(DATE),
            END_DATE=max(DATE),
            DATE=min(DATE),
            Q=mean(Q),
            L=mean(L),
            C=ifelse(L <= 0 | Q <= 0, NA, L/Q)) %>%
  ungroup
stopifnot(all(df_site_por_2010$PERIOD == '2010-2014'))

df_site_recent <- filter(df_wyr, DATASET=="RECENT", VAR!="TSS") %>%
  dplyr::group_by(DATASET, VAR, SITE_NAME, SEASON) %>%
  dplyr::summarise(N_YEAR=length(unique(WYEAR)),
            N_DAY=sum(N_DAY),
            PERIOD=paste0(min(WYEAR), '-', max(WYEAR)),
            START_DATE=min(DATE),
            END_DATE=max(DATE),
            DATE=min(DATE),
            Q=mean(Q),
            L=mean(L),
            C=ifelse(L <= 0 | Q <= 0, NA, L/Q)) %>%
  ungroup
stopifnot(all(df_site_recent$PERIOD == '2010-2014'))

df_site <- rbind(df_site_tss, df_site_por_2002, df_site_por_2010, df_site_recent) %>%
  mutate(PERIOD=factor(PERIOD),
         FREQ="SITE")

# combine all and compute summation and incremental nodes
df_all <- bind_rows(df_mon, df_wyr, df_site)

df_all <- df_all %>%
  gather(TERM, VALUE, Q, L, C) %>%
  filter(TERM != "C") %>%
  spread(SITE_NAME, VALUE) %>%
  mutate('Godowa+Sycan' = Godowa + Sycan,
         'SF+NF' = SF + NF,
         'SF_Ivory+NF_Ivory' = SF_Ivory + NF_Ivory) %>%
  gather(SITE_NAME, VALUE, Godowa:`SF_Ivory+NF_Ivory`, na.rm=TRUE) %>%
  mutate(SITE_NAME=as.character(SITE_NAME)) %>%
  spread(TERM, VALUE) %>%
  mutate(C=L/Q)

df_all <- df_all %>%
  gather(TERM, VALUE, Q, L, C) %>%
  spread(SITE_NAME, VALUE) %>%
  mutate('Power-Lone_Pine' = Power - Lone_Pine,
         'Lone_Pine-Godowa-Sycan' = Lone_Pine - `Godowa+Sycan`,
         'Godowa-SF-NF' = Godowa - `SF+NF`,
         'Godowa-SF_Ivory-NF_Ivory' = Godowa - `SF_Ivory+NF_Ivory`,
         'SF_Ivory-SF' = SF_Ivory - SF,
         'NF_Ivory-NF' = NF_Ivory - NF) %>%
  gather(SITE_NAME, VALUE, Godowa:`NF_Ivory-NF`, na.rm=TRUE) %>%
  mutate(SITE_NAME=as.character(SITE_NAME)) %>%
  spread(TERM, VALUE)

df_all <- left_join(df_all,
                    mutate(areas, SITE_NAME=as.character(SITE_NAME)),
                    by="SITE_NAME") %>%
  mutate(L_AREA=L/AREA_KM2,                 # kg/km2/day
         Q_AREA=Q/AREA_KM2*100) %>%         # cm/day
  gather(TERM, VALUE, Q, L, C, L_AREA, Q_AREA) %>%
  mutate(SITE_NAME=ordered(as.character(SITE_NAME),
                           levels=c("Power",
                                    "Power-Lone_Pine",
                                    "Lone_Pine",
                                    "Lone_Pine-Godowa-Sycan",
                                    "Godowa+Sycan",
                                    "Godowa",
                                    "Godowa-SF-NF",
                                    "Godowa-SF_Ivory-NF_Ivory",
                                    "Sycan",
                                    "SF_Ivory+NF_Ivory",
                                    "SF+NF",
                                    "SF_Ivory",
                                    "SF_Ivory-SF",
                                    "SF",
                                    "NF_Ivory",
                                    "NF_Ivory-NF",
                                    "NF")),
         TERM=ordered(as.character(TERM),
                      levels=c('Q','Q_AREA','L','L_AREA','C')))

# separate flow variable ----
df_all_flow <- filter(df_all, VAR=='TP') %>%
  filter(TERM %in% c('Q', 'Q_AREA')) %>%
  mutate(VAR='FLOW')
df_all <- filter(df_all, !(TERM %in% c('Q', 'Q_AREA'))) %>%
  rbind(df_all_flow) %>%
  mutate(TERM=ordered(as.character(TERM), levels=c('Q','Q_AREA','L','L_AREA','C')),
         VAR=ordered(as.character(VAR), levels=c('FLOW','TP','PO4','PP','TN','NH4','NO23','TSS')))

df_day <- gather(df_day, TERM, VALUE, Q:C)
df_day_flow <- filter(df_day, VAR=='TP') %>%
  filter(TERM %in% c('Q', 'Q_AREA')) %>%
  mutate(VAR='FLOW')
df_day <- filter(df_day, !(TERM %in% c('Q', 'Q_AREA'))) %>%
  rbind(df_day_flow) %>%
  mutate(TERM=ordered(as.character(TERM), levels=c('Q','Q_AREA','L','L_AREA','C')),
         VAR=ordered(as.character(VAR), levels=c('FLOW','TP','PO4','PP','TN','NH4','NO23','TSS')))

df_mon <- filter(df_all, FREQ=="MON") %>%
  select(-FREQ, -PERIOD, -START_DATE, -END_DATE, -SEASON, -N_DAY, -N_YEAR) %>%
  dplyr::rename(MONTHYEAR=DATE)
df_wyr <- filter(df_all, FREQ=="WYR") %>%
  select(-FREQ, -PERIOD, -START_DATE, -END_DATE, -MONTH, -N_YEAR)
df_site <- filter(df_all, FREQ=="SITE") %>%
  select(-FREQ, -MONTH, -DATE, -WYEAR)

# check df_day
dplyr::group_by(df_day, DATASET, VAR, TERM, SITE_NAME, DATE) %>%
  dplyr::summarise(N=n()) %>%
  (function(x) { stopifnot(all(x$N==1)) })

dplyr::group_by(df_mon, DATASET, VAR, TERM, SITE_NAME, MONTHYEAR) %>%
  dplyr::summarise(N=n()) %>%
  (function(x) { stopifnot(all(x$N==1)) })

dplyr::group_by(df_wyr, DATASET, VAR, TERM, SITE_NAME, WYEAR, SEASON) %>%
  dplyr::summarise(N=n()) %>%
  (function(x) { stopifnot(all(x$N==1)) })

dplyr::group_by(df_site, DATASET, VAR, TERM, SITE_NAME, PERIOD, SEASON) %>%
  dplyr::summarise(N=n()) %>%
  (function(x) { stopifnot(all(x$N==1)) })

table(df_site$SEASON, df_site$PERIOD, df_site$DATASET)

filter(df_site, DATASET=="POR", TERM=="C", SITE_NAME %in% subbasin_area$SITE_NAME) %>%
  ggplot(aes(SITE_NAME, VALUE, fill=PERIOD)) +
  geom_bar(stat="identity", position="dodge") +
  facet_grid(VAR~SEASON, scales='free_y')

loads_df <- list('day'=df_day,
                 'mon'=df_mon,
                 'wyr'=df_wyr,
                 'site'=df_site)
loads_input <- df

# save ----
cat('\n')
lapply(list('mon', 'wyr', 'site'), function (term) {
  filename <- file.path('csv', paste0('loads_', term,'.csv'))
  cat('Saving load results to:', filename, '\n')
  loads_df[[term]] %>%
    write.csv(file=filename, row.names=FALSE)
})

filename <- file.path('csv', 'areas.csv')
cat('Saving areas to:', filename, '\n')
areas %>%
  write.csv(file=filename, row.names=FALSE)

filename <- 'loads.Rdata'
cat('\nSaving load results to:', filename, '\n')
save(loads, loads_input, loads_df, file=filename)

cat('\n\n')
