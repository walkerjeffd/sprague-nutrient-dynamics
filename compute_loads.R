library(dplyr)
library(tidyr)
library(lubridate)
library(fluxr)
library(ggplot2)
theme_set(theme_bw())
library(gridExtra)

load('kt_sprague.Rdata')
load('gis.Rdata')

dataset_levels <- c("POR", "RECENT")
site_name_levels <- levels(stn.kt_sprague$SITE_NAME)
wyear_ranges <- list(POR=c(2001, 2014),
                     RECENT=c(2009, 2014),
                     TSS=c(2010, 2014))
predict_wyear_ranges <- list(POR=c(2002, 2014),
                             RECENT=c(2009, 2014),
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
  group_by(SITE_NAME, WYEAR) %>%
  summarize(Q=mean(Q)) %>%
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
  rbind_all(.)
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
  rbind_all(.)
# table(q$SITE_NAME, q$DATASET)

wq <- select(wq, DATASET, SITE, SITE_NAME, DATE, VAR, Qs, C)
wq.wide <- spread(wq, VAR, C)

df <- left_join(q, wq.wide, by=c('SITE_NAME', 'DATE', 'SITE', 'DATASET')) %>%
  gather(VAR, C, NH4, NO23, PO4, TN, TP, TSS) %>%
  arrange(DATASET, SITE, VAR, DATE)

df <- mutate(df,
             WYEAR=wyear(df$DATE),
             SAMPLED=!is.na(C),
             L=Q*C,
             SITE=ordered(SITE,
                          levels=levels(stn.kt_sprague$SITE)),
             SITE_NAME=ordered(SITE_NAME,
                               levels=levels(stn.kt_sprague$SITE_NAME))) %>%
  select(df, DATASET, SITE, SITE_NAME, DATE, WYEAR, VAR, Q, Qs, C, L, SAMPLED)
str(df)

# table(df$SITE_NAME, df$DATASET)
# summary(df)

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
estimate_loads <- function(df, dataset, variable, site) {
  # df must have columns DATE, Q, C, L
  if (variable == 'TSS') {
    x <- filter(df, WYEAR %in% seq(wyear_ranges[['TSS']][1], wyear_ranges[['TSS']][2]))
    predict_wyear_range <- predict_wyear_ranges[['TSS']]
  } else {
    x <- filter(df, WYEAR %in% seq(wyear_ranges[[dataset]][1], wyear_ranges[[dataset]][2]))
    predict_wyear_range <- predict_wyear_ranges[[dataset]]
  }

  x <- ungroup(x)
  x <- x[, c('DATE', 'Q', 'C')]

  loads <- flux_regression(x, interp = TRUE, max_interval = 90, predict_wyear_range = predict_wyear_range)

  loads
}
# x <- filter(df, DATASET=='RECENT', SITE=="SR0090", VAR=="TP") %>% estimate_loads(.)
x <- filter(df, DATASET=='POR', SITE=="SR0090", VAR=="TP") %>% estimate_loads(., 'POR', 'SR0090', 'TP')
str(df)

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
               select(incbasin_ivory_area, SITE_NAME=INC_SITE_NAME, AREA_KM2),
               select(incbasin_area, SITE_NAME=INC_SITE_NAME, AREA_KM2)) %>%
  unique

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
      rbind_all
  }) %>%
    rbind_all
}) %>%
  rbind_all %>%
  mutate(DATASET=ordered(DATASET, levels=dataset_levels),
         SITE_NAME=ordered(SITE_NAME, levels=site_name_levels),
         VAR=factor(VAR))

df_day <- select(df_day, DATASET, VAR, SITE_NAME, DATE, MONTH, WYEAR, Q, L) %>%
  gather(TERM, VALUE, Q, L) %>%
  spread(SITE_NAME, VALUE) %>%
  mutate('Power-Lone_Pine' = Power - Lone_Pine,
         'Lone_Pine-Godowa-Sycan' = Lone_Pine - Sycan - Godowa,
         'Godowa-SF-NF' = Godowa - SF - NF,
         'Godowa-SF_Ivory-NF_Ivory' = Godowa - SF_Ivory - NF_Ivory,
         'SF_Ivory-SF' = SF_Ivory - SF,
         'NF_Ivory-NF' = NF_Ivory - NF) %>%
  gather(SITE_NAME, VALUE, -DATASET, -VAR, -DATE, -MONTH, -WYEAR, -TERM, na.rm=TRUE) %>%
  spread(TERM, VALUE)

df_day_p <- filter(df_day, VAR %in% c('TP', 'PO4')) %>%
  spread(VAR, L) %>%
  mutate(PP = TP - PO4) %>%
  gather(VAR, L, TP, PO4, PP) %>%
  filter(VAR=='PP')
df_day <- rbind(df_day, df_day_p) %>%
  arrange(DATASET, VAR, SITE_NAME, DATE, MONTH, WYEAR)

df_day <- left_join(df_day, areas, by="SITE_NAME") %>%
  mutate(C=ifelse(L <= 0 | Q <= 0, NA, L/Q), # ppb
         L_AREA=L/AREA_KM2,                 # kg/km2/mon
         Q_AREA=Q/AREA_KM2*100) %>%         # cm/mon
  gather(TERM, VALUE, Q, L, C, L_AREA, Q_AREA) %>%
  mutate(SITE_NAME=ordered(as.character(SITE_NAME),
                           levels=c("Power",
                                    "Power-Lone_Pine",
                                    "Lone_Pine",
                                    "Lone_Pine-Godowa-Sycan",
                                    "Godowa",
                                    "Godowa-SF-NF",
                                    "Godowa-SF_Ivory-NF_Ivory",
                                    "Sycan",
                                    "SF_Ivory",
                                    "SF_Ivory-SF",
                                    "SF",
                                    "NF_Ivory",
                                    "NF_Ivory-NF",
                                    "NF")))

# monthly data frame ----
df_mon <- filter(df_day, TERM %in% c('Q', 'L', 'Q_AREA', 'L_AREA')) %>%
  spread(TERM, VALUE) %>%
  mutate(MONTHYEAR=floor_date(DATE, 'month')) %>%
  group_by(DATASET, VAR, SITE_NAME, AREA_KM2, MONTHYEAR, MONTH, WYEAR) %>%
  summarise(Q=sum(Q),
            L=sum(L),
            C=ifelse(L <= 0 | Q <= 0, NA, L/Q),
            Q_AREA=sum(Q_AREA),
            L_AREA=sum(L_AREA)) %>%
  gather(TERM, VALUE, Q, L, C, Q_AREA, L_AREA) %>%
  mutate(TERM=ordered(as.character(TERM), levels=c('Q','Q_AREA','L','L_AREA','C')))

# wyear data frame ----
df_wyr <- filter(df_mon, TERM %in% c('Q', 'L', 'Q_AREA', 'L_AREA')) %>%
  spread(TERM, VALUE) %>%
  group_by(DATASET, VAR, SITE_NAME, AREA_KM2, WYEAR) %>%
  summarise(Q=sum(Q),
            L=sum(L),
            C=ifelse(L <= 0 | Q <= 0, NA, L/Q),
            Q_AREA=sum(Q_AREA),
            L_AREA=sum(L_AREA)) %>%
  gather(TERM, VALUE, Q, L, C, Q_AREA, L_AREA)

# site data frame ----
df_site <- spread(df_wyr, TERM, VALUE) %>%
  group_by(DATASET, VAR, SITE_NAME, AREA_KM2) %>%
  summarise(N.WYEAR=n(),
            Q=mean(Q),
            L=mean(L),
            C=ifelse(L <= 0 | Q < 0, NA, L/Q),
            Q_AREA=mean(Q_AREA),
            L_AREA=mean(L_AREA)) %>%
  ungroup %>%
  gather(TERM, VALUE, Q, L, C, Q_AREA, L_AREA)

# separate flow variable ----
df_day_flow <- filter(df_day, VAR=='TP') %>%
  filter(TERM %in% c('Q', 'Q_AREA')) %>%
  mutate(VAR='FLOW')
df_day <- filter(df_day, !(TERM %in% c('Q', 'Q_AREA'))) %>%
  rbind(df_day_flow) %>%
  mutate(TERM=ordered(as.character(TERM), levels=c('Q','Q_AREA','L','L_AREA','C')),
         VAR=ordered(as.character(VAR), levels=c('FLOW','TP','PO4','PP','TN','NH4','NO23','TSS')))

df_mon_flow <- filter(df_mon, VAR=='TP') %>%
  filter(TERM %in% c('Q', 'Q_AREA')) %>%
  mutate(VAR='FLOW')
df_mon <- filter(df_mon, !(TERM %in% c('Q', 'Q_AREA'))) %>%
  rbind(df_mon_flow) %>%
  mutate(TERM=ordered(as.character(TERM), levels=c('Q','Q_AREA','L','L_AREA','C')),
         VAR=ordered(as.character(VAR), levels=c('FLOW','TP','PO4','PP','TN','NH4','NO23','TSS')))

df_wyr_flow <- filter(df_wyr, VAR=='TP') %>%
  filter(TERM %in% c('Q', 'Q_AREA')) %>%
  mutate(VAR='FLOW')
df_wyr <- filter(df_wyr, !(TERM %in% c('Q', 'Q_AREA'))) %>%
  rbind(df_wyr_flow) %>%
  mutate(TERM=ordered(as.character(TERM), levels=c('Q','Q_AREA','L','L_AREA','C')),
         VAR=ordered(as.character(VAR), levels=c('FLOW','TP','PO4','PP','TN','NH4','NO23','TSS')))

df_site_flow <- filter(df_site, VAR=='TP') %>%
  filter(TERM %in% c('Q', 'Q_AREA')) %>%
  mutate(VAR='FLOW')
df_site <- filter(df_site, !(TERM %in% c('Q', 'Q_AREA'))) %>%
  rbind(df_site_flow) %>%
  mutate(TERM=ordered(as.character(TERM), levels=c('Q','Q_AREA','L','L_AREA','C')),
         VAR=ordered(as.character(VAR), levels=c('FLOW','TP','PO4','PP','TN','NH4','NO23','TSS')))

table(df_day$VAR, df_day$TERM)
table(df_mon$VAR, df_mon$TERM)
table(df_wyr$VAR, df_wyr$TERM)
table(df_site$VAR, df_site$TERM)

loads_df <- list('day'=df_day,
                 'mon'=df_mon,
                 'wyr'=df_wyr,
                 'site'=df_site)
loads_input <- df

# save ----
loads_df[['mon']] %>%
  write.csv(file=file.path('csv', 'loads_mon.csv'), row.names=FALSE)

loads_df[['wyr']] %>%
  write.csv(file=file.path('csv', 'loads_wyr.csv'), row.names=FALSE)

loads_df[['site']] %>%
  write.csv(file=file.path('csv', 'loads_site.csv'), row.names=FALSE)

areas %>%
  write.csv(file=file.path('csv', 'areas.csv'), row.names=FALSE)

save(loads, loads_input, loads_df, file='loads.Rdata')
