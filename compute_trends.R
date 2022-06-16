library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggmap)
library(gridExtra)
library(scales)
library(fluxr)
#library(wq) # antiquated
library(EnvStats)
library(zoo)
theme_set(theme_bw())

rm(list=ls())

term_labs <- c('Q'='Flow', 'L'='Load', 'C'='FWM Conc')
term_units <- c('C'='ppb', 'L'='kg/d', 'Q'='hm3/d')
dataset <- "POR"
load('loads.Rdata')
load('kt_sprague.Rdata')
load('prism.Rdata')


source('functions.R')

# load data ----
year_range <- 2002:2020

df_wyr <- loads_df[['wyr']] %>%
  filter(DATASET=="POR",
         TERM %in% c('Q', 'L', 'C'),
         VAR != "TSS",
         SEASON=="Annual",
         SITE_NAME %in% as.character(stn.kt_sprague$SITE_NAME),
         !(SITE_NAME %in% c('SF_Ivory', 'NF_Ivory'))) %>%
  select(-DATASET) %>%
  spread(TERM, VALUE) %>%
  mutate(DATE_NUM=decimal_date(DATE),
         YEAR=year(DATE)) %>%
  droplevels

df_wyr.flow <- filter(df_wyr, VAR=='FLOW') %>%
  select(SITE_NAME, WYEAR, Q)
df_wyr <- filter(df_wyr, VAR!='FLOW') %>%
  select(-Q) %>%
  left_join(df_wyr.flow, by=c('SITE_NAME', 'WYEAR'))


df_mon <- loads_df[['mon']] %>%
  filter(DATASET=="POR",
         TERM %in% c('Q', 'L', 'C'),
         VAR != "TSS",
         SITE_NAME %in% as.character(stn.kt_sprague$SITE_NAME),
         !(SITE_NAME %in% c('SF_Ivory', 'NF_Ivory'))) %>%
  select(-DATASET) %>%
  mutate(N_DAY=days_in_month(MONTHYEAR)) %>%
  spread(TERM, VALUE) %>%
  dplyr::rename(DATE=MONTHYEAR) %>%
  mutate(DATE_NUM=decimal_date(DATE),
         YEAR=year(DATE)) %>%
  droplevels

df_mon.flow <- filter(df_mon, VAR=='FLOW') %>%
  select(SITE_NAME, DATE, Q)
df_mon <- filter(df_mon, VAR!='FLOW') %>%
  select(-Q) %>%
  left_join(df_mon.flow, by=c('SITE_NAME', 'DATE'))

# trend functions ----

# seasonal kendall
# with rkt package, the data do not need to be in time series format. by leaving dates as a vector, the package can account for 'non-regular sampling dates' within the data


kendallTrendTest <- purrr::possibly(EnvStats::kendallTrendTest, NULL)
kendallSeasonalTrendTest <- purrr::possibly(EnvStats::kendallSeasonalTrendTest, NULL)

trend.sk <- function(x,months,month_label,years,value_var,water_year=TRUE,log_trans=TRUE){

  if (water_year==TRUE) {
    x <- subset(x, MONTH %in% months & WYEAR %in% years)
  } else {
    x <- subset(x, MONTH %in% months & YEAR %in% years)
  }


 # if (log_trans) {
#    z <- zoo(log10(x[[value_var]]), x[['DATE']]) # this part of the function is to make the dates consistent with ts (time series), #which we don't need because the updated rkt function relies on decimal dates instead of time series data.
#  } else {
#    z <- zoo(x[[value_var]], x[['DATE']])
#  }

  if (log_trans) {
    x[[value_var]] <- log10(x[[value_var]]) # convert to a log transform of the value_var
  } else {
    x[[value_var]] <- x[[value_var]]
  }


  # sk <- rkt(date=x[['DATE_NUM']],y=x[[value_var]],block=x[['MONTH']])
  sk <- kendallSeasonalTrendTest(x[[value_var]], season = x[['MONTH']], year = x[['WYEAR']])

  slope <- sk$estimate[["slope"]]
  # intercept <- sk$estimate[["intercept"]]
  intercept = median(x[[value_var]]) - slope * median(x[['WYEAR']])
  pval <- sk$p.value[["z (Trend)"]]

  if (log_trans) {
    slope.pct <- 10^slope - 1
  } else {
    slope.pct <- slope / mean(x[[value_var]])
  }

  if (is.null(pval)) {
    sig <- "p>0.10"
  } else {
    sig <- cut(pval, breaks=c(0,0.05,0.1,1), labels=c("p<0.05","0.05<p<0.10","p>0.10"))
  }

  return(data.frame(
    TERM=value_var,
    LOG=log_trans,
    YEAR_SPAN=paste(min(years),max(years),sep='-'),
    MONTH_LABEL=month_label,
    METHOD='SeasonalKendall',
    INTERCEPT=intercept,
    MEAN.VAL=mean(x[[value_var]],na.rm=T),
    MEAN.TIME=mean(x[['DATE_NUM']]),
    SLOPE=slope,
    SLOPE.PCT=slope.pct,
    PVAL=pval,
    SIGNIF=sig,
    DIRECTION=ordered(ifelse(slope>0, 'Increasing', 'Decreasing'), levels=c('Increasing', 'Decreasing'))
  ))
}

trend.sk(x=filter(df_mon, SITE_NAME=="Power", VAR=="TP"),
             months=1:12,
             month_label='Annual',
             years=2002:2020,
             value_var='C',
             water_year=TRUE,
             log_trans=TRUE)


# mann kendall
trend.mk <- function(x, value_var, years, month_label, log_trans=FALSE, water_year=TRUE) {
  if (water_year==TRUE) {
    x <- subset(x, WYEAR %in% years)
    start_yr <- min(x[['WYEAR']])
  } else {
    x <- subset(x, YEAR %in% years)
    start_yr <- min(x[['YEAR']])
  }

 # if (log_trans) { # the code doesn't actually use this, it was to convert to a time series object and the rkt package requires a decimal date, not time series
 #   z <- ts(log10(x[[value_var]]), start=start_yr, freq=1)
#  } else {
#    z <- ts(x[[value_var]], start=start_yr, freq=1)
#  }

  if (log_trans) { # but i will do a log transform on the dataframe
    x[[value_var]] <- log10(x[[value_var]])
  } else {
    x[[value_var]] <- x[[value_var]]
  }

  # sk <- rkt(date=x[['DATE_NUM']],y=x[[value_var]])
  sk <- kendallTrendTest(x[[value_var]], x = x[['WYEAR']])

  slope <- sk$estimate[['slope']]
  # intercept <- sk$estimate[["intercept"]]
  intercept = median(x[[value_var]]) - slope * median(x[['WYEAR']])
  pval <- sk$p.value[['z']]

  if (log_trans) {
    slope.pct <- 10^slope-1
  } else {
    slope.pct <- slope / mean(x[[value_var]])
  }

  if (is.null(pval)) {
    sig <- "p>0.10"
  } else {
    sig <- cut(pval, breaks=c(0,0.05,0.1,1), labels=c("p<0.05","0.05<p<0.10","p>0.10"))
  }
  # intercept <- median(x[[value_var]] - slope*time(x[[value_var]]), na.rm=T) # is this correct? check w jeff

  data.frame(TERM=value_var,
             LOG=log_trans,
             YEAR_SPAN=paste(min(years),max(years),sep='-'),
             MONTH_LABEL=month_label,
             METHOD='MannKendall',
             MEAN.VAL=mean(x[[value_var]],na.rm=T),
             MEAN.TIME=mean(x[['DATE_NUM']]),
             INTERCEPT=intercept,
             SLOPE=slope,
             SLOPE.PCT=slope.pct,
             PVAL=pval,
             SIGNIF=sig,
             DIRECTION=ordered(ifelse(slope>0, 'Increasing', 'Decreasing'), levels=c('Increasing', 'Decreasing')))
}

trend.mk(x=filter(df_wyr, SITE_NAME=="Power", VAR=="TP"),
          value_var='C',
          month_label='Annual',
          years=2002:2014,
          log_trans=TRUE,
          water_year=TRUE)

trend.lm <- function(x, value_var, years, month_label, log_trans=FALSE, water_year=TRUE) {
  if (water_year==TRUE) {
    x <- subset(x, WYEAR %in% years)
    mean_time <- mean(x[['WYEAR']])
    form <- as.formula(paste(value_var, 'WYEAR', sep=' ~ '))
  } else {
    x <- subset(x, YEAR %in% years)
    mean_time <- mean(x[['YEAR']])
    form <- as.formula(paste(value_var, 'YEAR', sep=' ~ '))
  }

  if (log_trans) {
    x[[value_var]] <- log10(x[[value_var]])
  }

  fit <- lm(form, data=x)

  intercept <- coef(fit)[[1]]
  slope <- coef(fit)[[2]]
  if (log_trans) {
    slope.pct <- 10^slope-1
  } else {
    slope.pct <- slope/mean(x[[value_var]])
  }

  pval <- summary(fit)$coeff['WYEAR', 'Pr(>|t|)']
  sig <- cut(pval, breaks=c(0,0.05,0.1,1), labels=c("p<0.05","0.05<p<0.10","p>0.10"))
  if (is.na(sig)) sig <- "p>0.10"

  data.frame(TERM=value_var,
             LOG=log_trans,
             YEAR_SPAN=paste(min(years),max(years),sep='-'),
             MONTH_LABEL=month_label,
             METHOD='LinearRegression',
             MEAN.VAL=mean(x[[value_var]]),
             MEAN.TIME=mean_time,
             INTERCEPT=intercept,
             SLOPE=slope,
             SLOPE.PCT=slope.pct,
             PVAL=pval,
             SIGNIF=sig,
             DIRECTION=ordered(ifelse(slope>0, 'Increasing', 'Decreasing'), levels=c('Increasing', 'Decreasing')))
}

trend.batch <- function(x_mon, x_wyr, years, log_trans=FALSE, water_year=TRUE) {
  x.site_name <- unique(x_mon$SITE_NAME)
  x.var <- unique(x_mon$VAR)
  stopifnot(length(x.site_name)==1)
  stopifnot(length(x.var)==1)

  batch.mon <- function(x, value_var) {
    df.seasonal <- trend.sk(x, value_var=value_var,
                            months=1:12, month_label='All Months',
                            years=years, log_trans=log_trans, water_year=water_year)
    df.4_9 <- trend.sk(x, value_var=value_var,
                          months=4:9, month_label='Apr-Sep',
                          years=years, log_trans=log_trans, water_year=water_year)
    df.10_3 <- trend.sk(x, value_var=value_var,
                          months=c(1:3, 10:12), month_label='Oct-Mar',
                          years=years, log_trans=log_trans, water_year=water_year)
    df.fall <- trend.sk(x, value_var=value_var,
                          months=c(10:12), month_label='Oct-Dec',
                          years=years, log_trans=log_trans, water_year=water_year)
    df.winter <- trend.sk(x, value_var=value_var,
                        months=1:3, month_label='Jan-Mar',
                        years=years, log_trans=log_trans, water_year=water_year)
    df.spring <- trend.sk(x, value_var=value_var,
                          months=4:6, month_label='Apr-Jun',
                          years=years, log_trans=log_trans, water_year=water_year)
    df.summer <- trend.sk(x, value_var=value_var,
                          months=7:9, month_label='Jul-Sep',
                          years=years, log_trans=log_trans, water_year=water_year)
    df <- bind_rows(df.seasonal, df.fall, df.winter, df.spring, df.summer, df.4_9, df.10_3)

    for (m in 1:12) {
      df.m <- trend.mk(x=filter(x, MONTH == m), value_var=value_var,
                       years=years, month_label=as.character(m),
                       log_trans=log_trans, water_year=water_year)

      df <- bind_rows(df, df.m)
    }

    df
  }

  t_mon_Q <- batch.mon(x_mon, value_var='Q')
  t_mon_L <- batch.mon(x_mon, value_var='L')
  t_mon_C <- batch.mon(x_mon, value_var='C')
  t_mon <- rbind(t_mon_Q, t_mon_L, t_mon_C)

  t_wyr_mk_Q <- trend.mk(x_wyr, value_var='Q', years=years, log_trans=log_trans, month_label='Annual-MK')
  t_wyr_mk_L <- trend.mk(x_wyr, value_var='L', years=years, log_trans=log_trans, month_label='Annual-MK')
  t_wyr_mk_C <- trend.mk(x_wyr, value_var='C', years=years, log_trans=log_trans, month_label='Annual-MK')
  t_wyr_mk <- rbind(t_wyr_mk_Q, t_wyr_mk_L, t_wyr_mk_C)

  t_wyr_lm_Q <- trend.lm(x_wyr, value_var='Q', years=years, log_trans=log_trans, month_label='Annual-Reg')
  t_wyr_lm_L <- trend.lm(x_wyr, value_var='L', years=years, log_trans=log_trans, month_label='Annual-Reg')
  t_wyr_lm_C <- trend.lm(x_wyr, value_var='C', years=years, log_trans=log_trans, month_label='Annual-Reg')
  t_wyr_lm <- rbind(t_wyr_lm_Q, t_wyr_lm_L, t_wyr_lm_C)

  t_wyr <- rbind(t_wyr_mk, t_wyr_lm)

  t_all <- rbind(t_mon, t_wyr)

  t_all <- mutate(t_all,
                  MONTH_LABEL=ordered(MONTH_LABEL,
                                      levels=c(as.character(10:12), as.character(1:9),
                                               'Oct-Dec','Jan-Mar','Apr-Jun','Jul-Sep',
                                               'Oct-Mar','Apr-Sep','All Months',
                                               'Annual-MK','Annual-Reg')),
                  TERM=ordered(TERM, levels=names(term_labs)))
  t_all
}

# compute trends ----
cat('Computing trend analysis...\n\n')
trends <- lapply(as.character(unique(df_mon$VAR)), function(variable) {
  cat('..', variable, '\n')
  lapply(as.character(levels(df_mon$SITE_NAME)), function(site) {
    cat('....', site, '\n')
    # year_range <- 2002:2012
    x <- trend.batch(x_mon=filter(df_mon, SITE_NAME==site, VAR==variable),
                     x_wyr=filter(df_wyr, SITE_NAME==site, VAR==variable),
                     years=year_range,
                     log_trans=TRUE,
                     water_year=TRUE)
    x$SITE_NAME <- site
    x$VAR <- variable
    x
  }) %>%
    bind_rows
}) %>%
  bind_rows %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=levels(stn.kt_sprague$SITE_NAME)),
         VAR=ordered(VAR, levels=levels(df_mon$VAR)),
         SIGNIF=ordered(as.character(SIGNIF), levels=c("p<0.05","0.05<p<0.10","p>0.10"))) %>%
  droplevels

trends <- trends %>%
  mutate(LABEL=ifelse(SIGNIF=="p>0.10", "Not Significant",
                      ifelse(SIGNIF=="p<0.05",
                             ifelse(DIRECTION=="Increasing",
                                    "Increasing (p<0.05)",
                                    "Decreasing (p<0.05)"),
                             ifelse(DIRECTION=="Increasing",
                                    "Increasing (0.05<p<0.1)",
                                    "Decreasing (0.05<p<0.1)"))),
         LABEL=ordered(LABEL, levels=c("Increasing (p<0.05)",
                                       "Increasing (0.05<p<0.1)",
                                       "Not Significant",
                                       "Decreasing (0.05<p<0.1)",
                                       "Decreasing (p<0.05)")))



# precip trends ----


prism.mon <- prism_subbasin %>%
  select(-geometry) %>%
  mutate(DATE=MONTHYEAR,
         VAR='PRCP',
         N.DAY=as.numeric(difftime(DATE+months(1), DATE, units='days')),
         YEAR=year(DATE),
         MONTH=month(DATE),
         DATE_NUM=decimal_date(DATE),
         PRCP=PRCP/N.DAY,PRCP=as.numeric(PRCP)) %>% # mean mm/day
  filter(WYEAR>=min(year_range), WYEAR<=max(year_range),
         SITE_NAME %in% levels(df_mon$SITE_NAME)) %>%
  droplevels
prism.wyr <- dplyr::group_by(prism.mon, SITE_NAME, WYEAR) %>%
  dplyr::summarise(
    DATE = min(DATE),
    PRCP=sum(PRCP*N.DAY)/sum(N.DAY)
  ) %>%  # mm/day
  ungroup() %>%
  mutate(DATE_NUM = decimal_date(DATE))

trend.batch.prcp <- function(x.mon, x.wyr, years, log_trans=TRUE, water_year=TRUE) {
  x <- x.mon
  #  mutate(MONTH=month(MONTHYEAR))

  value_var <- 'PRCP'

  df.seasonal <- trend.sk(x, value_var,
                          months=1:12, month_label='All Months',
                          years=years, log_trans=log_trans, water_year=water_year)
  df.4_9 <- trend.sk(x, value_var,
                     months=4:9, month_label='Apr-Sep',
                     years=years, log_trans=log_trans, water_year=water_year)
  df.10_3 <- trend.sk(x, value_var,
                      months=c(1:3, 10:12), month_label='Oct-Mar',
                      years=years, log_trans=log_trans, water_year=water_year)
  df.fall <- trend.sk(x, value_var,
                      months=c(10:12), month_label='Oct-Dec',
                      years=years, log_trans=log_trans, water_year=water_year)
  df.winter <- trend.sk(x, value_var,
                        months=1:3, month_label='Jan-Mar',
                        years=years, log_trans=log_trans, water_year=water_year)
  df.spring <- trend.sk(x, value_var,
                        months=4:6, month_label='Apr-Jun',
                        years=years, log_trans=log_trans, water_year=water_year)
  df.summer <- trend.sk(x, value_var,
                        months=7:9, month_label='Jul-Sep',
                        years=years, log_trans=log_trans, water_year=water_year)
  df <- rbind(df.seasonal, df.fall, df.winter, df.spring, df.summer, df.4_9, df.10_3)

  for (m in 1:12) {
    df.m <- trend.mk(x=filter(x, MONTH == m), value_var,
                     years=years, month_label=as.character(m),
                     log_trans=log_trans, water_year=water_year)
    df <- rbind(df, df.m)
  }



  df.mk <- trend.mk(x=x.wyr, value_var,
                    years=years, month_label='Annual-MK',
                    log_trans=log_trans, water_year=water_year)

  df.lm <- trend.lm(x=x.wyr, value_var,
                    years=years, month_label='Annual-Reg',
                    log_trans=log_trans, water_year=water_year)

  df <- rbind(df, df.mk, df.lm)
  df <- mutate(df,
               MONTH_LABEL=ordered(MONTH_LABEL,
                                   levels=c(as.character(10:12), as.character(1:9),
                                            'Oct-Dec','Jan-Mar','Apr-Jun','Jul-Sep',
                                            'Oct-Mar','Apr-Sep','All Months',
                                            'Annual-MK','Annual-Reg')),
               TERM=ordered(TERM, levels=names(term_labs)))

  df
}

## test test

trend.batch.prcp(x.mon=prism.mon %>% filter(SITE_NAME=="SF"), # Godowa/NF/SF produces the two equal dates same block error
                 x.wyr=prism.wyr %>% filter(SITE_NAME=="SF"),
                 years=year_range, log_trans=FALSE, water_year=TRUE)

trend.batch.prcp.wut <- function(x.mon, years, log_trans=TRUE, water_year=TRUE) {
 # x <- x.mon
  value_var <- 'PRCP'

  this <- trend.sk(x.mon, value_var,
                   months=1:12, month_label='All Months',
                   years=years, log_trans=log_trans, water_year=water_year)

  return(this)
}


trend.sk(prism.mon %>% filter(SITE_NAME=="Power"), value_var='PRCP',
         months=1:12, month_label='All Months',
         years=year_range, log_trans=TRUE, water_year=TRUE)

trend.batch.prcp.wut(x.mon=prism.mon %>% filter(SITE_NAME=="Power"),
                     years=year_range,
                     log_trans=TRUE,
                     water_year=TRUE)

## end test




cat('Computing precip trends...\n')
trend.prcp <- lapply(levels(prism.mon$SITE_NAME), function(site) {
  df <- trend.batch.prcp(x.mon=prism.mon %>% filter(SITE_NAME==site),
                         x.wyr=prism.wyr %>% filter(SITE_NAME==site),
                         years=year_range,
                         log_trans=FALSE, # was originally noted as FALSE, won't run with this as FALSE, check with Jeff regarding log transforms and structure in trend.sk, trend.mk, and trend.lm
                         water_year=TRUE)

  df$SITE_NAME <- site
  df
}) %>%
  bind_rows() %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=levels(stn.kt_sprague$SITE_NAME)),
         TERM="P",
         VAR="PRECIP") %>%
  droplevels()

trend.prcp <- trend.prcp %>%
  mutate(LABEL=ifelse(SIGNIF=="p>0.10", "Not Significant",
                      ifelse(SIGNIF=="p<0.05",
                             ifelse(DIRECTION=="Increasing",
                                    "Increasing (p<0.05)",
                                    "Decreasing (p<0.05)"),
                             ifelse(DIRECTION=="Increasing",
                                    "Increasing (0.05<p<0.1)",
                                    "Decreasing (0.05<p<0.1)"))),
         LABEL=ordered(LABEL, levels=c("Increasing (p<0.05)",
                                       "Increasing (0.05<p<0.1)",
                                       "Not Significant",
                                       "Decreasing (0.05<p<0.1)",
                                       "Decreasing (p<0.05)")))

# save trends ----
cat('Saving trend results to trends.Rdata...\n')
rbind(trends, trend.prcp) %>%
  saveRDS(file='trends.Rdata')
cat('Saving trend results to csv/trends.csv...\n')
rbind(trends, trend.prcp) %>%
  write.csv(file=file.path('csv', 'trends.csv'), row.names=FALSE)

# plot functions ----
plot_dot_season_flow <- function(only4=FALSE, log_trans=TRUE) {
  x.trend <- filter(trends, VAR=='TP', TERM=='Q', MONTH_LABEL %in% c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep', 'Oct-Mar', 'Apr-Sep'), LOG==log_trans) %>%
    mutate(VAR='Flow',
           MONTH_LABEL=ordered(as.character(MONTH_LABEL), levels=c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep', 'Oct-Mar', 'Apr-Sep')),
           SITE_NAME=ordered(as.character(SITE_NAME), levels=rev(levels(SITE_NAME))))

  title <- paste0('Seasonal Kendall Trend Slopes and Significance\n',
                  'Period: WY', min(year_range), '-', max(year_range), ' | Term: Flow')

  p.4 <- filter(x.trend, MONTH_LABEL %in% c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep')) %>%
    ggplot(aes(SLOPE.PCT, SITE_NAME)) +
    geom_segment(mapping=aes(x=0, xend=SLOPE.PCT, y=SITE_NAME, yend=SITE_NAME)) +
    geom_point(mapping=aes(fill=LABEL), color='black', shape=21, size=4) +
    geom_vline(xintercept=0) +
    scale_fill_manual('',
                      values=c("Increasing (p<0.05)"='orangered',
                               "Increasing (0.05<p<0.1)"="#FFA895",
                               "Not Significant"='white',
                               "Decreasing (0.05<p<0.1)"="#AAC4DB",
                               "Decreasing (p<0.05)"="steelblue"),
                      drop=FALSE) +
    scale_x_continuous(labels=scales::percent_format(accuracy = 1)) +
    labs(x='Trend Slope (%/yr)', y='') +
    facet_grid(VAR~MONTH_LABEL) +
    theme(strip.background=element_blank())
  p.2 <- filter(x.trend, MONTH_LABEL %in% c('All Months', 'Oct-Mar', 'Apr-Sep')) %>%
    ggplot(aes(SLOPE.PCT, SITE_NAME)) +
    geom_segment(mapping=aes(x=0, xend=SLOPE.PCT, y=SITE_NAME, yend=SITE_NAME)) +
    geom_point(mapping=aes(fill=LABEL), color='black', shape=21, size=4) +
    geom_vline(xintercept=0) +
    scale_fill_manual('',
                      values=c("Increasing (p<0.05)"='orangered',
                               "Increasing (0.05<p<0.1)"="#FFA895",
                               "Not Significant"='white',
                               "Decreasing (0.05<p<0.1)"="#AAC4DB",
                               "Decreasing (p<0.05)"="steelblue"),
                      drop=FALSE) +
    scale_x_continuous(labels=scales::percent_format(accuracy = 1)) +
    labs(x='Trend Slope (%/yr)', y='') +
    facet_grid(VAR~MONTH_LABEL) +
    theme(strip.background=element_blank())

  if (only4) {
    grid.arrange(grobs=list(p.4),
                 nrow=3,
                 top=title)
  } else {

    grid.arrange(grobs=list(p.4, arrangeGrob(p.2, ncol=2, widths=c(2/3, 1/3))),
                 nrow=3,
                 top=title)
  }
}
# plot_dot_season_flow()

plot_dot_season <- function(variable, seasons=c('All Months', 'Oct-Mar', 'Apr-Sep'), log_trans=TRUE) {
  x.trend <- filter(trends, VAR==variable, MONTH_LABEL %in% seasons, LOG==log_trans) %>%
    mutate(MONTH_LABEL=ordered(as.character(MONTH_LABEL), levels=seasons),
           TERM_LABEL=plyr::revalue(TERM, term_labs),
           TERM_LABEL=ordered(TERM_LABEL, levels=unname(term_labs)),
           SITE_NAME=ordered(as.character(SITE_NAME), levels=rev(levels(SITE_NAME))))

  title <- paste0('Seasonal Kendall Trend Slopes\n',
                  'Period: WY', min(year_range), '-', max(year_range),
                  ' | Variable: ', variable)

  ggplot(x.trend, aes(SLOPE.PCT, SITE_NAME)) +
    geom_segment(mapping=aes(x=0, xend=SLOPE.PCT, y=SITE_NAME, yend=SITE_NAME)) +
    geom_point(mapping=aes(fill=LABEL), color='black', shape=21, size=4) +
    geom_vline(xintercept=0) +
    scale_fill_manual('',
                      values=c("Increasing (p<0.05)"='orangered',
                               "Increasing (0.05<p<0.1)"="#FFA895",
                               "Not Significant"='white',
                               "Decreasing (0.05<p<0.1)"="#AAC4DB",
                               "Decreasing (p<0.05)"="steelblue"),
                      drop=FALSE) +
    scale_x_continuous(labels=percent) +
    labs(x='Trend Slope (%/yr)', y='', title=title) +
    facet_grid(MONTH_LABEL~TERM_LABEL) +
    theme(strip.background=element_blank())
}
# plot_dot_season(variable='TP')
# plot_dot_season(variable='TP', seasons=c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep'))

plot_dot_term <- function(term, seasons=c('All Months', 'Oct-Mar', 'Apr-Sep'), variables=c("TP","PO4","PP","TN","NH4","NO23","TSS"), log_trans=TRUE) {
  x.trend <- filter(trends, TERM==term, MONTH_LABEL %in% seasons, LOG==log_trans, VAR %in% variables) %>%
    mutate(MONTH_LABEL=ordered(as.character(MONTH_LABEL), levels=seasons),
           TERM_LABEL=plyr::revalue(TERM, term_labs),
           TERM_LABEL=ordered(TERM_LABEL, levels=unname(term_labs)),
           VAR=ordered(VAR, levels=variables),
           SITE_NAME=ordered(as.character(SITE_NAME), levels=rev(levels(SITE_NAME))))

  title <- paste0('Seasonal Kendall Trend Slopes\n',
                  'Period: WY', min(year_range), '-', max(year_range),
                  ' | Term: ', term_labs[[term]])

  p <- ggplot(x.trend, aes(SLOPE.PCT, SITE_NAME)) +
    geom_segment(mapping=aes(x=0, xend=SLOPE.PCT, y=SITE_NAME, yend=SITE_NAME)) +
    geom_point(mapping=aes(fill=LABEL), color='black', shape=21, size=4) +
    geom_vline(xintercept=0) +
    scale_fill_manual('',
                      values=c("Increasing (p<0.05)"='orangered',
                               "Increasing (0.05<p<0.1)"="#FFA895",
                               "Not Significant"='white',
                               "Decreasing (0.05<p<0.1)"="#AAC4DB",
                               "Decreasing (p<0.05)"="steelblue"),
                      drop=FALSE) +
    scale_x_continuous(labels=percent) +
    labs(x='Trend Slope (%/yr)', y='', title=title) +
    facet_grid(VAR~MONTH_LABEL) +
    theme(strip.background=element_blank())
  print(p)
}
# plot_dot_term(term='C')
# plot_dot_term(term='C', c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep'))


#### test: where are the trend lines on the figures
# lm shows up, not seasonal or mann kendall
x.trend <- filter(trends, SITE_NAME=="Power",
                  VAR=="TP", TERM=='C', LOG==TRUE)
x.mon.trends <- filter(x.trend, METHOD=='SeasonalKendall') %>%
  select(SEASON=MONTH_LABEL, SLOPE, INTERCEPT)
x.wyr.trends <- filter(x.trend, MONTH_LABEL %in% c('Annual-MK', 'Annual-Reg'))
x.trends.all <- filter(x.trend, METHOD=='SeasonalKendall', MONTH_LABEL=='All Months') %>%
  as.list()

x.wyr <- filter(df_wyr, SITE_NAME=="Power", VAR=="TP")
x.mon <- filter(df_mon, SITE_NAME=="Power", VAR=="TP") %>%
  mutate(DDATE=decimal_date(DATE))
x.mon$SEASON <- ifelse(x.mon$MONTH %in% seq(4, 9), 'Apr-Sep', 'Oct-Mar')

n_year <- length(unique(x.wyr$WYEAR))

x.mon %>%
ggplot(aes(DDATE,C,color=SEASON))+
  geom_point(show.legend=TRUE) +
  geom_abline(data=x.mon.trends %>% filter(SEASON%in%c('Apr-Sep', 'Oct-Mar')),
              aes(intercept=INTERCEPT,slope=SLOPE,color=SEASON))

geom_abline(aes(intercept=INTERCEPT, slope=SLOPE, color=SEASON),
            data=filter(x.mon.trends, SEASON %in% c('Apr-Sep', 'Oct-Mar')), show.legend=TRUE)

p.mon.ts <-
  x.mon %>%
  ggplot() + aes_string(x='DDATE', y='C', color='SEASON') +
  geom_point(show.legend=TRUE) +
  geom_abline(aes(intercept=INTERCEPT, slope=SLOPE, color=SEASON),
              data=filter(x.mon.trends, SEASON %in% c('Apr-Sep', 'Oct-Mar')), show.legend=TRUE) +
  geom_abline(aes(intercept=INTERCEPT, slope=SLOPE, color=SEASON),
              data=filter(x.mon.trends, SEASON=='All Months'), show.legend=TRUE) +
  scale_color_manual('Season', values=c('Apr-Sep'='olivedrab3', 'Oct-Mar'='orange', 'All Months'='black')) +
  scale_x_continuous(breaks=seq(min(year_range), max(year_range),
                                by=ifelse(n_year >= 10, 2, 1))) +
  labs(x='', y=paste0('Monthly ')) +
  theme(legend.position='top')

##### end test
plot_diagnostic <- function(site_name, variable, term, log_trans=TRUE) {
  x.trend <- filter(trends, SITE_NAME==site_name,
                    VAR==variable, TERM==term, LOG==log_trans)
  x.mon.trends <- filter(x.trend, METHOD=='SeasonalKendall' | MONTH_LABEL %in% as.character(1:12)) %>%
    select(SEASON=MONTH_LABEL, SLOPE, INTERCEPT)
  x.wyr.trends <- filter(x.trend, MONTH_LABEL %in% c('Annual-MK', 'Annual-Reg'))
  x.trends.all <- filter(x.trend, METHOD=='SeasonalKendall', MONTH_LABEL=='All Months') %>%
    as.list()

  x.wyr <- filter(df_wyr, SITE_NAME==site_name, VAR==variable)
  x.mon <- filter(df_mon, SITE_NAME==site_name, VAR==variable) %>%
    mutate(DDATE=decimal_date(DATE))
  x.mon$SEASON <- ifelse(x.mon$MONTH %in% seq(4, 9), 'Apr-Sep', 'Oct-Mar')

  stopifnot(nrow(x.trend) > 0)
  if (log_trans) {
    x.mon[[term]] <- log10(x.mon[[term]])
    x.wyr[[term]] <- log10(x.wyr[[term]])
  }

  n_year <- length(unique(x.wyr$WYEAR))

  if (log_trans) {
    units <- paste0('log10(', term_units[[term]], ')')
  } else {
    units <- term_units[[term]]
  }

  ylabel <- paste0(term_labs[[term]], ' [', units, ']')

  if (term=='Q') {
    title <- paste(paste0('Period: ', 'WY', min(year_range), '-', max(year_range)),
                   paste0('Site: ', site_name),
                   paste0('Variable: ', term_labs[[term]]),
                   sep='  |  ')
  } else {
    title <- paste(paste0('Period: ', 'WY', min(year_range), '-', max(year_range)),
                   paste0('Site: ', site_name),
                   paste0('Variable: ', variable),
                   paste0('Term: ', term_labs[[term]]),
                   sep='  |  ')
  }

  p.mon.ts <- x.mon %>%
    ggplot() + aes_string(x='DDATE', y=term, color='SEASON') +
    geom_point(show.legend=TRUE) +
    geom_abline(aes(intercept=INTERCEPT, slope=SLOPE, color=SEASON),
                data=filter(x.mon.trends, SEASON %in% c('Apr-Sep', 'Oct-Mar')), show.legend=TRUE) +
    geom_abline(aes(intercept=INTERCEPT, slope=SLOPE, color=SEASON),
                data=filter(x.mon.trends, SEASON=='All Months'), show.legend=TRUE) +
    scale_color_manual('Season', values=c('Apr-Sep'='olivedrab3', 'Oct-Mar'='orange', 'All Months'='black')) +
    scale_x_continuous(breaks=seq(min(year_range), max(year_range),
                                  by=ifelse(n_year >= 10, 2, 1))) +
    labs(x='', y=paste0('Monthly ', ylabel)) +
    theme(legend.position='top')

  if (!log_trans) {
    p.mon.ts <- p.mon.ts + geom_hline(yintercept=0, alpha=0)
  }

  p.wyr.ts <- x.wyr %>%
    ggplot(aes_string(x='WYEAR', y=term)) +
    geom_point() +
    geom_abline(aes(intercept=INTERCEPT, slope=SLOPE, linetype=METHOD),
                data=x.wyr.trends, show.legend=TRUE) +
    scale_linetype_manual('Method', values=c('MannKendall'=1, 'LinearRegression'=2)) +
    scale_x_continuous(breaks=seq(min(year_range), max(year_range),
                                  by=ifelse(n_year >= 10, 2, 1))) +
    labs(x='', y=paste0('Annual ', ylabel)) +
    theme(legend.position='top')

  if (!log_trans) {
    p.wyr.ts <- p.wyr.ts + geom_hline(yintercept=0, alpha=0)
  }

  p.mon <- x.mon %>%
    mutate(MONTH=ordered(MONTH, levels=c(10:12, 1:9))) %>%
    ggplot() +
    aes_string(x='WYEAR', y=term) +
    geom_point(size=1.5, alpha = 0.5) +
    geom_abline(aes(intercept=INTERCEPT, slope=SLOPE),
                data=filter(x.mon.trends, SEASON %in% seq(1, 12)) %>%
                  dplyr::rename(MONTH=SEASON)) +
    facet_wrap(~MONTH, nrow=1) +
    scale_x_continuous(breaks=seq(min(year_range), max(year_range),
                                  by=ifelse(n_year >= 10, 4, 2))) +
    labs(x='', y=ylabel) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

  p.bar <- x.trend %>%
    mutate(MONTH_LABEL=plyr::revalue(MONTH_LABEL, c('All Months'='Annual-SK'))) %>%
    ggplot(aes(MONTH_LABEL, SLOPE.PCT, fill=LABEL)) +
    geom_bar(stat='identity') +
    geom_bar(stat='identity', color='grey30', alpha=0) +
    geom_vline(xintercept=0) +
    scale_fill_manual('',
                      values=c("Increasing (p<0.05)"='orangered',
                               "Increasing (0.05<p<0.1)"="#FFA895",
                               "Not Significant"='white',
                               "Decreasing (0.05<p<0.1)"="#AAC4DB",
                               "Decreasing (p<0.05)"="steelblue"),
                      drop=FALSE) +
    geom_vline(xintercept=c(12.5, 16.5, 18.5), color='grey70') +
    geom_hline(yintercept=0, color='grey50') +
    scale_y_continuous(labels=percent) +
    labs(x='', y='Trend Slope (%/yr)') +
    # guides(fill=guide_legend(override.aes = list(colour=NA))) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

  if (x.trends.all$PVAL < 1e-4) {
    pval <- '< 0.0001'
  } else {
    pval <- format(x.trends.all$PVAL, nsmall=4L, digits=0, scientific=FALSE)
  }

  tbl <- c('Site: '         = site_name,
           'Variable: '     = variable,
           'Term: '         = term_labs[[term]],
           'Water Years: '  = paste0('WY', min(year_range), '-', max(year_range)),
           # 'Transform: '    = if (log_trans) 'Log10' else 'None',
           ' '              = ' ',
           'Trend Results: ' = 'All Months (Seasonal Kendall)',
           'Slope: '        = paste(format(x.trends.all$SLOPE, nsmall=3L, digits=0), paste0(units, '/yr')),
           '% Slope: '      = paste(format(x.trends.all$SLOPE.PCT*100, nsmall=1L, digits=0), '%/yr'),
           'p-Value: '      = pval,
           ' ' = ' ',
           ' ' = ' ',
           ' ' = ' ')

  tblGrb <- tableGrob(data.frame(value=unname(tbl)),
                      rows=names(tbl), cols=NULL,
                      theme = ttheme_minimal(rowhead=list(fg_params=list(fontface=2L, hjust=1, fontsize=10),
                                                          padding=grid::unit(c(1, 1), 'mm')),
                                             core=list(fg_params=list(hjust=0, x=0.0, fontsize=10),
                                                       padding=grid::unit(c(1, 1),'mm'))))
  grid.arrange(grobs=list(arrangeGrob(grobs=list(p.mon.ts, p.wyr.ts),
                                      widths=c(2/3, 1/3), nrow=1),
                          p.mon,
                          arrangeGrob(grobs=list(p.bar, tblGrb),
                                      widths=c(2/3, 1/3), nrow=1,
                                      just=c('left', 'top'))),
               ncol=1,
               just=c('left', 'top'),
               heights=c(10/24, 6/24, 8/24),
               top=title)
}


plot_diagnostic(site_name='Power', variable='TP', term='L')

plot_diagnostic_prcp <- function(site_name, log_trans=TRUE) {
  term <- 'PRCP'
  x.trend <- filter(trend.prcp, SITE_NAME==site_name)
  x.mon.trends <- filter(x.trend, METHOD=='SeasonalKendall' | MONTH_LABEL %in% as.character(1:12)) %>%
    select(SEASON=MONTH_LABEL, SLOPE, INTERCEPT)
  x.wyr.trends <- filter(x.trend, MONTH_LABEL %in% c('Annual-MK', 'Annual-Reg'))
  x.trends.all <- filter(x.trend, METHOD=='SeasonalKendall', MONTH_LABEL=='All Months') %>%
    as.list()

  x.wyr <- filter(prism.wyr, SITE_NAME==site_name)
  x.mon <- filter(prism.mon, SITE_NAME==site_name) %>%
    mutate(MONTH=month(MONTHYEAR),
           SEASON=ifelse(MONTH %in% seq(4, 9),
                         'Apr-Sep', 'Oct-Mar'),
           DDATE=decimal_date(DATE))
  stopifnot(nrow(x.trend) > 0)
  if (log_trans) {
    x.mon[[term]] <- log10(x.mon[[term]])
    x.wyr[[term]] <- log10(x.wyr[[term]])
  }

  n_year <- length(unique(x.wyr$WYEAR))

  if (log_trans) {
    units <- paste0('log10(', 'mm/d', ')')
  } else {
    units <- 'mm/d'
  }

  ylabel <- paste0('Precip', ' [', units, ']')

  title <- paste0('Period: WY', min(year_range), '-', max(year_range),
                  '  |  Site: ', site_name,
                  '  |  Variable: Precipitation')

  p.mon.ts <- x.mon %>%
    ggplot() + aes_string(x='DDATE', y=term, color='SEASON') +
    geom_point(show.legend=TRUE) +
    geom_abline(aes(intercept=INTERCEPT, slope=SLOPE, color=SEASON),
                data=filter(x.mon.trends, SEASON %in% c('Apr-Sep', 'Oct-Mar')), show.legend=TRUE) +
    geom_abline(aes(intercept=INTERCEPT, slope=SLOPE, color=SEASON),
                data=filter(x.mon.trends, SEASON %in% c('All Months')), show.legend=TRUE) +
    scale_color_manual('Season', values=c('Apr-Sep'='olivedrab3', 'Oct-Mar'='orange', 'All Months'='black')) +
    scale_x_continuous(breaks=seq(min(year_range), max(year_range),
                                  by=ifelse(n_year >= 10, 2, 1))) +
    labs(x='', y=paste0('Monthly ', ylabel)) +
    theme(legend.position='top')

  if (!log_trans) {
    p.mon.ts <- p.mon.ts + geom_hline(yintercept=0, alpha=0)
  }

  p.wyr.ts <- x.wyr %>%
    ggplot(aes_string(x='WYEAR', y=term)) +
    geom_point() +
    geom_abline(aes(intercept=INTERCEPT, slope=SLOPE, linetype=METHOD),
                data=x.wyr.trends, show.legend=TRUE) +
    scale_linetype_manual('Method', values=c('MannKendall'=1, 'LinearRegression'=2)) +
    scale_x_continuous(breaks=seq(min(year_range), max(year_range),
                                  by=ifelse(n_year >= 10, 2, 1))) +
    labs(x='', y=paste0('Annual ', ylabel)) +
    theme(legend.position='top')

  if (!log_trans) {
    p.wyr.ts <- p.wyr.ts + geom_hline(yintercept=0, alpha=0)
  }

  p.mon <- x.mon %>%
    mutate(MONTH=ordered(MONTH, levels=c(10:12, 1:9))) %>%
    ggplot() + aes_string(x='WYEAR', y=term) +
    geom_point(size=1.5, alpha = 0.5) +
    geom_abline(aes(intercept=INTERCEPT, slope=SLOPE),
                data=filter(x.mon.trends, SEASON %in% seq(1, 12)) %>% dplyr::rename(MONTH=SEASON) %>% droplevels) +
    facet_wrap(~MONTH, nrow=1) +
    scale_x_continuous(breaks=seq(min(year_range), max(year_range),
                                  by=ifelse(n_year >= 10, 4, 2))) +
    labs(x='', y=ylabel) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

  p.bar <- x.trend %>%
    mutate(MONTH_LABEL=plyr::revalue(MONTH_LABEL, c('All Months'='Annual-SK'))) %>%
    ggplot(aes(MONTH_LABEL, SLOPE.PCT, fill=LABEL)) +
    geom_bar(stat='identity') +
    geom_bar(stat='identity', color='grey30', alpha=0) +
    geom_vline(xintercept=0) +
    scale_fill_manual('',
                      values=c("Increasing (p<0.05)"='orangered',
                               "Increasing (0.05<p<0.1)"="#FFA895",
                               "Not Significant"='white',
                               "Decreasing (0.05<p<0.1)"="#AAC4DB",
                               "Decreasing (p<0.05)"="steelblue"),
                      drop=FALSE) +
    geom_vline(xintercept=c(12.5, 16.5, 18.5), color='grey70') +
    geom_hline(yintercept=0, color='grey50') +
    scale_y_continuous(labels=percent) +
    labs(x='', y='Trend Slope (%/yr)') +
    # guides(fill=guide_legend(override.aes = list(colour=NA))) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

  if (x.trends.all$PVAL < 1e-4) {
    pval <- '< 0.0001'
  } else {
    pval <- format(x.trends.all$PVAL, nsmall=4L, digits=0, scientific=FALSE)
  }

  tbl <- c('Dataset: '      = 'PRISM',
           'Site: '         = site_name,
           'Variable: '     = 'Precip',
           'Water Years: '  = paste0('WY', min(year_range), '-', max(year_range)),
           'Transform: '    = if (log_trans) 'Log10' else 'None',
           ' '              = ' ',
           'Trend Results: ' = 'All Months (Seasonal Kendall)',
           'Slope: '        = paste(format(x.trends.all$SLOPE, nsmall=3L, digits=0), paste0(units, '/yr')),
           '% Slope: '      = paste(format(x.trends.all$SLOPE.PCT*100, nsmall=1L, digits=0), '%/yr'),
           'p-Value: '      = pval,
           ' ' = ' ',
           ' ' = ' ',
           ' ' = ' ')

  tblGrb <- tableGrob(data.frame(value=unname(tbl)),
                      rows=names(tbl), cols=NULL,
                      theme = ttheme_minimal(rowhead=list(fg_params=list(fontface=2L, hjust=1, fontsize=10),
                                                          padding=grid::unit(c(1, 1), 'mm')),
                                             core=list(fg_params=list(hjust=0, x=0.0, fontsize=10),
                                                       padding=grid::unit(c(1, 1),'mm'))))
  grid.arrange(grobs=list(arrangeGrob(grobs=list(p.mon.ts, p.wyr.ts),
                                      widths=c(2/3, 1/3), nrow=1),
                          p.mon,
                          arrangeGrob(grobs=list(p.bar, tblGrb),
                                      widths=c(2/3, 1/3), nrow=1)),
               ncol=1,
               heights=c(10/24, 6/24, 8/24),
               top=title)
}
# plot_diagnostic_prcp(site_name='Power', log_trans=FALSE)

plot_dot_precip <- function(seasons=c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep')) {
  x.trend <- filter(trend.prcp, MONTH_LABEL %in% seasons) %>%
    mutate(MONTH_LABEL=ordered(as.character(MONTH_LABEL), levels=seasons),
           SITE_NAME=ordered(as.character(SITE_NAME), levels=rev(levels(SITE_NAME))))

  p <- ggplot(x.trend, aes(SLOPE.PCT, SITE_NAME)) +
    geom_segment(mapping=aes(x=0, xend=SLOPE.PCT, y=SITE_NAME, yend=SITE_NAME)) +
    geom_point(mapping=aes(fill=LABEL), color='black', shape=21, size=4) +
    geom_vline(xintercept=0) +
    scale_fill_manual('',
                      values=c("Increasing (p<0.05)"='orangered',
                               "Increasing (0.05<p<0.1)"="#FFA895",
                               "Not Significant"='white',
                               "Decreasing (0.05<p<0.1)"="#AAC4DB",
                               "Decreasing (p<0.05)"="steelblue"),
                      drop=FALSE) +
    scale_x_continuous(labels=percent) +
    labs(x='Trend Slope (%/yr)', y='') +
    facet_grid(.~MONTH_LABEL) +
    theme(axis.text.x=element_text(size=8))
  p
}
# plot_dot_precip()


# pdf ----
filename <- file.path('pdf', tolower(dataset), paste0('trends-summary-term.pdf'))
cat('Printing:', filename, '\n')
pdf(filename, width=11, height=8.5, useDingbats = FALSE)
plot_dot_season_flow(only4=TRUE)
plot_dot_term(term='L', seasons=c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep'))
plot_dot_term(term='C', seasons=c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep'))
dev.off()

filename <- file.path('pdf', tolower(dataset), paste0('trends-summary-variable.pdf'))
cat('Printing:', filename, '\n')
pdf(filename, width=11, height=8.5)
for (variable in as.character(unique(df_mon$VAR))) {
  print(plot_dot_season(variable=variable, seasons=c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep'), log_trans=TRUE))
}
dev.off()

if (!file.exists(file.path('pdf', tolower(dataset), 'trends'))) {
  dir.create(file.path('pdf', tolower(dataset), 'trends'))
}
dataset <- 'POR'
for (variable in as.character(unique(df_mon$VAR))) {
  filename <- file.path('pdf', tolower(dataset), 'trends', paste0('trends-', tolower(variable), '.pdf'))
  cat('Printing:', filename, '\n')
  cat('..', variable, '\n')
  pdf(filename, width=11, height=8.5)
  for (site_name in as.character(unique(df_mon$SITE_NAME))) {
    cat('....', site_name, '\n')
    for (term in c('C', 'L')) {
      cat('......', term, '\n')
      plot_diagnostic(site_name=site_name, variable=variable, term=term, log_trans=TRUE)
      Sys.sleep(1)
    }
  }
  dev.off()
  Sys.sleep(1)
}

filename <- file.path('pdf', tolower(dataset), 'trends', paste0('trends-', 'flow', '.pdf'))
cat('Printing:', filename, '\n')
cat('..', 'FLOW', '\n')
pdf(filename, width=11, height=8.5)
for (site_name in as.character(unique(df_mon$SITE_NAME))) {
  cat('....', site_name, '\n')
  for (term in c('Q')) {
    cat('......', term, '\n')
    plot_diagnostic(site_name=site_name, variable="TP", term=term, log_trans=TRUE)
    Sys.sleep(1)
  }
}
dev.off()

filename <- file.path('pdf', 'por', 'trends-summary-precip.pdf')
cat('Printing:', filename, '\n')
pdf(filename, width=11, height=8.5)
p.2 <- plot_dot_precip(seasons=c('All Months', 'Oct-Mar', 'Apr-Sep'))
p.4 <- plot_dot_precip(seasons=c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep'))
grid.arrange(grobs=list(p.2, p.4),
             nrow=2,
             top=paste0('\nSeasonal Kendall Trend Slopes\nDataset: PRISM | ',
                        'Period: WY', min(year_range), '-', max(year_range), ', Variable: Precip'))
dev.off()

filename <- file.path('pdf', 'por', 'trends', 'trends-precip.pdf')
cat('Printing:', filename, '\n')
pdf(filename, width=11, height=8.5)
for (site in unique(trend.prcp$SITE_NAME)) {
  cat('..', site, '\n')
  plot_diagnostic_prcp(site_name=site, log_trans=FALSE)
  Sys.sleep(1)
}
dev.off()

# report ----
trend_rep_c <- filter(trends,
                      TERM=='C',
                      MONTH_LABEL %in% c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep'),
                      LOG==TRUE)
trend_rep_q <- filter(trends,
                      TERM=='Q',
                      VAR=='TP',
                      MONTH_LABEL %in% c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep'),
                      LOG==TRUE) %>%
  mutate(VAR="FLOW")
trend_rep_p <- filter(trend.prcp,
                      MONTH_LABEL %in% c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep'),
                      LOG==FALSE) %>%
  mutate(VAR="PRECIP",
         TERM="P")


trend_rep_c <- trend_rep_c %>%
  mutate(MONTH_LABEL=ordered(as.character(MONTH_LABEL),
                             levels=c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep')),
         TERM_LABEL=plyr::revalue(TERM, term_labs),
         TERM_LABEL=ordered(TERM_LABEL, levels=unname(term_labs)),
         SITE_NAME=ordered(as.character(SITE_NAME), levels=rev(levels(SITE_NAME)))) %>%
  arrange(VAR) %>%
  mutate(SITE_NAME=ordered(as.character(SITE_NAME), levels=rev(levels(SITE_NAME))))

p <- ggplot(trend_rep_c, aes(SLOPE.PCT, SITE_NAME)) +
  geom_vline(xintercept=0) +
  geom_segment(mapping=aes(x=0, xend=SLOPE.PCT, y=SITE_NAME, yend=SITE_NAME)) +
  geom_point(mapping=aes(fill=LABEL), color='black', shape=21, size=4) +
  geom_vline(xintercept=0) +
  scale_fill_manual('',
                    values=c("Increasing (p<0.05)"='orangered',
                             "Increasing (0.05<p<0.1)"="#FFA895",
                             "Not Significant"='white',
                             "Decreasing (0.05<p<0.1)"="#AAC4DB",
                             "Decreasing (p<0.05)"="steelblue"),
                    drop=FALSE) +
  scale_x_continuous(labels=scales::percent) +
  labs(x='Trend Slope (%/yr)', y='') +
  facet_grid(VAR~MONTH_LABEL) +
  theme(strip.background=element_blank(),
        strip.text=element_text(face='bold'))

filename <- 'report/results-trend-summary.png'
cat('Saving report figure to:', filename, '\n')
png(filename, width=10, height=8, res=200, units='in')
print(p)
dev.off()

trend_rep_tp <- filter(trends,
                       VAR %in% c('TP', 'TN'),
                       MONTH_LABEL %in% c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep'),
                       LOG==TRUE) %>%
  rbind(trend_rep_p) %>%
  unite(VAR_TERM, VAR, TERM, remove=FALSE) %>%
  filter(VAR_TERM != 'TN_Q') %>%
  mutate(VAR_TERM = ordered(VAR_TERM, levels=c('PRECIP_P', 'TP_Q', 'TP_L', 'TP_C', 'TN_L', 'TN_C'))) %>%
  mutate(TERM_LABEL=plyr::revalue(VAR_TERM, c("PRECIP_P"="Precip", "TP_Q"="Flow",
                                              "TP_L"="TP Load", "TP_C"="TP Conc",
                                              "TN_L"="TN Load", "TN_C"="TN Conc")),
         # TERM_LABEL=ordered(TERM_LABEL, levels=c("Precip", "Flow", "TP Load", "TP Conc")),
         MONTH_LABEL=ordered(as.character(MONTH_LABEL),
                             levels=c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep')))

p <- ggplot(trend_rep_tp, aes(SLOPE.PCT, SITE_NAME)) +
  geom_vline(xintercept=0) +
  geom_segment(mapping=aes(x=0, xend=SLOPE.PCT, y=SITE_NAME, yend=SITE_NAME)) +
  geom_point(mapping=aes(fill=LABEL), color='black', shape=21, size=4) +
  geom_vline(xintercept=0) +
  scale_fill_manual('',
                    values=c("Increasing (p<0.05)"='orangered',
                             "Increasing (0.05<p<0.1)"="#FFA895",
                             "Not Significant"='white',
                             "Decreasing (0.05<p<0.1)"="#AAC4DB",
                             "Decreasing (p<0.05)"="steelblue"),
                    drop=FALSE) +
  scale_x_continuous(labels=scales::percent, breaks=seq(-0.06, 0.04, by=0.02)) +
  labs(x='Trend Slope (%/yr)', y='') +
  facet_grid(TERM_LABEL~MONTH_LABEL) +
  theme(strip.background=element_blank(),
        strip.text=element_text(face='bold'),
        axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8))

filename <- 'report/results-trend-tp-tn.png'
cat('Saving report figure to:', filename, '\n')
png(filename, width=10, height=8, res=200, units='in')
print(p)
dev.off()
