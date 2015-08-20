library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(knitr)
library(ggmap)
library(gridExtra)
library(scales)
library(fluxr)
library(wq)
library(zoo)
theme_set(theme_bw())

rm(list=ls())

term_labs <- c('C'='FWM Conc', 'L'='Load', 'Q'='Flow')
term_units <- c('C'='ppb', 'L'='kg/d', 'Q'='hm3/d')

load('loads.Rdata')
load('kt_sprague.Rdata')
load('prism.Rdata')

source('functions.R')

dataset_levels <- c('POR')


df_wyr <- loads_df[['wyr']] %>%
  filter(DATASET %in% dataset_levels,
         TERM %in% c('Q', 'L', 'C'),
         SITE_NAME %in% as.character(stn.kt_sprague$SITE_NAME)) %>%
  # compute daily mean
  mutate(N.DAY=as.numeric(difftime(as.Date(paste(WYEAR, 10, 1, sep='-')), as.Date(paste(WYEAR-1, 10, 1, sep='-')), units='days')),
         VALUE=ifelse(TERM %in% c('Q', 'L'), VALUE/N.DAY, VALUE)) %>%
  spread(TERM, VALUE) %>%
  droplevels

df_wyr.flow <- filter(df_wyr, VAR=='FLOW') %>%
  select(DATASET, SITE_NAME, WYEAR, Q)
df_wyr <- filter(df_wyr, VAR!='FLOW') %>%
  select(-Q) %>%
  left_join(df_wyr.flow, by=c('DATASET', 'SITE_NAME', 'WYEAR'))

df_mon <- loads_df[['mon']] %>%
  filter(DATASET %in% dataset_levels,
         TERM %in% c('Q', 'L', 'C'),
         SITE_NAME %in% as.character(stn.kt_sprague$SITE_NAME)) %>%
  # compute daily mean
  mutate(N.DAY=as.numeric(difftime(MONTHYEAR+months(1), MONTHYEAR, units='days')),
         VALUE=ifelse(TERM %in% c('Q', 'L'), VALUE/N.DAY, VALUE)) %>%
  spread(TERM, VALUE) %>%
  rename(DATE=MONTHYEAR) %>%
  droplevels

df_mon.flow <- filter(df_mon, VAR=='FLOW') %>%
  select(DATASET, SITE_NAME, DATE, Q)
df_mon <- filter(df_mon, VAR!='FLOW') %>%
  select(-Q) %>%
  left_join(df_mon.flow, by=c('DATASET', 'SITE_NAME', 'DATE'))

# fix negative PP loads and conc
df_mon <- mutate(df_mon,
                 LIMITED=(VAR=='PP' & L < 0.1),
                 C=ifelse(LIMITED, 1, C),
                 L=ifelse(LIMITED, C*Q, L))

dataset_sites <- lapply(dataset_levels, function(dataset) {
  x.dataset <- filter(df_wyr, DATASET==dataset)
  intersect(unique(as.character(x.dataset$SITE_NAME)), as.character(stn.kt_sprague$SITE_NAME))
})
names(dataset_sites) <- dataset_levels

dataset_vars <- lapply(dataset_levels, function(dataset) {
  x.dataset <- filter(df_wyr, DATASET==dataset)
  unique(as.character(x.dataset$VAR))
})
names(dataset_vars) <- dataset_levels

dataset_years <- lapply(dataset_levels, function(dataset) {
  x.dataset <- filter(df_wyr, DATASET==dataset)
  c(min(x.dataset$WYEAR), max(x.dataset$WYEAR))
})
names(dataset_years) <- dataset_levels

dataset_labels <- lapply(names(dataset_years), function(dataset) {
  if (dataset=='RECENT') {
    dataset_label <- 'Recent'
  } else {
    dataset_label <- dataset
  }
  dataset_label
})
names(dataset_labels) <- dataset_levels

period_label <- function(dataset, variable) {
  if (dataset=='RECENT' & variable=='TSS') {
    label <- 'WY2011-WY2014'
  } else {
    label <- paste(paste0('WY', dataset_years[[dataset]]), collapse='-')
  }
  label
}

trend.sk <- function(x, value_var, months, month_label, years, log_trans=TRUE, water_year=TRUE) {
  # months <- 1:4
  # month_label <- 'Annual'
  # years <- 1992:2010
  # water_year <- TRUE

  if (water_year==TRUE) {
    x <- subset(x, MONTH %in% months & WYEAR %in% years)
  } else {
    x <- subset(x, MONTH %in% months & YEAR %in% years)
  }
  if (log_trans) {
    z <- zoo(log10(x[[value_var]]), x[['DATE']])
  } else {
    z <- zoo(x[[value_var]], x[['DATE']])
  }

  z <- aggregate(z, as.yearmon, mean, na.rm=TRUE)
  z <- as.ts(z)
  z <- window(z, start=c(start(z)[1],1), end=c(end(z)[1], frequency(z)), extend=TRUE)

  if (length(months)>1) {
    t <- z[cycle(z) %in% months]
    t <- ts(t, start=c(start(z)[1],1), end=c(end(z)[1],length(months)), frequency=length(months))
  } else {
    t <- z
  }

  sk <- seaKen(t)
#   slope <- ifelse(abs(sk$sen.slope) < 1E-5, 0, sk$sen.slope)
  slope <- sk$sen.slope
  if (log_trans) {
    slope.pct <- slope.pct <- 10^slope-1
  } else {
    slope.pct <- sk$sen.slope.pct/100
  }

  intercept <- median(t - slope*time(t), na.rm=T)
  pval <- sk$p.value
  sig <- cut(pval, breaks=c(0,0.05,0.1,1), labels=c("p<0.05","0.05<p<0.10","p>0.10"))
  if (is.na(sig)) sig <- "p>0.10"

  return(data.frame(TERM=value_var,
                    LOG=log_trans,
                    YEAR_SPAN=paste(min(years),max(years),sep='-'),
                    MONTH_LABEL=month_label,
                    METHOD='SeasonalKendall',
                    MEAN.VAL=mean(t, na.rm=T),
                    MEAN.TIME=decimal_date(mean(x$DATE)),
                    INTERCEPT=intercept,
                    SLOPE=slope,
                    SLOPE.PCT=slope.pct,
                    PVAL=pval,
                    SIGNIF=sig,
                    DIRECTION=ordered(ifelse(slope>0, 'Increasing', 'Decreasing'), levels=c('Increasing', 'Decreasing')))		)
}
# trend.sk(x=filter(df_mon, DATASET=='POR', SITE_NAME=="Power", VAR=="TP"),
#          value_var='C',
#          months=1:12,
#          month_label='Seasonal',
#          years=2002:2014,
#          log_trans=TRUE,
#          water_year=TRUE)


trend.mk <- function(x, value_var, years, month_label, log_trans=FALSE, water_year=TRUE) {
  if (water_year==TRUE) {
    x <- subset(x, WYEAR %in% years)
    start_yr <- min(x[['WYEAR']])
  } else {
    x <- subset(x, YEAR %in% years)
    start_yr <- min(x[['YEAR']])
  }

  if (log_trans) {
    z <- ts(log10(x[[value_var]]), start=start_yr, freq=1)
  } else {
    z <- ts(x[[value_var]], start=start_yr, freq=1)
  }

  mk <- mannKen(z)

#   slope <- ifelse(abs(mk$sen.slope) < 1E-5, 0, mk$sen.slope)
#   slope.pct <- mk$sen.slope.pct/100
#
  slope <- mk$sen.slope
  if (log_trans) {
    slope.pct <- 10^slope-1
  } else {
    slope.pct <- mk$sen.slope.pct/100
  }

  intercept <- median(z - slope*time(z), na.rm=T)
  pval <- mk$p.value
  sig <- cut(pval, breaks=c(0,0.05,0.1,1), labels=c("p<0.05","0.05<p<0.10","p>0.10"))
  if (is.na(sig)) sig <- "p>0.10"

  data.frame(TERM=value_var,
             LOG=log_trans,
             YEAR_SPAN=paste(min(years),max(years),sep='-'),
             MONTH_LABEL=month_label,
             METHOD='MannKendall',
             MEAN.VAL=mean(z, na.rm=T),
             MEAN.TIME=mean(time(z)),
             INTERCEPT=intercept,
             SLOPE=slope,
             SLOPE.PCT=slope.pct,
             PVAL=pval,
             SIGNIF=sig,
             DIRECTION=ordered(ifelse(slope>0, 'Increasing', 'Decreasing'), levels=c('Increasing', 'Decreasing')))
}
# trend.mk(x=filter(df_wyr, DATASET=='POR', SITE_NAME=="Power", VAR=="TP"),
#          value_var='C',
#          month_label='Annual',
#          years=2002:2014,
#          log_trans=TRUE,
#          water_year=TRUE)

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
# trend.lm(x=filter(df_wyr, DATASET=="POR", SITE_NAME=="Power", VAR=="TP"),
#          value_var='C',
#          month_label='Annual',
#          years=2002:2014,
#          log_trans=TRUE,
#          water_year=TRUE)

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
    df <- rbind(df.seasonal, df.fall, df.winter, df.spring, df.summer, df.4_9, df.10_3)

    for (m in 1:12) {
      df.m <- trend.sk(x=x, value_var=value_var,
                       months=m, month_label=as.character(m),
                       years=years, log_trans=log_trans, water_year=water_year)
      df <- rbind(df, df.m)
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
# trend.batch(x_mon=filter(df_mon, DATASET=="POR", SITE_NAME=="Power", VAR=="TP"),
#             x_wyr=filter(df_wyr, DATASET=="POR", SITE_NAME=="Power", VAR=="TP"),
#             years=2002:2014,
#             log_trans=TRUE,
#             water_year=TRUE) %>%
#   summary

cat('Computing trend analysis...\n\n')
trends <- lapply(levels(df_mon$DATASET), function(dataset) {
  cat(dataset, '\n')
  lapply(dataset_vars[[dataset]], function(variable) {
    cat('..', variable, '\n')
    lapply(dataset_sites[[dataset]], function(site) {
      if (variable == 'TSS') {
        year_range <- seq(2011, 2014)
      } else {
        year_range <- seq(dataset_years[[dataset]][1], dataset_years[[dataset]][2])
      }
      x <- trend.batch(x_mon=filter(df_mon, DATASET==dataset, SITE_NAME==site, VAR==variable),
                       x_wyr=filter(df_wyr, DATASET==dataset, SITE_NAME==site, VAR==variable),
                       years=year_range,
                       log_trans=TRUE,
                       water_year=TRUE)
      x$SITE_NAME <- site
      x$VAR <- variable
      x$DATASET <- dataset
      x
    }) %>%
      rbind_all
  }) %>%
    rbind_all
}) %>%
  rbind_all %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=levels(stn.kt_sprague$SITE_NAME)),
         VAR=ordered(VAR, levels=levels(df_mon$VAR))) %>%
  droplevels

cat('Saving trend results to trends.Rdata...\n')
saveRDS(trends, file='trends.Rdata')

# plot functions ----

plot_dot_season_flow <- function(dataset, log_trans=TRUE) {
  x.trend <- filter(trends, DATASET==dataset, VAR=='TP', TERM=='Q', MONTH_LABEL %in% c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep', 'Oct-Mar', 'Apr-Sep'), LOG==log_trans) %>%
    mutate(MONTH_LABEL=ordered(as.character(MONTH_LABEL), levels=c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep', 'Oct-Mar', 'Apr-Sep')),
           SITE_NAME=ordered(as.character(SITE_NAME), levels=rev(levels(SITE_NAME))))

  title <- paste0('Seasonal Kendall Trend Slopes\nDataset: ', dataset_labels[[dataset]], ' | Period: ', period_label(dataset, 'FLOW'), ' | Variable: Flow')
#   if (log_trans) {
#     title <- paste0(title, ' | Transform: Log10')
#   } else {
#     title <- paste0(title, ' | Transform: None')
#   }

  p.4 <- filter(x.trend, MONTH_LABEL %in% c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep')) %>%
    ggplot(aes(SLOPE.PCT, SITE_NAME)) +
    geom_segment(mapping=aes(x=0, xend=SLOPE.PCT, y=SITE_NAME, yend=SITE_NAME)) +
    geom_point(mapping=aes(), shape=16, size=4, color='white') +
    geom_point(mapping=aes(color=DIRECTION, alpha=SIGNIF), shape=16, size=4) +
    geom_point(mapping=aes(), shape=1, size=4) +
    geom_vline(xint=0) +
    scale_color_manual('Trend Direction', values=c('Increasing'='orangered', 'Decreasing'='steelblue')) +
    scale_alpha_manual('Significance', values=c('p>0.10'=0.0, '0.05<p<0.10'=0.5, 'p<0.05'=1), drop=FALSE) +
    scale_x_continuous(labels=percent) +
    labs(x='Trend Slope (%/yr)', y='') +
    facet_grid(.~MONTH_LABEL)
  p.2 <- filter(x.trend, MONTH_LABEL %in% c('All Months', 'Oct-Mar', 'Apr-Sep')) %>%
    ggplot(aes(SLOPE.PCT, SITE_NAME)) +
    geom_segment(mapping=aes(x=0, xend=SLOPE.PCT, y=SITE_NAME, yend=SITE_NAME)) +
    geom_point(mapping=aes(), shape=16, size=4, color='white') +
    geom_point(mapping=aes(color=DIRECTION, alpha=SIGNIF), shape=16, size=4) +
    geom_point(mapping=aes(), shape=1, size=4) +
    geom_vline(xint=0) +
    scale_color_manual('Trend Direction', values=c('Increasing'='orangered', 'Decreasing'='steelblue')) +
    scale_alpha_manual('Significance', values=c('p>0.10'=0.0, '0.05<p<0.10'=0.5, 'p<0.05'=1), drop=FALSE) +
    scale_x_continuous(labels=percent) +
    labs(x='Trend Slope (%/yr)', y='') +
    facet_grid(.~MONTH_LABEL)

  grid.arrange(grobs=list(p.4, arrangeGrob(p.2, ncol=2, widths=c(2/3, 1/3))),
               nrow=3,
               title=title)
}
# plot_dot_season_flow(dataset='POR')
# plot_dot_season_flow(dataset='RECENT')

plot_dot_season <- function(dataset, variable, seasons=c('All Months', 'Oct-Mar', 'Apr-Sep'), log_trans=TRUE) {
  x.trend <- filter(trends, DATASET==dataset, VAR==variable, MONTH_LABEL %in% seasons, LOG==log_trans) %>%
    mutate(MONTH_LABEL=ordered(as.character(MONTH_LABEL), levels=seasons),
           TERM_LABEL=plyr::revalue(TERM, term_labs),
           TERM_LABEL=ordered(TERM_LABEL, levels=unname(term_labs)),
           SITE_NAME=ordered(as.character(SITE_NAME), levels=rev(levels(SITE_NAME))))

  title <- paste0('Seasonal Kendall Trend Slopes\nDataset: ', dataset_labels[[dataset]], ' | Period: ', period_label(dataset, variable), ' | Variable: ', variable)
#   if (log_trans) {
#     title <- paste0(title, ' | Transform: Log10')
#   } else {
#     title <- paste0(title, ' | Transform: None')
#   }
#
  ggplot(x.trend, aes(SLOPE.PCT, SITE_NAME)) +
    geom_segment(mapping=aes(x=0, xend=SLOPE.PCT, y=SITE_NAME, yend=SITE_NAME)) +
    geom_point(mapping=aes(), shape=16, size=4, color='white') +
    geom_point(mapping=aes(color=DIRECTION, alpha=SIGNIF), shape=16, size=4) +
    geom_point(mapping=aes(), shape=1, size=4) +
    geom_vline(xint=0) +
    scale_color_manual('Trend Direction', values=c('Increasing'='orangered', 'Decreasing'='steelblue')) +
    scale_alpha_manual('Significance', values=c('p>0.10'=0.0, '0.05<p<0.10'=0.5, 'p<0.05'=1), drop=FALSE) +
    scale_x_continuous(labels=percent) +
    labs(x='Trend Slope (%/yr)', y='', title=title) +
    facet_grid(MONTH_LABEL~TERM_LABEL)
}
# plot_dot_season(dataset='POR', variable='TP')
# plot_dot_season(dataset='POR', variable='TP', seasons=c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep'))
# plot_dot_season(dataset='RECENT', variable='TP', log_trans=TRUE)

plot_dot_term <- function(dataset, term, seasons=c('All Months', 'Oct-Mar', 'Apr-Sep'), variables=c("TP","PO4","PP","TN","NH4","NO23","TSS"), log_trans=TRUE) {
  x.trend <- filter(trends, DATASET==dataset, TERM==term, MONTH_LABEL %in% seasons, LOG==log_trans, VAR %in% variables) %>%
    mutate(MONTH_LABEL=ordered(as.character(MONTH_LABEL), levels=seasons),
           TERM_LABEL=plyr::revalue(TERM, term_labs),
           TERM_LABEL=ordered(TERM_LABEL, levels=unname(term_labs)),
           VAR=ordered(VAR, levels=variables),
           SITE_NAME=ordered(as.character(SITE_NAME), levels=rev(levels(SITE_NAME))))

  title <- paste0('Seasonal Kendall Trend Slopes\nDataset: ', dataset_labels[[dataset]], ' | Period: ', period_label(dataset, 'TP'), ' | Term: ', term_labs[[term]])
#   if (log_trans) {
#     title <- paste0(title, ' | Transform: Log10')
#   } else {
#     title <- paste0(title, ' | Transform: None')
#   }

  p <- ggplot(x.trend, aes(SLOPE.PCT, SITE_NAME)) +
    geom_segment(mapping=aes(x=0, xend=SLOPE.PCT, y=SITE_NAME, yend=SITE_NAME)) +
    geom_point(mapping=aes(), shape=16, size=4, color='white') +
    geom_point(mapping=aes(color=DIRECTION, alpha=SIGNIF), shape=16, size=4) +
    geom_point(mapping=aes(), shape=1, size=4) +
    geom_vline(xint=0) +
    scale_color_manual('Trend Direction', values=c('Increasing'='orangered', 'Decreasing'='steelblue')) +
    scale_alpha_manual('Significance', values=c('p>0.10'=0.0, '0.05<p<0.10'=0.5, 'p<0.05'=1), drop=FALSE) +
    scale_x_continuous(labels=percent) +
    labs(x='Trend Slope (%/yr)', y='', title=title) +
    facet_grid(VAR~MONTH_LABEL)
  print(p)
  if('TSS' %in% unique(x.trend$VAR)) {
    makeFootnote('Note: TSS Trends based on period WY2011-WY2014')
  }
}
# plot_dot_term(dataset='POR', term='C')
# plot_dot_term(dataset='POR', term='C', c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep'))
# plot_dot_term(dataset='RECENT', term='C', log_trans=TRUE)

plot_diagnostic <- function(dataset, site_name, variable, term, log_trans=TRUE) {
  x.trend <- filter(trends,
                    DATASET==dataset, SITE_NAME==site_name,
                    VAR==variable, TERM==term, LOG==log_trans)
  x.mon.trends <- filter(x.trend, METHOD=='SeasonalKendall') %>%
    select(SEASON=MONTH_LABEL, SLOPE, INTERCEPT)
  x.wyr.trends <- filter(x.trend, MONTH_LABEL %in% c('Annual-MK', 'Annual-Reg'))
  x.trends.all <- filter(x.trend, METHOD=='SeasonalKendall', MONTH_LABEL=='All Months') %>%
    as.list()

  x.wyr <- filter(df_wyr, DATASET==dataset, SITE_NAME==site_name, VAR==variable)
  x.mon <- filter(df_mon, DATASET==dataset, SITE_NAME==site_name, VAR==variable) %>%
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
    title <- paste(paste0('Dataset: ', dataset_labels[[dataset]]),
                   paste0('Period: ', period_label(dataset, variable)),
                   paste0('Site: ', site_name),
                   paste0('Variable: ', term_labs[[term]]),
                   sep='  |  ')
  } else {
    title <- paste(paste0('Dataset: ', dataset_labels[[dataset]]),
                   paste0('Period: ', period_label(dataset, variable)),
                   paste0('Site: ', site_name),
                   paste0('Variable: ', variable),
                   paste0('Term: ', term_labs[[term]]),
                   sep='  |  ')
  }

  p.mon.ts <- x.mon %>%
    ggplot() + aes_string(x='DDATE', y=term, color='SEASON') +
    geom_point(show_guide=TRUE) +
    geom_abline(aes(intercept=INTERCEPT, slope=SLOPE, color=SEASON),
                data=filter(x.mon.trends, SEASON %in% c('Apr-Sep', 'Oct-Mar')), show_guide=TRUE) +
    geom_abline(aes(intercept=INTERCEPT, slope=SLOPE, color=SEASON),
                data=filter(x.mon.trends, SEASON=='All Months'), show_guide=TRUE) +
    scale_color_manual('Season', values=c('Apr-Sep'='olivedrab3', 'Oct-Mar'='orange', 'All Months'='black')) +
    scale_x_continuous(breaks=seq(2002, 2014, by=ifelse(n_year >= 10, 2, 1))) +
    labs(x='', y=paste0('Monthly ', ylabel)) +
    theme(legend.position='top')

  if (!log_trans) {
    p.mon.ts <- p.mon.ts + geom_hline(yint=0, alpha=0)
  }

  p.wyr.ts <- x.wyr %>%
    ggplot(aes_string(x='WYEAR', y=term)) +
    geom_point() +
    geom_abline(aes(intercept=INTERCEPT, slope=SLOPE, linetype=METHOD),
                data=x.wyr.trends, show_guide=TRUE) +
    scale_linetype_manual('Method', values=c('MannKendall'=1, 'LinearRegression'=2)) +
    scale_x_continuous(breaks=seq(2002, 2014, by=ifelse(n_year >= 10, 2, 1))) +
    labs(x='', y=paste0('Annual ', ylabel)) +
    theme(legend.position='top')

  if (!log_trans) {
    p.wyr.ts <- p.wyr.ts + geom_hline(yint=0, alpha=0)
  }

  p.mon <- x.mon %>%
    ggplot() +
    aes_string(x='WYEAR', y=term) +
    geom_point(size=1.5) +
    geom_abline(aes(intercept=INTERCEPT, slope=SLOPE),
                data=filter(x.mon.trends, SEASON %in% seq(1, 12)) %>%
                  rename(MONTH=SEASON)) +
    facet_wrap(~MONTH, nrow=1) +
    scale_x_continuous(breaks=seq(2002, 2014, by=ifelse(n_year >= 10, 4, 2))) +
    labs(x='', y=ylabel) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

  p.bar <- x.trend %>%
    mutate(MONTH_LABEL=plyr::revalue(MONTH_LABEL, c('All Months'='Annual-SK'))) %>%
    ggplot(aes(MONTH_LABEL, SLOPE.PCT, fill=DIRECTION, alpha=SIGNIF)) +
    geom_bar(stat='identity', fill='white', alpha=1) +
    geom_bar(stat='identity', color='grey30', alpha=0) +
    geom_bar(stat='identity') +
    geom_vline(xint=c(12.5, 16.5, 18.5), color='grey70') +
    geom_hline(yint=0, color='grey50') +
    scale_fill_manual('Trend Direction', values=c('Increasing'='orangered', 'Decreasing'='steelblue')) +
    scale_alpha_manual('Significance', values=c('p>0.10'=0.0, '0.05<p<0.10'=0.5, 'p<0.05'=1), drop=FALSE) +
    scale_y_continuous(labels=percent) +
    labs(x='', y='Trend Slope (%/yr)') +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

  if (x.trends.all$PVAL < 1e-4) {
    pval <- '< 0.0001'
  } else {
    pval <- format(x.trends.all$PVAL, nsmall=4L, digits=0, scientific=FALSE)
  }

  tbl <- c('Dataset: '      = dataset,
           'Site: '         = site_name,
           'Variable: '     = variable,
           'Term: '         = term_labs[[term]],
           'Water Years: '  = period_label(dataset, variable),
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
                                      widths=c(2/3, 1/3), nrow=1,
                                      just=c('left', 'top'))),
               ncol=1,
               just=c('left', 'top'),
               heights=c(10/24, 6/24, 8/24),
               top=title)
}
# plot_diagnostic(dataset='POR', site_name='Power', variable='TP', term='C')

for (dataset in dataset_levels) {
  cat(dataset, '\n')
  for (variable in dataset_vars[[dataset]]) {
    filename <- file.path('pdf', tolower(dataset), 'trends', paste0('trends-', tolower(variable), '.pdf'))
    cat('Printing:', filename, '\n')
    cat('..', variable, '\n')
    pdf(filename, width=11, height=8.5)
    print(plot_dot_season(dataset=dataset, variable=variable, log_trans=TRUE))
    Sys.sleep(1)
    print(plot_dot_season(dataset=dataset, variable=variable, seasons=c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep'), log_trans=TRUE))
    Sys.sleep(1)
    for (site_name in dataset_sites[[dataset]]) {
      cat('....', site_name, '\n')
      for (term in c('C', 'L')) {
        cat('......', term, '\n')
        plot_diagnostic(dataset=dataset, site_name=site_name, variable=variable, term=term, log_trans=TRUE)
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
  plot_dot_season_flow(dataset=dataset)
  Sys.sleep(1)
  for (site_name in dataset_sites[[dataset]]) {
    cat('....', site_name, '\n')
    for (term in c('Q')) {
      cat('......', term, '\n')
      plot_diagnostic(dataset=dataset, site_name=site_name, variable=dataset_vars[[dataset]][1], term=term, log_trans=TRUE)
      Sys.sleep(1)
    }
  }
  dev.off()
  Sys.sleep(1)
}

for (dataset in dataset_levels) {
  filename <- file.path('pdf', tolower(dataset), paste0('trends-summary.pdf'))
  cat('Printing:', filename, '\n')
  cat(dataset, '\n')
  pdf(filename, width=11, height=8.5)
  # plot_dot_term(dataset=dataset, term='C')
  plot_dot_term(dataset=dataset, term='C', seasons=c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep'))
  # plot_dot_term(dataset=dataset, term='L')
  plot_dot_term(dataset=dataset, term='L', seasons=c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep'))
  dev.off()
  Sys.sleep(1)
}

# pdf(file.path('pdf', 'trends-dataset-comparison.pdf'), width=11, height=8.5)
# filter(trends, TERM=='C', MONTH_LABEL %in% c('All Months', 'Oct-Mar', 'Apr-Sep'), LOG==TRUE,
#        VAR %in% dataset_vars[['POR']], SITE_NAME %in% dataset_sites[['POR']]) %>%
#   mutate(MONTH_LABEL=ordered(MONTH_LABEL, levels=c('All Months', 'Oct-Mar', 'Apr-Sep')),
#          TERM_LABEL=plyr::revalue(TERM, term_labs),
#          TERM_LABEL=ordered(TERM_LABEL, levels=unname(term_labs)),
#          VAR=ordered(VAR, levels=c('TP', 'PO4', 'PP', 'TN', 'NH4', 'NO23'))) %>%
#   ggplot(aes(SITE_NAME, SLOPE.PCT)) +
#   geom_hline(yint=0, color='grey20') +
#   geom_bar(aes(group=DATASET), fill='white', stat='identity', position='dodge') +
#   geom_bar(aes(fill=DATASET, alpha=SIGNIF), stat='identity', color='black', position='dodge') +
#   scale_alpha_manual('Significance', values=c('p>0.10'=0.0, '0.05<p<0.10'=0.5, 'p<0.05'=1), drop=FALSE) +
#   scale_fill_manual('Dataset', labels=c('POR'='POR', 'RECENT'='RECENT'),
#                     values=c('POR'='steelblue', 'RECENT'='chartreuse3')) +
#   scale_y_continuous(labels=percent) +
#   labs(y='Trend Slope (%/yr)', x='') +
#   ggtitle('Comparison of Trend Slopes and Significance by Dataset | FWM Concentration') +
#   facet_grid(VAR~MONTH_LABEL, scales='free_y') +
#   theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
# filter(trends, TERM=='L', MONTH_LABEL %in% c('All Months', 'Oct-Mar', 'Apr-Sep'), LOG==TRUE,
#        VAR %in% dataset_vars[['POR']], SITE_NAME %in% dataset_sites[['POR']]) %>%
#   mutate(MONTH_LABEL=ordered(MONTH_LABEL, levels=c('All Months', 'Oct-Mar', 'Apr-Sep')),
#          TERM_LABEL=plyr::revalue(TERM, term_labs),
#          TERM_LABEL=ordered(TERM_LABEL, levels=unname(term_labs)),
#          VAR=ordered(VAR, levels=c('TP', 'PO4', 'PP', 'TN', 'NH4', 'NO23'))) %>%
#   ggplot(aes(SITE_NAME, SLOPE.PCT)) +
#   geom_hline(yint=0, color='grey20') +
#   geom_bar(aes(group=DATASET), fill='white', stat='identity', position='dodge') +
#   geom_bar(aes(fill=DATASET, alpha=SIGNIF), stat='identity', color='black', position='dodge') +
#   scale_alpha_manual('Significance', values=c('p>0.10'=0.0, '0.05<p<0.10'=0.5, 'p<0.05'=1), drop=FALSE) +
#   scale_fill_manual('Dataset', labels=c('POR'='POR', 'RECENT'='RECENT'),
#                     values=c('POR'='steelblue', 'RECENT'='chartreuse3')) +
#   scale_y_continuous(labels=percent) +
#   labs(y='Trend Slope (%/yr)', x='') +
#   ggtitle('Comparison of Trend Slopes and Significance by Dataset | Load') +
#   facet_grid(VAR~MONTH_LABEL, scales='free_y') +
#   theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
# filter(trends, TERM=='L', MONTH_LABEL %in% c('All Months', 'Oct-Mar', 'Apr-Sep'), LOG==TRUE,
#        VAR=='TP', SITE_NAME %in% dataset_sites[['POR']]) %>%
#   mutate(VAR='FLOW',
#          MONTH_LABEL=ordered(MONTH_LABEL, levels=c('All Months', 'Oct-Mar', 'Apr-Sep')),
#          TERM_LABEL=plyr::revalue(TERM, term_labs),
#          TERM_LABEL=ordered(TERM_LABEL, levels=unname(term_labs))) %>%
#   ggplot(aes(SITE_NAME, SLOPE.PCT)) +
#   geom_hline(yint=0, color='grey20') +
#   geom_bar(aes(group=DATASET), fill='white', stat='identity', position='dodge') +
#   geom_bar(aes(fill=DATASET, alpha=SIGNIF), stat='identity', color='black', position='dodge') +
#   scale_alpha_manual('Significance', values=c('p>0.10'=0.0, '0.05<p<0.10'=0.5, 'p<0.05'=1), drop=FALSE) +
#   scale_fill_manual('Dataset', labels=c('POR'='POR', 'RECENT'='RECENT'),
#                     values=c('POR'='steelblue', 'RECENT'='chartreuse3')) +
#   scale_y_continuous(labels=percent) +
#   labs(y='Trend Slope (%/yr)', x='') +
#   ggtitle('Comparison of Trend Slopes and Significance by Dataset') +
#   facet_grid(VAR~MONTH_LABEL, scales='free_y') +
#   theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
# dev.off()

# precip trends ----
trend.batch.prcp <- function(x.mon, x.wyr, years, log_trans=FALSE, water_year=TRUE) {
  x <- x.mon %>%
    mutate(MONTH=month(MONTHYEAR))

  value_var <- 'PRCP'

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
  df <- rbind(df.seasonal, df.fall, df.winter, df.spring, df.summer, df.4_9, df.10_3)

  for (m in 1:12) {
    df.m <- trend.sk(x=x, value_var=value_var,
                     months=m, month_label=as.character(m),
                     years=years, log_trans=log_trans, water_year=water_year)
    df <- rbind(df, df.m)
  }

  df.mk <- trend.mk(x=x.wyr, value_var=value_var,
                    years=years, month_label='Annual-MK',
                    log_trans=log_trans, water_year=water_year)
  df.lm <- trend.lm(x=x.wyr, value_var=value_var,
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

prism.mon <- mutate(prism_subbasin,
                    DATASET='PRISM',
                    DATE=MONTHYEAR,
                    VAR='PRCP',
                    N.DAY=as.numeric(difftime(DATE+months(1), DATE, units='days')),
                    PRCP=PRCP/N.DAY) %>% # mean mm/day
  filter(WYEAR>=2002, WYEAR<=2014)
prism.wyr <- group_by(prism.mon, SITE_NAME, WYEAR) %>%
  summarise(PRCP=sum(PRCP*N.DAY)/sum(N.DAY)) # mm/day

cat('Computing precip trends...\n')
trend.prcp <- lapply(levels(prism.mon$SITE_NAME), function(site) {
  df <- trend.batch.prcp(filter(prism.mon, SITE_NAME==site),
                         filter(prism.wyr, SITE_NAME==site),
                         years=2002:2014,
                         log_trans=FALSE,
                         water_year=TRUE)
  df$SITE_NAME <- site
  df
}) %>%
  rbind_all() %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=levels(stn.kt_sprague$SITE_NAME)))


plot_diagnostic_prcp <- function(site_name, log_trans=FALSE) {
  term <- 'PRCP'
  x.trend <- filter(trend.prcp, SITE_NAME==site_name, LOG==log_trans)
  x.mon.trends <- filter(x.trend, METHOD=='SeasonalKendall') %>%
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

  title <- paste0('Precipitation Trends\nDataset: PRISM  |  Site: ', site_name)

  p.mon.ts <- x.mon %>%
    ggplot() + aes_string(x='DDATE', y=term, color='SEASON') +
    geom_point(show_guide=TRUE) +
    geom_abline(aes(intercept=INTERCEPT, slope=SLOPE, color=SEASON),
                data=filter(x.mon.trends, SEASON %in% c('Apr-Sep', 'Oct-Mar')), show_guide=TRUE) +
    geom_abline(aes(intercept=INTERCEPT, slope=SLOPE, color=SEASON),
                data=filter(x.mon.trends, SEASON %in% c('All Months')), show_guide=TRUE) +
    scale_color_manual('Season', values=c('Apr-Sep'='olivedrab3', 'Oct-Mar'='orange', 'All Months'='black')) +
    scale_x_continuous(breaks=seq(2002, 2014, by=ifelse(n_year >= 10, 2, 1))) +
    labs(x='', y=paste0('Monthly ', ylabel)) +
    theme(legend.position='top')

  if (!log_trans) {
    p.mon.ts <- p.mon.ts + geom_hline(yint=0, alpha=0)
  }

  p.wyr.ts <- x.wyr %>%
    ggplot(aes_string(x='WYEAR', y=term)) +
    geom_point() +
    geom_abline(aes(intercept=INTERCEPT, slope=SLOPE, linetype=METHOD),
                data=x.wyr.trends, show_guide=TRUE) +
    scale_linetype_manual('Method', values=c('MannKendall'=1, 'LinearRegression'=2)) +
    scale_x_continuous(breaks=seq(2002, 2014, by=ifelse(n_year >= 10, 2, 1))) +
    labs(x='', y=paste0('Annual ', ylabel)) +
    theme(legend.position='top')

  if (!log_trans) {
    p.wyr.ts <- p.wyr.ts + geom_hline(yint=0, alpha=0)
  }

  p.mon <- x.mon %>%
    ggplot() + aes_string(x='WYEAR', y=term) +
    geom_point(size=1.5) +
    geom_abline(aes(intercept=INTERCEPT, slope=SLOPE),
                data=filter(x.mon.trends, SEASON %in% seq(1, 12)) %>% rename(MONTH=SEASON) %>% droplevels) +
    facet_wrap(~MONTH, nrow=1) +
    scale_x_continuous(breaks=seq(2002, 2014, by=ifelse(n_year >= 10, 4, 2))) +
    labs(x='', y=ylabel) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

  p.bar <- x.trend %>%
    mutate(MONTH_LABEL=plyr::revalue(MONTH_LABEL, c('All Months'='Annual-SK'))) %>%
    ggplot(aes(MONTH_LABEL, SLOPE.PCT, fill=DIRECTION, alpha=SIGNIF)) +
    geom_bar(stat='identity', fill='white', alpha=1) +
    geom_bar(stat='identity', color='grey30', alpha=0) +
    geom_bar(stat='identity') +
    geom_vline(xint=c(12.5, 16.5, 18.5), color='grey70') +
    geom_hline(yint=0, color='grey50') +
    scale_fill_manual('Trend Direction', values=c('Increasing'='orangered', 'Decreasing'='steelblue')) +
    scale_alpha_manual('Significance', values=c('p>0.10'=0.0, '0.05<p<0.10'=0.5, 'p<0.05'=1), drop=FALSE) +
    scale_y_continuous(labels=percent) +
    labs(x='', y='Trend Slope (%/yr)') +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))

  if (x.trends.all$PVAL < 1e-4) {
    pval <- '< 0.0001'
  } else {
    pval <- format(x.trends.all$PVAL, nsmall=4L, digits=0, scientific=FALSE)
  }

  tbl <- c('Dataset: '      = 'PRISM',
           'Site: '         = site_name,
           'Variable: '     = 'Precip',
           'Water Years: '  = 'WY2002-WY2014',
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
# plot_diagnostic_prcp(site_name='Power')

plot_dot_precip <- function(seasons=c('All Months', 'Oct-Mar', 'Apr-Sep')) {
  x.trend <- filter(trend.prcp, MONTH_LABEL %in% seasons) %>%
    mutate(MONTH_LABEL=ordered(as.character(MONTH_LABEL), levels=seasons),
           SITE_NAME=ordered(as.character(SITE_NAME), levels=rev(levels(SITE_NAME))))

  p <- ggplot(x.trend, aes(SLOPE.PCT, SITE_NAME)) +
    geom_segment(mapping=aes(x=0, xend=SLOPE.PCT, y=SITE_NAME, yend=SITE_NAME)) +
    geom_point(mapping=aes(), shape=16, size=4, color='white') +
    geom_point(mapping=aes(color=DIRECTION, alpha=SIGNIF), shape=16, size=4) +
    geom_point(mapping=aes(), shape=1, size=4) +
    geom_vline(xint=0) +
    scale_color_manual('Trend Direction', values=c('Increasing'='orangered', 'Decreasing'='steelblue'), drop=FALSE) +
    scale_alpha_manual('Significance', values=c('p>0.10'=0.0, '0.05<p<0.10'=0.5, 'p<0.05'=1), drop=FALSE) +
    scale_x_continuous(labels=percent) +
    labs(x='Trend Slope (%/yr)', y='') +
    facet_grid(.~MONTH_LABEL) +
    theme(axis.text.x=element_text(size=8))
  p
}
# plot_dot_precip()

filename <- file.path('pdf', 'trends-precip.pdf')
cat('Printing:', filename, '\n')
pdf(filename, width=11, height=8.5)
p.2 <- plot_dot_precip()
p.4 <- plot_dot_precip(seasons=c('All Months', 'Oct-Dec', 'Jan-Mar', 'Apr-Jun', 'Jul-Sep'))
grid.arrange(grobs=list(p.2, p.4),
             nrow=2,
             top=paste0('\nSeasonal Kendall Trend Slopes\nDataset: PRISM | Period: WY2002-WY2014, Variable: Precip'))
for (site in unique(trend.prcp$SITE_NAME)) {
  cat('..', site, '\n')
  plot_diagnostic_prcp(site_name=site)
  Sys.sleep(1)
}
dev.off()
