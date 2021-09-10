library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
theme_set(theme_bw())

rm(list=ls())

load('kt_sprague.Rdata')
source('functions.R')

if (!file.exists(file.path('pdf', 'dataset', 'outliers'))) {
  dir.create(file.path('pdf', 'dataset', 'outliers'))
}

# load data
wq <- wq.kt_sprague[['RAW']] %>%
  select(SITE_NAME, VAR, DATE, VALUE, QAQC, FLAGGED) %>%
  filter(VAR %in% c('FLOW', 'TP', 'PO4', 'TN', 'NH4', 'NO23', 'TSS', 'TURBIDITY'),
         QAQC %in% c('PASS', 'RPD', 'OUTLIER')) %>%
  droplevels %>%
  mutate(WDAY=water_day(DATE),
         FLAGGED=as.factor(FLAGGED))

# comparison variables
var_ref <- list(TP=c('PO4', 'TSS', 'TN'),
                PO4=c('TP', 'TSS', 'NO23'),
                TN=c('NO23', 'NH4', 'TP'),
                NH4=c('TN', 'NO23', 'PO4'),
                NO23=c('TN', 'NH4', 'PO4'),
                TSS=c('TP', 'TN', 'TURBIDITY'))

# comparison sites
site_ref <- list(Power=c('Lone_Pine', 'Godowa', 'Sycan'),
                 Lone_Pine=c('Power', 'Godowa', 'Sycan'),
                 Sycan=c('Lone_Pine', 'Godowa', 'Power'),
                 Godowa=c('Lone_Pine', 'NF', 'SF'),
                 SF=c('Godowa', 'SF_Ivory', 'NF'),
                 SF_Ivory=c('Godowa', 'SF', 'NF_Ivory'),
                 NF=c('Godowa', 'NF_Ivory', 'SF'),
                 NF_Ivory=c('Godowa', 'NF', 'SF_Ivory'))

# scales
log_y <- scale_y_log10(breaks=log_breaks(seq(1, 9), 10^seq(-3, 3)),
                       labels=log_labels(c(1, 3, 5), 10^seq(-3, 3)))
log_x <- scale_x_log10(breaks=log_breaks(seq(1, 9), 10^seq(-3, 3)),
                       labels=log_labels(c(1, 3, 5), 10^seq(-3, 3)))

# functions ----
merge_sites <- function(variable, site1, site2, range=3) {
  x.var <- filter(wq, VAR==variable)
  x.site1 <- filter(x.var, SITE_NAME==site1)
  x.site2 <- filter(x.var, SITE_NAME==site2)

  x.site1$DATE2 <- NA
  x.site1$VALUE2 <- NA
  x.site1$FLAGGED2 <- NA
  for (i in seq(1, nrow(x.site1))) {
    x.date <- as.Date(x.site1[i, 'DATE'])
    x.site2.near <- filter(x.site2, as.Date(DATE) %in% seq.Date(x.date-days(range), x.date+days(range), by='day'))
    if (nrow(x.site2.near) > 0) {
      x.site2.nearest <- x.site2.near[which.min(abs(as.Date(x.site2.near$DATE)-x.date)),][1, ]
      x.site1[i, 'DATE2'] <- x.site2.nearest$DATE
      x.site1[i, 'VALUE2'] <- x.site2.nearest$VALUE
      x.site1[i, 'FLAGGED2'] <- x.site2.nearest$FLAGGED
    }
  }
  x.site1 <- mutate(x.site1,
                    DATE2=as.Date(x.site1$DATE2, origin='1970-01-01'),
                    DATE_DIFF=as.numeric(difftime(DATE, DATE2, units='day')))
  x.site1
}


merge_variables <- function(var1, var2, site) {
  x.site <- filter(wq, SITE_NAME==site)
  x.var <- filter(x.site, VAR==var1)
  x.var2 <- filter(x.site, VAR==var2) %>%
    select(DATE, VALUE2=VALUE, FLAGGED2=FLAGGED)

  left_join(x.var, x.var2, by='DATE')
}


plot_data <- function(variable, site, log.trans) {
  ylabel <- paste0(variable, ' @ ', site, ' (', get_units(variable), ')')
  p.time <- filter(wq, SITE_NAME==site, VAR==variable) %>%
    ggplot(aes(DATE, VALUE, color=FLAGGED, size=FLAGGED)) +
    geom_point() +
    scale_color_manual(values=c('FALSE'='grey50', 'TRUE'='red'), guide=FALSE) +
    scale_size_manual(values=c('FALSE'=1, 'TRUE'=3), guide=FALSE) +
    labs(y=ylabel, x='Date')
  p.jday <- filter(wq, SITE_NAME==site, VAR==variable) %>%
    ggplot(aes(WDAY, VALUE, color=FLAGGED, size=FLAGGED)) +
    geom_point() +
    scale_color_manual(values=c('FALSE'='grey50', 'TRUE'='red'), guide=FALSE) +
    scale_size_manual(values=c('FALSE'=1, 'TRUE'=3), guide=FALSE) +
    labs(y=ylabel, x='Water Year Day (0=Oct 1)')
  p.flow <- filter(wq, SITE_NAME==site, VAR==variable) %>%
    group_by(DATE,VAR) %>%
    mutate(row = 1:nrow(.)) %>%
    pivot_wider(names_from=VAR, values_from=VALUE) %>%
    left_join(filter(wq, SITE_NAME==site, VAR=="FLOW") %>%
                select(DATE, FLOW=VALUE),
              by = "DATE") %>%
    ggplot(aes_string('FLOW', variable, color='FLAGGED', size='FLAGGED')) +
    geom_point() +
    scale_color_manual(values=c('FALSE'='grey50', 'TRUE'='red'), guide=FALSE) +
    scale_size_manual(values=c('FALSE'=1, 'TRUE'=3), guide=FALSE) +
    labs(y=ylabel, x=paste0('Flow @ ', site, ' (cfs)'))

  p.var1 <- merge_variables(var1=variable, var2=var_ref[[variable]][1], site=site) %>%
    mutate(FLAGGED=ifelse(FLAGGED=="TRUE","1","0"),
           FLAGGED2=ifelse(FLAGGED2=="TRUE","1","0")) %>%
    mutate(FLAG=as.numeric(FLAGGED)*2+as.numeric(FLAGGED2)*1) %>%
    mutate(FLAG=as.factor(FLAG)) %>%
    ggplot(aes(VALUE2, VALUE, color=FLAG, size=FLAG)) +
    geom_point() +
    scale_color_manual(values=c('0'='grey50', '1'='deepskyblue', '2'='red', '3'='red'), guide=FALSE) +
    scale_size_manual(values=c('0'=1, '1'=2, '2'=3, '3'=3), guide=FALSE) +
    labs(x=paste0(var_ref[[variable]][1], ' @ ', site, ' (', get_units(var_ref[[variable]][1]), ')'), y=ylabel)
  p.var2 <- merge_variables(var1=variable, var2=var_ref[[variable]][2], site=site) %>%
    mutate(FLAGGED=ifelse(FLAGGED=="TRUE","1","0"),
           FLAGGED2=ifelse(FLAGGED2=="TRUE","1","0")) %>%
    mutate(FLAG=as.numeric(FLAGGED)*2+as.numeric(FLAGGED2)*1) %>%
    mutate(FLAG=as.factor(FLAG)) %>%
    ggplot(aes(VALUE2, VALUE, color=FLAG, size=FLAG)) +
    geom_point() +
    scale_color_manual(values=c('0'='grey50', '1'='deepskyblue', '2'='red', '3'='red'), guide=FALSE) +
    scale_size_manual(values=c('0'=1, '1'=2, '2'=3, '3'=3), guide=FALSE) +
    labs(x=paste0(var_ref[[variable]][2], ' @ ', site, ' (', get_units(var_ref[[variable]][2]), ')'), y=ylabel)
  p.var3 <- merge_variables(var1=variable, var2=var_ref[[variable]][3], site=site) %>%
    mutate(FLAGGED=ifelse(FLAGGED=="TRUE","1","0"),
           FLAGGED2=ifelse(FLAGGED2=="TRUE","1","0")) %>%
    mutate(FLAG=as.numeric(FLAGGED)*2+as.numeric(FLAGGED2)*1) %>%
    mutate(FLAG=as.factor(FLAG)) %>%
    ggplot(aes(VALUE2, VALUE, color=FLAG, size=FLAG)) +
    geom_point() +
    scale_color_manual(values=c('0'='grey50', '1'='deepskyblue', '2'='red', '3'='red'), guide=FALSE) +
    scale_size_manual(values=c('0'=1, '1'=2, '2'=3, '3'=3), guide=FALSE) +
    labs(x=paste0(var_ref[[variable]][3], ' @ ', site, ' (', get_units(var_ref[[variable]][3]), ')'), y=ylabel)

  p.site1 <- merge_sites(variable, site, site_ref[[site]][1],) %>%
    filter(!is.na(VALUE2)) %>%
    mutate(FLAGGED=ifelse(FLAGGED=="TRUE","1","0"),
           FLAGGED2=ifelse(FLAGGED2=="TRUE","1","0")) %>%
    mutate(FLAG=as.numeric(FLAGGED)*2+as.numeric(FLAGGED2)*1) %>%
    mutate(FLAG=as.factor(FLAG)) %>%
    ggplot(aes(VALUE2, VALUE, color=FLAG, size=FLAG)) +
    geom_point() +
    scale_color_manual(values=c('0'='grey50', '1'='deepskyblue', '2'='red', '3'='red'), guide=FALSE) +
    scale_size_manual(values=c('0'=1, '1'=2, '2'=3, '3'=3), guide=FALSE) +
    labs(x=paste0(variable, ' @ ', site_ref[[site]][1], ' (', get_units(variable), ')'), y=ylabel)
  p.site2 <- merge_sites(variable, site, site_ref[[site]][2]) %>%
    filter(!is.na(VALUE2)) %>%
    mutate(FLAGGED=ifelse(FLAGGED=="TRUE","1","0"),
           FLAGGED2=ifelse(FLAGGED2=="TRUE","1","0")) %>%
    mutate(FLAG=as.numeric(FLAGGED)*2+as.numeric(FLAGGED2)*1) %>%
    mutate(FLAG=as.factor(FLAG)) %>%
    ggplot(aes(VALUE2, VALUE, color=FLAG, size=FLAG)) +
    geom_point() +
    scale_color_manual(values=c('0'='grey50', '1'='deepskyblue', '2'='red', '3'='red'), guide=FALSE) +
    scale_size_manual(values=c('0'=1, '1'=2, '2'=3, '3'=3), guide=FALSE) +
    labs(x=paste0(variable, ' @ ', site_ref[[site]][2], ' (', get_units(variable), ')'), y=ylabel)
  p.site3 <- merge_sites(variable, site, site_ref[[site]][3]) %>%
    filter(!is.na(VALUE2)) %>%
    mutate(FLAGGED=ifelse(FLAGGED=="TRUE","1","0"),
           FLAGGED2=ifelse(FLAGGED2=="TRUE","1","0")) %>%
    mutate(FLAG=as.numeric(FLAGGED)*2+as.numeric(FLAGGED2)*1) %>%
    mutate(FLAG=as.factor(FLAG)) %>%
    ggplot(aes(VALUE2, VALUE, color=FLAG, size=FLAG)) +
    geom_point() +
    scale_color_manual(values=c('0'='grey50', '1'='deepskyblue', '2'='red', '3'='red'), guide=FALSE) +
    scale_size_manual(values=c('0'=1, '1'=2, '2'=3, '3'=3), guide=FALSE) +
    labs(x=paste0(variable, ' @ ', site_ref[[site]][3], ' (', get_units(variable), ')'), y=ylabel)

  if (log.trans) {
    p.time <- p.time + log_y + theme(panel.grid.minor.y=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
    p.jday <- p.jday + log_y + theme(panel.grid.minor.y=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
    p.flow <- p.flow + log_x + log_y + theme(panel.grid.minor=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
    p.var1 <- p.var1 + log_x + log_y + theme(panel.grid.minor=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
    p.var2 <- p.var2 + log_x + log_y + theme(panel.grid.minor=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
    p.var3 <- p.var3 + log_x + log_y + theme(panel.grid.minor=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
    p.site1 <- p.site1 + log_x + log_y + theme(panel.grid.minor=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
    p.site2 <- p.site2 + log_x + log_y + theme(panel.grid.minor=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
    p.site3 <- p.site3 + log_x + log_y + theme(panel.grid.minor=element_blank(), axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
  }

  grid.arrange(grobs=list(p.time, p.jday, p.flow,
                          p.var1, p.var2, p.var3,
                          p.site1, p.site2, p.site3),
               ncol=3,
               top=paste0('Variable: ', variable, ', Station: ', site, '\nBlack = Not Outlier, Red = Outlier for this Variable/Site, Blue = Outlier for Comparison Variable/Site'))
}
# plot_data(variable='TP', site='Power', log.trans=TRUE)

# pdf plots ----
for (variable in c('TP', 'PO4', 'TN', 'NH4', 'NO23', 'TSS')) {
  filename <- file.path('pdf', 'dataset', 'outliers', paste0('dataset-outliers-', tolower(variable), '.pdf'))
  cat('Printing:', filename, '\n')
  pdf(filename, width=11, height=8.5)

  for (site in levels(stn.kt_sprague$SITE_NAME)) {
    cat('..', site, '\n')
    plot_data(variable=variable, site=site, log.trans=TRUE)
  }
  dev.off()
  Sys.sleep(1)
}
