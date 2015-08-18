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

load('loads.Rdata')
load('kt_sprague.Rdata')

pred <- lapply(names(loads), function(dataset) {
    lapply(names(loads[[dataset]]), function(variable) {
      lapply(names(loads[[dataset]][[variable]]), function(site) {
        x <- loads[[dataset]][[variable]][[site]][['predict']]
        x <- mutate(x,
                    DATASET = dataset,
                    SITE_NAME = site,
                    VAR = variable)
        x
      }) %>%
        rbind_all
    }) %>%
      rbind_all
  }) %>%
  rbind_all %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=levels(stn.kt_sprague$SITE_NAME)))

prob <- 0.995
resid_limits <- filter(pred, !is.na(Cres)) %>%
  group_by(DATASET, VAR, SITE_NAME) %>%
  summarise(N=n(),
            MEAN=mean(Cres),
            SD=sd(Cres),
            SE=SD/sqrt(N),
            UPPER_T=MEAN+qt(prob, df=N-1)*SD,
            LOWER_T=MEAN-qt(prob, df=N-1)*SD,
            UPPER_NORM=MEAN+qnorm(prob)*SD,
            LOWER_NORM=MEAN-qnorm(prob)*SD) %>%
  ungroup


for (dataset in c('POR', 'RECENT')) {
  cat(dataset, '\n')
  pdf(file.path('pdf', tolower(dataset), 'loads-residuals.pdf'), width=11, height=8.5)
  
  dataset_limits <- filter(resid_limits, DATASET==dataset)
  
  p.box <- filter(pred, DATASET==dataset, !is.na(Cres)) %>%
    ggplot(aes(SITE_NAME, Cres)) +
    geom_hline(yint=0) +
    geom_boxplot() +
    facet_wrap(~VAR, nrow=1) +
    labs(x='', y='Residual') +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
    ggtitle(paste0('Distribution of Residuals | Dataset: ', dataset))
  print(p.box)
  
  p.se <- dataset_limits %>%
    ggplot(aes(SITE_NAME, MEAN)) +
    geom_pointrange(aes(ymax=MEAN+SE, ymin=MEAN-SE)) +
    facet_wrap(~VAR, nrow=1) +
    labs(x='', y='Mean Residual +/- 1 StdErr') +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
  p.sd <- dataset_limits %>%
    ggplot(aes(SITE_NAME, MEAN)) +
    geom_pointrange(aes(ymax=LOWER_T, ymin=UPPER_T)) +
    facet_wrap(~VAR, nrow=1) +
    labs(x='', y='Mean Residual +/- 2 StdDev') +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
  grid.arrange(p.se, p.sd, nrow=2,
               main=paste0('Mean Residual +/- SE or 95% Distribution | Dataset: ', dataset))
  for (variable in setdiff(unique(pred$VAR), 'TSS')) {
    p <- pred %>%
      filter(DATASET==dataset, VAR==variable) %>%
      ggplot(aes(DATE)) +
      geom_line(aes(y=Cpred)) +
      geom_hline(yint=0, alpha=0) +
      geom_point(aes(y=Cobs), size=1.5, color='orangered') +
      facet_wrap(~SITE_NAME, nrow=2) +
      labs(x='', y='Predicted/Observed Conc (ppb)') +
      ggtitle(paste0('Observed and Predicted Conc | Dataset: ', dataset, ' | Variable: ', variable))
    print(p)
    p <- pred %>%
      filter(DATASET==dataset, VAR==variable) %>%
      ggplot(aes(DATE)) +
      geom_hline(yint=0) +
      geom_hline(aes(x=NULL, yintercept=MEAN), data=filter(dataset_limits, VAR==variable), color='deepskyblue', linetype=2, size=1) +
      geom_hline(aes(x=NULL, yintercept=LOWER_T), data=filter(dataset_limits, VAR==variable), color='orangered', linetype=2) +
      geom_hline(aes(x=NULL, yintercept=UPPER_T), data=filter(dataset_limits, VAR==variable), color='orangered', linetype=2) +
      geom_point(aes(y=Cres), size=1.5) +
      facet_wrap(~SITE_NAME, nrow=2) +
      labs(x='', y='Log-Residual Conc') +
      ggtitle(paste0('Log-Residuals | Dataset: ', dataset, ' | Variable: ', variable, '\nBlue Line = Mean, Red Lines = Mean +/- 2*SD'))
    print(p)
    p <- pred %>%
      filter(DATASET==dataset, VAR==variable) %>%
      ggplot(aes(Cres)) +
      geom_vline(xint=0, color='grey50') +
      geom_histogram() +
      geom_vline(aes(x=NULL, xintercept=MEAN), data=filter(dataset_limits, VAR==variable), color='deepskyblue', linetype=2, size=1) +
      geom_vline(aes(x=NULL, xintercept=LOWER_T), data=filter(dataset_limits, VAR==variable), color='orangered', linetype=2) +
      geom_vline(aes(x=NULL, xintercept=UPPER_T), data=filter(dataset_limits, VAR==variable), color='orangered', linetype=2) +
      facet_wrap(~SITE_NAME, nrow=2) +
      labs(y='Count', x='Log-Residual Conc') +
      ggtitle(paste0('Log-Residuals | Dataset: ', dataset, ' | Variable: ', variable, '\nBlue Line = Mean, Red Lines = Mean +/- 2*SD'))
    print(p)
  }
  dev.off()  
}


# plots of outliers based on distribution of residuals
outliers <- select(pred, DATASET, VAR, SITE_NAME, DATE, Cobs, Cpred, Cres) %>%
  left_join(resid_limits) %>%
  mutate(FLAG=(Cres < LOWER_T | Cres > UPPER_T))

filter(outliers, DATASET=='POR', VAR=='TP') %>%
  ggplot(aes(DATE)) +
  geom_line(aes(y=Cpred)) +
  geom_point(aes(y=Cobs, color=FLAG)) +
  scale_y_log10() +
  scale_color_manual(values=c('black', 'red'), guide=FALSE) +
  facet_wrap(~SITE_NAME, scales='free_y')

filter(outliers, DATASET=='POR', VAR=='TP') %>%
  ggplot(aes(DATE)) +
  geom_point(aes(x=Cpred, y=Cobs, color=FLAG)) +
  scale_y_log10() +
  scale_x_log10() +
  geom_abline() +
  scale_color_manual(values=c('black', 'red'), guide=FALSE) +
  facet_wrap(~SITE_NAME, scales='free')

filter(outliers, DATASET=='POR', VAR=='TP', !is.na(Cres), SITE_NAME %in% c('Power', 'Lone_Pine')) %>%
  select(DATE, SITE_NAME, Cobs, FLAG) %>%
  gather(TERM, VALUE, Cobs, FLAG) %>%
  unite(SITE_TERM, SITE_NAME, TERM) %>%
  spread(SITE_TERM, VALUE) %>%
  filter(!is.na(Power_FLAG), !is.na(Lone_Pine_FLAG)) %>%
  ggplot(aes(Lone_Pine_Cobs, Power_Cobs, color=paste0(Power_FLAG,Lone_Pine_FLAG))) +
  geom_point()

filter(outliers, DATASET=='POR', VAR=='TP', !is.na(Cres), SITE_NAME %in% c('Godowa', 'Lone_Pine')) %>%
  select(DATE, SITE_NAME, Cobs, FLAG) %>%
  gather(TERM, VALUE, Cobs, FLAG) %>%
  unite(SITE_TERM, SITE_NAME, TERM) %>%
  spread(SITE_TERM, VALUE) %>%
  filter(!is.na(Godowa_FLAG), !is.na(Lone_Pine_FLAG)) %>%
  ggplot(aes(Lone_Pine_Cobs, Godowa_Cobs, color=paste0(Godowa_FLAG,Lone_Pine_FLAG))) +
  geom_point()
