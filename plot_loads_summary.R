library(dplyr)
library(tidyr)
library(lubridate)
library(fluxr)
library(ggplot2)
theme_set(theme_bw())
library(gridExtra)

# load data ----
load('kt_sprague.Rdata')
load('loads.Rdata')

dataset_levels <- names(loads)
site_name_levels <- levels(stn.kt_sprague$SITE_NAME)
term_colors <- c('L'='olivedrab3',
                 'L_AREA'='olivedrab3',
                 'C'='orangered',
                 'Q'='steelblue',
                 'Q_AREA'='steelblue')

# compare datasets ----
compare_dataset <- lapply(names(loads), function(dataset) {
  lapply(names(loads[[dataset]]), function(variable) {
    lapply(names(loads[[dataset]][[variable]]), function(site) {
      x <- loads[[dataset]][[variable]][[site]][['out']][['wyr']] %>%
        mutate(DATASET=dataset,
               SITE_NAME=site,
               VAR=variable)
      x
    }) %>%
      rbind_all
  }) %>%
    rbind_all
}) %>%
  rbind_all %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=site_name_levels),
         VAR=factor(VAR),
         DATASET=ordered(DATASET, dataset_levels))

upper_limits <- wq.kt_sprague[['POR']] %>%
  group_by(VAR) %>%
  summarise(DL=median(DL)) %>%
  filter(!is.na(DL))
lower_limits <- wq.kt_sprague[['RECENT']] %>%
  group_by(VAR) %>%
  summarise(DL=median(DL)) %>%
  filter(!is.na(DL))

fits <- lapply(names(loads), function(dataset) {
  lapply(names(loads[[dataset]]), function(variable) {
    lapply(names(loads[[dataset]][[variable]]), function(site) {
      x <- loads[[dataset]][[variable]][[site]]$stats %>% as.data.frame
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
  mutate(DATASET=ordered(DATASET, dataset_levels),
         SITE_NAME=ordered(SITE_NAME, levels=site_name_levels))

params <- lapply(names(loads), function(dataset) {
  lapply(names(loads[[dataset]]), function(variable) {
    lapply(names(loads[[dataset]][[variable]]), function(site) {
      model <- loads[[dataset]][[variable]][[site]]$model
      x <- broom::tidy(model)
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
  mutate(DATASET=ordered(DATASET, dataset_levels),
         SITE_NAME=ordered(SITE_NAME, levels=site_name_levels))


# pdf: summary ----
for (dataset in c('POR', 'RECENT')) {
  pdf(file.path('pdf', tolower(dataset), 'loads-summary-wyr.pdf'), width=11, height=8.5)
  p <- filter(loads_df[['wyr']], DATASET==dataset, TERM=='C',
         SITE_NAME %in% site_name_levels) %>%
    ggplot(aes(factor(WYEAR), VALUE)) +
    geom_bar(stat='identity', fill=term_colors['C'], width=0.8) +
    scale_y_continuous(labels=scales::comma) +
    labs(x='Water Year', y='FWM Conc (ppb)',
         title=paste0('Annual FWM Concentration by Site and Variable  |  Dataset: ', dataset)) +
    facet_grid(VAR~SITE_NAME, scales='free_y') +
    theme_bw() +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=6),
          strip.text.x=element_text(size=8))
  print(p)

  p <- filter(loads_df[['wyr']], DATASET==dataset, TERM=='L_AREA',
         SITE_NAME %in% site_name_levels) %>%
    ggplot(aes(factor(WYEAR), VALUE)) +
    geom_bar(stat='identity', fill=term_colors['L'], width=0.8) +
    scale_y_continuous(labels=scales::comma) +
    labs(x='Water Year', y='Load per Area (kg/km2/yr)',
         title=paste0('Annual Load per Area by Site and Variable  |  Dataset: ', dataset)) +
    facet_grid(VAR~SITE_NAME, scales='free_y') +
    theme_bw() +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=6),
          strip.text.x=element_text(size=8))
  print(p)

  p <- filter(loads_df[['wyr']], DATASET==dataset, TERM=='L',
         SITE_NAME %in% site_name_levels) %>%
    ggplot(aes(factor(WYEAR), VALUE)) +
    geom_bar(stat='identity', fill=term_colors['L'], width=0.8) +
    scale_y_continuous(labels=scales::comma) +
    labs(x='Water Year', y='Load (kg/yr)',
         title=paste0('Annual Load by Site and Variable  |  Dataset: ', dataset)) +
    facet_grid(VAR~SITE_NAME, scales='free_y') +
    theme_bw() +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=6),
          strip.text.x=element_text(size=8))
  print(p)

  p.q <- filter(loads_df[['wyr']], DATASET==dataset, VAR=='FLOW', TERM=='Q',
                SITE_NAME %in% site_name_levels) %>%
    ggplot(aes(factor(WYEAR), VALUE)) +
    geom_bar(stat='identity', fill=term_colors['Q'], width=0.8) +
    scale_y_continuous(labels=scales::comma) +
    labs(x='Water Year', y='Flow (hm3/yr)',
         title=paste0('Annual Flow by Site  |  Dataset: ', dataset)) +
    facet_grid(.~SITE_NAME, scales='free_y') +
    theme_bw() +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=6),
          strip.text.x=element_text(size=8))
  p.q_area <- filter(loads_df[['wyr']], DATASET==dataset, VAR=='FLOW', TERM=='Q_AREA',
                     SITE_NAME %in% site_name_levels) %>%
    ggplot(aes(factor(WYEAR), VALUE)) +
    geom_bar(stat='identity', fill=term_colors['Q'], width=0.8) +
    scale_y_continuous(labels=scales::comma) +
    labs(x='Water Year', y='Flow per Area (cm/yr)',
         title=paste0('Annual Flow per Area by Site  |  Dataset: ', dataset)) +
    facet_grid(.~SITE_NAME, scales='free_y') +
    theme_bw() +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=6),
          strip.text.x=element_text(size=8))
  grid.arrange(grobs=list(p.q, p.q_area), nrow=3)

  dev.off()
}


for (dataset in c('POR', 'RECENT')) {
  cat(dataset,'\n')
  pdf(file.path('pdf', tolower(dataset), 'loads-summary.pdf'), width=11, height=8.5)
  variables <- setdiff(levels(loads_df[['wyr']]$VAR), 'FLOW')
  if (dataset == 'POR') {
    variables <- setdiff(variables, 'TSS')
  }
  for (variable in variables) {
    cat('..', variable, '\n')
    p <- filter(loads_df[['wyr']], DATASET==dataset, VAR %in% c(variable, 'FLOW'),
           TERM %in% c('Q_AREA', 'L_AREA', 'C'),
           SITE_NAME %in% site_name_levels) %>%
      select(-VAR) %>%
      ggplot(aes(factor(WYEAR), VALUE, fill=TERM)) +
      geom_bar(stat='identity', width=0.8) +
      scale_y_continuous(labels=scales::comma) +
      scale_fill_manual(values=term_colors, guide=FALSE) +
      labs(x='Water Year', y='FWM Conc (ppb)                     Load per Area (kg/km2/yr)              Flow per Area (cm/yr)',
           title=paste0('Annual Flow per Area, Load per Area, and FWM Concentration\nDataset: ', dataset, '   |   Variable: ', variable, '\n')) +
      facet_grid(TERM~SITE_NAME, scales='free_y') +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
            strip.background=element_blank(),
            strip.text.y=element_blank())
    print(p)
  }
  dev.off()
}

for (dataset in c('RECENT', 'POR')) {
  pdf(file.path('pdf', tolower(dataset), 'loads-summary-site.pdf'), width=11, height=8.5)
  # p.q <- filter(loads_df[['site']], DATASET==dataset, VAR=='FLOW', TERM=='Q',
  #               SITE_NAME %in% site_name_levels) %>%
  #   ggplot(aes(SITE_NAME, VALUE)) +
  #   geom_bar(stat='identity', fill=term_colors['Q'], width=0.8) +
  #   scale_y_continuous(labels=scales::comma) +
  #   labs(x='', y='Flow (hm3/yr)',
  #        title=paste0('Mean Annual Annual Flow by Site\nDataset: ', dataset)) +
  #   theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
  p.q_area <- filter(loads_df[['site']], DATASET==dataset, VAR=='FLOW', TERM=='Q_AREA',
                     SITE_NAME %in% site_name_levels) %>%
    ggplot(aes(SITE_NAME, VALUE)) +
    geom_bar(stat='identity', fill=term_colors['Q'], width=0.8) +
    scale_y_continuous(labels=scales::comma) +
    labs(x='', y='Flow per Area (cm/yr)',
         title=paste0('Annual Flow per Area')) +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
  # grid.arrange(p.q, p.q_area, nrow=2)

  # p.l <- filter(loads_df[['site']], DATASET==dataset, TERM=='L',
  #        SITE_NAME %in% site_name_levels) %>%
  #   ggplot(aes(SITE_NAME, VALUE)) +
  #   geom_bar(stat='identity', fill=term_colors['L'], width=0.8) +
  #   scale_y_continuous(labels=scales::comma) +
  #   labs(x='', y='Load (kg/yr)',
  #        title=paste0('Mean Annual Load by Site and Variable\nDataset: ', dataset)) +
  #   facet_grid(VAR~., scales='free_y') +
  #   theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
  p.l_area <- filter(loads_df[['site']], DATASET==dataset, TERM=='L_AREA',
                     SITE_NAME %in% site_name_levels) %>%
    ggplot(aes(SITE_NAME, VALUE)) +
    geom_bar(stat='identity', fill=term_colors['L'], width=0.8) +
    scale_y_continuous(labels=scales::comma) +
    labs(x='', y='Load per Area (kg/km2/yr)',
         title=paste0('Annual Load per Area')) +
    facet_grid(VAR~., scales='free_y') +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
  p.c <- filter(loads_df[['site']], DATASET==dataset, TERM=='C',
                SITE_NAME %in% site_name_levels) %>%
    ggplot(aes(SITE_NAME, VALUE)) +
    geom_bar(stat='identity', fill=term_colors['C'], width=0.8) +
    scale_y_continuous(labels=scales::comma) +
    labs(x='', y='FWM Conc (ppb)',
         title=paste0('FWM Concentration')) +
    facet_grid(VAR~., scales='free_y') +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
  grid.arrange(grobs=list(ggplotGrob(p.c), ggplotGrob(p.l_area),
                          arrangeGrob(p.q_area, nrow=3)),
               nrow=1,
               top=paste0('\nFWM Concentration, Flow per Area, Load per Area by Site\nDataset: ', dataset))
  dev.off()
}

# pdf: diagnostics ----
variable_levels <- list(POR=c('TP', 'PO4', 'TN', 'NH4', 'NO23'),
                        RECENT=c('TP', 'PO4', 'TN', 'NH4', 'NO23', 'TSS'))
site_levels <- list(POR=c('Power', 'Lone_Pine', 'Sycan', 'Godowa', 'SF', 'NF'),
                    RECENT=c('Power', 'Lone_Pine', 'Sycan', 'Godowa', 'SF_Ivory', 'SF', 'NF_Ivory', 'NF'))
for (dataset in c('POR', 'RECENT')) {
  cat(dataset, '\n')
  for (variable in variable_levels[[dataset]]) {
    cat('..', variable, '\n')
    pdf(file.path('pdf', tolower(dataset), 'loads-model', paste0('loads-model-', tolower(variable), '.pdf')), width=11, height=8.5)
    for (site in site_levels[[dataset]]) {
      cat('....', site, '\n')
      plot_flux_summary(loads[[dataset]][[variable]][[site]], site=site, variable=variable)
      plot_flux_monthly(loads[[dataset]][[variable]][[site]], site=site, variable=variable)
      plot_flux_residuals(loads[[dataset]][[variable]][[site]], site=site, variable=variable)
    }
    dev.off()
  }
}

# pdf: compare datasets ----
pdf('pdf/loads-model-fits.pdf', width=11, height=8.5)

dataset_descriptions <- c('RAW'='Raw Dataset before Removing/Fixing Erroneous Data Points\nWY2002-2014',
                          'CLEAN'='Raw Dataset with Removed/Fixed Erroneous Data Points\nWY2002-2014',
                          'POR'='Clean Dataset with Concentrations Limited to Upper Detection Limit\nWY2002-2014, Excludes Ivory Stations and TSS',
                          'RECENT'='Clean Dataset with Concentrations Limited to Lower Detection Limit\nWY2009-2014')
grid.arrange(gridExtra::tableGrob(data.frame(value=unname(dataset_descriptions)),
                                  rows=names(dataset_descriptions),
                                  cols=c('Dataset Descriptions'),
                                  theme=ttheme_minimal(rowhead=list(fg_params=list(fontface=2L, hjust=1)),
                                                       core=list(fg_params=list(hjust=0, x=0.05, vjust=0, y=0)))),
             top='\n\nComparison of Load Computations across Datasets')

wq.kt_sprague[['CLEAN']] %>%
  filter(VAR %in% lower_limits$VAR) %>%
  ggplot() +
  geom_point(aes(DATE, VALUE*1000), size=1) +
  geom_hline(aes(yintercept=DL*1000), data=upper_limits, color='red', linetype=2) +
  geom_hline(aes(yintercept=DL*1000), data=lower_limits, color='deepskyblue', linetype=2) +
  scale_y_log10(breaks=fluxr::log_breaks(c(1, 5), 10^seq(-3, 3))) +
  facet_grid(VAR~SITE_NAME, scales='free_y') +
  labs(x='Date', y='Concentration (ppb)') +
  ggtitle('Concentration Data for Clean Dataset with Upper and Lower Detection Limits') +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        legend.position='top')

wq.kt_sprague[['POR']] %>%
  filter(VAR %in% upper_limits$VAR) %>%
  ggplot() +
  geom_hline(aes(yintercept=DL*1000), data=upper_limits, color='red', linetype=2) +
  geom_point(aes(DATE, VALUE*1000, color=LIMITED), size=1) +
  scale_color_manual('Set to POR', values=c('FALSE'='black', 'TRUE'='deepskyblue')) +
  scale_y_log10(breaks=fluxr::log_breaks(c(1, 5), 10^seq(-3, 3))) +
  facet_grid(VAR~SITE_NAME, scales='free_y') +
  labs(x='Date', y='Concentration (ppb)') +
  ggtitle('Concentration Data for POR Dataset with Upper Detection Limit') +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        legend.position='top')

wq.kt_sprague[['POR']] %>%
  filter(VAR %in% upper_limits$VAR) %>%
  mutate(WYEAR=wyear(DATE)) %>%
  group_by(SITE_NAME, VAR, WYEAR, LIMITED) %>%
  summarise(N=n()) %>%
  ggplot(aes(WYEAR, N, fill=LIMITED)) +
  geom_bar(position='fill', stat='identity') +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual('Set to POR', values=c('FALSE'='grey50', 'TRUE'='deepskyblue')) +
  labs(x='Water Year', y='Percent of Samples Set to POR') +
  facet_grid(VAR~SITE_NAME) +
  ggtitle('Fraction of Samples Set to Upper Detection Limit in POR Dataset') +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        legend.position='top')

wq.kt_sprague[['RECENT']] %>%
  filter(VAR %in% lower_limits$VAR) %>%
  mutate(WYEAR=wyear(DATE)) %>%
  group_by(SITE_NAME, VAR, WYEAR, LIMITED) %>%
  summarise(N=n()) %>%
  ggplot(aes(factor(WYEAR), N, fill=LIMITED)) +
  geom_bar(position='fill', stat='identity') +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual('Set to Recent', values=c('FALSE'='grey50', 'TRUE'='deepskyblue')) +
  labs(x='Water Year', y='Percent of Samples Set to Recent') +
  facet_grid(VAR~SITE_NAME) +
  ggtitle('Fraction of Samples Set to Lower Detection Limit in RECENT Dataset') +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        legend.position='top')

ggplot(fits, aes(SITE_NAME, rse_L, fill=DATASET)) +
  geom_bar(stat='identity', position='dodge') +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_brewer(palette=6, type = 'qual', labels=c('RAW'='Raw', 'CLEAN'='Clean', 'POR'='POR', 'RECENT'='Recent')) +
  labs(x='', y='Relative Std. Error') +
  facet_grid(VAR~., scales='free_y') +
  ggtitle('Relative Standard Error of Load Model') +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))

ggplot(fits, aes(SITE_NAME, lm.adj.r.squared, fill=DATASET)) +
  geom_bar(stat='identity', position='dodge') +
  geom_hline(yint=0, color='grey50') +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_brewer(palette=6, type = 'qual', labels=c('RAW'='Raw', 'CLEAN'='Clean', 'POR'='POR', 'RECENT'='Recent')) +
  labs(x='', y='Adjusted R2') +
  facet_grid(VAR~., scales='free_y') +
  ggtitle('Adjusted R2 of Load Model') +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))

ggplot(compare_dataset, aes(WYEAR, L, fill=DATASET)) +
  geom_bar(stat='identity', position='dodge') +
  facet_grid(VAR ~ SITE_NAME, scales='free_y') +
  scale_fill_brewer(palette=6, type = 'qual', labels=c('RAW'='Raw', 'CLEAN'='Clean', 'POR'='POR', 'RECENT'='Recent')) +
  labs(x='Water Year', y='Annual Load (kg/yr)') +
  ggtitle('Annual Loads by Site, Variable and Dataset')

ggplot(compare_dataset, aes(WYEAR, C, fill=DATASET)) +
  geom_bar(stat='identity', position='dodge') +
  facet_grid(VAR ~ SITE_NAME, scales='free_y') +
  scale_fill_brewer(palette=6, type = 'qual', labels=c('RAW'='Raw', 'CLEAN'='Clean', 'POR'='POR', 'RECENT'='Recent')) +
  labs(x='Water Year', y='Annual FWM Conc (ppb)') +
  ggtitle('Annual FWM Concentration by Site, Variable and Dataset')

# # primary vs raw
# select(compare_dataset, SITE_NAME, WYEAR, VAR, DATASET, L) %>%
#   spread(DATASET, L) %>%
#   mutate(DIFF=RAW-CLEAN,
#          REL_DIFF=DIFF/CLEAN,
#          REL_DIFF=ifelse(abs(REL_DIFF) < 1e-5, 0, REL_DIFF)) %>%
#   ggplot(aes(WYEAR, REL_DIFF)) +
#   geom_hline(yint=0, color='grey30') +
#   geom_bar(stat='identity', position='dodge') +
#   scale_y_continuous(labels=scales::percent) +
#   labs(x='Water Year', y='Percent Change in Annual Load') +
#   facet_grid(VAR ~ SITE_NAME, scales='free_y') +
#   ggtitle('Percent Change in Annual Load: Clean Dataset vs. Raw Dataset')
#
# select(compare_dataset, SITE_NAME, WYEAR, VAR, DATASET, L) %>%
#   spread(DATASET, L) %>%
#   mutate(DIFF=RAW-CLEAN,
#          DIFF=ifelse(abs(DIFF) < 1e-3, 0, DIFF)) %>%
#   ggplot(aes(WYEAR, DIFF)) +
#   geom_hline(yint=0, color='grey30') +
#   geom_bar(stat='identity', position='dodge') +
#   labs(x='Water Year', y='Change in Annual Load (kg/yr)') +
#   facet_grid(VAR ~ SITE_NAME, scales='free_y') +
#   ggtitle('Absolute Change in Annual Load: Clean Dataset vs. Raw Dataset')
#
# select(compare_dataset, SITE_NAME, WYEAR, VAR, DATASET, C) %>%
#   spread(DATASET, C) %>%
#   mutate(DIFF=RAW-CLEAN,
#          REL_DIFF=DIFF/CLEAN,
#          REL_DIFF=ifelse(abs(REL_DIFF) < 1e-5, 0, REL_DIFF)) %>%
#   ggplot(aes(WYEAR, REL_DIFF)) +
#   geom_hline(yint=0, color='grey30') +
#   geom_bar(stat='identity', position='dodge') +
#   scale_y_continuous(labels=scales::percent) +
#   labs(x='Water Year', y='Percent Change in Annual FWM Concentration') +
#   facet_grid(VAR ~ SITE_NAME, scales='free_y') +
#   ggtitle('Percent Change in Annual FWM Concentration: Clean Dataset vs. Raw Dataset')
#
# select(compare_dataset, SITE_NAME, WYEAR, VAR, DATASET, C) %>%
#   spread(DATASET, C) %>%
#   mutate(DIFF=RAW-CLEAN,
#          DIFF=ifelse(abs(DIFF) < 1e-3, 0, DIFF)) %>%
#   ggplot(aes(WYEAR, DIFF)) +
#   geom_hline(yint=0, color='grey30') +
#   geom_bar(stat='identity', position='dodge') +
#   labs(x='Water Year', y='Change in Annual FWM Concentration (ppb)') +
#   facet_grid(VAR ~ SITE_NAME, scales='free_y') +
#   ggtitle('Absolute Change in Annual FWM Concentration: Clean Dataset vs. Raw Dataset')
#
# # primary vs limit
# select(compare_dataset, SITE_NAME, WYEAR, VAR, DATASET, L) %>%
#   spread(DATASET, L) %>%
#   mutate(DIFF=POR-CLEAN,
#          REL_DIFF=DIFF/CLEAN,
#          REL_DIFF=ifelse(abs(REL_DIFF) < 1e-5, 0, REL_DIFF)) %>%
#   ggplot(aes(WYEAR, REL_DIFF)) +
#   geom_hline(yint=0, color='grey30') +
#   geom_bar(stat='identity', position='dodge') +
#   scale_y_continuous(labels=scales::percent) +
#   labs(x='Water Year', y='Percent Change in Annual Load') +
#   facet_grid(VAR ~ SITE_NAME, scales='free_y') +
#   ggtitle('Percent Change in Annual Load: Clean Dataset vs. POR Dataset')
#
# select(compare_dataset, SITE_NAME, WYEAR, VAR, DATASET, L) %>%
#   spread(DATASET, L) %>%
#   mutate(DIFF=POR-CLEAN,
#          DIFF=ifelse(abs(DIFF) < 1e-3, 0, DIFF)) %>%
#   ggplot(aes(WYEAR, DIFF)) +
#   geom_hline(yint=0, color='grey30') +
#   geom_bar(stat='identity', position='dodge') +
#   labs(x='Water Year', y='Change in Annual Load (kg/yr)') +
#   facet_grid(VAR ~ SITE_NAME, scales='free_y') +
#   ggtitle('Absolute Change in Annual Load: Clean Dataset vs. POR Dataset')
#
# select(compare_dataset, SITE_NAME, WYEAR, VAR, DATASET, C) %>%
#   spread(DATASET, C) %>%
#   mutate(DIFF=POR-CLEAN,
#          REL_DIFF=DIFF/CLEAN,
#          REL_DIFF=ifelse(abs(REL_DIFF) < 1e-5, 0, REL_DIFF)) %>%
#   ggplot(aes(WYEAR, REL_DIFF)) +
#   geom_hline(yint=0, color='grey30') +
#   geom_bar(stat='identity', position='dodge') +
#   scale_y_continuous(labels=scales::percent) +
#   labs(x='Water Year', y='Percent Change in Annual FWM Concentration') +
#   facet_grid(VAR ~ SITE_NAME, scales='free_y') +
#   ggtitle('Percent Change in Annual FWM Concentration: Clean Dataset vs. POR Dataset')
#
# select(compare_dataset, SITE_NAME, WYEAR, VAR, DATASET, C) %>%
#   spread(DATASET, C) %>%
#   mutate(DIFF=POR-CLEAN,
#          DIFF=ifelse(abs(DIFF) < 1e-3, 0, DIFF)) %>%
#   ggplot(aes(WYEAR, DIFF)) +
#   geom_hline(yint=0, color='grey30') +
#   geom_bar(stat='identity', position='dodge') +
#   labs(x='Water Year', y='Change in Annual FWM Concentration (ppb)') +
#   facet_grid(VAR ~ SITE_NAME, scales='free_y') +
#   ggtitle('Absolute Change in Annual FWM Concentration: Clean Dataset vs. POR Dataset')

dev.off()

# pdf: parameter estimates ----
pdf('pdf/loads-model-parameters.pdf', width=11, height=8.5)
for (variable in unique(params$VAR)) {
  p.mean <- filter(params, VAR==variable) %>%
    ggplot(aes(SITE_NAME, estimate, fill=DATASET)) +
    geom_bar(stat='identity', position='dodge') +
    geom_hline(yint=0, color='grey50') +
    scale_fill_brewer(palette=6, type = 'qual', labels=c('RAW'='Raw', 'CLEAN'='Clean', 'POR'='POR', 'RECENT'='Recent')) +
    labs(x='', y='Parameter Estimate') +
    facet_grid(term~., scales='free_y') +
    ggtitle(paste0('Parameter Estimates')) +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
#   print(p)
  p.ste <- filter(params, VAR==variable) %>%
    ggplot(aes(SITE_NAME, std.error, fill=DATASET)) +
    geom_bar(stat='identity', position='dodge') +
    scale_fill_brewer(palette=6, type = 'qual', labels=c('RAW'='Raw', 'CLEAN'='Clean', 'POR'='POR', 'RECENT'='Recent')) +
    labs(x='', y='Parameter Std. Error') +
    facet_grid(term~., scales='free_y') +
    ggtitle(paste0('Parameter Std Error')) +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
#   print(p)
  grid.arrange(grobs=list(p.mean, p.ste),
               ncol=2,
               top=paste0('\nVariable: ', variable))
}
dev.off()