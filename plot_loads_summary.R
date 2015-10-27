library(dplyr)
library(tidyr)
library(lubridate)
library(fluxr)
library(ggplot2)
theme_set(theme_bw())
library(gridExtra)

rm(list=ls())

# load data ----
load('kt_sprague.Rdata')
load('loads.Rdata')
load('gis.Rdata')
source('functions.R')

dataset_levels <- names(loads)
site_name_levels <- levels(stn.kt_sprague$SITE_NAME)
term_colors <- c('L'='olivedrab3',
                 'L_AREA'='olivedrab3',
                 'C'='orangered',
                 'Q'='steelblue',
                 'Q_AREA'='steelblue')

# pdf: summary ----
for (dataset in c('POR')) {
  cat('Printing:', file.path('pdf', tolower(dataset), 'loads-summary-wyr.pdf'), '\n')
  pdf(file.path('pdf', tolower(dataset), 'loads-summary-wyr.pdf'), width=11, height=8.5)
  p <- filter(loads_df[['wyr']], DATASET==dataset, TERM=='C',
              SITE_NAME %in% site_name_levels,
              SEASON=="Annual") %>%
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
              SITE_NAME %in% site_name_levels,
              SEASON=="Annual") %>%
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
              SITE_NAME %in% site_name_levels,
              SEASON=="Annual") %>%
    ggplot(aes(factor(WYEAR), VALUE/N_DAY)) +
    geom_bar(stat='identity', fill=term_colors['L'], width=0.8) +
    scale_y_continuous(labels=scales::comma) +
    labs(x='Water Year', y='Load (kg/day)',
         title=paste0('Annual Load by Site and Variable  |  Dataset: ', dataset)) +
    facet_grid(VAR~SITE_NAME, scales='free_y') +
    theme_bw() +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=6),
          strip.text.x=element_text(size=8))
  print(p)

  p.q <- filter(loads_df[['wyr']], DATASET==dataset, VAR=='FLOW', TERM=='Q',
                SITE_NAME %in% site_name_levels,
                SEASON=="Annual") %>%
    ggplot(aes(factor(WYEAR), VALUE)) +
    geom_bar(stat='identity', fill=term_colors['Q'], width=0.8) +
    scale_y_continuous(labels=scales::comma) +
    labs(x='Water Year', y='Flow (hm3/d)',
         title=paste0('Annual Flow by Site  |  Dataset: ', dataset)) +
    facet_grid(.~SITE_NAME, scales='free_y') +
    theme_bw() +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=6),
          strip.text.x=element_text(size=8))
  p.q_area <- filter(loads_df[['wyr']], DATASET==dataset, VAR=='FLOW', TERM=='Q_AREA',
                     SITE_NAME %in% site_name_levels,
                     SEASON=="Annual") %>%
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


for (dataset in c('POR')) {
  filename <- file.path('pdf', tolower(dataset), 'loads-summary.pdf')
  cat('Printing:', filename, '\n')
  pdf(filename, width=11, height=8.5)
  variables <- setdiff(levels(loads_df[['wyr']]$VAR), 'FLOW')
  for (variable in variables) {
    cat('..', variable, '\n')
    p <- filter(loads_df[['wyr']], DATASET==dataset, VAR %in% c(variable, 'FLOW'),
                TERM %in% c('Q_AREA', 'L_AREA', 'C'),
                SITE_NAME %in% site_name_levels,
                SEASON=="Annual") %>%
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

periods <- list(P2002=c('2002-2014'),
                P2010=c('2010-2014', '2011-2014'))
dataset <- 'POR'
filename <- file.path('pdf', tolower(dataset), 'loads-summary-site.pdf')
cat('Printing:', filename, '\n')
pdf(filename, width=11, height=8.5)
lapply(periods, function(period) {
  p.q_area <- filter(loads_df[['site']], DATASET==dataset, VAR=='FLOW', TERM=='Q_AREA',
                     SEASON=="Annual", PERIOD %in% period,
                     SITE_NAME %in% site_name_levels) %>%
    ggplot(aes(SITE_NAME, VALUE)) +
    geom_bar(stat='identity', fill=term_colors['Q'], width=0.8) +
    scale_y_continuous(labels=scales::comma) +
    labs(x='', y='Flow per Area (cm/yr)',
         title=paste0('Annual Flow per Unit Area')) +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
  p.l_area <- filter(loads_df[['site']], DATASET==dataset, TERM=='L_AREA',
                     SEASON=="Annual", PERIOD %in% period,
                     SITE_NAME %in% site_name_levels) %>%
    ggplot(aes(SITE_NAME, VALUE)) +
    geom_bar(stat='identity', fill=term_colors['L'], width=0.8) +
    scale_y_continuous(labels=scales::comma) +
    labs(x='', y='Load per Area (kg/km2/yr)',
         title=paste0('Annual Load per Unit Area')) +
    facet_grid(VAR~., scales='free_y') +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
  p.c <- filter(loads_df[['site']], DATASET==dataset, TERM=='C',
                SEASON=="Annual", PERIOD %in% period,
                SITE_NAME %in% site_name_levels) %>%
    ggplot(aes(SITE_NAME, VALUE)) +
    geom_bar(stat='identity', fill=term_colors['C'], width=0.8) +
    scale_y_continuous(labels=scales::comma) +
    labs(x='', y='FWM Conc (ppb)',
         title=paste0('Annual FWM Concentration')) +
    facet_grid(VAR~., scales='free_y') +
    theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
  grid.arrange(grobs=list(ggplotGrob(p.c), ggplotGrob(p.l_area),
                          arrangeGrob(p.q_area, nrow=3)),
               nrow=1,
               top=paste0('\nFWM Concentration, Flow per Area, Load per Area by Site\nDataset: ', dataset,
                          ', Period: WY', period[1]))
})

p.q_area <- filter(loads_df[['site']], DATASET==dataset, VAR=='FLOW', TERM=='Q_AREA',
                   SEASON=="Annual",
                   SITE_NAME %in% site_name_levels) %>%
  ggplot(aes(SITE_NAME, VALUE, fill=PERIOD)) +
  geom_bar(stat='identity', width=0.8, position='dodge') +
  scale_y_continuous(labels=scales::comma) +
  labs(x='', y='Flow per Area (cm/yr)',
       title=paste0('Annual Flow per Unit Area')) +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
p.l_area <- filter(loads_df[['site']], DATASET==dataset, TERM=='L_AREA',
                   SEASON=="Annual",
                   SITE_NAME %in% site_name_levels) %>%
  ggplot(aes(SITE_NAME, VALUE, fill=PERIOD)) +
  geom_bar(stat='identity', width=0.8, position='dodge') +
  scale_y_continuous(labels=scales::comma) +
  labs(x='', y='Load per Area (kg/km2/yr)',
       title=paste0('Annual Load per Unit Area')) +
  facet_grid(VAR~., scales='free_y') +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
p.c <- filter(loads_df[['site']], DATASET==dataset, TERM=='C',
              SEASON=="Annual",
              SITE_NAME %in% site_name_levels) %>%
  ggplot(aes(SITE_NAME, VALUE, fill=PERIOD)) +
  geom_bar(stat='identity', width=0.8, position='dodge') +
  scale_y_continuous(labels=scales::comma) +
  labs(x='', y='FWM Conc (ppb)',
       title=paste0('Annual FWM Concentration')) +
  facet_grid(VAR~., scales='free_y') +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
grid.arrange(grobs=list(ggplotGrob(p.c), ggplotGrob(p.l_area),
                        arrangeGrob(p.q_area, nrow=3)),
             nrow=1,
             top=paste0('\nFWM Concentration, Flow per Area, Load per Area by Site\nDataset: ', dataset))
dev.off()

# pdf: diagnostics ----
variable_levels <- c('TP', 'PO4', 'TN', 'NH4', 'NO23', 'TSS')
site_levels <- c('Power', 'Lone_Pine', 'Sycan', 'Godowa', 'SF_Ivory', 'SF', 'NF_Ivory', 'NF')
for (dataset in c('POR')) {
  if (!file.exists(file.path('pdf', tolower(dataset), 'loads-model'))) {
    dir.create(file.path('pdf', tolower(dataset), 'loads-model'))
  }
  for (variable in variable_levels) {
    filename <- file.path('pdf', tolower(dataset), 'loads-model', paste0('loads-model-', tolower(variable), '.pdf'))
    cat('Printing:', filename, '\n')
    pdf(filename, width=11, height=8.5)
    for (site in site_levels) {
      cat('..', site, '\n')
      plot_flux_summary(loads[[dataset]][[variable]][[site]], site=site, variable=variable)
      plot_flux_monthly(loads[[dataset]][[variable]][[site]], site=site, variable=variable)
      plot_flux_residuals(loads[[dataset]][[variable]][[site]], site=site, variable=variable)
    }
    dev.off()
  }
}

# pdf: compare datasets ----
# filename <- 'pdf/loads-model-fits.pdf'
# cat('Printing:', filename, '\n')
# pdf(filename, width=11, height=8.5)
#
# dataset_descriptions <- c('RAW'='Raw Dataset before Removing/Fixing Erroneous Data Points\nWY2002-2014',
#                           'CLEAN'='Raw Dataset with Removed/Fixed Erroneous Data Points\nWY2002-2014',
#                           'POR'='Clean Dataset with Concentrations Limited to Upper Detection Limit\nWY2002-2014, Excludes Ivory Stations and TSS',
#                           'RECENT'='Clean Dataset with Concentrations Limited to Lower Detection Limit\nWY2009-2014')
# grid.arrange(gridExtra::tableGrob(data.frame(value=unname(dataset_descriptions)),
#                                   rows=names(dataset_descriptions),
#                                   cols=c('Dataset Descriptions'),
#                                   theme=ttheme_minimal(rowhead=list(fg_params=list(fontface=2L, hjust=1)),
#                                                        core=list(fg_params=list(hjust=0, x=0.05, vjust=0, y=0)))),
#              top='\n\nComparison of Load Computations across Datasets')
#
# p <- wq.kt_sprague[['CLEAN']] %>%
#   filter(VAR %in% lower_limits$VAR) %>%
#   ggplot() +
#   geom_point(aes(DATE, VALUE*1000), size=1) +
#   geom_hline(aes(yintercept=DL*1000), data=upper_limits, color='red', linetype=2) +
#   geom_hline(aes(yintercept=DL*1000), data=lower_limits, color='deepskyblue', linetype=2) +
#   scale_y_log10(breaks=fluxr::log_breaks(c(1, 5), 10^seq(-3, 3))) +
#   facet_grid(VAR~SITE_NAME, scales='free_y') +
#   labs(x='Date', y='Concentration (ppb)') +
#   ggtitle('Concentration Data for Clean Dataset with Upper and Lower Detection Limits') +
#   theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
#         legend.position='top')
# print(p)
#
# p <- wq.kt_sprague[['POR']] %>%
#   filter(VAR %in% upper_limits$VAR) %>%
#   ggplot() +
#   geom_hline(aes(yintercept=DL*1000), data=upper_limits, color='red', linetype=2) +
#   geom_point(aes(DATE, VALUE*1000, color=LIMITED), size=1) +
#   scale_color_manual('Set to POR', values=c('FALSE'='black', 'TRUE'='deepskyblue')) +
#   scale_y_log10(breaks=fluxr::log_breaks(c(1, 5), 10^seq(-3, 3))) +
#   facet_grid(VAR~SITE_NAME, scales='free_y') +
#   labs(x='Date', y='Concentration (ppb)') +
#   ggtitle('Concentration Data for POR Dataset with Upper Detection Limit') +
#   theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
#         legend.position='top')
# print(p)
#
# p <- wq.kt_sprague[['POR']] %>%
#   filter(VAR %in% upper_limits$VAR) %>%
#   mutate(WYEAR=wyear(DATE)) %>%
#   group_by(SITE_NAME, VAR, WYEAR, LIMITED) %>%
#   summarise(N=n()) %>%
#   ggplot(aes(WYEAR, N, fill=LIMITED)) +
#   geom_bar(position='fill', stat='identity') +
#   scale_y_continuous(labels=scales::percent) +
#   scale_fill_manual('Set to POR', values=c('FALSE'='grey50', 'TRUE'='deepskyblue')) +
#   labs(x='Water Year', y='Percent of Samples Set to POR') +
#   facet_grid(VAR~SITE_NAME) +
#   ggtitle('Fraction of Samples Set to Upper Detection Limit in POR Dataset') +
#   theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
#         legend.position='top')
# print(p)
#
# p <- wq.kt_sprague[['RECENT']] %>%
#   filter(VAR %in% lower_limits$VAR) %>%
#   mutate(WYEAR=wyear(DATE)) %>%
#   group_by(SITE_NAME, VAR, WYEAR, LIMITED) %>%
#   summarise(N=n()) %>%
#   ggplot(aes(factor(WYEAR), N, fill=LIMITED)) +
#   geom_bar(position='fill', stat='identity') +
#   scale_y_continuous(labels=scales::percent) +
#   scale_fill_manual('Set to Recent', values=c('FALSE'='grey50', 'TRUE'='deepskyblue')) +
#   labs(x='Water Year', y='Percent of Samples Set to Recent') +
#   facet_grid(VAR~SITE_NAME) +
#   ggtitle('Fraction of Samples Set to Lower Detection Limit in RECENT Dataset') +
#   theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
#         legend.position='top')
# print(p)
#
# p <- ggplot(fits, aes(SITE_NAME, rse_L, fill=DATASET)) +
#   geom_bar(stat='identity', position='dodge') +
#   scale_y_continuous(labels=scales::percent) +
#   scale_fill_brewer(palette=6, type = 'qual', labels=c('RAW'='Raw', 'CLEAN'='Clean', 'POR'='POR', 'RECENT'='Recent')) +
#   labs(x='', y='Relative Std. Error') +
#   facet_grid(VAR~., scales='free_y') +
#   ggtitle('Relative Standard Error of Load Model') +
#   theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
# print(p)
#
# p <- ggplot(fits, aes(SITE_NAME, lm.adj.r.squared, fill=DATASET)) +
#   geom_bar(stat='identity', position='dodge') +
#   geom_hline(yint=0, color='grey50') +
#   scale_y_continuous(labels=scales::percent) +
#   scale_fill_brewer(palette=6, type = 'qual', labels=c('RAW'='Raw', 'CLEAN'='Clean', 'POR'='POR', 'RECENT'='Recent')) +
#   labs(x='', y='Adjusted R2') +
#   facet_grid(VAR~., scales='free_y') +
#   ggtitle('Adjusted R2 of Load Model') +
#   theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
# print(p)
#
# p <- ggplot(compare_dataset, aes(WYEAR, L, fill=DATASET)) +
#   geom_bar(stat='identity', position='dodge') +
#   facet_grid(VAR ~ SITE_NAME, scales='free_y') +
#   scale_fill_brewer(palette=6, type = 'qual', labels=c('RAW'='Raw', 'CLEAN'='Clean', 'POR'='POR', 'RECENT'='Recent')) +
#   labs(x='Water Year', y='Annual Load (kg/yr)') +
#   ggtitle('Annual Loads by Site, Variable and Dataset')
# print(p)
#
# p <- ggplot(compare_dataset, aes(WYEAR, C, fill=DATASET)) +
#   geom_bar(stat='identity', position='dodge') +
#   facet_grid(VAR ~ SITE_NAME, scales='free_y') +
#   scale_fill_brewer(palette=6, type = 'qual', labels=c('RAW'='Raw', 'CLEAN'='Clean', 'POR'='POR', 'RECENT'='Recent')) +
#   labs(x='Water Year', y='Annual FWM Conc (ppb)') +
#   ggtitle('Annual FWM Concentration by Site, Variable and Dataset')
# print(p)
#
# dev.off()
#
# # pdf: parameter estimates ----
# filename <- 'pdf/loads-model-parameters.pdf'
# cat('Printing:', filename, '\n')
# pdf(filename, width=11, height=8.5)
# for (variable in unique(params$VAR)) {
#   p.mean <- filter(params, VAR==variable) %>%
#     ggplot(aes(SITE_NAME, estimate, fill=DATASET)) +
#     geom_bar(stat='identity', position='dodge') +
#     geom_hline(yint=0, color='grey50') +
#     scale_fill_brewer(palette=6, type = 'qual', labels=c('RAW'='Raw', 'CLEAN'='Clean', 'POR'='POR', 'RECENT'='Recent')) +
#     labs(x='', y='Parameter Estimate') +
#     facet_grid(term~., scales='free_y') +
#     ggtitle(paste0('Parameter Estimates')) +
#     theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
# #   print(p)
#   p.ste <- filter(params, VAR==variable) %>%
#     ggplot(aes(SITE_NAME, std.error, fill=DATASET)) +
#     geom_bar(stat='identity', position='dodge') +
#     scale_fill_brewer(palette=6, type = 'qual', labels=c('RAW'='Raw', 'CLEAN'='Clean', 'POR'='POR', 'RECENT'='Recent')) +
#     labs(x='', y='Parameter Std. Error') +
#     facet_grid(term~., scales='free_y') +
#     ggtitle(paste0('Parameter Std Error')) +
#     theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
# #   print(p)
#   grid.arrange(grobs=list(p.mean, p.ste),
#                ncol=2,
#                top=paste0('\nVariable: ', variable))
# }
# dev.off()

# report ----
loads_wyr_por_tp <- lapply(names(loads[['POR']][['TP']]), function (site_name) {
  x <- loads[['POR']][['TP']][[site_name]][['out']][['wyr']]
  x$SITE_NAME <- site_name
  x
}) %>%
  rbind_all %>%
  left_join(select(subbasin_area, SITE_NAME, AREA_KM2)) %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=site_name_levels)) %>%
  droplevels %>%
  select(SITE_NAME, AREA_KM2, WYEAR, Q_mean=Q, L_mean=L, C_mean=C, L_se, C_se) %>%
  mutate(QAREA_mean=Q_mean/AREA_KM2*100, # m/yr -> cm/yr
         LAREA_mean=L_mean/AREA_KM2,
         LAREA_se=L_se/AREA_KM2) %>%
  gather(TERM_STAT, VALUE, Q_mean:LAREA_se) %>%
  separate(TERM_STAT, c('TERM', 'STAT')) %>%
  spread(STAT, VALUE)

loads_wyr_por <- lapply(names(loads[['POR']]), function (variable) {
    lapply(names(loads[['POR']][[variable]]), function (site_name) {
      x <- loads[['POR']][[variable]][[site_name]][['out']][['wyr']]
      x$SITE_NAME <- site_name
      x$VAR <- variable
      x
    }) %>%
      rbind_all
  }) %>%
  rbind_all %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=site_name_levels),
         VAR=ordered(VAR, levels=names(loads[['POR']])))

filename <- 'report/results-load-annual-tp.png'
cat('Saving report figure:', filename, '\n')
png(filename, width=10, height=8, res=200, units='in')
p <- ggplot(loads_wyr_por_tp, aes(factor(WYEAR), mean, fill=TERM)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=0.4, size=0.2) +
  scale_fill_manual('', values=c(C='orangered', LAREA='olivedrab3', L='olivedrab3', QAREA='steelblue', Q='steelblue')) +
  facet_grid(TERM~SITE_NAME, scales='free_y') +
  scale_x_discrete(labels=c(2002, "", 2004, "", 2006, "", 2008, "", 2010, "", 2012, "", 2014)) +
  guides(fill='none') +
  labs(x='Water Year',
       y=paste(c('    Runoff (cm/yr)   ',
                 '    Flow (hm3/yr)    ',
                 'TP Export (kg/km2/yr)',
                 '  TP Load (kg/yr)    ',
                 '  FWM TP Conc (ppb)  '),
               collapse='     ')) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
        axis.title.y=element_text(size=10),
        strip.background=element_blank(),
        strip.text.y=element_blank(),
        strip.text.x=element_text(face='bold'))
print(p)
dev.off()

filename <- 'report/results-load-annual.png'
cat('Saving report figure:', filename, '\n')
png(filename, width=10, height=8, res=200, units='in')
p <- ggplot(loads_wyr_por, aes(factor(WYEAR), C)) +
  geom_bar(stat='identity', fill='orangered') +
  geom_errorbar(aes(ymin=C-C_se, ymax=C+C_se), width=0.4, size=0.2) +
  facet_grid(VAR~SITE_NAME, scales='free_y') +
  scale_x_discrete(labels=c(2002, "", 2004, "", 2006, "", 2008, "", 2010, "", 2012, "", 2014)) +
  scale_y_continuous(labels=scales::comma) +
  labs(x='Water Year',
       y='FWM Concentration (ppb)') +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()


flows_day <- filter(loads_df[['day']],
                    DATASET=="POR",
                    TERM %in% c("Q", "Q_AREA")) %>%
  droplevels %>%
  spread(TERM, VALUE) %>%
  mutate(Q=hm3d_cfs(Q))

flows_mon <- filter(loads_df[['mon']],
                    DATASET=="POR",
                    TERM %in% c("Q", "Q_AREA")) %>%
  droplevels %>%
  spread(TERM, VALUE) %>%
  mutate(Q=hm3d_cfs(Q))


flows_site <- filter(loads_df[['site']],
                    DATASET=="POR",
                    PERIOD=="2010-2014",
                    TERM %in% c("Q", "Q_AREA")) %>%
  droplevels %>%
  spread(TERM, VALUE) %>%
  mutate(Q=hm3d_cfs(Q))


filename <- 'report/flows-seasonal.png'
cat('Saving report figure:', filename, '\n')
png(filename, width=10, height=4, res=200, units='in')
p <- flows_site %>%
  filter(SITE_NAME %in% c("Power", "Lone_Pine", "Godowa", "Sycan", "NF_Ivory", "NF", "SF", "SF_Ivory",
                          "SF_Ivory+NF_Ivory", "Godowa+Sycan")) %>%
  ggplot(aes(SITE_NAME, Q)) +
  geom_bar(stat='identity', fill='grey50') +
  facet_wrap(~SEASON, nrow=1) +
  labs(x="", y="Mean Flow (cfs)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()

filename <- 'report/flows-net.png'
cat('Saving report figure:', filename, '\n')
png(filename, width=10, height=4, res=200, units='in')
p <- flows_site %>%
  filter(SITE_NAME %in% c("Power-Lone_Pine", "Lone_Pine-Godowa-Sycan", "Sycan", "Godowa-SF_Ivory-NF_Ivory",
                          "SF_Ivory-SF", "NF_Ivory-NF", "SF", "NF")) %>%
  mutate(SITE_NAME=plyr::revalue(SITE_NAME, incbasin_names),
         DIR=Q>0,
         DIR=ordered(DIR, levels=c("TRUE", "FALSE"))) %>%
  ggplot(aes(SITE_NAME, Q, fill=DIR)) +
  geom_bar(stat='identity') +
  geom_hline(yint=0, fill='grey20') +
  scale_fill_manual('',
                    values=c('TRUE'='steelblue', 'FALSE'='orangered'),
                    labels=c('TRUE'='Increase', 'FALSE'='Decrease')) +
  facet_wrap(~SEASON, nrow=1) +
  labs(x="", y="Net Flow (cfs)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
        strip.background=element_blank(),
        strip.text=element_text(face='bold'))
print(p)
dev.off()

log_x <- scale_x_log10(breaks=log_breaks(seq(1, 9), 10^seq(-3, 3)),
                       labels=log_labels(c(1, 5), 10^seq(-3, 3)))
log_y <- scale_y_log10(breaks=log_breaks(seq(1, 9), 10^seq(-3, 3)),
                       labels=log_labels(c(1, 5), 10^seq(-3, 3)))

load('owrd.Rdata')
load('usgs.Rdata')

q.usgs <- filter(q.usgs, SITE_NAME=="Power")
q.owrd <- filter(q.owrd, SITE_NAME=="Lone_Pine")
q.obs <- rbind(select(q.usgs, -FLAG), q.owrd)

filename <- 'report/flows-power-lonepine-scatter.png'
cat('Saving report figure:', filename, '\n')
png(filename, width=10, height=4, res=200, units='in')

p1 <- select(flows_day, DATE, SITE_NAME, Q) %>%
  spread(SITE_NAME, Q) %>%
  ggplot(aes(Lone_Pine, Power)) +
  geom_point(size=1, alpha=0.5) +
  geom_abline(linetype='dashed', color='red') +
  log_x +
  log_y +
  theme(aspect.ratio=1) +
  labs(x="Flow @ Lone_Pine (cfs)", y="Flow @ Power (cfs)",
       title="(a) Interpolated Daily Flows at WQ Stations")
p2 <- select(q.obs, SITE_NAME, DATE, FLOW) %>%
  spread(SITE_NAME, FLOW) %>%
  filter(!is.na(Lone_Pine)) %>%
  ggplot(aes(Lone_Pine, Power)) +
  geom_point(size=1, alpha=0.5) +
  geom_abline(linetype='dashed', color='red') +
  log_x +
  log_y +
  theme(aspect.ratio=1) +
  labs(x="Flow @ OWRD:11500500 (Lone_Pine) (cfs)",
       y="Flow @ USGS:11501000 (Power) (cfs)",
       title="(b) Observed at USGS/OWRD Stations")

grid.arrange(grobs=list(p1, p2),
             nrow=1)
dev.off()


filename <- 'report/flows-power-lonepine-scatter-mon.png'
cat('Saving report figure:', filename, '\n')
png(filename, width=10, height=4, res=200, units='in')

p1 <- select(flows_mon, MONTHYEAR, SITE_NAME, Q) %>%
  spread(SITE_NAME, Q) %>%
  ggplot(aes(Lone_Pine, Power)) +
  geom_point(size=1.5) +
  geom_abline(linetype='dashed', color='red') +
  log_x +
  log_y +
  theme(aspect.ratio=1) +
  labs(x="Flow @ Lone_Pine (cfs)", y="Flow @ Power (cfs)",
       title="(a) Interpolated Flows at WQ Stations")
p2 <- select(q.obs, SITE_NAME, DATE, FLOW) %>%
  mutate(DATE=floor_date(DATE, unit="month")) %>%
  group_by(SITE_NAME, DATE) %>%
  summarise(FLOW=mean(FLOW, na.rm=TRUE)) %>%
  ungroup %>%
  spread(SITE_NAME, FLOW) %>%
  filter(!is.na(Lone_Pine)) %>%
  ggplot(aes(Lone_Pine, Power)) +
  geom_point(size=1.5) +
  geom_abline(linetype='dashed', color='red') +
  log_x +
  log_y +
  theme(aspect.ratio=1) +
  labs(x="Flow @ OWRD:11500500 (Lone_Pine) (cfs)",
       y="Flow @ USGS:11501000 (Power) (cfs)",
       title="(b) Observed at USGS/OWRD Stations")

grid.arrange(grobs=list(p1, p2),
             nrow=1)
dev.off()

filename <- 'report/flows-power-lonepine-ts.png'
cat('Saving report figure:', filename, '\n')
png(filename, width=8, height=4, res=200, units='in')
p <- select(flows_day, WYEAR, DATE, SITE_NAME, Q) %>%
  filter(SITE_NAME %in% c("Power", "Lone_Pine"),
         WYEAR >= 2010) %>%
  ggplot(aes(DATE, Q, color=SITE_NAME)) +
  geom_line() +
  labs(x="Date", y="Flow (cfs)") +
  scale_color_manual('', values=c('Power'='steelblue', 'Lone_Pine'='orangered')) +
  scale_x_datetime(breaks=scales::date_breaks("6 months"),
                   labels=scales::date_format("%b %Y")) +
  log_y +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
print(p)
dev.off()
