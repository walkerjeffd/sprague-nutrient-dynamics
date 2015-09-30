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

stn_subbasin <- list(P2002=c('Power', 'Lone_Pine', 'Sycan', 'Godowa', 'SF', 'NF'),
                    P2010=c('Power', 'Lone_Pine', 'Sycan', 'Godowa', 'SF_Ivory', 'SF', 'NF_Ivory', 'NF'))
stn_incbasin <- list(P2002=c('Power-Lone_Pine', 'Lone_Pine-Godowa-Sycan', 'Godowa-SF-NF'),
                   P2010=c('Power-Lone_Pine', 'Lone_Pine-Godowa-Sycan', 'Godowa-SF_Ivory-NF_Ivory', 'SF_Ivory-SF', 'NF_Ivory-NF'))
variables <- list(P2002=c('TP', 'PO4', 'PP', 'TN', 'NH4', 'NO23'),
                  P2010=c('TP', 'PO4', 'PP', 'TN', 'NH4', 'NO23', 'TSS'))
wyears <- list(P2002=c(2002, 2014),
               P2010=c(2010, 2014))
period_labels <- list(P2002='WY2002-2014',
                      P2010='WY2010-2014')
term_names <- c('Q'='Flow', 'Q_AREA'='Flow per Area', 'C'='FWM Concentration', 'L'='Load', 'L_AREA'='Load per Area')

wyr_labels <- seq(2002, 2014)
wyr_labels[as.logical(wyr_labels %% 2)] <- ""
mon_labels <- c(10:12, 1:9)
mon_labels[as.logical(mon_labels %% 2)] <- ""
mon_labels <- rev(mon_labels)

rm(loads)
df_mon <- loads_df[['mon']]

# plot_tiles <- function(dataset, period, variable, term, sites, log.trans=TRUE) {
#   x <- filter(df_mon, DATASET==dataset, VAR==variable,
#               WYEAR >= wyears[[period]][1],
#               WYEAR <= wyears[[period]][2],
#               TERM==term, SITE_NAME %in% sites) %>%
#     mutate(MONTH=ordered(MONTH, levels=rev(c(10:12, 1:9))))
#   lbl <- term
#   if (log.trans) {
#     x <- mutate(x, VALUE=log10(VALUE))
#     lbl <- paste0('log10(', term, ')')
#   }
#   p <- x %>%
#     ggplot(aes(factor(WYEAR), MONTH, fill=VALUE)) +
#     geom_tile() +
#     facet_wrap(~SITE_NAME, nrow=2)
#   p <- p + scale_fill_gradientn(lbl, colours=rev(scales::brewer_pal(type = "seq", palette = 'GnBu')(9)))
#   p <- p +
#     theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
#           strip.background=element_blank()) +
#     labs(x='Water Year', y='Month') +
#     ggtitle(paste0('Period: ', period_labels[[period]],
#                    ' | Variable: ', variable,
#                    ' | Term: ', term_names[[term]]))
#   print(p)
# }
# plot_tiles('POR', 'P2002', 'TP', 'C', stn_subbasin[['P2002']])
# plot_tiles('POR', 'P2002', 'TP', 'C', stn_incbasin[['P2002']], log.trans = FALSE)
#
# dataset <- 'POR'
# for (period in c('P2002', 'P2010')) {
#   filename <- file.path('pdf', tolower(dataset),
#                         paste0('loads-tiles-sites', period_labels[[period]], '.pdf'))
#   cat('Printing:', filename, '\n')
#   cat(dataset, '\n')
#   pdf(filename, width=11, height=8.5)
#   cat('..', 'FLOW', '\n')
#   plot_tiles(dataset, 'FLOW', 'Q')
#   plot_tiles(dataset, 'FLOW', 'Q_AREA')
#   for (variable in variables[[period]]) {
#     cat('..', variable, '\n')
#     plot_tiles(dataset, variable, 'C')
#     plot_tiles(dataset, variable, 'L_AREA')
#     plot_tiles(dataset, variable, 'L')
#
#   }
#   dev.off()
# }
#
# for (dataset in c('POR', 'RECENT')) {
#   filename <- file.path('pdf', tolower(dataset), paste0('loads-tiles-incremental-basins.pdf'))
#   cat('Printing:', filename, '\n')
#   cat(dataset, '\n')
#   pdf(filename, width=11, height=8.5)
#   cat('..', 'FLOW', '\n')
#   plot_tiles(dataset, 'FLOW', 'Q', sites=stn_incbasin)
#   plot_tiles(dataset, 'FLOW', 'Q_AREA', sites=stn_incbasin)
#   for (variable in variables[[dataset]]) {
#     cat('..', variable, '\n')
#     plot_tiles(dataset, variable, 'C', sites=stn_incbasin, log.trans=FALSE)
#     plot_tiles(dataset, variable, 'L_AREA', sites=stn_incbasin)
#     plot_tiles(dataset, variable, 'L', sites=stn_incbasin)
#   }
#   dev.off()
# }

dataset <- 'POR'
term <- 'C'
plot_tiles_large <- function(dataset, period, sites, variables, term) {
  x <- filter(df_mon, DATASET==dataset, TERM==term,
              VAR %in% variables,
              SITE_NAME %in% sites) %>%
    mutate(MONTH=ordered(MONTH, levels=rev(c(10:12, 1:9))),
           VALUE=log10(VALUE)) %>%
    group_by(VAR) %>%
    mutate(VALUE=scale(VALUE)) %>%
    ungroup
  title <- paste0('Period: ', period_labels[[period]],
                  ' | Term: ', term_names[[term]],
                  '\nStandardized log10[Value] by Variable')

  if (term %in% c('Q', 'Q_AREA')) {
    p <- x %>%
      ggplot(aes(factor(WYEAR), MONTH, fill=VALUE)) +
      geom_tile() +
      facet_grid(VAR~SITE_NAME) +
      scale_fill_gradientn(term, colours=rev(scales::brewer_pal(type = "seq", palette = 'GnBu')(9))) +
      scale_x_discrete(labels=wyr_labels) +
      scale_y_discrete(labels=mon_labels) +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
            axis.text.y=element_text(size=8),
            panel.grid.major=element_blank(),
            strip.background=element_blank()) +
      labs(x='Water Year', y='Month') +
      ggtitle(title)
    grid.arrange(p, nrow=2, heights=c(1/3, 2/3))
  } else {
    p <- x %>%
      ggplot(aes(factor(WYEAR), MONTH, fill=VALUE)) +
      geom_tile() +
      facet_grid(VAR~SITE_NAME) +
      scale_fill_gradientn(term, colours=rev(scales::brewer_pal(type = "seq", palette = 'GnBu')(9))) +
      scale_x_discrete(labels=wyr_labels) +
      scale_y_discrete(labels=mon_labels) +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
            axis.text.y=element_text(size=8),
            panel.grid.major=element_blank(),
            strip.background=element_blank()) +
      labs(x='Water Year', y='Month') +
      ggtitle(title)
    print(p)
  }
}
# plot_tiles_large(dataset='POR', period='P2002',
#                  sites=stn_subbasin[['P2002']],
#                  variables=variables[['P2002']],
#                  term='C')
# plot_tiles_large(dataset='POR', period='P2002',
#                  sites=stn_subbasin[['P2002']],
#                  variables='FLOW',
#                  term='Q_AREA')

dataset <- 'POR'
for (period in c('P2002')) {
  filename <- file.path('pdf', tolower(dataset), 'loads-tiles-overview.pdf')
  cat('Printing:', filename, '\n')
  pdf(filename, width=11, height=8.5)
  plot_tiles_large(dataset=dataset, period=period,
                   sites=stn_subbasin[['P2010']],
                   variables=variables[['P2010']],
                   term='C')
  plot_tiles_large(dataset=dataset, period=period,
                   sites=stn_subbasin[['P2010']],
                   variables=variables[['P2010']],
                   term='L_AREA')
  plot_tiles_large(dataset=dataset, period=period,
                   sites=stn_subbasin[['P2010']],
                   variables='FLOW',
                   term='Q_AREA')
  dev.off()
}

# report ----
df_mon_report <- filter(loads_df[['mon']],
                        DATASET=="POR",
                        SITE_NAME %in% stn.kt_sprague$SITE_NAME) %>%
  filter(TERM %in% c("Q", "C")) %>%
  # filter(VAR != "PP") %>%
  arrange(VAR, SITE_NAME) %>%
  mutate(SITE_NAME=ordered(as.character(SITE_NAME),
                           levels=levels(stn.kt_sprague$SITE_NAME)))
df_mon_report$VAR_LABEL <- ifelse(df_mon_report$VAR=="FLOW", "Flow", paste0(df_mon_report$VAR, " Conc"))
df_mon_report$VAR_LABEL <- ordered(df_mon_report$VAR_LABEL, levels=unique(df_mon_report$VAR_LABEL))

df_mon_report.tile <- mutate(df_mon_report,
                      MONTH=ordered(MONTH, levels=rev(c(10:12, 1:9))),
                      VALUE=log10(VALUE)) %>%
  group_by(VAR_LABEL) %>%
  mutate(VALUE=scale(VALUE)) %>%
  ungroup

p <- df_mon_report.tile %>%
  ggplot(aes(factor(WYEAR), MONTH, fill=VALUE)) +
  geom_tile() +
  facet_grid(VAR_LABEL~SITE_NAME) +
  scale_fill_gradientn('Standardized\nLog(Value)',
                       colours=rev(scales::brewer_pal(type = "seq",
                                                      palette = 'GnBu')(9))) +
  scale_x_discrete(labels=wyr_labels) +
  scale_y_discrete(labels=mon_labels) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
        axis.text.y=element_text(size=8),
        panel.grid=element_blank(),
        strip.background=element_blank(),
        strip.text=element_text(face='bold')) +
  labs(x='Water Year', y='Month')

filename <- 'report/results-load-tile-mon.png'
cat("Saving report figure to:", filename, '\n')
png(filename, width=10, height=8, res=200, units='in')
print(p)
dev.off()

# # annual tile plot
# df_wyr_report <- filter(loads_df[['wyr']],
#                         DATASET=="POR",
#                         SITE_NAME %in% stn.kt_sprague$SITE_NAME,
#                         TERM %in% c("Q", "C")) %>%
#   mutate(VAR_LABEL=ifelse(VAR=="FLOW", "Flow", paste0(VAR, " Conc")),
#          VAR_LABEL=ordered(VAR_LABEL, levels=unique(VAR_LABEL))) %>%
#   mutate(VALUE_LOG=log10(VALUE)) %>%
#   group_by(VAR_LABEL) %>%
#   mutate(VALUE_SCALE=as.numeric(scale(VALUE_LOG))) %>%
#   ungroup %>%
#   arrange(VAR) %>%
#   mutate(VAR_LABEL=ordered(VAR_LABEL, levels=unique(VAR_LABEL)))
#
# stopifnot(
#   group_by(df_wyr_report, SEASON, WYEAR, VAR_LABEL, SITE_NAME) %>%
#     mutate(N=n()) %>%
#     filter(N>1) %>%
#     nrow == 0
# )
#
# # limit low values to -3
# df_wyr_report$VALUE_SCALE <- with(df_wyr_report, ifelse(VALUE_SCALE < -3, -3, VALUE_SCALE))
#
# p <- df_wyr_report %>%
#   ggplot(aes(factor(WYEAR), SITE_NAME, fill=VALUE_SCALE)) +
#   geom_tile() +
#   facet_grid(VAR_LABEL~SEASON) +
#   scale_fill_gradientn('Std. Value',
#                        colours=rev(scales::brewer_pal(type = "seq",
#                                                       palette = 'GnBu')(9))) +
#   scale_x_discrete(labels=wyr_labels) +
#   theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=10),
#         axis.text.y=element_text(size=7),
#         panel.grid=element_blank(),
#         strip.background=element_blank()) +
#   labs(x='Water Year', y='Station')
#
# png('report/results-load-tile-wyr.png', width=10, height=8, res=200, units='in')
# print(p)
# dev.off()


# line chart variations
# df_wyr.tile %>%
#   ggplot(aes(WYEAR, VALUE, color=SEASON)) +
#   geom_line() +
#   facet_grid(VAR_LABEL~SITE_NAME, scales='free_y') +
#   scale_y_log10() +
#   labs(x='Water Year', y='Value')
# df_wyr.tile %>%
#   ggplot(aes(WYEAR, VALUE, color=SITE_NAME)) +
#   geom_line() +
#   facet_grid(VAR_LABEL~SEASON, scales='free_y') +
#   scale_x_continuous(breaks=seq(2002, 2014, by=2)) +
#   scale_y_log10() +
#   labs(x='Water Year', y='Value')

cat('\n\n')