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
load('nlcd.Rdata')
load('gis.Rdata')
load('geomorph.Rdata')
load('pou.Rdata')

source('functions.R')

units <- c(Q='hm3/d', L='kg/d', C='ppb')
term_labels <- c(Q='Flow (hm3/d)', L='Load (kg/d)', C='Conc (ppb)')
period_labels <- c(P2010='WY2010-2020',
                   P2002='WY2002-2020')
period_wyears <- c(P2010=c(2010, 2020),
                   P2002=c(2002, 2020))
variables <- list(P2010=c('TP', 'PO4', 'PP', 'TN', 'NH4', 'NO23', 'TSS'),
                  P2002=c('TP', 'PO4', 'PP', 'TN', 'NH4', 'NO23'))
stn_subbasin <- list(P2010=c('Power', 'Lone_Pine', 'Godowa+Sycan', 'Godowa', 'Sycan', 'SF_Ivory+NF_Ivory', 'SF_Ivory', 'NF_Ivory', 'SF', 'NF'),
                     P2002=c('Power', 'Lone_Pine', 'Godowa+Sycan', 'Godowa', 'Sycan', 'SF+NF', 'SF', 'NF'))

subbasin_area_basin <- select(geomorph_subbasin, SITE_NAME, AREA_KM2=BASIN_AREA_KM2) %>%
  mutate(EXTENT='basin')
subbasin_area_valley <- select(geomorph_subbasin, SITE_NAME, AREA_KM2=VALLEY_AREA_KM2) %>%
  mutate(EXTENT='valley')
subbasin_area <- rbind(subbasin_area_basin, subbasin_area_valley) %>%
  spread(SITE_NAME, AREA_KM2) %>%
  mutate('Godowa+Sycan'=Godowa+Sycan,
         'SF+NF'=SF+NF,
         'SF_Ivory+NF_Ivory'=SF_Ivory+NF_Ivory) %>%
  gather(SITE_NAME, AREA_KM2, -EXTENT)

# load nlcd ----
nlcd.subbasin <- nlcd.subbasin %>%
  select(-TOTAL_AREA_KM2, -AREA_FRAC, -SOURCE)
nlcd.categories <- levels(nlcd.subbasin$LANDUSE)

nlcd.subbasin <- spread(nlcd.subbasin, SITE_NAME, AREA_KM2) %>%
  mutate('Godowa+Sycan'=Godowa+Sycan,
         'SF+NF'=SF+NF,
         'SF_Ivory+NF_Ivory'=SF_Ivory+NF_Ivory) %>%
  gather(SITE_NAME, AREA_KM2, -EXTENT, -LANDUSE) %>%
  mutate(AREA_KM2=ifelse(is.na(AREA_KM2), 0, AREA_KM2)) %>%
  arrange(EXTENT, SITE_NAME, LANDUSE)

nlcd.subbasin <- mutate(nlcd.subbasin, SITE_NAME=as.character(SITE_NAME)) %>%
  left_join(mutate(subbasin_area,
                   SITE_NAME=as.character(SITE_NAME)) %>%
              dplyr::rename(TOTAL_AREA_KM2=AREA_KM2),
            by=c("EXTENT", "SITE_NAME")) %>%
  mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2))

nlcd.subbasin <- mutate(nlcd.subbasin,
                        SITE_NAME=ordered(SITE_NAME,
                                          levels=c('Power', 'Lone_Pine', 'Godowa+Sycan',
                                                   'Godowa', 'Sycan',
                                                   'SF_Ivory+NF_Ivory', 'SF_Ivory', 'NF_Ivory',
                                                   'SF+NF', 'SF', 'NF')))

# check that sum of fraction areas is one for each site/extent
filter(nlcd.subbasin, LANDUSE %in% nlcd.categories) %>%
  dplyr::group_by(EXTENT, SITE_NAME) %>%
  dplyr::summarise(AREA_FRAC=sum(AREA_FRAC)) %>%
  filter(AREA_FRAC != 0) %>%
  (function(x) {
    stopifnot(all(abs(x$AREA_FRAC-1) < 0.01))
  })

# extract wq data ----
df_mon <- loads_df[['mon']] %>%
  dplyr::rename(BASIN_AREA_KM2=AREA_KM2) %>%
  filter(DATASET=="POR")

df_wyr <- loads_df[['wyr']] %>%
  filter(DATASET=="POR")

df_site <- loads_df[['site']] %>%
  filter(DATASET=="POR") %>%
  #mutate(PERIOD=plyr::revalue(PERIOD, c("2002-2014"="P2002",
   #                                     "2010-2014"="P2010",
    #                                    "2011-2014"="P2010")),
  mutate(PERIOD=ifelse(PERIOD=="2002-2020","P2002",
                       ifelse(PERIOD=="2010-2020","P2010",
                       ifelse(PERIOD=="2011-2020","P2010",PERIOD)))) %>%
  mutate(PERIOD=as.character(PERIOD))

# set up pou subbasin
pou_subbasin <- pou_subbasin %>%
  mutate( LANDUSE="POU") %>%
  select(EXTENT,LANDUSE,SITE_NAME,AREA_KM2,TOTAL_AREA_KM2,AREA_FRAC)

# join nlcd and pou
lu.subbasin <- rbind(nlcd.subbasin, pou_subbasin) %>%
  mutate(SITE_NAME=as.character(SITE_NAME))

df_wyr_area <- select(mutate(df_wyr,SITE_NAME=as.character(SITE_NAME)), -AREA_KM2) %>%
  filter(SITE_NAME %in% unique(lu.subbasin$SITE_NAME)) %>%
  left_join(lu.subbasin %>%
              select(EXTENT, SITE_NAME, LANDUSE, AREA_KM2, TOTAL_AREA_KM2),
            by='SITE_NAME') %>%
  mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2)) %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=levels(df_wyr$SITE_NAME))) %>%
  droplevels()

df_site_area <- select(df_site, -AREA_KM2) %>%
  filter(SITE_NAME %in% unique(lu.subbasin$SITE_NAME)) %>%
  left_join(lu.subbasin %>%
              select(EXTENT, SITE_NAME, LANDUSE, AREA_KM2, TOTAL_AREA_KM2),
            by='SITE_NAME') %>%
  mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2)) %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=levels(df_site$SITE_NAME))) %>%
  droplevels()

# pou loads pdfs ----
extent <- "valley"
period <- "P2002"
dataset <- "POR"
for (period in c("P2002", "P2010")) {
  for (extent in c("basin", "valley")) {
    filename <- file.path("pdf", tolower(dataset),
                          paste0("loads-pou-irrigation-", extent, "-",
                                 period_labels[[period]], ".pdf"))
    cat('Printing:', filename, '\n')
    pdf(filename, width=8.5, height=11)
    p <- df_site_area %>%
      filter( PERIOD==period, TERM=="C",
                EXTENT==extent, LANDUSE=="POU",
                SITE_NAME %in% stn_subbasin[[period]]) %>%
      ggplot(aes(AREA_FRAC, VALUE, color=SITE_NAME)) +
      geom_point(size=2) +
      facet_wrap(VAR ~ SEASON, scales="free_y") +
      scale_x_continuous(labels=scales::percent) +
      scale_color_manual('Station', values=color_site) +
      labs(x="Cumulative Fraction POU Irrigation Area (%)",
           y="Concentration (ppb)",
           title=paste0("FWM Concentrations vs. Cumulative Fraction POU Irrigation Area\n",
                        "Period: ", period_labels[[period]], " | Extent: ", extent)) +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
            strip.text.x=element_text(size=8),
            strip.background=element_blank(),
            aspect.ratio=1)
    print(p)

    p <- filter(df_site_area, PERIOD==period, TERM=="C",
                EXTENT==extent, LANDUSE=="POU",
                SITE_NAME %in% stn_subbasin[[period]]) %>%
      mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2)) %>%
      ggplot(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
      geom_point(size=2) +
      facet_grid(VAR ~ SEASON, scales="free_y") +
      scale_color_manual('Station', values=color_site) +
      labs(x="Cumulative POU Irrigation Area (km2)",
           y="Concentration (ppb)",
           title=paste0("FWM Concentrations vs. Cumulative POU Irrigation Area\n",
                        "Dataset: ", dataset, " | Extent: ", extent)) +
      theme(aspect.ratio=1,
            strip.text.x=element_text(size=8),
            axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
            strip.background=element_blank())
    print(p)

    p <- filter(df_site_area, PERIOD==period, TERM=="C",
                EXTENT==extent, LANDUSE=="POU",
                SITE_NAME %in% stn_subbasin[[period]]) %>%
      mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2)) %>%
      ggplot(aes(TOTAL_AREA_KM2, VALUE, color=SITE_NAME)) +
      geom_point(size=2) +
      facet_grid(VAR ~ SEASON, scales="free_y") +
      scale_color_manual('Station', values=color_site) +
      labs(x="Total Cumulative Drainage Area (km2)",
           y="Concentration (ppb)",
           title=paste0("FWM Concentrations vs. Total Cumulative Area\n",
                        "Dataset: ", dataset, " | Extent: ", extent)) +
      theme(aspect.ratio=1,
            strip.text.x=element_text(size=8),
            axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
            strip.background=element_blank())
    print(p)

    dev.off()
  }
}

# pou area pdf ----
filename <- file.path("pdf", "land-use-pou-irrigation.pdf")
cat('Printing:', filename, '\n')
pdf(filename, width=11, height=8.5)

p.area <- pou_subbasin %>%
  filter(SITE_NAME != "SF+NF") %>%
  ggplot(aes(SITE_NAME, AREA_KM2)) +
  geom_bar(stat="identity", fill='gray50') +
  facet_wrap(~EXTENT) +
  labs(x="", y="Cumulative POU Irrigation\nArea (km2)",
       title="Cumulative POU Irrigation Area by Station") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  theme(aspect.ratio=1,
        strip.background=element_blank())
p.area_frac <- pou_subbasin %>%
  filter(SITE_NAME != "SF+NF") %>%
  ggplot(aes(SITE_NAME, AREA_FRAC)) +
  geom_bar(stat="identity", fill='gray50') +
  facet_wrap(~EXTENT, scales='free_y') +
  labs(x="", y="Cumulative Fraction POU\nIrrigation (%)",
       title="Cumulative Fraction POU Irrigation Area by Station") +
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  theme(aspect.ratio=1,
        strip.background=element_blank())
p.area_total <- pou_subbasin %>%
  filter(SITE_NAME != "SF+NF") %>%
  ggplot(aes(SITE_NAME, TOTAL_AREA_KM2)) +
  geom_bar(stat="identity", fill='gray50') +
  facet_wrap(~EXTENT, scales='free_y') +
  labs(x="", y="Total Cumulative Drainage\nArea (km2)",
       title="Total Cumulative Drainage Area by Station") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  theme(aspect.ratio=1,
        strip.background=element_blank())
p.scatter <- pou_subbasin %>%
  filter(SITE_NAME != "SF+NF") %>%
  ggplot(aes(TOTAL_AREA_KM2, AREA_KM2)) +
  geom_point() +
  facet_wrap(~EXTENT, scales='free') +
  labs(y="Cumulative POU Irrigation\nArea (km2)", x="Total Cumulative Drainage Area (km2)",
       title="Cumulative POU Irrigation Area vs Total Cumulative Drainage Area") +
  theme(aspect.ratio=1,
        strip.background=element_blank())

grid.arrange(grobs=list(p.area, p.area_total, p.area_frac, p.scatter),
             nrow=2)

dev.off()

# nlcd loads pdfs ----
# cumulative land use area
dataset <- 'POR'
variable <- 'TP'
season <- 'Annual'
period <- 'P2002'
extent <- 'basin'
term <- 'C'
for (period in c('P2002', 'P2010')) {
  cat(dataset, '\n')
  # variables <- filter(df_mon, ) %>% (function(x) unique(x$VAR))
  for (extent in c('basin', 'valley')) {
    filename <- file.path('pdf', tolower(dataset),
                          paste0('loads-nlcd-', extent, '-',
                                 period_labels[[period]],'.pdf'))
    cat('Printing:', filename, '\n')
    cat('..', extent, '\n')
    pdf(filename, width=11, height=8.5)

    p <- filter(df_site_area, PERIOD==period, TERM==term,
                EXTENT==extent, !(LANDUSE %in% c("POU")),
                SEASON==season) %>%
      filter(SITE_NAME %in% stn_subbasin[[period]]) %>%
      filter(TOTAL_AREA_KM2>0) %>%
      ggplot() +
      geom_smooth(aes(AREA_KM2/TOTAL_AREA_KM2, VALUE), method='lm', se=FALSE, color='grey50') +
      geom_point(aes(AREA_KM2/TOTAL_AREA_KM2, VALUE, color=SITE_NAME), size=2) +
      facet_grid(VAR~LANDUSE, scales='free') +
      scale_color_manual('Station', values=color_site) +
      scale_x_continuous(labels=scales::percent) +
      labs(x='Fraction Cumulative Land Use Area (%)', y=paste0(term_labels[[term]]),
           title=paste0('Annual FWM Concentration vs Fraction Cumulative Land Use Area\n',
                        'Period: ', period_labels[[period]],  ' | Extent: ', extent)) +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
            axis.text.y=element_text(size=8),
            strip.text.x=element_text(size=6),
            strip.text.y=element_text(size=8),
            strip.background=element_blank(),
            aspect.ratio=1)
    print(p)

    for (variable in variables[[period]]) {
      cat('....', variable, '\n')
      # cumulative fraction land use area
      p <- filter(df_site_area, PERIOD==period, VAR==variable, TERM==term,
                  EXTENT==extent, !(LANDUSE %in% c("POU"))) %>%
        filter(SITE_NAME %in% stn_subbasin[[period]]) %>%
        filter(TOTAL_AREA_KM2>0) %>%
        ggplot() +
        geom_smooth(aes(AREA_KM2/TOTAL_AREA_KM2, VALUE), method='lm', se=FALSE, color='grey50') +
        geom_point(aes(AREA_KM2/TOTAL_AREA_KM2, VALUE, color=SITE_NAME), size=2) +
        facet_grid(SEASON~LANDUSE, scales='free') +
        scale_color_manual('Station', values=color_site) +
        scale_x_continuous(labels=scales::percent) +
        labs(x='Fraction Cumulative Area per Land Use (%)', y=paste0(variable, " ", term_labels[[term]]),
             title=paste0('Seasonal FWM Concentration vs Fraction Cumulative Land Use Area\n',
                          'Period: ', period_labels[[period]],  ' | Extent: ', extent,  ' | Variable: ', variable)) +
        theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
              axis.text.y=element_text(size=8),
              strip.text.x=element_text(size=6),
              strip.text.y=element_text(size=6),
              strip.background=element_blank(),
              aspect.ratio=1)
      print(p)

    }

    dev.off()
  }
}

# report ----

df_pou_rho <- df_site_area %>%
  filter(PERIOD=="P2010",
         TERM=="C",
         EXTENT=='valley',
         LANDUSE=="POU",
         SITE_NAME %in% c('Power', 'Lone_Pine', 'Godowa', 'Sycan',
                          'SF_Ivory', 'SF', 'NF_Ivory', 'NF')) %>%
  dplyr::group_by(VAR) %>%
  mutate(MAX_VALUE=max(VALUE)) %>%
  dplyr::group_by(VAR, SEASON, MAX_VALUE) %>%
  dplyr::summarise(rho=cor.test(x=.$VALUE, y=.$AREA_FRAC, method="pearson")$estimate,
                   p=cor.test(x=.$VALUE, y=.$AREA_FRAC, method="pearson")$p.value) %>%
  #do(ct=cor.test(x=.$VALUE, y=.$AREA_FRAC, method="pearson")) %>%
  #mutate(rho=ct$estimate,
  #       p=ct$p.value) %>%
  #select(-ct) %>%
  mutate(LABEL=ifelse(p<0.1, 'p <= 0.1', 'p > 0.1'))

filename <- 'report/results-load-pou-WY2010-2020-valley.png'
cat('Saving report figure to:', filename, '\n')
png(filename, width=8, height=8, res=200, units='in')
p <- filter(df_site_area, PERIOD=="P2010", TERM=="C",
            EXTENT=='valley', LANDUSE=="POU",
            SITE_NAME %in% c('Power', 'Lone_Pine', 'Godowa', 'Sycan',
                             'SF_Ivory', 'SF', 'NF_Ivory', 'NF')) %>%
  left_join(df_pou_rho) %>%
  ggplot(aes(AREA_FRAC, VALUE)) +
  geom_point(aes(color=SITE_NAME), size=2.5) +
  geom_smooth(aes(linetype=LABEL), method='lm',
              color='grey50', se=FALSE, alpha=0.5) +
  facet_grid(VAR ~ SEASON, scales="free_y") +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::comma) +
  scale_color_manual('Station', values=color_site) +
  scale_linetype_manual('Significance',
                        values=c('solid', 'dotted')) +
  labs(x="Percent Cumulative Lower Valley Area as POU (%)",
       y="Concentration (ppb)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        strip.text.x=element_text(size=8, face='bold'),
        strip.text.y=element_text(face='bold'),
        aspect.ratio=1,
        strip.background=element_blank())
print(p)
dev.off()

# filename <- 'report/results-load-pou-WY2002-2014-valley.png'
# cat('Saving report figure to:', filename, '\n')
# png(filename, width=8, height=8, res=200, units='in')
# p <- filter(df_site_area, PERIOD=='P2002', TERM=="C", EXTENT=='valley', LANDUSE=="POU") %>%
#   ggplot(aes(AREA_FRAC, VALUE, color=SITE_NAME)) +
#   geom_point(size=2) +
#   facet_grid(VAR ~ SEASON, scales="free_y") +
#   scale_x_continuous(labels=scales::percent) +
#   scale_color_discrete('') +
#   labs(x="Cumulative Fraction POU Irrigation Area (%)\nLower Valley Only",
#        y="Concentration (ppb)") +
#   theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
#         strip.text.x=element_text(size=8),
#         aspect.ratio=1,
#         strip.background=element_blank())
# print(p)
# dev.off()
#
# filename <- 'report/results-load-pou-WY2010-2014-basin.png'
# cat('Saving report figure to:', filename, '\n')
# png(filename, width=8, height=8, res=200, units='in')
# p <- filter(df_site_area, PERIOD=="P2010", TERM=="C", EXTENT=='basin', LANDUSE=="POU") %>%
#   ggplot(aes(AREA_FRAC, VALUE, color=SITE_NAME)) +
#   geom_point(size=2) +
#   facet_grid(VAR ~ SEASON, scales="free_y") +
#   scale_x_continuous(labels=scales::percent) +
#   scale_color_discrete('') +
#   labs(x="Cumulative Fraction POU Irrigation Area (%)",
#        y="Concentration (ppb)") +
#   theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
#         strip.text.x=element_text(size=8),
#         aspect.ratio=1,
#         strip.background=element_blank())
# print(p)
# dev.off()
#
# filename <- 'report/results-load-pou-WY2002-2014-basin.png'
# cat('Saving report figure to:', filename, '\n')
# png(filename, width=8, height=8, res=200, units='in')
# p <- filter(df_site_area, PERIOD=="P2002", TERM=="C", EXTENT=='basin', LANDUSE=="POU") %>%
#   mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2)) %>%
#   ggplot(aes(AREA_FRAC, VALUE, color=SITE_NAME)) +
#   geom_point(size=2) +
#   facet_grid(VAR ~ SEASON, scales="free_y") +
#   scale_x_continuous(labels=scales::percent) +
#   scale_color_discrete('') +
#   labs(x="Cumulative Fraction POU Irrigation Area (%)",
#        y="Concentration (ppb)") +
#   theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
#         strip.text.x=element_text(size=8),
#         aspect.ratio=1,
#         strip.background=element_blank())
# print(p)
# dev.off()

# nlcd

df_nlcd_rho <- df_site_area %>%
  mutate(SITE_NAME=as.factor(SITE_NAME)) %>%
  filter( PERIOD=="P2010", TERM=="C",
          EXTENT=="basin", !(LANDUSE %in% c("POU", "Total")),
          SITE_NAME %in% c('Power', 'Lone_Pine', 'Godowa', 'Sycan',
                           'SF_Ivory', 'SF', 'NF_Ivory', 'NF'),
          SEASON=="Annual") %>%
  filter(SITE_NAME %in% stn_subbasin[["P2010"]]) %>%
  filter(TOTAL_AREA_KM2>0) %>%
  mutate(AREA_FRAC=AREA_KM2/TOTAL_AREA_KM2) %>%
  dplyr::group_by(VAR) %>%
  mutate(MAX_VALUE=max(VALUE)) %>%
  dplyr::group_by(LANDUSE, VAR, SEASON, MAX_VALUE) %>%
  dplyr::summarise(rho=cor.test(x=.$VALUE, y=.$AREA_FRAC, method="pearson")$estimate,
                   p=cor.test(x=.$VALUE, y=.$AREA_FRAC, method="pearson")$p.value) %>%
  #do(ct=cor.test(x=.$VALUE, y=.$AREA_FRAC, method="pearson")) %>%
 # mutate(rho=ct$estimate,
  #       p=ct$p.value) %>%
 # select(-ct) %>%
  mutate(LABEL=ifelse(p<0.1, 'p <= 0.1', 'p > 0.1'))



filename <- 'report/results-load-nlcd-WY2010-2020-basin.png'
cat('Saving report figure to:', filename, '\n')
png(filename, width=11, height=8, res=200, units='in')
p <- filter(df_site_area, PERIOD=="P2010", TERM=="C",
            EXTENT=="basin", !(LANDUSE %in% c("POU", "Total")),
            SITE_NAME %in% c('Power', 'Lone_Pine', 'Godowa', 'Sycan',
                             'SF_Ivory', 'SF', 'NF_Ivory', 'NF'),
            SEASON=="Annual") %>%
  filter(SITE_NAME %in% stn_subbasin[["P2010"]]) %>%
  filter(TOTAL_AREA_KM2>0) %>%
  mutate(AREA_FRAC=AREA_KM2/TOTAL_AREA_KM2) %>%
  left_join(df_nlcd_rho) %>%
  ggplot() +
  geom_point(aes(AREA_FRAC, VALUE, color=SITE_NAME),
             size=2.5) +
  geom_smooth(aes(AREA_FRAC, VALUE, linetype=LABEL), method='lm',
              color='grey50', se=FALSE, alpha=0.5) +
  facet_grid(VAR~LANDUSE, scales='free') +
  scale_color_manual('Station', values=color_site) +
  scale_linetype_manual('Significance',
                        values=c('solid', 'dotted')) +
  scale_x_continuous(labels=scales::percent) +
  scale_y_continuous(labels=scales::comma) +
  labs(x='Percent Cumulative Subbasin Area (%)', y='Concentration (ppb)') +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
        axis.text.y=element_text(size=8),
        strip.text.x=element_text(size=8, face='bold'),
        strip.text.y=element_text(size=10, face='bold'),
        aspect.ratio=1,
        strip.background=element_blank())
print(p)
dev.off()
