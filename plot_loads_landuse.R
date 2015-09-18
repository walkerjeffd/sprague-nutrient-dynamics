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
load('network.Rdata')
load('pou.Rdata')

units <- c(Q='hm3/d', L='kg/d', C='ppb')
term_labels <- c(Q='Flow (hm3/d)', L='Load (kg/d)', C='Conc (ppb)')

periods <- c(RECENT='WY2009-2014',
             POR='WY2002-2014')

stn_primary <- list(RECENT=c('Power', 'Lone_Pine', 'Godowa+Sycan', 'Godowa', 'Sycan', 'SF_Ivory+NF_Ivory', 'SF_Ivory', 'NF_Ivory', 'SF', 'NF'),
                    POR=c('Power', 'Lone_Pine', 'Godowa+Sycan', 'Godowa', 'Sycan', 'SF+NF', 'SF', 'NF'))

# compute total subbasin areas of junction stations
basin_subbasin_area <- select(subbasin_area, SITE_NAME, AREA_KM2) %>%
  mutate(GROUP=1) %>%
  spread(SITE_NAME, AREA_KM2) %>%
  mutate('Godowa+Sycan'=Godowa+Sycan,
         'SF+NF'=SF+NF,
         'SF_Ivory+NF_Ivory'=SF_Ivory+NF_Ivory) %>%
  gather(SITE_NAME, TOTAL_AREA_KM2, -GROUP) %>%
  select(-GROUP) %>%
  mutate(EXTENT="basin")

valley_incbasin_area <- geomorph_incbasin %>%
  filter(INC_SITE_NAME != "Godowa-SF-NF") %>%
  left_join(select(incbasin_ivory_area, SITE_NAME, INC_SITE),
            by="INC_SITE") %>%
  select(SITE_NAME, TOTAL_AREA_KM2=VALLEY_AREA_KM2) %>%
  mutate(EXTENT="valley") %>%
  select(EXTENT, SITE_NAME, TOTAL_AREA_KM2)

valley_subbasin_area <- spread(valley_incbasin_area, SITE_NAME, TOTAL_AREA_KM2) %>%
  mutate(SF_Ivory=SF_Ivory+SF,
         NF_Ivory=NF_Ivory+NF,
         Godowa=Godowa+NF_Ivory+SF_Ivory,
         Lone_Pine=Lone_Pine+Godowa+Sycan,
         Power=Power+Lone_Pine,
         'Godowa+Sycan'=Godowa+Sycan,
         'SF+NF'=SF+NF,
         'SF_Ivory+NF_Ivory'=SF_Ivory+NF_Ivory) %>%
  gather(SITE_NAME, TOTAL_AREA_KM2, -EXTENT)

subbasin_area <- rbind(basin_subbasin_area, valley_subbasin_area)

# load pou ----
pou <- left_join(pou, subbasin_area, by=c("SITE_NAME", "EXTENT")) %>%
  mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2))

# load nlcd ----
nlcd.subbasin <- filter(nlcd.subbasin, SOURCE=="NLCD_CAT") %>%
  droplevels %>%
  select(-TOTAL_AREA_KM2, -AREA_FRAC)
nlcd.categories <- levels(nlcd.subbasin$LANDUSE)

nlcd.subbasin.total <- group_by(nlcd.subbasin, EXTENT, SITE_NAME) %>%
  summarise(TOTAL_AREA_KM2=sum(AREA_KM2)) %>%
  spread(SITE_NAME, TOTAL_AREA_KM2) %>%
  mutate('Godowa+Sycan'=Godowa+Sycan,
         'SF+NF'=SF+NF,
         'SF_Ivory+NF_Ivory'=SF_Ivory+NF_Ivory) %>%
  gather(SITE_NAME, TOTAL_AREA_KM2, -EXTENT)

nlcd.subbasin <- spread(nlcd.subbasin, SITE_NAME, AREA_KM2) %>%
  mutate('Godowa+Sycan'=Godowa+Sycan,
         'SF+NF'=SF+NF,
         'SF_Ivory+NF_Ivory'=SF_Ivory+NF_Ivory) %>%
  gather(SITE_NAME, AREA_KM2, -SOURCE, -EXTENT, -LANDUSE) %>%
  # compute agriculture and total area categories
  spread(LANDUSE, AREA_KM2) %>%
  mutate(Agriculture=`Planted/Cultivated`+Wetlands,
         # Agriculture=`Planted/Cultivated`+Wetlands+Herbaceous,
         Impacted=Agriculture+Developed) %>%
  gather(LANDUSE, AREA_KM2, Developed:Impacted) %>%
  mutate(AREA_KM2=ifelse(is.na(AREA_KM2), 0, AREA_KM2)) %>%
  arrange(EXTENT, SITE_NAME, LANDUSE) %>%
  mutate(SITE_NAME=ordered(SITE_NAME,
                           levels=c('Power', 'Lone_Pine', 'Godowa+Sycan',
                                    'Godowa', 'Sycan',
                                    'SF_Ivory+NF_Ivory', 'SF_Ivory', 'NF_Ivory',
                                    'SF+NF', 'SF', 'NF')))

nlcd.subbasin <- nlcd.subbasin %>%
  left_join(nlcd.subbasin.total, by=c("EXTENT", "SITE_NAME")) %>%
  spread(LANDUSE, AREA_KM2) %>%
  mutate(Total=TOTAL_AREA_KM2) %>%
  gather(LANDUSE, AREA_KM2, Developed:Total) %>%
  mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2))

# check that sum of fraction areas is one for each site/extent
filter(nlcd.subbasin, LANDUSE %in% nlcd.categories) %>%
  group_by(EXTENT, SITE_NAME) %>%
  summarise(AREA_FRAC=sum(AREA_FRAC)) %>%
  filter(AREA_FRAC != 0) %>%
  (function(x) {
    stopifnot(all(abs(x$AREA_FRAC-1) < 1e-6))
  })

# extract wq data ----
df_mon <- loads_df[['mon']] %>%
  rename(AREA_KM2_BASIN=AREA_KM2)

# compute flows/loads/concs at junction locations
df_mon_flow <- filter(df_mon,
                      DATASET %in% c('RECENT', 'POR'),
                      TERM == 'Q',
                      SITE_NAME %in% stn_primary[['RECENT']]) %>%
  select(-TERM, -AREA_KM2_BASIN, -VAR) %>%
  spread(SITE_NAME, VALUE) %>%
  mutate('Godowa+Sycan'=Godowa+Sycan,
         'SF+NF'=SF+NF,
         'SF_Ivory+NF_Ivory'=SF_Ivory+NF_Ivory) %>%
  gather(SITE_NAME, Q, -c(DATASET:WYEAR), na.rm=TRUE) %>%
  mutate(SITE_NAME=ordered(SITE_NAME,
                           levels=c('Power', 'Lone_Pine', 'Godowa+Sycan',
                                    'Godowa', 'Sycan',
                                    'SF_Ivory+NF_Ivory', 'SF_Ivory', 'NF_Ivory',
                                    'SF+NF', 'SF', 'NF')))
df_mon_load <- filter(df_mon,
                      DATASET %in% c('RECENT', 'POR'),
                      TERM == 'L',
                      SITE_NAME %in% stn_primary[['RECENT']]) %>%
  select(-TERM, -AREA_KM2_BASIN) %>%
  spread(SITE_NAME, VALUE) %>%
  mutate('Godowa+Sycan'=Godowa+Sycan,
         'SF+NF'=SF+NF,
         'SF_Ivory+NF_Ivory'=SF_Ivory+NF_Ivory) %>%
  gather(SITE_NAME, L, -c(DATASET:WYEAR), na.rm=TRUE) %>%
  mutate(SITE_NAME=ordered(SITE_NAME,
                           levels=c('Power', 'Lone_Pine', 'Godowa+Sycan',
                                    'Godowa', 'Sycan',
                                    'SF_Ivory+NF_Ivory', 'SF_Ivory', 'NF_Ivory',
                                    'SF+NF', 'SF', 'NF')))

df_mon <- left_join(df_mon_load, df_mon_flow,
                    by=c('DATASET', 'MONTHYEAR', 'MONTH', 'WYEAR', 'SITE_NAME')) %>%
  droplevels %>%
  mutate(C=L/Q)

# remove SF+NF from RECENT
df_mon <- df_mon[-which(df_mon$DATASET=="RECENT" & df_mon$SITE_NAME=="SF+NF"),]
table(df_mon$SITE_NAME, df_mon$DATASET)

# compute seasonal loads
seasons <- list(Annual=1:12,
                'Fall (Oct-Dec)'=c(10:12),
                'Winter (Jan-Mar)'=c(1:3),
                'Spring (Apr-Jun)'=c(4:6),
                'Summer (Jul-Sep)'=c(7:9))
df_seasons <- lapply(names(seasons), function(s) {
  data.frame(MONTH=seasons[[s]], SEASON=s, stringsAsFactors=FALSE)
}) %>%
  rbind_all

df_mon <- left_join(df_seasons, df_mon, by='MONTH') %>%
  left_join(filter(subbasin_area, EXTENT=="basin") %>% select(SITE_NAME, TOTAL_AREA_KM2), by='SITE_NAME') %>%
  mutate(SITE_NAME=ordered(SITE_NAME,
                           levels=c('Power', 'Lone_Pine', 'Godowa+Sycan',
                                    'Godowa', 'Sycan',
                                    'SF_Ivory+NF_Ivory', 'SF_Ivory', 'NF_Ivory',
                                    'SF+NF', 'SF', 'NF')),
         SEASON=ordered(SEASON, levels=names(seasons)))

# compute annual flows/loads/concs
df_wyr <- group_by(df_mon, DATASET, VAR, SITE_NAME, TOTAL_AREA_KM2, SEASON, WYEAR) %>%
  summarise(N.MONTH=n(),
            N.DAY=sum(days_in_month(MONTHYEAR)),
            L=sum(L)/N.DAY,
            Q=sum(Q)/N.DAY,
            C=L/Q)

# compute overall mean flows/loads/concs
df_site <- group_by(df_wyr, DATASET, VAR, SITE_NAME, TOTAL_AREA_KM2, SEASON) %>%
  summarise(N.MONTH=sum(N.MONTH),
            L=sum(L*N.DAY)/sum(N.DAY),
            Q=sum(Q*N.DAY)/sum(N.DAY),
            N.DAY=sum(N.DAY),
            C=L/Q)

# gather terms
df_mon <- gather(df_mon, TERM, VALUE, L, Q, C) %>%
  mutate(TERM=ordered(TERM, levels=c('C', 'L', 'Q')))
df_wyr <- gather(df_wyr, TERM, VALUE, L, Q, C) %>%
  mutate(TERM=ordered(TERM, levels=c('C', 'L', 'Q')))
df_site <- gather(df_site, TERM, VALUE, L, Q, C) %>%
  mutate(TERM=ordered(TERM, levels=c('C', 'L', 'Q')))

# join nlcd and pou
lu.subbasin <- rbind(nlcd.subbasin, pou)

df_wyr_area <- rename(df_wyr, AREA_KM2_BASIN=TOTAL_AREA_KM2) %>%
  left_join(lu.subbasin %>% select(SOURCE, EXTENT, SITE_NAME, LANDUSE, AREA_KM2, TOTAL_AREA_KM2), by='SITE_NAME') %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=levels(df_wyr$SITE_NAME)))

df_site_area <- rename(df_site, AREA_KM2_BASIN=TOTAL_AREA_KM2) %>%
  left_join(lu.subbasin %>% select(SOURCE, EXTENT, SITE_NAME, LANDUSE, AREA_KM2, TOTAL_AREA_KM2), by='SITE_NAME') %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=levels(df_site$SITE_NAME)))

df_segments_site <- left_join(network, df_site_area, by=c(FROM='SITE_NAME', DATASET='DATASET')) %>%
  left_join(df_site_area, by=c(TO='SITE_NAME', 'EXTENT', 'DATASET', 'SEASON', 'VAR', 'TERM', 'N.MONTH', 'N.DAY', 'LANDUSE')) %>%
  mutate(TO=ordered(TO, levels=levels(df_site$SITE_NAME)),
         FROM=ordered(FROM, levels=levels(df_site$SITE_NAME))) %>%
  rename(VALUE.FROM=VALUE.x,
         AREA_KM2.FROM=AREA_KM2.x,
         AREA_KM2_BASIN.FROM=AREA_KM2_BASIN.x,
         TOTAL_AREA_KM2.FROM=TOTAL_AREA_KM2.x,
         VALUE.TO=VALUE.y,
         AREA_KM2.TO=AREA_KM2.y,
         AREA_KM2_BASIN.TO=AREA_KM2_BASIN.y,
         TOTAL_AREA_KM2.TO=TOTAL_AREA_KM2.y) %>%
  droplevels

df_segments_wyr <- left_join(network, df_wyr_area, by=c(FROM='SITE_NAME', DATASET='DATASET')) %>%
  left_join(df_wyr_area, by=c(TO='SITE_NAME', 'EXTENT', 'DATASET', 'SEASON', 'VAR', 'TERM', 'WYEAR', 'N.MONTH', 'N.DAY', 'LANDUSE')) %>%
  mutate(TO=ordered(TO, levels=levels(df_site$SITE_NAME)),
         FROM=ordered(FROM, levels=levels(df_site$SITE_NAME))) %>%
  rename(VALUE.FROM=VALUE.x,
         AREA_KM2.FROM=AREA_KM2.x,
         AREA_KM2_BASIN.FROM=AREA_KM2_BASIN.x,
         TOTAL_AREA_KM2.FROM=TOTAL_AREA_KM2.x,
         VALUE.TO=VALUE.y,
         AREA_KM2.TO=AREA_KM2.y,
         AREA_KM2_BASIN.TO=AREA_KM2_BASIN.y,
         TOTAL_AREA_KM2.TO=TOTAL_AREA_KM2.y) %>%
  droplevels


# pou loads pdfs ----
dataset <- "RECENT"
extent <- "valley"
for (dataset in c("POR", "RECENT")) {
  for (extent in c("basin", "valley")) {
    filename <- file.path("pdf", tolower(dataset), paste0("loads-pou-irrigation-", extent, ".pdf"))
    cat('Printing:', filename, '\n')
    pdf(filename, width=8.5, height=11)
    p <- filter(df_site_area, DATASET==dataset, TERM=="C", EXTENT==extent, LANDUSE=="POU") %>%
      mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2)) %>%
      ggplot(aes(AREA_FRAC, VALUE, color=SITE_NAME)) +
      geom_point(size=2) +
      facet_grid(VAR ~ SEASON, scales="free_y") +
      scale_x_continuous(labels=scales::percent) +
      scale_color_discrete('') +
      labs(x="Cumulative Fraction POU Irrigation Area (%)",
           y="Concentration (ppb)",
           title=paste0("FWM Concentrations vs. Cumulative Fraction POU Irrigation Area\n",
                        "Dataset: ", dataset, " | Extent: ", extent)) +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
            strip.text.x=element_text(size=8),
            aspect.ratio=1)
    print(p)

    p <- filter(df_site_area, DATASET==dataset, TERM=="C", EXTENT==extent, LANDUSE=="POU") %>%
      mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2)) %>%
      ggplot(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
      geom_point(size=2) +
      facet_grid(VAR ~ SEASON, scales="free_y") +
      scale_color_discrete('') +
      labs(x="Cumulative POU Irrigation Area (km2)",
           y="Concentration (ppb)",
           title=paste0("FWM Concentrations vs. Cumulative POU Irrigation Area\n",
                        "Dataset: ", dataset, " | Extent: ", extent)) +
      theme(aspect.ratio=1,
            strip.text.x=element_text(size=8),
            axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
    print(p)

    p <- filter(df_site_area, DATASET==dataset, TERM=="C", EXTENT==extent, LANDUSE=="POU") %>%
      mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2)) %>%
      ggplot(aes(TOTAL_AREA_KM2, VALUE, color=SITE_NAME)) +
      geom_point(size=2) +
      facet_grid(VAR ~ SEASON, scales="free_y") +
      scale_color_discrete('') +
      labs(x="Total Cumulative Drainage Area (km2)",
           y="Concentration (ppb)",
           title=paste0("FWM Concentrations vs. Total Cumulative Area\n",
                        "Dataset: ", dataset, " | Extent: ", extent)) +
      theme(aspect.ratio=1,
            strip.text.x=element_text(size=8),
            axis.text.x=element_text(angle=90, hjust=1, vjust=0.5))
    print(p)

    dev.off()
  }
}

# pou area pdf ----
filename <- file.path("pdf", "land-use-pou-irrigation.pdf")
cat('Printing:', filename, '\n')
pdf(filename, width=11, height=8.5)

p.area <- pou %>%
  mutate(SITE_NAME=ordered(as.character(SITE_NAME),
                           levels=levels(df_mon$SITE_NAME))) %>%
  ggplot(aes(SITE_NAME, AREA_KM2)) +
  geom_bar(stat="identity", fill='gray50') +
  facet_wrap(~EXTENT) +
  labs(x="", y="Cumulative POU Irrigation\nArea (km2)",
       title="Cumulative POU Irrigation Area by Station") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  theme(aspect.ratio=1)
p.area_frac <- pou %>%
  mutate(SITE_NAME=ordered(as.character(SITE_NAME),
                           levels=levels(df_mon$SITE_NAME))) %>%
  ggplot(aes(SITE_NAME, AREA_FRAC)) +
  geom_bar(stat="identity", fill='gray50') +
  facet_wrap(~EXTENT, scales='free_y') +
  labs(x="", y="Cumulative Fraction POU\nIrrigation (%)",
       title="Cumulative Fraction POU Irrigation Area by Station") +
  scale_y_continuous(labels=scales::percent) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  theme(aspect.ratio=1)
p.area_total <- pou %>%
  mutate(SITE_NAME=ordered(as.character(SITE_NAME),
                           levels=levels(df_mon$SITE_NAME))) %>%
  ggplot(aes(SITE_NAME, TOTAL_AREA_KM2)) +
  geom_bar(stat="identity", fill='gray50') +
  facet_wrap(~EXTENT, scales='free_y') +
  labs(x="", y="Total Cumulative Drainage\nArea (km2)",
       title="Total Cumulative Drainage Area by Station") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  theme(aspect.ratio=1)
p.scatter <- pou %>%
  mutate(SITE_NAME=ordered(as.character(SITE_NAME),
                           levels=levels(df_mon$SITE_NAME))) %>%
  ggplot(aes(TOTAL_AREA_KM2, AREA_KM2)) +
  geom_point() +
  facet_wrap(~EXTENT, scales='free') +
  labs(y="Cumulative POU Irrigation\nArea (km2)", x="Total Cumulative Drainage Area (km2)",
       title="Cumulative POU Irrigation Area vs Total Cumulative Drainage Area") +
  theme(aspect.ratio=1)

grid.arrange(grobs=list(p.area, p.area_total, p.area_frac, p.scatter),
             nrow=2)

dev.off()

# cumul-area loads detail pdfs ----
dataset <- 'RECENT'
variable <- 'TP'
term <- 'C'
season <- 'Annual'
for (dataset in c("POR", "RECENT")) {
  cat(dataset, '\n')
  variables <- filter(df_mon, DATASET==dataset) %>% (function(x) unique(x$VAR))
  for (variable in variables) {
    filename <- file.path('pdf', tolower(dataset), 'loads-cumul-area',
                          paste0('loads-cumul-area-', tolower(variable), '.pdf'))
    cat('Printing:', filename, '\n')
    cat('..', variable, '\n')
    pdf(filename, width=17, height=11)

    # TERM vs WYEAR by WYEAR
    p1 <- filter(df_wyr_area, DATASET==dataset, VAR==variable, SEASON==season, EXTENT=='basin', LANDUSE=='Total') %>%
      filter(SITE_NAME %in% stn_primary[[dataset]]) %>%
      ggplot() +
      geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
      geom_segment(aes(x=AREA_KM2.FROM, xend=AREA_KM2.TO, y=VALUE.FROM, yend=VALUE.TO, size=MAINSTEM),
                   data=filter(df_segments_wyr, DATASET==dataset, VAR==variable, SEASON==season, LANDUSE=='Total', EXTENT=='basin'),
                   alpha=0.5) +
      geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME), size=3) +
      facet_grid(TERM~WYEAR, scales='free_y') +
      scale_size_manual(guide=FALSE, values=c('FALSE'=0.5, 'TRUE'=1)) +
      scale_color_discrete('') +
      labs(x='Total Cumulative Drainage Area (km2)', y=paste0('Flow (hm3/d) / Load (kg/d) / Conc (ppb)'),
           title=paste0('Annual Mean by Water Year\nDataset: ', dataset, ' | Period: ', periods[[dataset]], ' | Variable: ', variable)) +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
            aspect.ratio=1)
    # print(p1)

    # SEASON vs TERM by SITE
    p2 <- filter(df_site_area, DATASET==dataset, VAR==variable, EXTENT=='basin', LANDUSE=='Total') %>%
      filter(SITE_NAME %in% stn_primary[[dataset]]) %>%
      ggplot() +
      geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
      geom_segment(aes(x=AREA_KM2.FROM, xend=AREA_KM2.TO, y=VALUE.FROM, yend=VALUE.TO, size=MAINSTEM),
                   data=filter(df_segments_site, DATASET==dataset, VAR==variable, EXTENT=='basin', LANDUSE=='Total'),
                   alpha=0.5) +
      geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME), size=3) +
      facet_grid(TERM~SEASON, scales='free_y') +
      scale_color_discrete('') +
      scale_size_manual(guide=FALSE, values=c('FALSE'=0.5, 'TRUE'=1)) +
      labs(x='Cumulative Drainage Area (km2)', y=paste0('Flow (hm3/d) / Load (kg/d) / Conc (ppb)'),
           title=paste0('Seasonal Mean by Site\nDataset: ', dataset,' | Period: ', periods[[dataset]],  ' | Variable: ', variable)) +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
            aspect.ratio=1)
    # print(p2)
    grid.arrange(grobs=list(p1, p2), nrow=2)

    # SEASON vs WYEAR by WYEAR
    for (term2 in c('C', 'L', 'Q')) {
      if (term2 == "Q") {
        ylabel <- 'Flow (hm3/d)'
      } else {
        ylabel <- paste0(variable, " ", term_labels[[term2]])
      }
      p3 <- filter(df_wyr_area, DATASET==dataset, VAR==variable, TERM==term2, EXTENT=='basin', LANDUSE=='Total') %>%
        filter(SITE_NAME %in% stn_primary[[dataset]]) %>%
        ggplot() +
        geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
        geom_segment(aes(x=AREA_KM2.FROM, xend=AREA_KM2.TO, y=VALUE.FROM, yend=VALUE.TO, size=MAINSTEM),
                     data=filter(df_segments_wyr, DATASET==dataset, VAR==variable, TERM==term2, EXTENT=='basin', LANDUSE=='Total'),
                     alpha=0.5) +
        geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME), size=3) +
        facet_grid(SEASON~WYEAR, scales='free_y') +
        scale_size_manual(guide=FALSE, values=c('FALSE'=0.5, 'TRUE'=1)) +
        scale_color_discrete('') +
        labs(x='Cumulative Drainage Area (km2)', y=ylabel,
             title=paste0('Seasonal Mean by Water Year\nDataset: ', dataset,' | Period: ', periods[[dataset]],  ' | Variable: ', ylabel)) +
        theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
              aspect.ratio=1)

      if (dataset == "POR") {
        p3 <- p3 + theme(strip.text.y=element_text(size=8))
      }

      print(p3)
    }

    dev.off()
  }
}

# cumul-area summary pdfs ----
dataset <- 'RECENT'
for (dataset in c("POR", "RECENT")) {
  filename <- file.path('pdf', tolower(dataset), 'loads-cumul-area-summary.pdf')
  cat('Printing:', filename, '\n')
  cat(dataset, '\n')

  pdf(filename, width=11, height=8.5)

  for (term in c('C', 'L')) {
    p <- filter(df_site_area, DATASET==dataset, TERM==term,
                VAR %in% c('TP', 'PO4', 'PP', 'TN', 'TSS'),
                EXTENT=='basin', LANDUSE=='Total') %>%
      filter(SITE_NAME %in% stn_primary[[dataset]]) %>%
      ggplot() +
      geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
      geom_segment(aes(x=AREA_KM2.FROM, xend=AREA_KM2.TO, y=VALUE.FROM, yend=VALUE.TO, size=MAINSTEM),
                   data=filter(df_segments_site, DATASET==dataset, TERM==term,
                               VAR %in% c('TP', 'PO4', 'PP', 'TN', 'TSS'),
                               EXTENT=='basin', LANDUSE=='Total'),
                   alpha=0.5) +
      geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME), size=3) +
      facet_grid(VAR~SEASON, scales='free_y') +
      scale_color_discrete('') +
      scale_size_manual(guide=FALSE, values=c('FALSE'=0.5, 'TRUE'=1)) +
      labs(x='Cumulative Drainage Area (km2)', y=term_labels[[term]],
           title=paste0('Annual and Seasonal ', term_labels[[term]], ' vs Cumulative Drainage Area\nDataset: ', dataset,' | Period: ', periods[[dataset]])) +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
            aspect.ratio=1)
    print(p)
  }
  term <- 'Q'
  p <- filter(df_site_area, DATASET==dataset, TERM==term,
              VAR %in% c('TP'),
              EXTENT=='basin', LANDUSE=='Total') %>%
    mutate(VAR='FLOW') %>%
    filter(SITE_NAME %in% stn_primary[[dataset]]) %>%
    ggplot() +
    geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
    geom_segment(aes(x=AREA_KM2.FROM, xend=AREA_KM2.TO, y=VALUE.FROM, yend=VALUE.TO, size=MAINSTEM),
                 data=filter(df_segments_site, DATASET==dataset, TERM==term,
                             VAR %in% c('TP'),
                             EXTENT=='basin', LANDUSE=='Total') %>%
                   mutate(VAR='FLOW'),
                 alpha=0.5) +
    geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME), size=3) +
    facet_grid(VAR~SEASON, scales='free_y') +
    scale_color_discrete('') +
    scale_size_manual(guide=FALSE, values=c('FALSE'=0.5, 'TRUE'=1)) +
    labs(x='Cumulative Drainage Area (km2)', y=term_labels[[term]],
         title=paste0('Annual and Seasonal ', term_labels[[term]], ' vs Cumulative Drainage Area\nDataset: ', dataset,' | Period: ', periods[[dataset]])) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
          aspect.ratio=1)
  print(p)
  dev.off()
}

# nlcd loads pdfs ----
# cumulative land use area
dataset <- 'RECENT'
variable <- 'TP'
season <- 'Annual'

term <- 'C'
for (dataset in c('POR', 'RECENT')) {
  cat(dataset, '\n')
  variables <- filter(df_mon, DATASET==dataset) %>% (function(x) unique(x$VAR))
  for (extent in c('basin', 'valley')) {
    filename <- file.path('pdf', tolower(dataset), paste0('loads-nlcd-', extent, '.pdf'))
    cat('Printing:', filename, '\n')
    cat('..', extent, '\n')
    pdf(filename, width=11, height=8.5)

    p <- filter(df_site_area, DATASET==dataset, TERM==term,
                EXTENT==extent, !(LANDUSE %in% c("POU", "Total")),
                SEASON=="Annual") %>%
      filter(SITE_NAME %in% stn_primary[[dataset]]) %>%
      filter(TOTAL_AREA_KM2>0) %>%
      ggplot() +
      geom_smooth(aes(AREA_KM2/TOTAL_AREA_KM2, VALUE), method='lm', se=FALSE, color='grey50') +
      geom_point(aes(AREA_KM2/TOTAL_AREA_KM2, VALUE, color=SITE_NAME), size=2) +
      facet_grid(VAR~LANDUSE, scales='free') +
      scale_color_discrete('') +
      scale_x_continuous(labels=scales::percent) +
      labs(x='Fraction Cumulative Land Use Area (%)', y=paste0(term_labels[[term]]),
           title=paste0('Annual FWM Concentration vs Fraction Cumulative Land Use Area\nDataset: ',
                        dataset,' | Period: ', periods[[dataset]],  ' | Extent: ', extent)) +
      theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
            axis.text.y=element_text(size=8),
            strip.text.x=element_text(size=6),
            strip.text.y=element_text(size=8),
            aspect.ratio=1)
    print(p)

#     p <- filter(df_site_area, DATASET==dataset, TERM==term,
#                 EXTENT==extent, !(LANDUSE %in% c("POU", "Total")),
#                 SEASON=="Summer (Jul-Sep)") %>%
#       filter(SITE_NAME %in% stn_primary[[dataset]]) %>%
#       filter(TOTAL_AREA_KM2>0) %>%
#       ggplot() +
#       geom_smooth(aes(AREA_KM2/TOTAL_AREA_KM2, VALUE), method='lm', se=FALSE, color='grey50') +
#       geom_point(aes(AREA_KM2/TOTAL_AREA_KM2, VALUE, color=SITE_NAME), size=2) +
#       facet_grid(VAR~LANDUSE, scales='free') +
#       scale_color_discrete('') +
#       scale_x_continuous(labels=scales::percent) +
#       labs(x='Fraction Cumulative Land Use Area (%)', y=paste0(term_labels[[term]]),
#            title=paste0('Summer (Jul-Sep) FWM Concentration vs Fraction Cumulative Land Use Area\nDataset: ',
#                         dataset,' | Period: ', periods[[dataset]],  ' | Extent: ', extent,
#                         ' | Season: Summer (Jul-Sep)')) +
#       theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
#             aspect.ratio=1)
#     print(p)

    for (variable in variables) {
      cat('....', variable, '\n')
      # cumulative fraction land use area
      p <- filter(df_site_area, DATASET==dataset, VAR==variable, TERM==term,
                  EXTENT==extent, !(LANDUSE %in% c("POU", "Total"))) %>%
        filter(SITE_NAME %in% stn_primary[[dataset]]) %>%
        filter(TOTAL_AREA_KM2>0) %>%
        ggplot() +
        geom_smooth(aes(AREA_KM2/TOTAL_AREA_KM2, VALUE), method='lm', se=FALSE, color='grey50') +
        geom_point(aes(AREA_KM2/TOTAL_AREA_KM2, VALUE, color=SITE_NAME), size=2) +
        facet_grid(SEASON~LANDUSE, scales='free') +
        scale_color_discrete('') +
        scale_x_continuous(labels=scales::percent) +
        labs(x='Fraction Cumulative Area per Land Use (%)', y=paste0(variable, " ", term_labels[[term]]),
             title=paste0('Seasonal FWM Concentration vs Fraction Cumulative Land Use Area\nDataset: ', dataset,' | Period: ', periods[[dataset]],  ' | Extent: ', extent,  ' | Variable: ', variable)) +
        theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
              axis.text.y=element_text(size=8),
              strip.text.x=element_text(size=6),
              strip.text.y=element_text(size=6),
              aspect.ratio=1)
      print(p)

#       p <- filter(df_site_area, DATASET==dataset, VAR==variable, TERM==term,
#                   EXTENT==extent, !(LANDUSE %in% c("POU"))) %>%
#         filter(SITE_NAME %in% stn_primary[[dataset]]) %>%
#         ggplot() +
#         geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
#         geom_segment(aes(x=AREA_KM2.FROM, xend=AREA_KM2.TO,
#                          y=VALUE.FROM, yend=VALUE.TO,
#                          size=MAINSTEM),
#                      data=filter(df_segments_site, DATASET==dataset, VAR==variable,
#                                  TERM==term, EXTENT==extent, !(LANDUSE %in% c("POU"))),
#                      alpha=0.5) +
#         geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME), size=3) +
#         facet_grid(SEASON~LANDUSE, scales='free') +
#         scale_color_discrete('') +
#         scale_size_manual(guide=FALSE, values=c('FALSE'=0.5, 'TRUE'=1)) +
#         labs(x='Cumulative Land Use Area (km2)', y=paste0(variable, " ",
#                                                           term_labels[[term]]),
#              title=paste0('Seasonal Mean vs Cumulative Land Use Area\nDataset: ',
#                           dataset,' | Period: ', periods[[dataset]],  ' | Extent: ',
#                           extent, ' | Variable: ', variable, ' | Term: ', term)) +
#         theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
#               aspect.ratio=1)
#       print(p)
    }

    dev.off()
  }
}

# alternative seasons ----

# seasons <- list(Annual=1:12,
#                 'Dec-Feb'=c(10:12),
#                 'Mar-May'=c(1:3),
#                 'Jun-Sep'=c(4:6),
#                 'Oct'=10,
#                 'Nov'=11,
#                 'Dec'=12,
#                 'Jan'=1,
#                 'Feb'=2,
#                 'Mar'=3,
#                 'Apr'=4,
#                 'May'=5,
#                 'Jun'=6,
#                 'Jul'=7,
#                 'Aug'=8,
#                 'Sep'=9)

# pdf(file.path("pdf", "recent", "loads-pou-valley-alt-seasons.pdf"), width=11, height=8.5)
# p <- filter(df_site_area, DATASET==dataset, TERM=="C", EXTENT==extent, LANDUSE=="POU",
#             VAR %in% c("TP", "PO4", "PP", "TSS")) %>%
#   mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2)) %>%
#   ggplot(aes(AREA_FRAC, VALUE, color=SITE_NAME)) +
#   geom_point(size=3) +
#   facet_grid(VAR ~ SEASON, scales="free_y") +
#   scale_x_continuous(labels=scales::percent) +
#   labs(x="Fraction Cumulative Drainage Area\nwith POU Irrigation Water Right (%)",
#        y="Concentration (ppb)",
#        title=paste0("FWM Concentrations vs. Fraction Drainage Area with POU Irrigation Water Right\n",
#                     "Dataset: ", dataset, " | Extent: ", extent))
# print(p)
#
# p <- filter(df_site_area, DATASET==dataset, TERM=="C", EXTENT==extent, LANDUSE=="POU", VAR=="TP",
#             !(SEASON %in% c("Annual", "Dec-Feb", "Mar-May", "Jun-Sep"))) %>%
#   mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2)) %>%
#   ggplot(aes(AREA_FRAC, VALUE, color=SITE_NAME)) +
#   geom_point(size=3) +
#   facet_wrap( ~ SEASON, scales="free_y") +
#   scale_x_continuous(labels=scales::percent) +
#   labs(x="Fraction Cumulative Drainage Area\nwith POU Irrigation Water Right (%)",
#        y="Concentration (ppb)",
#        title=paste0("FWM Concentrations vs. Fraction Drainage Area with POU Irrigation Water Right\n",
#                     "Dataset: ", dataset, " | Extent: ", extent))
# print(p)
#
#
# p <- filter(df_site_area, DATASET==dataset, TERM=="C", EXTENT==extent, LANDUSE=="POU", SEASON %in% c("Annual", "Dec-Feb", "Mar-May", "Jun-Sep"),
#             VAR %in% c("TP", "PO4", "PP", "TSS")) %>%
#   mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2)) %>%
#   ggplot(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
#   geom_point(size=3) +
#   facet_grid(VAR ~ SEASON, scales="free_y") +
#   labs(x="Cumulative Drainage Area\nwith POU Irrigation Water Right (km2)",
#        y="Concentration (ppb)",
#        title=paste0("FWM Concentrations vs. Cumulative Area with POU Irrigation Water Right\n",
#                     "Dataset: ", dataset, " | Extent: ", extent))
# print(p)
#
# p <- filter(df_site_area, DATASET==dataset, TERM=="C", EXTENT==extent, LANDUSE=="POU", VAR=="TP",
#             !(SEASON %in% c("Annual", "Dec-Feb", "Mar-May", "Jun-Sep"))) %>%
#   mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2)) %>%
#   ggplot(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
#   geom_point(size=3) +
#   facet_wrap( ~ SEASON, scales="free_y") +
#   labs(x="Cumulative Drainage Area\nwith POU Irrigation Water Right (km2)",
#        y="Concentration (ppb)",
#        title=paste0("FWM Concentrations vs. Cumulative Area with POU Irrigation Water Right\n",
#                     "Dataset: ", dataset, " | Extent: ", extent))
# print(p)
#
#
# p <- filter(df_site_area, DATASET==dataset, TERM=="C", EXTENT==extent, LANDUSE=="POU", SEASON %in% c("Annual", "Dec-Feb", "Mar-May", "Jun-Sep"),
#             VAR %in% c("TP", "PO4", "PP", "TSS")) %>%
#   ggplot(aes(TOTAL_AREA_KM2, VALUE, color=SITE_NAME)) +
#   geom_point(size=3) +
#   facet_grid(VAR ~ SEASON, scales="free_y") +
#   labs(x="Total Cumulative Drainage Area (km2)",
#        y="Concentration (ppb)",
#        title=paste0("FWM Concentrations vs. Total Cumulative Area\n",
#                     "Dataset: ", dataset, " | Extent: ", extent))
# print(p)
#
# p <- filter(df_site_area, DATASET==dataset, TERM=="C", EXTENT==extent, LANDUSE=="POU", VAR=="TP",
#             !(SEASON %in% c("Annual", "Dec-Feb", "Mar-May", "Jun-Sep"))) %>%
#   ggplot(aes(TOTAL_AREA_KM2, VALUE, color=SITE_NAME)) +
#   geom_point(size=3) +
#   facet_wrap( ~ SEASON, scales="free_y") +
#   labs(x="Total Cumulative Drainage Area (km2)",
#        y="Concentration (ppb)",
#        title=paste0("FWM Concentrations vs. Total Cumulative Area\n",
#                     "Dataset: ", dataset, " | Extent: ", extent))
# print(p)
#
#
# dev.off()


# report ----
png('report/results-loads-pou-recent-valley.png', width=8, height=8, res=200, units='in')
p <- filter(df_site_area, DATASET=='RECENT', TERM=="C", EXTENT=='valley', LANDUSE=="POU") %>%
  mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2)) %>%
  ggplot(aes(AREA_FRAC, VALUE, color=SITE_NAME)) +
  geom_point(size=2) +
  facet_grid(VAR ~ SEASON, scales="free_y") +
  scale_x_continuous(labels=scales::percent) +
  scale_color_discrete('') +
  labs(x="Cumulative Fraction POU Irrigation Area (%)\nLower Valley Only",
       y="Concentration (ppb)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        strip.text.x=element_text(size=8),
        aspect.ratio=1)
print(p)
dev.off()

png('report/results-loads-pou-por-valley.png', width=8, height=8, res=200, units='in')
p <- filter(df_site_area, DATASET=='POR', TERM=="C", EXTENT=='valley', LANDUSE=="POU") %>%
  mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2)) %>%
  ggplot(aes(AREA_FRAC, VALUE, color=SITE_NAME)) +
  geom_point(size=2) +
  facet_grid(VAR ~ SEASON, scales="free_y") +
  scale_x_continuous(labels=scales::percent) +
  scale_color_discrete('') +
  labs(x="Cumulative Fraction POU Irrigation Area (%)\nLower Valley Only",
       y="Concentration (ppb)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        strip.text.x=element_text(size=8),
        aspect.ratio=1)
print(p)
dev.off()

png('report/results-loads-pou-recent-basin.png', width=8, height=8, res=200, units='in')
p <- filter(df_site_area, DATASET=='RECENT', TERM=="C", EXTENT=='basin', LANDUSE=="POU") %>%
  mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2)) %>%
  ggplot(aes(AREA_FRAC, VALUE, color=SITE_NAME)) +
  geom_point(size=2) +
  facet_grid(VAR ~ SEASON, scales="free_y") +
  scale_x_continuous(labels=scales::percent) +
  scale_color_discrete('') +
  labs(x="Cumulative Fraction POU Irrigation Area (%)",
       y="Concentration (ppb)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        strip.text.x=element_text(size=8),
        aspect.ratio=1)
print(p)
dev.off()

png('report/results-loads-pou-por-basin.png', width=8, height=8, res=200, units='in')
p <- filter(df_site_area, DATASET=='POR', TERM=="C", EXTENT=='basin', LANDUSE=="POU") %>%
  mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2)) %>%
  ggplot(aes(AREA_FRAC, VALUE, color=SITE_NAME)) +
  geom_point(size=2) +
  facet_grid(VAR ~ SEASON, scales="free_y") +
  scale_x_continuous(labels=scales::percent) +
  scale_color_discrete('') +
  labs(x="Cumulative Fraction POU Irrigation Area (%)",
       y="Concentration (ppb)") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        strip.text.x=element_text(size=8),
        aspect.ratio=1)
print(p)
dev.off()

# nlcd
png('report/results-loads-nlcd-recent-basin.png', width=12, height=8, res=200, units='in')
p <- filter(df_site_area, DATASET=="RECENT", TERM=="C",
            EXTENT=="basin", !(LANDUSE %in% c("POU", "Total")),
            SEASON=="Annual") %>%
  filter(SITE_NAME %in% stn_primary[["RECENT"]]) %>%
  filter(TOTAL_AREA_KM2>0) %>%
  ggplot() +
  geom_smooth(aes(AREA_KM2/TOTAL_AREA_KM2, VALUE), method='lm', se=FALSE, color='grey50') +
  geom_point(aes(AREA_KM2/TOTAL_AREA_KM2, VALUE, color=SITE_NAME), size=2) +
  facet_grid(VAR~LANDUSE, scales='free') +
  scale_color_discrete('') +
  scale_x_continuous(labels=scales::percent) +
  labs(x='Fraction Cumulative Land Use Area (%)', y='Concentration (ppb)') +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8),
        axis.text.y=element_text(size=8),
        strip.text.x=element_text(size=6),
        strip.text.y=element_text(size=8),
        aspect.ratio=1)
print(p)
dev.off()

# cumulative area
png('report/results-loads-cumarea-tp-recent-basin.png', width=10, height=5, res=200, units='in')
p <- filter(df_site_area, DATASET=='RECENT', VAR=='TP', EXTENT=='basin', LANDUSE=='Total') %>%
  filter(SITE_NAME %in% stn_primary[['RECENT']]) %>%
  ggplot() +
  geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
  geom_segment(aes(x=AREA_KM2.FROM, xend=AREA_KM2.TO, y=VALUE.FROM, yend=VALUE.TO, size=MAINSTEM),
               data=filter(df_segments_site, DATASET=='RECENT', VAR=='TP', EXTENT=='basin', LANDUSE=='Total'),
               alpha=0.5) +
  geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME), size=3) +
  facet_grid(TERM~SEASON, scales='free_y') +
  scale_color_discrete('') +
  scale_size_manual(guide=FALSE, values=c('FALSE'=0.5, 'TRUE'=1)) +
  labs(x='Cumulative Drainage Area (km2)', y=paste(c('Flow (hm3/d)', 'Load (kg/d)', 'Conc (ppb)'),
                                                   collapse='         ')) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        aspect.ratio=1)
print(p)
dev.off()

png('report/results-loads-cumarea-conc-recent-basin.png', width=10, height=5, res=200, units='in')
p <- filter(df_site_area, DATASET=='RECENT', TERM=='C', EXTENT=='basin', LANDUSE=='Total') %>%
  filter(SITE_NAME %in% stn_primary[['RECENT']]) %>%
  ggplot() +
  geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME)) +
  geom_segment(aes(x=AREA_KM2.FROM, xend=AREA_KM2.TO, y=VALUE.FROM, yend=VALUE.TO, size=MAINSTEM),
               data=filter(df_segments_site, DATASET=='RECENT', TERM=='C', EXTENT=='basin', LANDUSE=='Total'),
               alpha=0.5) +
  geom_point(aes(AREA_KM2, VALUE, color=SITE_NAME), size=3) +
  facet_grid(VAR~SEASON, scales='free_y') +
  scale_color_discrete('') +
  scale_size_manual(guide=FALSE, values=c('FALSE'=0.5, 'TRUE'=1)) +
  labs(x='Cumulative Drainage Area (km2)', y='Concentration (ppb)') +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
        aspect.ratio=1)
print(p)
dev.off()