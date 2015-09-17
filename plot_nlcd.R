library(dplyr)
library(tidyr)
library(ggplot2)
theme_set(theme_bw())

rm(list=ls())

load('gis.Rdata')
load('nlcd.Rdata')
load('network.Rdata')
nlcd.subbasin.raw <- nlcd.subbasin
nlcd.subbasin <- filter(nlcd.subbasin.raw, SOURCE=="NLCD_CAT") %>%
  select(-TOTAL_AREA_KM2, -AREA_FRAC) %>%
  group_by(EXTENT, LANDUSE, SITE_NAME) %>%
  summarise(AREA_KM2=sum(AREA_KM2)) %>%
  ungroup %>%
  spread(SITE_NAME, AREA_KM2) %>%
  mutate('Godowa+Sycan'=Godowa+Sycan,
         'SF+NF'=SF+NF,
         'SF_Ivory+NF_Ivory'=SF_Ivory+NF_Ivory) %>%
  gather(SITE_NAME, AREA_KM2, -EXTENT, -LANDUSE) %>%
  group_by(EXTENT, SITE_NAME) %>%
  mutate(TOTAL_AREA_KM2=sum(AREA_KM2)) %>%
  ungroup %>%
  mutate(AREA_FRAC=ifelse(TOTAL_AREA_KM2==0, 0, AREA_KM2/TOTAL_AREA_KM2)) %>%
  mutate(SITE_NAME=ordered(SITE_NAME,
                           levels=c('Power', 'Lone_Pine', 'Godowa+Sycan',
                                    'Godowa', 'Sycan',
                                    'SF_Ivory+NF_Ivory', 'SF+NF',
                                    'SF_Ivory', 'SF', 'NF_Ivory', 'NF')))
stopifnot(all(table(nlcd.subbasin$EXTENT, nlcd.subbasin$SITE_NAME)==8))

group_by(nlcd.subbasin, EXTENT, SITE_NAME) %>%
  summarise(AREA_KM2=sum(AREA_KM2),
            AREA_FRAC=sum(AREA_FRAC)) %>%
  as.data.frame

nlcd_segments_basin <- filter(network, DATASET=='RECENT') %>%
  select(-DATASET) %>%
  left_join(filter(nlcd.subbasin, EXTENT=='basin') %>%
              select(SITE_NAME, LANDUSE, AREA_KM2, TOTAL_AREA_KM2),
            by=c(FROM='SITE_NAME')) %>%
  left_join(filter(nlcd.subbasin, EXTENT=='basin') %>%
              select(SITE_NAME, LANDUSE, AREA_KM2, TOTAL_AREA_KM2),
            by=c(TO='SITE_NAME', 'LANDUSE')) %>%
  rename(AREA_KM2.FROM=AREA_KM2.x,
         TOTAL_AREA_KM2.FROM=TOTAL_AREA_KM2.x,
         AREA_KM2.TO=AREA_KM2.y,
         TOTAL_AREA_KM2.TO=TOTAL_AREA_KM2.y) %>%
  droplevels
nlcd_segments_valley <- filter(network, DATASET=='RECENT') %>%
  select(-DATASET) %>%
  left_join(filter(nlcd.subbasin, EXTENT=='valley') %>%
              select(SITE_NAME, LANDUSE, AREA_KM2, TOTAL_AREA_KM2),
            by=c(FROM='SITE_NAME')) %>%
  left_join(filter(nlcd.subbasin, EXTENT=='valley') %>%
              select(SITE_NAME, LANDUSE, AREA_KM2, TOTAL_AREA_KM2),
            by=c(TO='SITE_NAME', 'LANDUSE')) %>%
  rename(AREA_KM2.FROM=AREA_KM2.x,
         TOTAL_AREA_KM2.FROM=TOTAL_AREA_KM2.x,
         AREA_KM2.TO=AREA_KM2.y,
         TOTAL_AREA_KM2.TO=TOTAL_AREA_KM2.y) %>%
  droplevels

# pdf ----
scale_fill_nlcd <- scale_fill_manual('Land Use',
                                     values=c('Shrubland'='#AF963C',
                                              'Herbaceous'='#FDE9AA',
                                              'Forest'='#85C77E',
                                              'Barren'='#D3CDC0',
                                              'Wetlands'='#C8E6F8',
                                              'Water'='#5475A8',
                                              'Planted/Cultivated'='red',
                                              'Developed'='#E8D1D1'))
pdf(file.path('pdf', 'land-use-nlcd.pdf'), width=11, height=8.5)
p <- nlcd.subbasin %>%
  ggplot(aes(SITE_NAME, AREA_FRAC, fill=LANDUSE)) +
  geom_bar(position='fill', stat='identity') +
  labs(x='Subbasin', y='Fraction Area') +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) +
  scale_fill_nlcd +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~EXTENT) +
  ggtitle('NLCD Land Use Composition by Subbasin')
print(p)

p <- nlcd.subbasin %>%
  ggplot(aes(SITE_NAME, AREA_FRAC, fill=LANDUSE)) +
  geom_bar(stat='identity') +
  labs(x='Subbasin', y='Fraction Area') +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_nlcd +
  facet_grid(LANDUSE~EXTENT, scales='free_y') +
  ggtitle('NLCD Land Use Composition by Subbasin')
print(p)

p <- nlcd.subbasin %>%
  filter(EXTENT=='basin', SITE_NAME %in% union(nlcd_segments_basin$TO, nlcd_segments_basin$FROM)) %>%
  group_by(EXTENT, SITE_NAME, TOTAL_AREA_KM2, LANDUSE) %>%
  summarise(AREA_KM2=sum(AREA_KM2)) %>%
  ggplot(aes(TOTAL_AREA_KM2, AREA_KM2)) +
  geom_segment(aes(x=TOTAL_AREA_KM2.FROM, xend=TOTAL_AREA_KM2.TO, y=AREA_KM2.FROM, yend=AREA_KM2.TO, size=MAINSTEM),
               data=nlcd_segments_basin, alpha=0.5) +
  geom_point(aes(color=SITE_NAME), size=3) +
  facet_wrap(~LANDUSE, scales='free') +
  scale_color_discrete('') +
  scale_size_manual(guide=FALSE, values=c('FALSE'=0.25, 'TRUE'=1)) +
  labs(x='Total Cumulative Drainage Area (km2)', y='Cumulative Land Use Area (km2)',
       title='Cumulative Land Use Area vs. Total Cumulative Drainage Area\nDataset: NLCD, Extent: Basin')
print(p)

p <- nlcd.subbasin %>%
  filter(EXTENT=='valley', SITE_NAME %in% union(nlcd_segments_valley$TO, nlcd_segments_valley$FROM)) %>%
  group_by(EXTENT, SITE_NAME, TOTAL_AREA_KM2, LANDUSE) %>%
  summarise(AREA_KM2=sum(AREA_KM2)) %>%
  ggplot(aes(TOTAL_AREA_KM2, AREA_KM2)) +
  geom_segment(aes(x=TOTAL_AREA_KM2.FROM, xend=TOTAL_AREA_KM2.TO, y=AREA_KM2.FROM, yend=AREA_KM2.TO, size=MAINSTEM),
               data=nlcd_segments_valley, alpha=0.5) +
  geom_point(aes(color=SITE_NAME), size=3) +
  facet_wrap(~LANDUSE, scales='free') +
  scale_color_discrete('') +
  scale_size_manual(guide=FALSE, values=c('FALSE'=0.25, 'TRUE'=1)) +
  labs(x='Total Cumulative Drainage Area (km2)', y='Cumulative Land Use Area (km2)',
       title='Cumulative Land Use Area vs. Total Cumulative Valley Area\nDataset: NLCD, Extent: Valley')
print(p)

dev.off()

# report ----

png("report/nlcd-subbasin-composition.png", width=8, height=4, res=200, units="in")
p <- nlcd.subbasin %>%
  filter(SITE_NAME %in% c("Power", "Lone_Pine", "Godowa", "Sycan", "SF_Ivory", "SF", "NF_Ivory", "NF")) %>%
  mutate(EXTENT=plyr::revalue(EXTENT, c("basin"="Total Basin", "valley"="Lower Valley"))) %>%
  arrange(desc(LANDUSE)) %>%
  ggplot(aes(SITE_NAME, AREA_FRAC, fill=LANDUSE)) +
  geom_bar(position='fill', stat='identity') +
  labs(x='Station', y='Fraction of Drainage Area (%)') +
  theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) +
  scale_fill_nlcd +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~EXTENT)
print(p)
dev.off()

# # pdf
# pdf(file.path('pdf', 'land-use-gap.pdf'), width=11, height=8.5)
#
# gap %>%
#   arrange(NVC_CLASS) %>%
#   group_by(EXTENT, INC_SITE_ABBR, NVC_CLASS) %>%
#   summarise(AREA_FRAC=sum(AREA_FRAC)) %>%
#   ggplot(aes(INC_SITE_ABBR, AREA_FRAC, fill=NVC_CLASS)) +
#   geom_bar(position='fill', stat='identity') +
#   labs(x='', y='Fraction Area') +
#   theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) +
#   scale_y_continuous(labels=scales::percent) +
#   scale_fill_manual('Land Use', values=c('Shrubland & Grassland'='#AF963C',
#                                          'Recently Disturbed or Modified'='#FDE9AA',
#                                          'Forest & Woodland'='#85C77E',
#                                          'Semi-Desert'='#D3CDC0',
#                                          'Polar & High Montane Vegetation'='#C8E6F8',
#                                          'Nonvascular & Sparse Vascular Rock Vegetation'='green',
#                                          'Introduced & Semi Natural Vegetation'='orange',
#                                          'Open Water'='#5475A8',
#                                          'Agricultural Vegetation'='red',
#                                          'Developed & Other Human Use'='#E8D1D1')) +
#   facet_wrap(~EXTENT) +
#   ggtitle('USGS GAP Dataset')
#
# gap %>%
#   arrange(NVC_CLASS) %>%
#   group_by(EXTENT, INC_SITE_ABBR, NVC_CLASS) %>%
#   summarise(AREA_FRAC=sum(AREA_FRAC)) %>%
#   ggplot(aes(INC_SITE_ABBR, AREA_FRAC, fill=NVC_CLASS)) +
#   geom_bar(stat='identity') +
#   labs(x='', y='Fraction Area') +
#   scale_y_continuous(labels=scales::percent) +
#   theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1)) +
#   scale_fill_manual('Land Use', values=c('Shrubland & Grassland'='#AF963C',
#                                          'Recently Disturbed or Modified'='#FDE9AA',
#                                          'Forest & Woodland'='#85C77E',
#                                          'Semi-Desert'='#D3CDC0',
#                                          'Polar & High Montane Vegetation'='#C8E6F8',
#                                          'Nonvascular & Sparse Vascular Rock Vegetation'='green',
#                                          'Introduced & Semi Natural Vegetation'='orange',
#                                          'Open Water'='#5475A8',
#                                          'Agricultural Vegetation'='red',
#                                          'Developed & Other Human Use'='#E8D1D1')) +
#   facet_grid(NVC_CLASS~EXTENT, scales='free_y') +
#   ggtitle('USGS GAP Dataset')
#
# dev.off()