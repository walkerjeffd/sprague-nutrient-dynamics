
# gap ----
# gap_codes <- read.csv(file.path(DATA_DIR, 'sprague', 'gap', 'metadata.csv'), stringsAsFactors=FALSE) %>%
#   select(VALUE, NVC_CLASS, NVC_SUBCL, NVC_FORM, NVC_DIV, NVC_MACRO)
# 
# gap_basin <- read.csv(file.path(GIS_DIR, 'sprague/gap/zonal_basin.csv'), stringsAsFactors=FALSE) %>%
#   mutate(EXTENT='basin')
# gap_valley <- read.csv(file.path(GIS_DIR, 'sprague/gap/zonal_valley.csv'), stringsAsFactors=FALSE) %>%
#   mutate(EXTENT='valley') %>%
#   filter(!(SITE %in% c('SR0050', 'SR0040')))
# 
# gap <- rbind_list(gap_basin, gap_valley) %>%
#   dplyr::select(-X__fid__) %>%
#   gather(LULC, COUNT, -SITE, -AreaSqKM, -EXTENT) %>%
#   mutate(LULC=as.integer(extract_numeric(LULC)),
#          AREA_KM2=COUNT*30*30/1000000) %>%
#   filter(!is.na(AREA_KM2), LULC != 0)
# 
# # compute area fractions
# gap <- group_by(gap, EXTENT, SITE) %>%
#   mutate(AREA_KM2_SUM=sum(AREA_KM2),
#          AREA_FRAC=AREA_KM2/AREA_KM2_SUM) %>%
#   ungroup
# 
# # add columns
# gap <- gap %>%
#   filter(SITE != 'WR1000') %>%
#   left_join(dplyr::select(gap_codes, VALUE, NVC_CLASS, NVC_SUBCL), by=c('LULC'='VALUE')) %>%
#   left_join(dplyr::select(stn_ivory, SITE, INC_SITE, INC_SITE_NAME, INC_SITE_ABBR), by='SITE') %>%
#   mutate(SITE=ordered(SITE, levels=stn_ivory$SITE))
# 
# # set factor level order
# gap <- mutate(gap, NVC_CLASS=ordered(NVC_CLASS,
#   levels=c('Developed & Other Human Use', 'Recently Disturbed or Modified', 'Agricultural Vegetation',
#            'Introduced & Semi Natural Vegetation', 'Nonvascular & Sparse Vascular Rock Vegetation',
#            'Polar & High Montane Vegetation', 'Shrubland & Grassland', 'Semi-Desert',
#            'Forest & Woodland', 'Open Water')))
# 
# # check area sums (AreaSqKM is official area)
# filter(gap, EXTENT=='basin') %>%
#   group_by(SITE, AreaSqKM) %>%
#   summarise(AREA_KM2=sum(AREA_KM2)) %>%
#   ungroup %>%
#   mutate(AREA_DIFF=AREA_KM2-AreaSqKM)
# 
# # check that sum of area_frac is unity
# group_by(gap, EXTENT, SITE) %>%
#   summarise(SUM=sum(AREA_FRAC))
# 
# # summary of basin/valley areas
# group_by(gap, EXTENT, INC_SITE_NAME, AreaSqKM) %>%
#   summarise(AREA_KM2=sum(AREA_KM2)) %>%
#   spread(EXTENT, AREA_KM2)
