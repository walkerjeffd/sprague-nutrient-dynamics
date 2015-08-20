library(dplyr)

rm(list=ls())

# create network ----
network <- list(RECENT=data.frame(FROM =  c('Lone_Pine', 'Godowa+Sycan', 'Sycan',        'Godowa',       'SF_Ivory+NF_Ivory', 'NF_Ivory',          'SF_Ivory',          'NF',       'SF'),
                                  TO =  c(  'Power',     'Lone_Pine',    'Godowa+Sycan', 'Godowa+Sycan', 'Godowa',            'SF_Ivory+NF_Ivory', 'SF_Ivory+NF_Ivory', 'NF_Ivory', 'SF_Ivory'),
                                  MAINSTEM=c(TRUE,        TRUE,           FALSE,          TRUE,           TRUE,                FALSE,               FALSE,               FALSE,      FALSE),
                                  stringsAsFactors=FALSE),
                POR=data.frame(FROM =  c('Lone_Pine', 'Godowa+Sycan', 'Sycan',        'Godowa',       'SF+NF',  'SF',    'NF'),
                               TO =    c('Power',     'Lone_Pine',    'Godowa+Sycan', 'Godowa+Sycan', 'Godowa', 'SF+NF', 'SF+NF'),
                               MAINSTEM=c(TRUE,        TRUE,           FALSE,          TRUE,           TRUE,     FALSE,   FALSE),
                               stringsAsFactors=FALSE))
network <- lapply(names(network), function(d) {
  x <- network[[d]]
  x$DATASET <- d
  x
}) %>%
  rbind_all

# save ----
save(network, file='network.Rdata')
