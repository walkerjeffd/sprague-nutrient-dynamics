library(dplyr)
library(lubridate)

outliers_list <- list(TP=list(Power=c('2013-09-05'),
                              Godowa=c('2003-12-22', '2012-07-25', '2013-07-09', '2013-07-23'),
                              Sycan=c('2003-12-22'),
                              NF_Ivory=c('2013-07-09', '2013-07-17')),
                      TSS=list(Lone_Pine=c('2011-04-04')),
                      NH4=list(Sycan=c('2008-11-04')),
                      NO23=list(Sycan=c('2008-11-04')))

outliers <- lapply(names(outliers_list), function(variable) {
  lapply(names(outliers_list[[variable]]), function(site) {
    data.frame(DATE=outliers_list[[variable]][[site]],
               VAR=variable,
               SITE_NAME=site,
               stringsAsFactors=FALSE)
  }) %>%
    rbind_all
}) %>%
  rbind_all %>%
  mutate(FLAGGED=TRUE,
         DATE=ymd(DATE))

saveRDS(outliers, file='outliers.Rdata')
