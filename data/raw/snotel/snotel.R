library(dplyr)
library(rvest)
library(httr)

DATADIR <- getOption('UKL_DATA')


stn.snotel <- html("http://www.nrcs.usda.gov/wps/portal/nrcs/detail/or/snow/?cid=nrcs142p2_046159") %>%
  html_node(xpath = '//*[@id="detail"]/table') %>%
  html_table
names(stn.snotel) <- plyr::revalue(names(stn.snotel),
                                   c('SITE NAME'='SITE_NAME',
                                     'ELEV (ft)'='ELEV_FT',
                                     'LAND OWNERSHIP'='LAND_OWNERSHIP',
                                     'LATITUDE (degrees)'='LATITUDE',
                                     'LONGITUDE (degrees)'='LONGITUDE',
                                     'SITE ID'='SITE_ID',
                                     'INSTALLED In WY'='START_WYEAR'))
stn.snotel$LONGITUDE <- -stn.snotel$LONGITUDE

snotel_dir <- file.path(DATADIR, 'met', 'snotel')

write.csv(stn.snotel, file = file.path(snotel_dir, 'snotel_stations.csv'), row.names = FALSE)


ids <- c('SWAN LAKE MTN'='1077',
         'CRAZYMAN FLAT'='1010',
         'QUARTZ MOUNTAIN'='706',
         'SUMMER RIM'='800',
         'TAYLOR BUTTE'='810')

stopifnot(all(names(ids) %in% stn.snotel$SITE_NAME))

today <- Sys.Date() %>% format("%Y%m%d")
for (i in seq_along(ids)) {
  site <- ids[i]
  url <- paste0('http://www.wcc.nrcs.usda.gov/reportGenerator/view_csv/customSingleStationReport/daily/', unname(site),':OR:SNTL%7Cid=%22%22%7Cname/POR_BEGIN,POR_END/WTEQ::value')
  fname <- paste0(paste(unname(site), names(site), today, sep='_'), '.txt')
  txt <- GET(url)
  content(txt, "raw") %>%
    writeBin(file.path(snotel_dir, fname))
  Sys.sleep(5)
}



