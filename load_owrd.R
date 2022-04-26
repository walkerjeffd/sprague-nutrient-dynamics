library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(cowplot)
theme_set(theme_bw())

rm(list=ls())

cat(paste0(rep('=', 80), collapse=''), '\n')
cat("Loading OWRD dataset...\n\n")

#DATA_DIR <- getOption('UKL_DATA')
DATA_DIR <- './data'

# load stn ----
filename <- file.path(DATA_DIR, 'raw', 'owrd', 'owrd_stations.csv')
cat("Loading stations from:", filename, "\n")
stn.owrd <- read.csv(filename, stringsAsFactors=FALSE)

stn.owrd <- mutate(stn.owrd,
                   LAT=as.numeric(gsub("[^0-9.]+", "", as.character(LAT))),
                   LON=as.numeric(gsub("[^0-9.]+", "", as.character(LON))))
stn.owrd <- mutate(stn.owrd,
                   LAT_DEG=floor(LAT/10000),
                   LAT_MIN=floor((LAT-LAT_DEG*10000)/100),
                   LAT_SEC=LAT-LAT_DEG*10000-LAT_MIN*100,
                   LON_DEG=floor(LON/10000),
                   LON_MIN=floor((LON-LON_DEG*10000)/100),
                   LON_SEC=LON-LON_DEG*10000-LON_MIN*100,
                   LAT=LAT_DEG+(LAT_MIN+LAT_SEC/60)/60,
                   LON=-(LON_DEG+(LON_MIN+LON_SEC/60)/60),
                   DRAINAGE_AREA_SQMI=DRAINAGE_AREA_MI2)
stn.owrd <- select(stn.owrd, STATION_ID, DESCRIPTION, LAT, LON, DRAINAGE_AREA_SQMI, SITE_NAME)

# load ----
q.owrd.prior <- lapply(stn.owrd$STATION_ID, function(site) {
  filename <- file.path(DATA_DIR, 'raw', 'owrd',
                        paste0('Station_', site, '_mean_daily_flow.txt'))


  cat("Loading flow data from:", filename, "\n")

  df <- read.table(filename,
                   sep='\t',
                   header=TRUE,
                   as.is=TRUE)
  return(df)
}) %>%
  do.call(rbind, .)

q.owrd.prior <- mutate(q.owrd.prior,
                 STATION_ID=station_nbr,
                 DATE=mdy(record_date),
                 FLOW=mean_daily_flow_cfs,
                 YEAR=year(DATE)) %>%
  select(STATION_ID, DATE, FLOW,YEAR) %>%
  mutate(SOURCE='OWRD') %>%
  left_join(select(stn.owrd, STATION_ID, SITE_NAME), by="STATION_ID")

stn_period <- group_by(q.owrd.prior, STATION_ID) %>%
  dplyr::summarise(START_DATE=min(DATE),
            END_DATE=max(DATE))

stn.owrd <- filter(stn.owrd, STATION_ID %in% stn_period$STATION_ID) %>%
  left_join(stn_period, by="STATION_ID")

# plot ----
filter(q.owrd.prior, !is.na(FLOW)) %>%
  ggplot(aes(DATE, FLOW)) +
  geom_line() +
  facet_wrap(~STATION_ID+SITE_NAME, scales='free_y', ncol=1) +
  labs(x='', y='Flow (cfs)', title='OWRD Daily Flow Data')

# import data

# some flow data were labeled as missing, more recent (2019 - 2021). they were filtered out for this dataframe
# there are slight differences in some cases, check units
q.owrd <- lapply(stn.owrd$STATION_ID,function(x) {
  data <- read.csv(url(paste0(
    "https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx?station_nbr=",
    x, # site number
    "&start_date=1/1/1982%2012:00:00%20AM", # set start date
    "&end_date=7/21/2021%2012:00:00%20AM&dataset=MDF&format=html"))) # set end date
  data_tidy <- data %>%
    dplyr::rename(all = 1) %>% # tidy the output
    mutate(
      station_nbr = substr(all,1,8),
      record_date = substr(all,9,19),
      mean_daily_flow_cfs = substr(all,20,24),
      temporary = substr(all,25,nchar(all)),
      download_datetime = substr(temporary,(nchar(temporary)+1)-16,nchar(temporary)),
      published_status = substr(temporary,1,nchar(temporary)-16),
      SOURCE="OWRD") %>%
    select(-c(all,temporary)) %>%
    dplyr::rename(FLOW=mean_daily_flow_cfs,DATE=record_date,STATION_ID=station_nbr) %>%
    filter(!FLOW%in%c("","Mis")) # filter out blank header rows from binding the dataframes and rows with 'Missing' data

})

q.owrd <- do.call(bind_rows,q.owrd)

q.owrd <- q.owrd %>%
  select(DATE,STATION_ID,FLOW,SOURCE) %>%
  as.data.frame() %>%
  mutate(DATE=gsub("\t","",DATE),
         FLOW=gsub("\t","",FLOW),
         DATE_formatted=as.Date(DATE,format="%m-%d-%Y"),
         YEAR=year(DATE_formatted)) %>%
 # mutate(Flow__ = ifelse(FLOW=="Mis",NA,FLOW))
  mutate(STATION_ID=as.integer(STATION_ID),
         FLOW=as.numeric(FLOW)) %>%
  select(-DATE) %>% dplyr::rename(DATE=DATE_formatted)%>%
  left_join(select(stn.owrd, STATION_ID, SITE_NAME), by="STATION_ID")

owrd.orig <- q.owrd.prior %>%
  mutate(DATE=ymd(DATE),
         YEAR=year(DATE))

# compare previous data with new import
q.owrd %>%
 # filter(YEAR=="2013") %>%
  ggplot()+
  geom_point( aes(x=DATE,y=FLOW),color="red",alpha=0.5)+
  geom_point(data=q.owrd.prior,aes(x=DATE,y=FLOW),color="blue",alpha=0.5)+
  facet_wrap(~STATION_ID)+
  theme_bw()

# combine prior and updated data----

# data prior to 2008 are labeled as missing (Mis) in the data import
# combine updated data import with prior data set to produce the appropriate range
# could this be a cutoff in the online data repository?

q.owrd <- full_join(q.owrd.prior %>% filter(!DATE %in% q.owrd$DATE),
                            q.owrd)


plot_grid(q.owrd %>%
   filter(YEAR>1980) %>%
  ggplot()+
  geom_point( aes(x=DATE,y=FLOW),color="black",alpha=0.5)+
 # geom_point(data=q.owrd.prior,aes(x=DATE,y=FLOW),color="blue",alpha=0.5)+
  facet_wrap(~STATION_ID,ncol=1)+
  theme_bw(),

q.owrd.prior %>%
  filter(YEAR>1980) %>%
  ggplot()+
  geom_point( aes(x=DATE,y=FLOW),color="black",alpha=0.5)+
  # geom_point(data=q.owrd.prior,aes(x=DATE,y=FLOW),color="blue",alpha=0.5)+
  facet_wrap(~STATION_ID,ncol=1)+
  theme_bw())

# time series of FLOW to verify the data are the same with the new and prior imports ----
names <- unique(q.owrd$YEAR)
plot_list = list()
for (ii in names) {

  p <-
    q.owrd %>%
    mutate(FLOW=as.numeric(FLOW)) %>%
    filter(YEAR == ii) %>%
    ggplot()+
    geom_point( aes(x=DATE,y=FLOW),color="red",alpha=0.5)+
    geom_point(data=filter(q.owrd.prior, YEAR==ii),aes(x=DATE,y=FLOW),color="blue",alpha=0.5)+
    facet_wrap(~STATION_ID+SITE_NAME)+
    theme_bw() +
    ggtitle(as.character(ii))+
    labs(x="Date",y=as.character(ii))
  plot_list[[ii]] = p
  ggsave(p, file=paste0("explore/owrd/owrd_", ii,".png"), width = 11, height = 8.5, units = "in")
}

filename <- "./pdf/import/owrd.pdf"
pdf(file = filename,height=8.5,width=11)

for(ii in names) {
  print(plot_list[[ii]])

}
dev.off()




# save ----
filename <- file.path('csv', 'stn_owrd.csv')
cat('\nSaving OWRD stations to:', filename, '\n')
select(stn.owrd, SITE_NAME, STATION_ID, DESCRIPTION, LAT, LON,
       DRAINAGE_AREA_SQMI, START_DATE, END_DATE) %>%
  write.csv(file=filename, row.names=FALSE)

filename <- 'owrd.Rdata'
cat('Saving OWRD flow dataset to:', filename, '\n')
save(q.owrd, stn.owrd, file=filename)

cat('\n\n')

