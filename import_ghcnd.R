library(tidyverse)
library(rnoaa)
library(lubridate)
library(waldo)

# ghcnd

theme_set(theme_bw())

rm(list=ls())

cat(paste0(rep('=', 80), collapse=''), '\n')
cat("Loading GHCND dataset...\n\n")

#DATA_DIR <- getOption('UKL_DATA')
DATA_DIR <- './data'


# load site info ----
filename <- file.path(DATA_DIR, 'sprague', 'ghcnd', 'ghcnd_sprague.csv')
cat('Loading GHCND dataset from:', filename, '\n\n')

ghcnd <- read.csv(filename, stringsAsFactors=FALSE, na.strings='-9999') %>%
  select(STATION, STATION_NAME, ELEVATION, LATITUDE, LONGITUDE, DATE, TMIN, TMAX, PRCP, SNOW) %>%
  mutate(TMIN=TMIN/10, # degC
         TMAX=TMAX/10, # degC
         PRCP=PRCP/10) # mm/day
ghcnd <- mutate(ghcnd,
                DATE=ymd(DATE),
                MONTH=month(DATE),
                WYEAR=fluxr::wyear(DATE))
stn.ghcnd <- ghcnd %>% select(STATION, STATION_NAME, ELEVATION, LATITUDE, LONGITUDE) %>%  mutate(STATION_ID=substr(STATION,7,nchar(STATION)))  %>% unique

stn.ghcnd.update <- stn.ghcnd %>%
  select(STATION_ID) %>%
  add_row(STATION_ID=c("USR0000OGER","USS0021G04S")) %>%
  mutate(STATION_ID=as.factor(STATION_ID)) %>% unique


# import with ghcnd_search
#data <- ghcnd_search(stationid = "USS0021G04S" ,date_min = "1999-10-01",var=c("PRCP","TMIN","TMAX","TAVG","SNOW"))

#data <- as.data.frame(data)
# not all variables available for all sites
# importing variables separately as i couldn't combine them imported together due to date issues i think
prcp.ghcnd <- lapply(stn.ghcnd.update$STATION_ID,function(x) {
  data <- ghcnd_search(stationid = x ,date_min = "1999-10-01",var="PRCP")
})
prcp.ghcnd <- do.call(bind_rows,prcp.ghcnd)
prcp.ghcnd <- do.call(bind_cols,prcp.ghcnd)


tmin.ghcnd <- lapply(stn.ghcnd.update$STATION_ID,function(x) {
  data <- ghcnd_search(stationid = x ,date_min = "1999-10-01",var="TMIN")
})
tmin.ghcnd <- do.call(bind_rows,tmin.ghcnd)
tmin.ghcnd <- do.call(bind_cols,tmin.ghcnd)

tmax.ghcnd <- lapply(stn.ghcnd.update$STATION_ID,function(x) {
  data <- ghcnd_search(stationid = x ,date_min = "1999-10-01",var="tmax")
})
tmax.ghcnd <- do.call(bind_rows,tmax.ghcnd)
tmax.ghcnd <- do.call(bind_cols,tmax.ghcnd)

snow.ghcnd <- lapply(stn.ghcnd.update$STATION_ID,function(x) {
  data <- ghcnd_search(stationid = x ,date_min = "1999-10-01",var="snow")
})

snow.ghcnd <- do.call(bind_rows,snow.ghcnd)
snow.ghcnd <- do.call(bind_cols,snow.ghcnd)


df.ghcnd <- prcp.ghcnd %>%
  left_join(snow.ghcnd,by=c("id","date")) %>%
  left_join(tmax.ghcnd,by=c("id","date")) %>%
  left_join(tmin.ghcnd,by=c("id","date")) %>%
  select(id,date,tmax,tmin,prcp,snow) %>%
  as.data.frame


# is the site code for Gerber correct? I am not pulling anything. I checked and there are two other possibilities, I added them to the data retrieval:
# 1. GERBER RESERVOIR OREGON, OR US
#Station ID: GHCND:USR0000OGER
#Period of Record:  1985-05-23 to 2021-07-21
# 2. GERBER RESERVOIR, OR US
#Station ID: GHCND:USS0021G04S
#Period of Record:  1998-09-30 to 2021-07-21

# tidy data

df.ghcnd <- df.ghcnd %>%
  dplyr::rename(
    STATION_ID=id,
    DATE=date,
    PRCP=prcp,
    TMIN=tmin,
    TMAX=tmax,
    SNOW=snow
  ) %>%
 # mutate(STATION_ID=ifelse(STATION_ID=="USS0021G04S","USC00353232",STATION_ID)) %>%  #change the new import data station ID to match the prior work
  mutate(TMIN=TMIN/10, # degC
         TMAX=TMAX/10, # degC
         PRCP=PRCP/10, # mm/day
         DATE=ymd(DATE),
         YEAR=year(DATE),
         MONTH=month(DATE),
         WYEAR=fluxr::wyear(DATE))%>%
  pivot_longer(c(TMAX:SNOW),"PARAMETER","VALUE")
ghcnd_orig <- ghcnd %>%
  mutate(STATION_ID=gsub("GHCND:","",STATION)) %>%
  pivot_longer(c(TMIN:SNOW),"PARAMETER","VALUE")

# verify date ranges
ghcnd_orig %>%
  filter(WYEAR == "2005"&PARAMETER=="PRCP") %>%
  ggplot() +
  geom_point(aes(x=DATE,y=value),color="blue",alpha=0.5) +
  geom_point(data=filter(ghcnd_orig,WYEAR == "2005"&PARAMETER=="PRCP"),aes(x=DATE,y=value),color="red",alpha=0.5)+
  facet_wrap(~STATION_ID)+
  theme_bw()



# create list of basin names to loop over
names <- unique(df.ghcnd$WYEAR)

# time series of PRCP to verify the data are the same
plot_list = list()
for (ii in names) {

 # colors <- c("GHCND original import"="blue","GHCND updated import"="red")

  p <-
    df.ghcnd %>%
    filter(PARAMETER=="PRCP") %>%
    filter(WYEAR == ii) %>%
    ggplot()+
    geom_point( aes(x=DATE,y=value),color="red",alpha=0.5)+
    geom_point(data=filter(ghcnd_orig,PARAMETER=="PRCP" & WYEAR==ii),aes(x=DATE,y=value),color="blue",alpha=0.5)+
    facet_wrap(~STATION_ID)+
    theme_bw() +
    ggtitle(as.character(ii))+
    labs(x="Date",y=as.character(ii))#,color="Legend")#+
    #scale_color_manual(values=colors,labels=c("Temp_C"="Temperature Original","QA_Temp_C"="Temperature QA"))

  plot_list[[ii]] = p
  ggsave(p, file=paste0("explore/ghcnd_PRCP_", ii,".png"), width = 11, height = 8.5, units = "in")
}

filename <- "./pdf/import/thermo_ghcnd_prcp.pdf"
pdf(file = filename,height=8.5,width=11)

for(ii in names) {
  print(plot_list[[ii]])

}
dev.off()


# time series of TMAX to verify the data are the same
plot_list = list()
for (ii in names) {

  # colors <- c("GHCND original import"="blue","GHCND updated import"="red")

  p <-
    df.ghcnd %>%
    filter(PARAMETER=="TMAX") %>%
    filter(WYEAR == ii) %>%
    ggplot()+
    geom_point( aes(x=DATE,y=value),color="red",alpha=0.5)+
    geom_point(data=filter(ghcnd_orig,PARAMETER=="TMAX" & WYEAR==ii),aes(x=DATE,y=value),color="blue",alpha=0.5)+
    facet_wrap(~STATION_ID)+
    theme_bw() +
    ggtitle(as.character(ii))+
    labs(x="Date",y=as.character(ii))#,color="Legend")#+
  #scale_color_manual(values=colors,labels=c("Temp_C"="Temperature Original","QA_Temp_C"="Temperature QA"))

  plot_list[[ii]] = p
  ggsave(p, file=paste0("explore/ghcnd/ghcnd_TMAX_", ii,".png"), width = 11, height = 8.5, units = "in")
}

filename <- "./pdf/import/thermo_ghcnd_TMAX.pdf"
pdf(file = filename,height=8.5,width=11)

for(ii in names) {
  print(plot_list[[ii]])

}
dev.off()


# time series of TMAX to verify the data are the same
plot_list = list()
for (ii in names) {

  # colors <- c("GHCND original import"="blue","GHCND updated import"="red")

  p <-
    df.ghcnd %>%
    filter(PARAMETER=="TMIN") %>%
    filter(WYEAR == ii) %>%
    ggplot()+
    geom_point( aes(x=DATE,y=value),color="red",alpha=0.5)+
    geom_point(data=filter(ghcnd_orig,PARAMETER=="TMIN" & WYEAR==ii),aes(x=DATE,y=value),color="blue",alpha=0.5)+
    facet_wrap(~STATION_ID)+
    theme_bw() +
    ggtitle(as.character(ii))+
    labs(x="Date",y=as.character(ii))#,color="Legend")#+
  #scale_color_manual(values=colors,labels=c("Temp_C"="Temperature Original","QA_Temp_C"="Temperature QA"))

  plot_list[[ii]] = p
  ggsave(p, file=paste0("explore/ghcnd/ghcnd_TMIN_", ii,".png"), width = 11, height = 8.5, units = "in")
}

filename <- "./pdf/import/thermo_ghcnd_TMIN.pdf"
pdf(file = filename,height=8.5,width=11)

for(ii in names) {
  print(plot_list[[ii]])

}
dev.off()



# time series of TMAX to verify the data are the same
plot_list = list()
for (ii in names) {

  # colors <- c("GHCND original import"="blue","GHCND updated import"="red")

  p <-
    df.ghcnd %>%
    filter(PARAMETER=="SNOW") %>%
    filter(WYEAR == ii) %>%
    ggplot()+
    geom_point( aes(x=DATE,y=value),color="red",alpha=0.5)+
    geom_point(data=filter(ghcnd_orig,PARAMETER=="SNOW" & WYEAR==ii),aes(x=DATE,y=value),color="blue",alpha=0.5)+
    facet_wrap(~STATION_ID)+
    theme_bw() +
    ggtitle(as.character(ii))+
    labs(x="Date",y=as.character(ii))#,color="Legend")#+
  #scale_color_manual(values=colors,labels=c("Temp_C"="Temperature Original","QA_Temp_C"="Temperature QA"))

  plot_list[[ii]] = p
  ggsave(p, file=paste0("explore/ghcnd/ghcnd_SNOW_", ii,".png"), width = 11, height = 8.5, units = "in")
}

filename <- "./pdf/import/thermo_ghcnd_SNOW.pdf"
pdf(file = filename,height=8.5,width=11)

for(ii in names) {
  print(plot_list[[ii]])

}
dev.off()



