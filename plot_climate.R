library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())
library(gridExtra)

rm(list=ls())

load('prism.Rdata')

prcp <- filter(prism_subbasin, SITE_NAME=="Power") %>%
  dplyr::group_by(WYEAR) %>%
  dplyr::mutate(N_MONTH=n()) %>%
  filter(N_MONTH==12)

prcp <- mutate(prcp,
               MONTH=month(MONTHYEAR),
               PRCP=PRCP/10)

seasons <- list(Annual=1:12,
                'Fall (Oct-Dec)'=c(10:12),
                'Winter (Jan-Mar)'=c(1:3),
                'Spring (Apr-Jun)'=c(4:6),
                'Summer (Jul-Sep)'=c(7:9))

df_seasons <- lapply(names(seasons), function(s) {
  data.frame(MONTH=seasons[[s]], SEASON=s, stringsAsFactors=FALSE)
}) %>%
  bind_rows %>%
  mutate(SEASON=ordered(SEASON, levels=names(seasons)))

prcp <- left_join(df_seasons, prcp, by="MONTH")

prcp_wyr <- prcp %>%
dplyr::group_by( WYEAR, SEASON) %>%
  dplyr::summarise(N_DAY=n(),
            PRCP=sum(PRCP)) %>%
  ungroup



load('ghcnd.Rdata')
ghcnd <- filter(ghcnd, STATION_NAME != "GERBER DAM OR US")
stn.ghcnd.rename <- c("KLAMATH FALLS INTERNATIONAL AIRPORT OR US"="KFA",
                      "SUMMER LAKE 1 S OR US"="SUMMER LAKE")
ghcnd <- mutate(ghcnd, SITE=plyr::revalue(STATION_NAME, stn.ghcnd.rename))

stn.ghcnd <- select(ghcnd, STATION:LONGITUDE, SITE) %>% unique

ghcnd <- select(ghcnd, SITE, DATE, WYEAR, PRCP) %>%
  mutate(MONTHYEAR=floor_date(DATE, unit="month"),
         MONTH=month(DATE)) %>%
  dplyr::group_by(SITE, MONTHYEAR, WYEAR, MONTH) %>%
  dplyr::summarise(N_DAY=sum(!is.na(PRCP)),
            PRCP=sum(PRCP, na.rm=TRUE)) %>%
  mutate(N_MISS=days_in_month(MONTHYEAR)-N_DAY)

ghcnd <- left_join(df_seasons, ghcnd, by="MONTH")

ghcnd_wyr <- dplyr::group_by(ghcnd, SITE, WYEAR, SEASON) %>%
  dplyr::summarise(PRCP=sum(PRCP),
            N_MISS=sum(N_MISS),
            N_DAY=sum(N_DAY))





load('usgs.Rdata')
q <- filter(q.usgs, SITE_NAME=="Power") %>%
  mutate(WYEAR=fluxr::wyear(DATE),
         MONTH=month(DATE),
         MONTHYEAR=floor_date(DATE, unit="month")) %>%
  filter(WYEAR >= 2002, WYEAR <= 2014) %>%
  dplyr::group_by(WYEAR, MONTH, MONTHYEAR) %>%
  dplyr::summarise(N_DAY=sum(!is.na(FLOW)),
            FLOW=mean(FLOW)) %>%
  ungroup %>%
  mutate(N_MISS=days_in_month(MONTHYEAR)-N_DAY)

q <- left_join(df_seasons, q, by=c("MONTH"))

q_wyr <- dplyr::group_by(q, WYEAR, SEASON) %>%
  dplyr:: summarise(FLOW=sum(FLOW*N_DAY)/sum(N_DAY),
            N_DAY=sum(N_DAY)) %>%
  ungroup


load('snotel.Rdata')
source('functions.R')

snotel <- filter(snotel, SITE_NAME != "QUARTZ MOUNTAIN")
snotel <- mutate(snotel,
                 WDAY=water_day(DATE),
                 WDAY_LABEL=as.Date("2001-10-01")+days(WDAY))

snotel.wyr <- filter(snotel.wyr, SITE_NAME != "QUARTZ MOUNTAIN")
snotel.wyr <- filter(snotel.wyr, WYEAR >= 2002)


filename <- file.path('pdf', 'climate.pdf')
cat('Printing:', filename, '\n')
pdf(filename, width=11, height=8.5)

# prism
p1 <- filter(prcp_wyr, WYEAR>=2002, SEASON!="Annual") %>%
  ggplot(aes(factor(WYEAR), PRCP, fill=SEASON)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~SEASON, nrow=1) +
  labs(x="", y="Precip (cm)") +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5),
        strip.background=element_blank(),
        strip.text=element_text(face="bold"))

p2 <- filter(prcp_wyr, WYEAR>=2002, SEASON!="Annual") %>%
  ggplot(aes(factor(WYEAR), PRCP, fill=SEASON)) +
  geom_bar(stat="identity", position="stack") +
  labs(x="", y="Precip (cm)") +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5))

grid.arrange(grobs=list(p1, p2),
             top="PRISM Dataset: Sprague River Basin")

p <- filter(prcp, SEASON=="Annual", WYEAR>=2002) %>%
  mutate(WDAY=water_day(MONTHYEAR),
         WDAY_LABEL=as.Date("2001-10-01")+days(WDAY)) %>%
  arrange(MONTHYEAR) %>%
  dplyr::group_by(WYEAR) %>%
  mutate(CUMUL_PRCP=cumsum(PRCP)) %>%
  ggplot(aes(WDAY_LABEL, CUMUL_PRCP, group=WYEAR, color=factor(WYEAR))) +
  geom_line() +
  scale_color_discrete('WYear') +
  labs(x="Water Year Day", y="Cumulative Precip (mm)",
       title="Cumulative Precipitation by Water Year") +
  scale_x_date(breaks=scales::date_breaks("1 month"),
               labels=scales::date_format("%b %d"))
print(p)


# ghcnd
p1 <- filter(ghcnd_wyr, WYEAR>=2002, SEASON!="Annual", SITE=="SUMMER LAKE") %>%
  ggplot(aes(factor(WYEAR), PRCP, fill=SEASON)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~SEASON, nrow=1) +
  labs(x="", y="Precip (mm)") +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5),
        strip.background=element_blank(),
        strip.text=element_text(face="bold"))

p2 <- filter(ghcnd_wyr, WYEAR>=2002, SEASON!="Annual", SITE=="SUMMER LAKE") %>%
  ggplot(aes(factor(WYEAR), PRCP, fill=SEASON)) +
  geom_bar(stat="identity", position="stack") +
  labs(x="", y="Precip (mm)") +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5))

grid.arrange(grobs=list(p1, p2),
             top="GHCND Dataset: Summer Lake")


p1 <- filter(ghcnd_wyr, WYEAR>=2002, SEASON!="Annual", SITE=="KFA") %>%
  ggplot(aes(factor(WYEAR), PRCP, fill=SEASON)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~SEASON, nrow=1) +
  labs(x="", y="Precip (mm)") +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5),
        strip.background=element_blank(),
        strip.text=element_text(face="bold"))

p2 <- filter(ghcnd_wyr, WYEAR>=2002, SEASON!="Annual", SITE=="KFA") %>%
  ggplot(aes(factor(WYEAR), PRCP, fill=SEASON)) +
  geom_bar(stat="identity", position="stack") +
  labs(x="", y="Precip (mm)") +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5))

grid.arrange(grobs=list(p1, p2),
             top="GHCND Dataset: KFA")

# ghcnd missing
p1 <- filter(ghcnd_wyr, WYEAR>=2002, SEASON!="Annual", SITE=="SUMMER LAKE") %>%
  ggplot(aes(factor(WYEAR), N_MISS, fill=SEASON)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~SEASON, nrow=1) +
  labs(x="", y="# Missing Days") +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5),
        strip.background=element_blank(),
        strip.text=element_text(face="bold"))

p2 <- filter(ghcnd_wyr, WYEAR>=2002, SEASON!="Annual", SITE=="SUMMER LAKE") %>%
  ggplot(aes(factor(WYEAR), N_MISS, fill=SEASON)) +
  geom_bar(stat="identity", position="stack") +
  labs(x="", y="# Missing Days") +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5))

grid.arrange(grobs=list(p1, p2),
             top="GHCND Dataset: Summer Lake")


# flows
p1 <- filter(q_wyr, WYEAR>=2002, SEASON!="Annual") %>%
  ggplot(aes(factor(WYEAR), FLOW, fill=SEASON)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~SEASON, nrow=1) +
  labs(x="", y="Flow (cfs)") +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5),
        strip.background=element_blank(),
        strip.text=element_text(face="bold"))

p2 <- filter(q_wyr, WYEAR>=2002, SEASON=="Annual") %>%
  ggplot(aes(factor(WYEAR), FLOW, fill=SEASON)) +
  geom_bar(stat="identity", position="stack") +
  scale_fill_manual(values='grey50') +
  labs(x="", y="Flow (cfs)") +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5))

grid.arrange(grobs=list(p1, p2), nrow=2,
             top="USGS Streamflow: Power")


# snotel
p <- ggplot(snotel.wyr, aes(factor(WYEAR), SWE_MAX_cm)) +
  geom_bar(stat="identity", fill="grey50") +
  facet_wrap(~SITE_NAME) +
  labs(x="Water Year", y="SWE Max (cm)", title="SNOTEL: Max Snowpack by Water Year") +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5),
        strip.background=element_blank(),
        strip.text=element_text(face="bold"))
print(p)

p <- filter(snotel, WYEAR >= 2002) %>%
  mutate(WYEAR=factor(WYEAR)) %>%
  filter(WDAY<=280) %>%
  ggplot(aes(WDAY_LABEL, SWE_cm, color=WYEAR)) +
  geom_line() +
  labs(x="Water Year Date", y="SWE (cm)", title="SNOTEL: Daily Snowpack") +
  scale_x_date(breaks=scales::date_breaks("1 month"),
               labels=scales::date_format("%b %d")) +
  facet_wrap(~SITE_NAME) +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5),
        strip.background=element_blank(),
        strip.text=element_text(face="bold"))
print(p)

p <- filter(snotel, WYEAR >= 2002) %>%
  mutate(WDAY=water_day(DATE),
         WYEAR=factor(WYEAR)) %>%
  filter(WDAY >= 180, WDAY <= 250) %>%
  ggplot(aes(WDAY_LABEL, SWE_cm, color=WYEAR)) +
  geom_line() +
  labs(x="Water Year Date", y="SWE (cm)", title="SNOTEL: Daily Snowpack (Spring Season)") +
  scale_x_date(breaks=scales::date_breaks("1 month"),
               labels=scales::date_format("%b %d")) +
  facet_wrap(~SITE_NAME) +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5),
        strip.background=element_blank(),
        strip.text=element_text(face="bold"))
print(p)

p <- filter(snotel, WYEAR >= 2002) %>%
  mutate(WYEAR=factor(WYEAR)) %>%
  filter(WDAY<=280) %>%
  ggplot(aes(WDAY_LABEL, SWE_cm, color=WYEAR)) +
  geom_line() +
  scale_x_date(breaks=scales::date_breaks("2 month"),
               labels=scales::date_format("%b %d")) +
  facet_grid(SITE_NAME~WYEAR, scales='free_y') +
  labs(x="Water Year Date", y="SWE (cm)", title="SNOTEL: Daily Snowpack by Water Year") +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5, size=8),
        strip.background=element_blank(),
        strip.text=element_text(face="bold"))
print(p)

dev.off()



# report ----
filename <- 'report/precip-seasonal.png'
cat('Saving report figure to:', filename, '\n')
png(filename, width=10, height=4, res=200, units='in')
p <- filter(prcp_wyr, WYEAR>=2002, SEASON!="Annual") %>%
  ggplot(aes(factor(WYEAR), PRCP, fill=SEASON)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~SEASON, nrow=1) +
  labs(x="Water Year", y="Precip (cm)") +
  scale_fill_discrete('') +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5),
        strip.background=element_blank(),
        strip.text=element_text(face="bold"))
print(p)
dev.off()

filename <- 'report/precip-annual.png'
cat('Saving report figure to:', filename, '\n')
png(filename, width=10, height=4, res=200, units='in')
p <- filter(prcp_wyr, WYEAR>=2002, SEASON!="Annual") %>%
  ggplot(aes(factor(WYEAR), PRCP, fill=SEASON)) +
  geom_bar(stat="identity", position="stack") +
  scale_y_continuous(breaks=seq(0, 70, by=10)) +
  scale_fill_discrete('') +
  labs(x="Water Year", y="Precip (cm)") +
  theme(axis.text.x=element_text(angle=90, hjust=0, vjust=0.5))
print(p)
dev.off()

grid.arrange(grobs=list(p1, p2),
             top="PRISM Dataset: Sprague River Basin")

# flows ----
mutate(q.usgs,
       WYEAR=fluxr::wyear(DATE),
       WDAY=water_day(DATE)) %>%
  filter(WYEAR >= 2002,
         WYEAR <= 2014,
         SITE_NAME == 'Power') %>%
  ggplot(aes(WDAY, FLOW, color=factor(WYEAR))) +
  scale_y_log10() +
  geom_line(aes(linetype=WYEAR %in% c(2013, 2014)))

mutate(q.usgs,
       WYEAR=fluxr::wyear(DATE),
       WDAY=water_day(DATE)) %>%
  filter(WYEAR >= 2002,
         WYEAR <= 2014,
         SITE_NAME == 'Power') %>%
  ggplot(aes(WDAY, FLOW, color=factor(WYEAR))) +
  scale_y_log10() +
  geom_line(aes(linetype=WYEAR %in% c(2013, 2014))) +
  facet_wrap(~WYEAR)

# air temperature ----
temp <- read.csv('./data/sprague/prism/prism_temp_beatty.csv', stringsAsFactors=FALSE)
temp <- mutate(temp, MONTHYEAR=mdy(MONTHYEAR), WYEAR=fluxr::wyear(MONTHYEAR), MONTH=month(MONTHYEAR))

temp %>%
  ggplot(aes(MONTHYEAR)) +
  geom_ribbon(aes(ymin=TMIN, ymax=TMAX), fill='grey80') +
  geom_line(aes(y=TMEAN))

temp %>%
  mutate(WMONTH=ifelse(MONTH >= 10, MONTH-9, MONTH+3)) %>%
  gather(VAR, VALUE, TMIN, TMAX, TMEAN) %>%
  ggplot(aes(WMONTH, VALUE, group=WYEAR)) +
  geom_line(aes(color=factor(WYEAR), linetype=WYEAR==2014)) +
  facet_wrap(~VAR) +
  scale_x_continuous(breaks=seq(1, 12),
                     labels=c('Oct', 'Nov', 'Dec', 'Jan', 'Feb',
                              'Mar', 'Apr', 'May', 'Jun', 'Jul',
                              'Aug', 'Sep'))

