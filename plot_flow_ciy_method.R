library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
theme_set(theme_bw())
library(fluxr)
library(gridExtra)

rm(list=ls())

source('functions.R')
load('usgs.Rdata')

ciy_years <- c(1977, 1981, 1990, 1991, 1994, 2001)

q.day <- filter(q.usgs, SITE_NAME=='Power') %>%
  select(DATE, FLOW) %>%
  mutate(YEAR=year(DATE),
         WYEAR=wyear(DATE),
         MONTH=month(DATE),
         YDAY=yday(DATE),
         WDAY=water_day(DATE),
         WDATE=as.Date('2001-10-01')+days(WDAY)) %>%
  mutate(GROUP=ifelse(WYEAR %in% c(2013, 2014), as.character(WYEAR), ifelse(WYEAR %in% ciy_years, 'CIY', 'Other')))


q.ciy <- filter(q.day, YEAR %in% ciy_years) %>%
  select(-DATE) %>%
  group_by(WDAY, WDATE) %>%
  summarise(FLOW=mean(FLOW)) %>%
  mutate(MONTH=month(WDATE))

filter(q.day, WYEAR %in% ciy_years, MONTH %in% seq(5, 9)) %>%
  ggplot(aes(WDATE, FLOW)) +
  geom_line(aes(color=factor(YEAR), group=YEAR)) +
  geom_line(data=filter(q.day, WYEAR %in% 2013, MONTH %in% seq(5, 9)), color='red', size=1) +
  geom_line(data=filter(q.ciy, MONTH %in% seq(5, 9)), color='black', size=1) +
  labs(x='', y='Flow (cfs)')

load('prism.Rdata')
p.mon <- filter(prism_subbasin, SITE_NAME=='Power') %>%
  mutate(WYEAR=wyear(MONTHYEAR),
         YEAR=year(MONTHYEAR),
         MONTH=month(MONTHYEAR),
         WDAY=water_day(MONTHYEAR),
         WDATE=as.Date('2001-10-01')+days(WDAY)) %>%
  mutate(GROUP=ifelse(WYEAR %in% c(2013, 2014), as.character(WYEAR), ifelse(WYEAR %in% ciy_years, 'CIY', 'Other')))

p.wyr <- p.mon %>%
  group_by(WYEAR) %>%
  summarise(PRCP=sum(PRCP),
            N=n()) %>%
  filter(N==12) %>%
  mutate(GROUP=ifelse(WYEAR %in% c(2013, 2014), as.character(WYEAR), ifelse(WYEAR %in% ciy_years, 'CIY', 'Other')))

p.winter <- filter(p.mon, MONTH %in% c(10:12, 1:3)) %>%
  group_by(WYEAR) %>%
  summarise(PRCP=sum(PRCP),
            N=n()) %>%
  filter(N==6) %>%
  mutate(GROUP=ifelse(WYEAR %in% c(2013, 2014), as.character(WYEAR), ifelse(WYEAR %in% ciy_years, 'CIY', 'Other')))

p.springsummer <- filter(p.mon, MONTH %in% c(4:9)) %>%
  group_by(WYEAR) %>%
  summarise(PRCP=sum(PRCP),
            N=n()) %>%
  filter(N==6) %>%
  mutate(GROUP=ifelse(WYEAR %in% c(2013, 2014), as.character(WYEAR), ifelse(WYEAR %in% ciy_years, 'CIY', 'Other')))

p.summer <- filter(p.mon, MONTH %in% c(7:9)) %>%
  group_by(WYEAR) %>%
  summarise(PRCP=sum(PRCP),
            N=n()) %>%
  filter(N==3) %>%
  mutate(GROUP=ifelse(WYEAR %in% c(2013, 2014), as.character(WYEAR), ifelse(WYEAR %in% ciy_years, 'CIY', 'Other')))


p.spring <- filter(p.mon, MONTH %in% c(4:6)) %>%
  group_by(WYEAR) %>%
  summarise(PRCP=sum(PRCP),
            N=n()) %>%
  filter(N==3) %>%
  mutate(GROUP=ifelse(WYEAR %in% c(2013, 2014), as.character(WYEAR), ifelse(WYEAR %in% ciy_years, 'CIY', 'Other')))


load('snotel.Rdata')
s.day <- filter(snotel, SITE_NAME=='SUMMER RIM') %>%
  mutate(WYEAR=wyear(DATE),
         YEAR=year(DATE),
         MONTH=month(DATE),
         WDAY=water_day(DATE),
         WDATE=as.Date('2001-10-01')+days(WDAY)) %>%
  mutate(GROUP=ifelse(WYEAR %in% c(2013, 2014), as.character(WYEAR), ifelse(WYEAR %in% ciy_years, 'CIY', 'Other')))


pdf(file.path('pdf', 'flow-ciy-method.pdf'), width=11, height=8.5)

filter(q.day, WYEAR %in% c(ciy_years, 2013, 2014), MONTH %in% seq(5, 9)) %>%
  filter(WYEAR >= 1981) %>%
  ggplot(aes(WDATE, FLOW, color=factor(WYEAR))) +
  geom_line(aes(size=WYEAR %in% c(2013, 2014))) +
  geom_line(data=filter(q.ciy, MONTH %in% seq(5, 9)) %>% mutate(YEAR='CIY'), color='black', size=1) +
  scale_x_date(labels = scales::date_format("%b"), breaks=scales::date_breaks('1 month')) +
  scale_color_discrete('Water Year') +
  scale_size_manual('', values=c('TRUE'=1, 'FALSE'=0.5), guide=FALSE) +
  labs(x='', y='Flow (cfs)', title='Daily Summer Flows for 2013, 2014 and Reference CIY Years\nBlack Line is Average CIY Daily Flow')

p.q.day <- filter(q.day, WYEAR %in% c(ciy_years, 2013, 2014)) %>%
  filter(WYEAR >= 1981) %>%
  ggplot(aes(WDATE, FLOW, color=factor(WYEAR))) +
  geom_line(aes(size=WYEAR %in% c(2013, 2014))) +
  geom_line(data=filter(q.ciy) %>% mutate(YEAR='CIY'), color='black', size=1) +
  scale_x_date(labels = scales::date_format("%b"), breaks=scales::date_breaks('1 month')) +
  scale_color_discrete('Water Year') +
  scale_size_manual('', values=c('TRUE'=1, 'FALSE'=0.5), guide=FALSE) +
  labs(x='', y='Flow (cfs)', title='Daily Flow for 2013, 2014 and Reference CIY Years')

p.p.mon <- filter(p.mon, WYEAR %in% c(ciy_years, c(2013, 2014))) %>%
  ggplot(aes(WDATE+days(15), PRCP, group=WYEAR, color=factor(WYEAR))) +
  stat_summary(fun.y=sum, geom="line") +
  stat_summary(aes(color=factor(WYEAR)), fun.y=sum, geom="line", data=filter(p.mon, WYEAR %in% c(2013, 2014)), size=1) +
  scale_x_date(labels = scales::date_format("%b"), breaks=scales::date_breaks('1 month'), limits=c(as.Date('2001-10-15'), as.Date('2002-09-15')), expand=c(0.1,1)) +
  labs(x="Month", y="Monthly Precip (mm)", title="Monthly Precip for 2013, 2014 and Reference CIY Years") +
  scale_color_discrete('Water Year')

p.s.day <- filter(s.day, WYEAR %in% ciy_years) %>%
  ggplot(aes(WDATE, SWE_in, color=factor(WYEAR))) +
  geom_line() +
  geom_line(data=filter(s.day, WYEAR %in% c(2013, 2014)), size=1) +
  scale_x_date(labels = scales::date_format("%b"), breaks=scales::date_breaks('1 month')) +
  scale_color_discrete('Water Year') +
  labs(x='Water Year Date', y='Snow Water Equiv (in)', title='Daily Snow Water Equiv. for 2013, 2014 and Reference and CIY Years')

grid.arrange(p.q.day, p.p.mon, p.s.day, nrow=3)

p.p.wyr <- ggplot(p.wyr, aes(WYEAR, PRCP, fill=GROUP)) +
  geom_bar(stat='identity') +
  scale_fill_manual('', values=c('2013'='orangered', '2014'='chartreuse3', 'CIY'='deepskyblue', 'Other'='gray50')) +
  labs(x='Water Year', y='Annual Precip (mm)', title='Total Annual Precip by Water Year')

p.p.winter <- ggplot(p.winter, aes(WYEAR, PRCP, fill=GROUP)) +
  geom_bar(stat='identity') +
  scale_fill_manual('', values=c('2013'='orangered', '2014'='chartreuse3',  'CIY'='deepskyblue', 'Other'='gray50')) +
  labs(x='Water Year', y='Fall/Winter (Oct-Mar) Precip (mm)', title='Fall/Winter Precip by Water Year')

p.p.springsummer <- ggplot(p.springsummer, aes(WYEAR, PRCP, fill=GROUP)) +
  geom_bar(stat='identity') +
  scale_fill_manual('', values=c('2013'='orangered', '2014'='chartreuse3',  'CIY'='deepskyblue', 'Other'='gray50')) +
  labs(x='Water Year', y='Spring/Summer (Apr-Sep) Precip (mm)', title='Spring/Summer Precip by Water Year')

grid.arrange(p.p.wyr, p.p.winter, p.p.springsummer, nrow=3)

# highlight 2013
p1 <- filter(q.day) %>%
  group_by(GROUP, WYEAR) %>%
  summarise(FLOW=mean(FLOW)) %>%
  left_join(select(p.wyr, WYEAR, PRCP)) %>%
  filter(!is.na(PRCP)) %>%
  ggplot(aes(PRCP, FLOW)) +
  geom_smooth() +
  geom_point(aes(color=GROUP), size=3) +
  scale_color_manual(values=c('Other'='grey50', '2013'='orangered', '2014'='chartreuse3', 'CIY'='steelblue')) +
  labs(x='Water Year Precip (mm)', y='Mean Annual Flow (cfs)', title="Mean Annual Flow vs Annual Precip")
p2 <- filter(q.day, MONTH %in% seq(7, 9)) %>%
  group_by(GROUP, WYEAR) %>%
  summarise(FLOW=mean(FLOW)) %>%
  left_join(select(p.wyr, WYEAR, PRCP)) %>%
  filter(!is.na(PRCP)) %>%
  ggplot(aes(PRCP, FLOW)) +
  geom_smooth() +
  geom_point(aes(color=GROUP), size=3) +
  scale_color_manual(values=c('Other'='grey50', '2013'='orangered', '2014'='chartreuse3', 'CIY'='steelblue')) +
  labs(x='Water Year Precip (mm)', y='Mean Summer (July-Sept) Flow (cfs)', title='Mean Summer Flow vs Annual Precip')
# p3 <- filter(q.day, MONTH %in% seq(7, 9)) %>%
#   group_by(WYEAR) %>%
#   summarise(FLOW=mean(FLOW)) %>%
#   left_join(select(p.wyr, WYEAR, PRCP) %>% mutate(WYEAR=WYEAR+1)) %>%
#   filter(!is.na(PRCP)) %>%
#   ggplot(aes(PRCP, FLOW)) +
#   geom_point(aes(color=WYEAR==2013), size=3) +
#   geom_smooth() +
#   scale_color_manual(values=c('FALSE'='grey50', 'TRUE'='orangered')) +
#   labs(x='Previous Water Year Precip (mm)', y='Mean Summer (July-Sept) Flow (cfs)', title='Mean Summer Flow vs Previous Annual Precip')
p3 <- filter(q.day, MONTH %in% seq(7, 9)) %>%
  group_by(GROUP, WYEAR) %>%
  summarise(FLOW=mean(FLOW)) %>%
  left_join(select(p.springsummer, WYEAR, PRCP)) %>%
  filter(!is.na(PRCP)) %>%
  ggplot(aes(PRCP, FLOW)) +
  geom_smooth() +
  geom_point(aes(color=GROUP), size=3) +
  scale_color_manual(values=c('Other'='grey50', '2013'='orangered', '2014'='chartreuse3', 'CIY'='steelblue')) +
  labs(x='Spring/Summer (Apr-Sept) Precip (mm)', y='Mean Summer (July-Sept) Flow (cfs)', title='Mean Summer Flow vs Spring/Summer Precip')
p4 <- filter(q.day, MONTH %in% seq(7, 9)) %>%
  group_by(GROUP, WYEAR) %>%
  summarise(FLOW=mean(FLOW)) %>%
  left_join(select(p.winter, WYEAR, PRCP)) %>%
  filter(!is.na(PRCP)) %>%
  ggplot(aes(PRCP, FLOW)) +
  geom_smooth() +
  geom_point(aes(color=GROUP), size=3) +
  scale_color_manual(values=c('Other'='grey50', '2013'='orangered', '2014'='chartreuse3', 'CIY'='steelblue')) +
  labs(x='Previous Fall/Winter (Oct-Mar) Precip (mm)', y='Mean Summer (July-Sept) Flow (cfs)', title='Mean Summer Flow vs Previous Fall/Winter Precip')
grid.arrange(p1, p2, p3, p4)

p1 <- filter(q.day, MONTH %in% seq(7, 9)) %>%
  group_by(GROUP, WYEAR) %>%
  summarise(FLOW=mean(FLOW)) %>%
  left_join(select(p.summer, WYEAR, PRCP)) %>%
  filter(!is.na(PRCP)) %>%
  ggplot(aes(PRCP, FLOW)) +
  geom_smooth() +
  geom_point(aes(color=GROUP), size=3) +
  scale_color_manual(values=c('Other'='grey50', '2013'='orangered', '2014'='chartreuse3', 'CIY'='steelblue')) +
  labs(x='Summer (July-Sept) Precip (mm)', y='Mean Summer (July-Sept) Flow (cfs)', title='Mean Summer Flow vs Summer Precip')
p2 <- filter(q.day, MONTH %in% seq(4, 6)) %>%
  group_by(GROUP, WYEAR) %>%
  summarise(FLOW=mean(FLOW)) %>%
  left_join(select(p.spring, WYEAR, PRCP)) %>%
  filter(!is.na(PRCP)) %>%
  ggplot(aes(PRCP, FLOW)) +
  geom_smooth() +
  geom_point(aes(color=GROUP), size=3) +
  scale_color_manual(values=c('Other'='grey50', '2013'='orangered', '2014'='chartreuse3', 'CIY'='steelblue')) +
  labs(x='Spring (Apr-June) Precip (mm)', y='Mean Spring (Apr-June) Flow (cfs)', title='Mean Spring Flow vs Spring Precip')
grid.arrange(p1, p2, nrow=2, ncol=2)

wyrs <- filter(p.winter, PRCP<400)$WYEAR
p1 <- filter(q.day, WYEAR %in% wyrs, MONTH %in% seq(6, 9)) %>%
  ggplot(aes(WDATE, FLOW, group=WYEAR, color=GROUP, size=GROUP, alpha=GROUP)) +
  geom_line() +
  scale_color_manual(values=c('Other'='grey50', '2013'='orangered', '2014'='chartreuse3', 'CIY'='steelblue')) +
  scale_alpha_manual(values=c('Other'=0.3, '2013'=1, '2014'=1, 'CIY'=1)) +
  scale_size_manual(values=c('Other'=0.5, '2013'=1, '2014'=1, 'CIY'=0.5)) +
  ylim(0, 1000) +
  labs(x="", y="Mean Summer (July-Sept) Flow (cfs)", title='Daily Summer Flows among Years with <400 mm Winter (Dec-Mar) Precip')
p2 <- filter(q.day, MONTH %in% seq(6, 9)) %>%
  ggplot(aes(WDATE, FLOW, group=WYEAR, color=GROUP, size=GROUP, alpha=GROUP)) +
  geom_line() +
  scale_color_manual(values=c('Other'='grey50', '2013'='orangered', '2014'='chartreuse3', 'CIY'='steelblue')) +
  scale_alpha_manual(values=c('Other'=0.3, '2013'=1, '2014'=1, 'CIY'=1)) +
  scale_size_manual(values=c('Other'=0.5, '2013'=1, '2014'=1, 'CIY'=0.5)) +
  ylim(0, 1000) +
  labs(x="", y="Mean Summer (July-Sept) Flow (cfs)", title='Daily Summer Flows among All Years')
grid.arrange(p1, p2, nrow=2)

dev.off()
