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

gannett <- read.csv('csv/gannett_compare.csv', stringsAsFactors = FALSE) %>%
  select(SITE_NAME, Q_GW=Gannett) %>%
  mutate(Q_GW=cfs_to_hm3d(Q_GW)*365.25) # hm3/yr

gannett <- mutate(gannett, IDX=1) %>%
  spread(SITE_NAME, Q_GW) %>%
  mutate(NF=`Upper NF`,
         SF=`Upper SF`,
         NF_Ivory=`Lower NF` + NF,
         SF_Ivory=`Lower SF` + SF,
         Godowa=`Upper Sprague` + NF_Ivory + SF_Ivory,
         Sycan=Sycan,
         Lone_Pine=`Middle Sprague` + Godowa + Sycan,
         Power=Lone_Pine + `Lower Sprague`) %>%
  gather(SITE_NAME, Q_GW, -IDX) %>%
  select(-IDX) %>%
  filter(SITE_NAME %in% stn.kt_sprague$SITE_NAME) %>%
  mutate(SITE_NAME=as.character(SITE_NAME),
         SITE_NAME=ordered(SITE_NAME, levels=levels(stn.kt_sprague$SITE_NAME))) %>%
  arrange(SITE_NAME)

df <- loads_df$site %>%
  filter(DATASET=="POR",
         PERIOD=="2010-2014",
         SEASON=="Annual",
         VAR %in% c("FLOW", "TP"),
         TERM %in% c("Q", "L", "C"),
         SITE_NAME %in% stn.kt_sprague$SITE_NAME) %>%
  select(-VAR, -DATASET, -START_DATE, -END_DATE) %>%
  spread(TERM, VALUE) %>%
  mutate(Q=Q*365.25,          # hm3/d -> hm3/yr
         L=L*365.25/1000) %>% # kg/d -> mt/yr
  rename(Q_TOT=Q,
         L_TOT=L,
         C_TOT=C) %>%
  left_join(gannett, by="SITE_NAME")

df <- mutate(df,
             C_GW=60,
             L_GW=Q_GW*C_GW/1000,
             Q_RO=Q_TOT-Q_GW,
             L_RO=L_TOT-L_GW,
             C_RO=L_RO/Q_RO*1000)

df_sfnf <- filter(df, SITE_NAME %in% c("SF", "NF"))
c_runoff <- sum(df_sfnf$L_RO)/sum(df_sfnf$Q_RO)*1000

df <- mutate(df,
             Q_BRO=Q_RO,
             C_BRO=c_runoff,
             L_BRO=Q_BRO*C_BRO/1000,
             Q_BACK=Q_TOT,
             L_BACK=L_GW+L_BRO,
             C_BACK=L_BACK/Q_BACK*1000,
             Q_ANTH=Q_TOT,
             L_ANTH=L_TOT-L_BACK,
             C_ANTH=L_ANTH/Q_ANTH*1000)

df <- gather(df, TERM_VAR, VALUE, Q_TOT:C_ANTH)

df <- separate(df, TERM_VAR, into=c("TERM", "VAR"))

df <- mutate(df, SITE_NAME=ordered(SITE_NAME, levels=stn.kt_sprague$SITE_NAME))

write.csv(df, file=file.path('csv', 'loads_anthro.csv'), row.names=FALSE)

# pdf ----
pdf(file.path('pdf', 'loads-background-anthro.pdf'), width=11, height=8.5)

spread(df, TERM, VALUE) %>%
  filter(VAR %in% c("GW", "BRO", "ANTH")) %>%
  arrange(SITE_NAME, desc(VAR)) %>%
  ggplot(aes(SITE_NAME, L, fill=VAR)) +
  geom_bar(stat='identity', position='stack') +
  scale_fill_manual('', labels=c(ANTH="Anthropogenic Runoff",
                                 BRO="Background Runoff",
                                 GW="Groundwater"),
                    values=c(ANTH="orangered",
                             BRO="olivedrab3",
                             GW="steelblue")) +
  scale_y_continuous(breaks=seq(0, 30, 5)) +
  labs(y="TP Load (mt/yr)", x="",
       title="TP Load by Source")

spread(df, TERM, VALUE) %>%
  filter(VAR %in% c("GW", "BRO", "ANTH")) %>%
  arrange(SITE_NAME, desc(VAR)) %>%
  ggplot(aes(SITE_NAME, L, fill=VAR)) +
  geom_bar(stat='identity', position='fill') +
  scale_fill_manual('', labels=c(ANTH="Anthropogenic Runoff",
                                 BRO="Background Runoff",
                                 GW="Groundwater"),
                    values=c(ANTH="orangered",
                             BRO="olivedrab3",
                             GW="steelblue")) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Fraction TP Load", x="",
       title="Fraction TP Load by Source")

spread(df, TERM, VALUE) %>%
  filter(VAR %in% c("GW", "RO")) %>%
  arrange(SITE_NAME, VAR) %>%
  mutate(L=ifelse(Q<0, 0, Q)) %>%
  ggplot(aes(SITE_NAME, Q, fill=VAR)) +
  geom_bar(stat='identity', position='stack') +
  scale_fill_manual('', labels=c(RO="Runoff",
                                 GW="Groundwater"),
                    values=c(RO="olivedrab3",
                             GW="steelblue")) +
  labs(y="Flow (hm3/yr)", x="",
       title="Flow by Source")

spread(df, TERM, VALUE) %>%
  filter(VAR %in% c("GW", "RO")) %>%
  arrange(SITE_NAME, VAR) %>%
  ggplot(aes(SITE_NAME, C, fill=VAR)) +
  geom_bar(stat='identity', position='dodge') +
  scale_fill_manual('', labels=c(RO="Runoff",
                                 GW="Groundwater"),
                    values=c(RO="olivedrab3",
                             GW="steelblue")) +
  labs(y="Flow (hm3/yr)", x="",
       title="Flow by Source")

filter(df, TERM=="C", VAR=="BACK") %>%
  ggplot(aes(SITE_NAME, VALUE)) +
  geom_bar(stat='identity', fill='grey50') +
  labs(x="", y="TP Concentration (ppb)",
       title="Background Concentrations (GW+Runoff)") +
  scale_y_continuous(breaks=seq(0, 60, 10))

dev.off()

# report ----


filename <- 'report/anthro-load.png'
cat('Saving report figure to:', filename, '\n')
png(filename, width=8, height=5, res=200, units='in')

p <- spread(df, TERM, VALUE) %>%
  filter(VAR %in% c("GW", "BRO", "ANTH")) %>%
  arrange(SITE_NAME, desc(VAR)) %>%
  ggplot(aes(SITE_NAME, L, fill=VAR)) +
  geom_bar(stat='identity', position='stack') +
  scale_fill_manual('', labels=c(ANTH="Anthropogenic",
                                 BRO="Runoff-Background",
                                 GW="GW-Background"),
                    values=c(ANTH="orangered",
                             BRO="olivedrab3",
                             GW="steelblue")) +
  scale_y_continuous(breaks=seq(0, 30, 5)) +
  labs(y="TP Load (mt/yr)", x="")
print(p)

dev.off()

filename <- 'report/anthro-load-pct.png'
cat('Saving report figure to:', filename, '\n')
png(filename, width=8, height=5, res=200, units='in')

p <- spread(df, TERM, VALUE) %>%
  filter(VAR %in% c("GW", "BRO", "ANTH")) %>%
  arrange(SITE_NAME, desc(VAR)) %>%
  ggplot(aes(SITE_NAME, L, fill=VAR)) +
  geom_bar(stat='identity', position='fill') +
  scale_fill_manual('', labels=c(ANTH="Anthropogenic",
                                 BRO="Runoff-Background",
                                 GW="GW-Background"),
                    values=c(ANTH="orangered",
                             BRO="olivedrab3",
                             GW="steelblue")) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="% Total TP Load", x="")
print(p)

dev.off()

# compare pou to % total as anthro
load('pou.Rdata')
pou <- pou_subbasin %>%
  filter(EXTENT=="valley") %>%
  select(SITE_NAME, POU=AREA_FRAC)
spread(df, TERM, VALUE) %>%
  filter(VAR %in% c("TOT", "GW", "BRO", "ANTH")) %>%
  arrange(SITE_NAME, desc(VAR)) %>%
  left_join(pou, by='SITE_NAME') %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=levels(pou$SITE_NAME))) %>%
  droplevels %>%
  select(SITE_NAME, VAR, L, POU) %>%
  spread(VAR, L) %>%
  ggplot(aes(POU, ANTH/TOT, color=SITE_NAME)) +
  geom_point()
spread(df, TERM, VALUE) %>%
  filter(VAR %in% c("ANTH")) %>%
  arrange(SITE_NAME, desc(VAR)) %>%
  left_join(pou, by='SITE_NAME') %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=levels(pou$SITE_NAME))) %>%
  droplevels %>%
  ggplot(aes(POU, C, color=SITE_NAME)) +
  geom_point()


# by season ----

# df.seas <- loads_df$site %>%
#   filter(DATASET=="POR",
#          PERIOD=="2010-2014",
#          VAR %in% c("FLOW", "TP"),
#          TERM %in% c("Q", "L", "C"),
#          SITE_NAME %in% stn.kt_sprague$SITE_NAME) %>%
#   select(-VAR, -DATASET, -START_DATE, -END_DATE) %>%
#   spread(TERM, VALUE) %>%
#   mutate(Q=Q*365.25,          # hm3/d -> hm3/yr
#          L=L*365.25/1000) %>% # kg/d -> mt/yr
#   rename(Q_TOT=Q,
#          L_TOT=L,
#          C_TOT=C) %>%
#   left_join(gannett, by="SITE_NAME")
#
# df.seas <- mutate(df.seas,
#                   C_GW=60,
#                   L_GW=Q_GW*C_GW/1000,
#                   Q_RO=Q_TOT-Q_GW,
#                   L_RO=L_TOT-L_GW,
#                   C_RO=L_RO/Q_RO*1000)

# by year ----
df_bg <- filter(df, TERM=="C", VAR=="BACK") %>%
  select(SITE_NAME, C_BACK=VALUE)

df.wyr <- loads_df$wyr %>%
  filter(DATASET=="POR",
         SEASON=="Annual",
         VAR %in% c("FLOW", "TP"),
         TERM %in% c("Q", "L", "C"),
         SITE_NAME %in% stn.kt_sprague$SITE_NAME) %>%
  select(-VAR, -DATASET, -DATE, -SEASON, -N_DAY, -AREA_KM2) %>%
  spread(TERM, VALUE) %>%
  mutate(Q=Q*365.25,          # hm3/d -> hm3/yr
         L=L*365.25/1000) %>% # kg/d -> mt/yr
  rename(Q_TOT=Q,
         L_TOT=L,
         C_TOT=C) %>%
  left_join(df_bg, by="SITE_NAME") %>%
  mutate(C_BACK=ifelse(C_BACK > C_TOT, C_TOT, C_BACK))

df.wyr <- df.wyr %>%
  mutate(Q_BACK=Q_TOT,
         L_BACK=Q_BACK*C_BACK/1000,
         Q_ANTH=Q_TOT,
         L_ANTH=ifelse(L_TOT>L_BACK, L_TOT-L_BACK, 0),
         C_ANTH=L_ANTH/Q_ANTH*1000)

df.wyr <- gather(df.wyr, TERM_VAR, VALUE, Q_TOT:C_ANTH) %>%
  separate(TERM_VAR, into=c("TERM", "VAR")) %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=stn.kt_sprague$SITE_NAME))


spread(df.wyr, TERM, VALUE) %>%
  filter(VAR %in% c("BACK", "ANTH")) %>%
  arrange(SITE_NAME, desc(VAR)) %>%
  ggplot(aes(factor(WYEAR), C, fill=VAR)) +
  geom_bar(stat='identity', position='stack') +
  scale_fill_manual('', labels=c(ANTH="Anthropogenic",
                                 BACK="Background"),
                    values=c(ANTH="orangered",
                             BACK="steelblue")) +
  facet_wrap(~SITE_NAME, scales='free_y', nrow=2) +
  labs(y="TP Load (mt/yr)", x="Water Year",
       title="TP Load by Source")





df.wyr <- loads_df$wyr %>%
  filter(DATASET=="POR",
         SEASON=="Annual",
         VAR %in% c("FLOW", "TP"),
         TERM %in% c("Q", "L", "C"),
         SITE_NAME %in% stn.kt_sprague$SITE_NAME) %>%
  select(-VAR, -DATASET, -DATE, -SEASON, -N_DAY, -AREA_KM2) %>%
  spread(TERM, VALUE) %>%
  mutate(Q=Q*365.25,          # hm3/d -> hm3/yr
         L=L*365.25/1000) %>% # kg/d -> mt/yr
  rename(Q_TOT=Q,
         L_TOT=L,
         C_TOT=C) %>%
  left_join(gannett, by="SITE_NAME") %>%
  mutate(Q_GW = ifelse(Q_GW > Q_TOT, Q_TOT, Q_GW),
         C_GW=60,
         L_GW=Q_GW*C_GW/1000) %>%
  left_join(rename(df_bg, C_RUN=C_BACK), by="SITE_NAME") %>%
  mutate(Q_RUN=Q_TOT-Q_GW,
         L_RUN=Q_RUN*C_RUN/1000,
         L_BACK=L_RUN+L_GW,
         Q_BACK=Q_TOT,
         C_BACK=L_BACK/Q_BACK*1000,
         Q_ANTH=Q_TOT,
         L_ANTH=ifelse(L_TOT>(L_GW+L_RUN), L_TOT-L_GW-L_RUN, 0),
         C_ANTH=L_ANTH/Q_ANTH*1000) %>%
  gather(TERM_VAR, VALUE, Q_TOT:C_ANTH) %>%
  separate(TERM_VAR, into=c("TERM", "VAR")) %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=stn.kt_sprague$SITE_NAME),
         VAR=ordered(VAR, levels=rev(c('GW', 'RUN', 'BACK', 'ANTH', 'TOT'))))

df.wyr %>%
  filter(VAR %in% c("GW", "RUN", "ANTH")) %>%
  spread(TERM, VALUE) %>%
  arrange(desc(VAR)) %>%
  ggplot(aes(factor(WYEAR), L, fill=VAR)) +
  geom_bar(stat='identity', position='stack') +
  facet_wrap(~SITE_NAME, scales='free_y', nrow=2)


df.wyr %>%
  filter(VAR %in% c("ANTH")) %>%
  spread(TERM, VALUE) %>%
  arrange(desc(VAR)) %>%
  ggplot(aes(factor(WYEAR), L)) +
  geom_bar(stat='identity', position='stack') +
  facet_wrap(~SITE_NAME, scales='free_y', nrow=2)


df.wyr %>%
  spread(VAR, VALUE) %>%
  ggplot(aes(BACK, ANTH, color=SITE_NAME)) +
  geom_point() +
  facet_wrap(~TERM, scales='free')


p1 <- df.wyr %>%
  filter(VAR %in% c("BACK", "ANTH")) %>%
  spread(TERM, VALUE) %>%
  arrange(desc(VAR)) %>%
  ggplot(aes(factor(WYEAR), C, fill=VAR)) +
  geom_bar(stat='identity', position='stack') +
  facet_wrap(~SITE_NAME, scales='free_y', nrow=2)
print(p1)

p2 <- df.wyr %>%
  filter(VAR %in% c("TOT")) %>%
  spread(TERM, VALUE) %>%
  arrange(desc(VAR)) %>%
  ggplot(aes(factor(WYEAR), C, fill=VAR)) +
  geom_bar(stat='identity', position='stack') +
  facet_wrap(~SITE_NAME, scales='free_y', nrow=2)

library(gridExtra)
grid.arrange(grobs=list(p1, p2), nrow=2)

df.2002 <- loads_df$site %>%
  filter(DATASET=="POR",
         PERIOD=="2002-2014",
         SEASON=="Annual",
         VAR %in% c("FLOW", "TP"),
         TERM %in% c("Q", "L", "C"),
         SITE_NAME %in% stn.kt_sprague$SITE_NAME) %>%
  select(-VAR, -DATASET, -START_DATE, -END_DATE) %>%
  spread(TERM, VALUE) %>%
  mutate(Q=Q*365.25,          # hm3/d -> hm3/yr
         L=L*365.25/1000) %>% # kg/d -> mt/yr
  rename(Q_TOT=Q,
         L_TOT=L,
         C_TOT=C) %>%
  left_join(gannett, by="SITE_NAME")

df.2002 <- mutate(df.2002,
                  C_GW=60,
                  L_GW=Q_GW*C_GW/1000,
                  Q_RO=Q_TOT-Q_GW,
                  L_RO=L_TOT-L_GW,
                  C_RO=L_RO/Q_RO*1000)

df_2002_sfnf <- filter(df.2002, SITE_NAME %in% c("SF", "NF"))
c_runoff <- sum(df_2002_sfnf$L_RO)/sum(df_2002_sfnf$Q_RO)*1000

df.2002 <- mutate(df.2002,
                  Q_BRO=Q_RO,
                  C_BRO=c_runoff,
                  L_BRO=Q_BRO*C_BRO/1000,
                  Q_BACK=Q_TOT,
                  L_BACK=L_GW+L_BRO,
                  C_BACK=L_BACK/Q_BACK*1000,
                  Q_ANTH=Q_TOT,
                  L_ANTH=L_TOT-L_BACK,
                  C_ANTH=L_ANTH/Q_ANTH*1000)

df.2002 <- gather(df.2002, TERM_VAR, VALUE, Q_TOT:C_ANTH)

df.2002 <- separate(df.2002, TERM_VAR, into=c("TERM", "VAR"))

df.2002 <- mutate(df.2002, SITE_NAME=ordered(SITE_NAME, levels=stn.kt_sprague$SITE_NAME))


pdf(file.path('pdf', 'loads-background-anthro-2002.pdf'), width=11, height=8.5)

spread(df.2002, TERM, VALUE) %>%
  filter(VAR %in% c("GW", "BRO", "ANTH")) %>%
  arrange(SITE_NAME, desc(VAR)) %>%
  mutate(L=ifelse(L<0, 0, L)) %>%
  ggplot(aes(SITE_NAME, L, fill=VAR)) +
  geom_bar(stat='identity', position='stack') +
  scale_fill_manual('', labels=c(ANTH="Anthropogenic Runoff",
                                 BRO="Background Runoff",
                                 GW="Groundwater"),
                    values=c(ANTH="orangered",
                             BRO="olivedrab3",
                             GW="steelblue")) +
  scale_y_continuous(breaks=seq(0, 30, 5)) +
  labs(y="TP Load (mt/yr)", x="",
       title="TP Load by Source")


spread(df.2002, TERM, VALUE) %>%
  filter(VAR %in% c("GW", "RO")) %>%
  arrange(SITE_NAME, VAR) %>%
  mutate(L=ifelse(Q<0, 0, Q)) %>%
  ggplot(aes(SITE_NAME, Q, fill=VAR)) +
  geom_bar(stat='identity', position='stack') +
  scale_fill_manual('', labels=c(RO="Runoff",
                                 GW="Groundwater"),
                    values=c(RO="olivedrab3",
                             GW="steelblue")) +
  labs(y="Flow (hm3/yr)", x="",
       title="Flow by Source")

spread(df.2002, TERM, VALUE) %>%
  filter(VAR %in% c("GW", "BRO", "ANTH")) %>%
  arrange(SITE_NAME, desc(VAR)) %>%
  mutate(L=ifelse(L<0, 0, L)) %>%
  ggplot(aes(SITE_NAME, L, fill=VAR)) +
  geom_bar(stat='identity', position='fill') +
  scale_fill_manual('', labels=c(ANTH="Anthropogenic Runoff",
                                 BRO="Background Runoff",
                                 GW="Groundwater"),
                    values=c(ANTH="orangered",
                             BRO="olivedrab3",
                             GW="steelblue")) +
  scale_y_continuous(labels=scales::percent) +
  labs(y="Fraction TP Load", x="",
       title="Fraction TP Load by Source")

filter(df.2002, TERM=="C", VAR=="BACK") %>%
  ggplot(aes(SITE_NAME, VALUE)) +
  geom_bar(stat='identity', fill='grey50') +
  labs(x="", y="TP Concentration (ppb)",
       title="Background Concentrations (GW+Runoff)") +
  scale_y_continuous(breaks=seq(0, 60, 10))

dev.off()


filter(df, SITE_NAME %in% c("SF", "NF")) %>%
  filter(VAR %in% c("TOT", "GW", "RO")) %>%
  unite(VAR_TERM, VAR, TERM) %>%
  spread(VAR_TERM, VALUE) %>%
  select(SEASON, N_DAY, N_YEAR, PERIOD, SITE_NAME,
         TOT_Q, TOT_L, TOT_C,
         GW_Q, GW_L, GW_C,
         RO_Q, RO_L, RO_C) %>%
  as.data.frame

