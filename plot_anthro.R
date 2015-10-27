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

# by season ----

df.seas <- loads_df$site %>%
  filter(DATASET=="POR",
         PERIOD=="2010-2014",
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

df.seas <- mutate(df.seas,
                  C_GW=60,
                  L_GW=Q_GW*C_GW/1000,
                  Q_RO=Q_TOT-Q_GW,
                  L_RO=L_TOT-L_GW,
                  C_RO=L_RO/Q_RO*1000)

# by year ----
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
  left_join(gannett, by="SITE_NAME")

df.wyr <- mutate(df.wyr,
             C_GW=60,
             L_GW=Q_GW*C_GW/1000,
             Q_RO=Q_TOT-Q_GW,
             L_RO=L_TOT-L_GW,
             C_RO=L_RO/Q_RO*1000)

df.wyr.sfnf <- filter(df.wyr, SITE_NAME %in% c("SF", "NF"))
c_runoff <- sum(df.wyr.sfnf$L_RO)/sum(df.wyr.sfnf$Q_RO)*1000

df.wyr <- mutate(df.wyr,
                 Q_BRO=Q_RO,
                 C_BRO=c_runoff,
                 L_BRO=Q_BRO*C_BRO/1000,
                 Q_BACK=Q_TOT,
                 L_BACK=L_GW+L_BRO,
                 C_BACK=L_BACK/Q_BACK*1000,
                 Q_ANTH=Q_TOT,
                 L_ANTH=L_TOT-L_BACK,
                 C_ANTH=L_ANTH/Q_ANTH*1000)

df.wyr <- gather(df.wyr, TERM_VAR, VALUE, Q_TOT:C_ANTH) %>%
  separate(TERM_VAR, into=c("TERM", "VAR")) %>%
  mutate(SITE_NAME=ordered(SITE_NAME, levels=stn.kt_sprague$SITE_NAME))


spread(df.wyr, TERM, VALUE) %>%
  filter(VAR %in% c("GW", "RO")) %>%
  arrange(SITE_NAME, VAR) %>%
  ggplot(aes(factor(WYEAR), Q, fill=VAR)) +
  geom_bar(stat='identity', position='stack') +
  scale_fill_manual('', labels=c(ANTH="Anthropogenic Runoff",
                                 RO="Background Runoff",
                                 GW="Groundwater"),
                    values=c(ANTH="orangered",
                             RO="olivedrab3",
                             GW="steelblue")) +
  facet_wrap(~SITE_NAME, scales='free_y') +
  labs(y="TP Load (mt/yr)", x="Water Year",
       title="TP Load by Source")
















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