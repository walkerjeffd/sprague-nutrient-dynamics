library(tidyverse)

rm(list=ls())

source('functions.R')

# load data ----
load('kt_sprague.Rdata')
load('loads.Rdata')
load('gis.Rdata')

subbasin_levels <- list(
  P2010 = levels(subbasin_area$SITE_NAME),
  P2002 = setdiff(levels(subbasin_area$SITE_NAME), c("NF_Ivory", "SF_Ivory"))
)
incbasin_levels <- list(
  P2010 = setdiff(levels(incbasin_area$INC_SITE_NAME), c("Godowa-SF-NF")),
  P2002 = setdiff(levels(incbasin_area$INC_SITE_NAME), c("Godowa-SF_Ivory-NF_Ivory", "SF_Ivory-SF", "NF_Ivory-NF"))
)

df_incbasin_subbasin <- tribble(
  ~SITE,~LABEL,~INC_SITE_NAME,~CUM_SITE_NAME,
  "SR0090","Lower Sprague","Power-Lone_Pine","Power",
  "SR0080","Middle Sprage","Lone_Pine-Godowa-Sycan","Lone_Pine",
  "SR0060","Upper Sprague","Godowa-SF_Ivory-NF_Ivory","Godowa",
  "SR0070","Sycan","Sycan","Sycan",
  "SR0150","Lower SF","SF_Ivory-SF","SF_Ivory",
  "SR0050","Upper SF","SF","SF",
  "SR0140","Lower NF","NF_Ivory-NF","NF_Ivory",
  "SR0040","Upper NF","NF","NF"
)

df_site <- loads_df$site %>%
  mutate(
    PERIOD=plyr::revalue(PERIOD, c(
      "2011-2014" = "P2010",
      "2010-2014" = "P2010",
      "2002-2014" = "P2002"
    ))
  ) %>%
  mutate(
    VALUE = ifelse(TERM %in% c('Q', 'Q_AREA', 'L', 'L_AREA'), VALUE * 365.25, VALUE),
    TERM = as.character(TERM),
    SITE_NAME = as.character(SITE_NAME)
  )

dataset <- 'POR'
title <- ''
period <- 'P2010'

x_inc_C <- df_site %>%
  filter(
    DATASET == dataset,
    PERIOD == period,
    # VAR == "TP",
    TERM %in% c("C", "L", "L_AREA"),
    SITE_NAME %in% incbasin_levels[[period]]
  ) %>%
  mutate(
    VALUE = if_else(TERM == "C" & SITE_NAME %in% c('SF', 'NF', 'Sycan'), NA_real_, VALUE)
  ) %>%
  select(SEASON, INC_SITE_NAME = SITE_NAME, VAR, TERM, VALUE) %>%
  mutate(NET_CUM = "NET") %>%
  left_join(select(df_incbasin_subbasin, INC_SITE_NAME, CUM_SITE_NAME), by = "INC_SITE_NAME")

x_sub_C <- df_site %>%
  filter(
    DATASET == dataset,
    PERIOD == period,
    # VAR == "TP",
    TERM %in% c("C", "L", "L_AREA"),
    SITE_NAME %in% subbasin_levels[[period]]
  ) %>%
  mutate(
    SITE_NAME = as.character(SITE_NAME),
    TERM = as.character(TERM)
  ) %>%
  select(SEASON, CUM_SITE_NAME = SITE_NAME, VAR, TERM, VALUE) %>%
  mutate(NET_CUM = "CUM") %>%
  left_join(select(df_incbasin_subbasin, INC_SITE_NAME, CUM_SITE_NAME), by = "CUM_SITE_NAME")

x_inc_Q <- df_site %>%
  filter(
    DATASET == dataset,
    PERIOD == period,
    VAR == "FLOW",
    TERM %in% c("Q", "Q_AREA"),
    SITE_NAME %in% incbasin_levels[[period]]
  ) %>%
  mutate(
    SITE_NAME = as.character(SITE_NAME),
    TERM = as.character(TERM)
  ) %>%
  select(SEASON, INC_SITE_NAME = SITE_NAME, TERM, VALUE) %>%
  mutate(NET_CUM = "NET") %>%
  left_join(select(df_incbasin_subbasin, INC_SITE_NAME, CUM_SITE_NAME), by = "INC_SITE_NAME")

x_sub_Q <- df_site %>%
  filter(
    DATASET == dataset,
    PERIOD == period,
    VAR == "FLOW",
    TERM %in% c("Q", "Q_AREA"),
    SITE_NAME %in% subbasin_levels[[period]]
  ) %>%
  mutate(
    SITE_NAME = as.character(SITE_NAME),
    TERM = as.character(TERM)
  ) %>%
  select(SEASON, CUM_SITE_NAME = SITE_NAME, TERM, VALUE) %>%
  mutate(NET_CUM = "CUM") %>%
  left_join(select(df_incbasin_subbasin, INC_SITE_NAME, CUM_SITE_NAME), by = "CUM_SITE_NAME")

x_Q <- bind_rows(x_inc_Q, x_sub_Q) %>%
  mutate(
    VALUE = round(VALUE, digits = 2)
  )
x_C <- bind_rows(x_inc_C, x_sub_C) %>%
  mutate(
    VALUE = case_when(
      TERM == "L_AREA" ~ round(VALUE, 3),
      TERM == "L" ~ round(VALUE, 1),
      TRUE ~ round(VALUE, 2)
    )
  ) %>%
  unite(TERM, c("VAR", "TERM"))

x <- bind_rows(x_Q, x_C) %>%
  mutate(
    TERM = factor(TERM, ordered = TRUE, levels = c("Q", "Q_AREA", sort(unique(x_C$TERM))))
  ) %>%
  unite(VAR, c("NET_CUM", "TERM"), remove = FALSE) %>%
  arrange(TERM, NET_CUM) %>%
  mutate(VAR = factor(VAR, levels = unique(VAR), ordered = TRUE)) %>%
  select(-TERM, -NET_CUM) %>%
  spread(VAR, VALUE) %>%
  left_join(df_incbasin_subbasin, by = c("INC_SITE_NAME", "CUM_SITE_NAME")) %>%
  select(SITE, `INCREMENTAL_BASIN` = INC_SITE_NAME, `CUMULATIVE_BASIN` = CUM_SITE_NAME, LABEL, SEASON, everything()) %>%
  arrange(SEASON, SITE)

x %>%
  write_csv("csv/basin_values.csv", na = "")
