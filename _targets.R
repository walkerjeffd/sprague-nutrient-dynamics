library(targets)

# load all targets
invisible(sapply(list.files("R", pattern = ".R$", full.names = TRUE), source))
invisible(sapply(list.files("R/report", pattern = ".R$", full.names = TRUE), source))
invisible(sapply(list.files("R/report/appendices", pattern = ".R$", full.names = TRUE), source))

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate", "sf", "here", "janitor", "glue", "units", "patchwork"))

# load packages into session
if (interactive()) {
  sapply(tar_option_get("packages"), require, character.only = TRUE)
}

list(
  tar_target(por, ymd(c("20011001", "20200930"))),
  tar_target(por_wyr, water_year(por, start_month = 10)),
  
  targets_labels,
  
  targets_kt,
  targets_gis,
  targets_nlcd,
  targets_geomorph,
  targets_pou,
  targets_snotel,
  targets_usgs,
  targets_owrd,
  targets_ghcnd,
  targets_prism,
  
  targets_network,
  
  targets_flows,
  targets_loads,
  targets_anthro,
  targets_reg,
  targets_trends,
  
  targets_report_climate,
  targets_report_gis,
  targets_report_flows,
  targets_report_loads,
  targets_report_splots,
  targets_report_anthro,
  targets_report_reg,
  targets_report_trends,
  
  targets_report_es,

  targets_app_A,
  targets_app_B,
  targets_app_C,
  targets_app_D,
  targets_app_E,
  targets_app_F,
  targets_app_G,
  targets_app_H
)
