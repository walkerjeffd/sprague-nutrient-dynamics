targets_pou <- list(
  tar_target(pou_basin_file, here("data", "pou", "pou_irrigation_basin.csv")),
  tar_target(pou_basin, {
    read_csv(pou_basin_file, show_col_types = FALSE) %>% 
      select(site_code = SITE, pou_area_km2 = AreaSqKM) %>% 
      filter(site_code != "WR1000") %>% 
      add_row(site_code = "SR0040", pou_area_km2 = 0)
  }),
  
  tar_target(pou_valley_file, here("data", "pou", "pou_irrigation_valley.csv")),
  tar_target(pou_valley, {
    read_csv(pou_valley_file, show_col_types = FALSE) %>% 
      select(site_code = SITE, pou_area_km2 = AreaSqKM) %>% 
      filter(site_code != "WR1000") %>% 
      add_row(site_code = "SR0040", pou_area_km2 = 0) %>% 
      add_row(site_code = "SR0050", pou_area_km2 = 0)
  }),
  
  tar_target(pou_areas, {
    bind_rows(
      basin = pou_basin,
      valley = pou_valley,
      .id = "extent"
    ) %>% 
      left_join(
        incbasin_sitecode,
        by = "site_code"
      ) %>% 
      select(-site_code) %>% 
      pivot_wider(names_from = "basin", values_from = "pou_area_km2") %>% 
      mutate(
        `Godowa-SF-NF` = `Godowa-SF_Ivory-NF_Ivory` + `SF_Ivory-SF` + `NF_Ivory-NF`,
        NF_Ivory = `NF_Ivory-NF` + NF,
        SF_Ivory = `SF_Ivory-SF` + SF,
        Godowa = `Godowa-SF_Ivory-NF_Ivory` + SF_Ivory + NF_Ivory,
        Lone_Pine = `Lone_Pine-Godowa-Sycan` + Godowa + Sycan,
        Power = `Power-Lone_Pine` + Lone_Pine
      ) %>% 
      pivot_longer(-c(extent), names_to = "basin", values_to = "pou_area_km2")
  })
)