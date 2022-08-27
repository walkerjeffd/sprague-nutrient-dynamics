tar_option_set(packages = c("tidyverse", "lubridate", "sf", "here", "janitor", "glue", "units", "patchwork"))

targets_geomorph <- list(
  tar_target(geomorph_file, here("data", "geomorph", "incremental_basins_geomorphology_clip.csv"), format = "file"),
  tar_target(geomorph_areas, {
    read_csv(geomorph_file, show_col_types = FALSE) %>% 
      select(
        site_code = SITE,
        valley_area_km2 = AreaSqKM
      ) %>% 
      add_row(site_code = "SR0040", valley_area_km2 = 0) %>% 
      filter(site_code != "WR1000") %>% 
      left_join(
        incbasin_sitecode,
        by = "site_code"
      ) %>% 
      select(-site_code) %>% 
      pivot_longer(ends_with("area_km2")) %>% 
      pivot_wider(names_from = "basin") %>% 
      mutate(
        `Godowa-SF-NF` = `Godowa-SF_Ivory-NF_Ivory` + `SF_Ivory-SF` + `NF_Ivory-NF`,
        NF_Ivory = `NF_Ivory-NF` + NF,
        SF_Ivory = `SF_Ivory-SF` + SF,
        Godowa = `Godowa-SF_Ivory-NF_Ivory` + SF_Ivory + NF_Ivory,
        Lone_Pine = `Lone_Pine-Godowa-Sycan` + Godowa + Sycan,
        Power = `Power-Lone_Pine` + Lone_Pine
      ) %>% 
      pivot_longer(-name, names_to = "basin", values_to = "value") %>% 
      pivot_wider()
  })
)