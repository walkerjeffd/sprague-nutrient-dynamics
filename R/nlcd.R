tar_option_set(packages = c("tidyverse", "lubridate", "sf", "here", "janitor", "glue", "units", "patchwork"))

targets_nlcd <- list(
  tar_target(nlcd_codes_file, here("data", "nlcd", "nlcd_codes.csv"), format = "file"),
  tar_target(nlcd_codes, {
    read_csv(nlcd_codes_file, show_col_types = FALSE) %>% 
      mutate(
        lulc_cat = factor(lulc_cat, levels = c(
          "Developed", "Planted/Cultivated",
          "Herbaceous", "Shrubland", "Barren",
          "Forest", "Wetlands", "Water"
        ))
      )
  }),
  tar_target(nlcd_basin_file, here("data", "nlcd", "nlcd_basin.csv"), format = "file"),
  tar_target(nlcd_basin, {
    read_csv(nlcd_basin_file, show_col_types = FALSE) %>% 
      select(-`__fid__`) %>% 
      rename(site_code = SITE, total_area_km2 = AreaSqKM)
  }),
  tar_target(nlcd_valley_file, here("data", "nlcd", "nlcd_valley.csv"), format = "file"),
  tar_target(nlcd_valley, {
    read_csv(nlcd_valley_file, show_col_types = FALSE) %>% 
      select(-`__fid__`) %>% 
      rename(site_code = SITE, total_area_km2 = AreaSqKM) %>% 
      filter(!site_code %in% c("SR0050", "SR0040"))
  }),
  
  tar_target(nlcd_areas, {
    bind_rows(
      basin = nlcd_basin,
      valley = nlcd_valley,
      .id = "extent"
    ) %>% 
      pivot_longer(-c(extent, site_code, total_area_km2), names_to = "lulc", values_to = "count", values_drop_na = TRUE) %>% 
      mutate(
        lulc = parse_number(lulc)
      ) %>% 
      filter(lulc > 0) %>% 
      left_join(
        select(nlcd_codes, lulc, landuse = lulc_cat),
        by = "lulc"
      ) %>% 
      group_by(extent, site_code, landuse) %>% 
      summarise(count = sum(count), .groups = "drop") %>% 
      mutate(
        area_km2 = count * 30 * 30 / 1e6 # 30 m x 30 m resolution
      ) %>% 
      inner_join(
        incbasin_sitecode,
        by = "site_code"
      ) %>% 
      select(extent, basin, landuse, area_km2) %>% 
      pivot_wider(names_from = "basin", values_from = "area_km2", values_fill = 0) %>% 
      mutate(
        `Godowa-SF-NF` = `Godowa-SF_Ivory-NF_Ivory` + `SF_Ivory-SF` + `NF_Ivory-NF`,
        NF_Ivory = `NF_Ivory-NF` + NF,
        SF_Ivory = `SF_Ivory-SF` + SF,
        Godowa = `Godowa-SF_Ivory-NF_Ivory` + SF_Ivory + NF_Ivory,
        Lone_Pine = `Lone_Pine-Godowa-Sycan` + Godowa + Sycan,
        Power = `Power-Lone_Pine` + Lone_Pine
      ) %>% 
      pivot_longer(-c(extent, landuse), names_to = "basin", values_to = "area_km2") %>% 
      complete(extent, basin, landuse, fill = list(area_km2 = 0))
  }),
  tar_target(nlcd_areas_plot, {
    nlcd_areas %>% 
      ggplot(aes(basin, area_km2)) +
      geom_col(aes(fill = landuse), position = "stack") +
      coord_flip() +
      facet_grid(vars(), vars(extent), scales = "free")
  })
)