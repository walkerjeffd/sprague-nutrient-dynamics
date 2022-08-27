tar_option_set(packages = c("tidyverse", "lubridate", "sf", "here", "janitor", "glue", "units", "patchwork"))

targets_gis <- list(
  tar_target(gis_flowlines_file, here("data", "gis", "sprague_flowlines.shp"), format = "file"),
  tar_target(gis_flowlines, {
    st_read(gis_flowlines_file) %>% 
      clean_names()
  }),
  
  tar_target(gis_basin_file, here("data", "gis", "sprague_basin.shp"), format = "file"),
  tar_target(gis_basin, {
    st_read(gis_basin_file) %>% 
      clean_names() %>% 
      select(huc8, name, area_km2 = area_sq_km)
  }),
  
  tar_target(gis_incbasins_por_file, here("data", "gis", "sprague_incbasins.shp"), format = "file"),
  tar_target(gis_incbasins_por, {
    basins <- tribble(
      ~station, ~basin,
      "Power", "Power-Lone_Pine",
      "Lone_Pine", "Lone_Pine-Godowa-Sycan",
      "Godowa", "Godowa-SF-NF",
      "Sycan", "Sycan",
      "SF", "SF",
      "NF", "NF"
    )
    st_read(gis_incbasins_por_file) %>% 
      clean_names() %>% 
      filter(site != "WR1000") %>% 
      select(site_code = site, area_km2 = area_sq_km) %>% 
      left_join(
        select(kt_stations, site_code, station),
        by = "site_code"
      ) %>% 
      left_join(
        basins,
        by = "station"
      ) %>% 
      select(basin, site_code, area_km2)
  }),
  tar_target(gis_incbasins_recent_file, here("data", "gis", "sprague_incbasins_ivory.shp"), format = "file"),
  tar_target(gis_incbasins_recent, {
    basins <- tribble(
      ~station, ~basin,
      "Power", "Power-Lone_Pine",
      "Lone_Pine", "Lone_Pine-Godowa-Sycan",
      "Godowa", "Godowa-SF_Ivory-NF_Ivory",
      "Sycan", "Sycan",
      "SF_Ivory", "SF_Ivory-SF",
      "SF", "SF",
      "NF_Ivory", "NF_Ivory-NF",
      "NF", "NF"
    )
    st_read(gis_incbasins_recent_file) %>% 
      clean_names() %>% 
      filter(site != "WR1000") %>% 
      select(site_code = site, area_km2 = area_sq_km) %>% 
      left_join(
        select(kt_stations, site_code, station),
        by = "site_code"
      ) %>% 
      left_join(
        basins,
        by = "station"
      ) %>% 
      transmute(basin, site_code, area_km2)
  }),
  tar_target(gis_incbasins, {
    rbind(
      gis_incbasins_por,
      gis_incbasins_recent
    ) %>% 
      filter(str_detect(basin, "-")) %>% 
      distinct()
  }),
  tar_target(gis_incbasins_areas, {
    st_drop_geometry(gis_incbasins) %>%
      select(basin, site_code, basin_area_km2 = area_km2)
  }),
  
  tar_target(gis_subbasins_file, here("data", "gis", "sprague_subbasins.shp"), format = "file"),
  tar_target(gis_subbasins, {
    st_read(gis_subbasins_file) %>% 
      clean_names() %>% 
      filter(site != "WR1000") %>% 
      select(site_code = site, area_km2 = area_sq_km) %>% 
      left_join(
        select(kt_stations, site_code, basin = station),
        by = "site_code"
      ) %>% 
      select(basin, site_code, area_km2) %>% 
      arrange(basin)
  }),
  
  tar_target(gis_basins, {
    x <- bind_rows(gis_incbasins, gis_subbasins) %>% 
      select(-site_code)
    x_union <- tribble(
      ~union_name,  ~basin,
      "Godowa+Sycan",      "Godowa",
      "Godowa+Sycan",      "Sycan",
      "SF_Ivory+NF_Ivory", "SF_Ivory",
      "SF_Ivory+NF_Ivory", "NF_Ivory",
      "SF+NF",             "SF",
      "SF+NF",             "NF"
    )
    x_union_area <- x_union %>% 
      left_join(st_drop_geometry(x), by = "basin") %>% 
      group_by(basin = union_name) %>% 
      summarise(area_km2 = sum(area_km2))
    x_union_sf <- x_union %>% 
      left_join(x, by = "basin") %>% 
      group_by(basin = union_name) %>% 
      summarise(geometry = st_union(geometry)) %>% 
      left_join(x_union_area, by = "basin") %>% 
      st_as_sf()
    
    # gis_basins %>% 
    #   filter(basin %in% station_levels) %>% 
    #   st_simplify(dTolerance = 100) %>%
    #   ggplot() +
    #   geom_sf() +
    #   facet_wrap(vars(basin), nrow = 2)
    
    bind_rows(x, x_union_sf)
  }),
  tar_target(gis_basins_map, {
    gis_basins %>% 
      ggplot() +
      geom_sf(aes(fill = area_km2)) +
      scale_fill_viridis_c(trans = "log10") +
      facet_wrap(vars(factor(basin, levels = basin_levels)), ncol = 4)
  }),
  
  tar_target(gis_stations, {
    kt_stations %>% 
      st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326")
  }),
  
  tar_target(gis_basemap, ggmap::get_stamenmap(bbox = c(-122.1, 42.15, -120.6, 43), zoom = 10)),
  
  tar_target(incbasin_sitecode, {
    st_drop_geometry(gis_incbasins) %>% 
      filter(basin != "Godowa-SF-NF") %>% 
      bind_rows(
        st_drop_geometry(gis_subbasins) %>% 
          filter(basin %in% c("Sycan", "SF", "NF"))
      ) %>% 
      select(-area_km2)
  })
)
