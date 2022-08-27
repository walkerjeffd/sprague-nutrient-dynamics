targets_prism <- list(
  tar_target(prism_fetch, {
    if (!dir.exists("data/prism/rasters")) {
      dir.create("data/prism/rasters", recursive = TRUE)
    }
    prism::prism_set_dl_dir("data/prism/rasters")
    prism::get_prism_monthlys(
      type = "ppt",
      year = 1981:year(por[2]),
      mon = 1:12,
      keepZip = FALSE
    )
  }, cue = tar_cue("never")),
  tar_target(prism_extract, {
    prism::prism_set_dl_dir("data/prism/rasters")
    prism_files <- prism::pd_to_file(
      prism::prism_archive_subset(
        "ppt", "monthly",
        years = 1981:year(por[2]),
        mon = 1:12
      )
    )
    prism_stack <- raster::stack(prism_files)
    raster::crs(prism_stack) <- sp::CRS("+init=EPSG:4269")
    
    incbasins_nad83 <- gis_subbasins %>%
      st_transform(crs = "EPSG:4269")
    
    exactextractr::exact_extract(
      prism_stack,
      incbasins_nad83,
      "mean",
      append_cols = c("basin")
    )
  }),
  tar_target(prism_mon, {
    prism_extract %>%
      pivot_longer(-basin, values_to = "precip_mm") %>%
      mutate(
        date = ymd(str_c(str_sub(name, 29, 34), "01")),
        precip_cm = change_units(precip_mm, "mm", "cm"),
        wyear = water_year(date)
      ) %>% 
      select(-name)
  }),
  tar_target(prism_wyr, {
    prism_mon %>%
      group_by(basin, wyear) %>% 
      summarise(
        n = n(),
        precip_cm = sum(precip_cm),
        .groups = "drop"
      ) %>% 
      filter(n == 12) %>% 
      select(-n)
  }),
  tar_target(prism, list(mon = prism_mon, wyr = prism_wyr)),
  tar_target(prism_wyr_plot, {
    prism_wyr %>% 
      ggplot(aes(wyear, precip_cm)) +
      geom_line() +
      facet_wrap(vars(basin))
  })
)