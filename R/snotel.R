targets_snotel <- list(
  tar_target(snotel_stations_file, here("data", "snotel", "snotel_stations.csv"), format = "file"),
  tar_target(snotel_stations, {
    read_csv(snotel_stations_file, show_col_types = FALSE) %>% 
      clean_names() %>% 
      filter(site_name %in% c(
        "CRAZYMAN FLAT",
        "SUMMER RIM",
        "TAYLOR BUTTE"
      ))
  }),
  tar_target(snotel_fetch, {
    snotelr::snotel_download(site_id = c("810" , "1010" , "800"), internal=T)
  }),
  tar_target(snotel_day, {
    as_tibble(snotel_fetch) %>% 
      transmute(
        site_name = str_trim(str_to_upper(site_name)), 
        date = ymd(date), 
        swe_cm = snow_water_equivalent / 10
      )
  }),
  tar_target(snotel_wyr, {
    x_mean <- snotel_day %>% 
      group_by(site_name, wyear = water_year(date)) %>% 
      summarise(swe_cm = mean(swe_cm), .groups = "drop")
    x_max <- snotel_day %>% 
      group_by(site_name, wyear = water_year(date)) %>% 
      arrange(desc(swe_cm)) %>% 
      slice(1)
    x_apr1 <- snotel_day %>% 
      mutate(wyear = water_year(date)) %>% 
      filter(month(date) == 4, day(date) == 1)
    x_melt <- snotel_day %>%
      arrange(site_name, desc(date)) %>%
      filter(month(date) %in% c(3:6)) %>%
      group_by(site_name, wyear = water_year(date)) %>%
      mutate(cumul_swe_cm = cumsum(coalesce(swe_cm, 0))) %>%
      filter(swe_cm > 1) %>%
      slice(1) %>%
      filter(wyear >= por_wyr[1]) %>%
      transmute(site_name, wyear, swe_cm = yday(date))
    bind_rows(
      mean = x_mean,
      max = x_max,
      apr1 = x_apr1,
      melt = x_melt,
      .id = "stat"
    ) %>% 
      mutate(stat = fct_inorder(stat))
  }),
  tar_target(snotel_plot, {
    snotel_day %>% 
      ggplot(aes(date, swe_cm)) +
      geom_line() +
      geom_point(
        data = snotel_wyr %>% 
          filter(stat %in% c("max", "apr1")),
        aes(color = stat)
      ) +
      facet_wrap(vars(site_name), scales = "free_y", ncol = 1)
  })
)
