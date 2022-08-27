targets_ghcnd <- list(
  tar_target(ghcnd_station_names, {
    tribble(
      ~station_id, ~station,
      "GHCND:USC00353232","GERBER",
      "GHCND:USW00094236","KFA",
      "GHCND:USC00358173","SUMMER"
    )
  }),
  tar_target(ghcnd_stations_fetch, {
    map_df(ghcnd_station_names$station_id, ~ rnoaa::ncdc_stations(.)$data)
  }),
  tar_target(ghcnd_stations, {
    ghcnd_stations_fetch %>% 
      select(
        station_id = id,
        station_name = name,
        latitude,
        longitude,
        elevation,
        elevation_unit = elevationUnit,
        start = mindate,
        end = maxdate
      ) %>% 
      left_join(ghcnd_station_names, by = "station_id") %>% 
      relocate(station)
  }),
  tar_target(ghcnd_fetch, {
    ghcnd_stations %>% 
      select(station, station_id) %>% 
      rowwise() %>% 
      mutate(
        data = list(rnoaa::ghcnd(stationid = str_remove(station_id, "GHCND:")))
      )
  }),
  tar_target(ghcnd_day, {
    make_date <- possibly(make_date, otherwise = NA_character_)
    ghcnd_fetch %>% 
      unnest(data) %>% 
      filter(
        element %in% c("PRCP", "TMIN", "TMAX"),
        year >= year(por[1]),
        year <= year(por[2])
      ) %>% 
      select(station, station_id, id, year, month, element, starts_with("VALUE")) %>% 
      pivot_longer(starts_with("VALUE"), names_prefix = "VALUE", names_to = "day") %>% 
      pivot_wider(names_from = "element") %>% 
      transmute(
        station,
        station_id,
        date = make_date(year, month, day),
        prcp_mm = PRCP / 10,
        tmax_c = TMAX / 10,
        tmin_c = TMIN / 10
      ) %>% 
      filter(!is.na(date))
  }),
  tar_target(ghcnd_day_plot, {
    ghcnd_day %>% 
      pivot_longer(-c(station, station_id, date)) %>% 
      ggplot(aes(date, value)) +
      geom_line() +
      facet_grid(vars(name), vars(station), scales = "free_y")
  })
)