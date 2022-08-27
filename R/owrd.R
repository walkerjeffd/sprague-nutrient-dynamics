targets_owrd <- list(
  tar_target(owrd_stations_file, here("data", "owrd", "owrd_stations.csv"), format = "file"),
  tar_target(owrd_stations, {
    read_csv(owrd_stations_file, show_col_types = FALSE) %>% 
      clean_names() %>% 
      mutate(
        station_id = as.character(station_id),
        latitude = map(str_split(lat, "_", n = 3), parse_number),
        latitude = map_dbl(latitude, ~ .[1] + (.[2] + .[3] / 60) / 60),
        longitude = map(str_split(lon, "_", n = 3), parse_number),
        longitude = map_dbl(longitude, ~ .[1] + (.[2] + .[3] / 60) / 60),
        start = mdy(start),
        area_km2 = change_units(drainage_area_mi2, "mi2", "km2")
      ) %>% 
      select(station_id, station = site_name, description, latitude, longitude, area_km2, start)
  }),
  tar_target(owrd_fetch, {
    owrd_stations %>% 
      select(station, station_id, start) %>%
      rowwise() %>% 
      mutate(
        fetch_start = min(start, por[1]),
        fetch_end = por[2],
        fetch_url = glue("https://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx?station_nbr={station_id}&start_date={format(fetch_start, '%m/%d/%Y')}%2012:00:00%20AM&end_date={format(fetch_end, '%m/%d/%Y')}%2012:00:00%20AM&dataset=MDF&format=tsv"),
        data = list(read_tsv(url(fetch_url), show_col_types = FALSE))
      ) %>% 
      ungroup() %>% 
      select(-start, -fetch_start, -fetch_end)
  }),
  tar_target(owrd_day, {
    owrd_fetch %>% 
      select(station, station_id, data) %>% 
      unnest(data) %>% 
      transmute(
        station, station_id, 
        date = mdy(record_date),
        flow_cfs = mean_daily_flow_cfs,
        status = published_status,
        estimated
      ) %>% 
      filter(!is.na(flow_cfs))
  }),
  tar_target(owrd_day_plot, {
    owrd_day %>% 
      ggplot(aes(date, flow_cfs)) +
      geom_line() +
      facet_wrap(vars(station), ncol = 1, scales = "free_y")
  })
)