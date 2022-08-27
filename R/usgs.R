targets_usgs <- list(
  tar_target(usgs_station_labels, tribble(
    ~station_id, ~station,
    "11495800", "NF",
    "11497500", "Beatty",
    "11497550", "Godowa",
    "11501000", "Power"
  )),
  tar_target(usgs_stations, {
    dataRetrieval::readNWISsite(usgs_station_labels$station_id) %>% 
      as_tibble() %>% 
      transmute(
        station_id = site_no, description = station_nm,
        latitude = dec_lat_va, longitude = dec_long_va,
        area_km2 = change_units(drain_area_va, "mi2", "km2") 
      ) %>% 
      left_join(usgs_station_labels, by = "station_id") %>% 
      relocate(station)
  }),
  tar_target(usgs_fetch, {
    dataRetrieval::readNWISdv(
      siteNumbers = usgs_stations$station_id,
      parameterCd = "00060"
    )
  }),
  tar_target(usgs_day, {
    usgs_fetch %>% 
      dataRetrieval::renameNWISColumns() %>% 
      as_tibble() %>% 
      clean_names() %>% 
      left_join(
        select(usgs_stations, station_id, station),
        by = c(site_no = "station_id")
      ) %>% 
      select(station, station_id = site_no, date, flow_cfs = flow)
  }),
  tar_target(usgs_day_plot, {
    usgs_day %>% 
      ggplot(aes(date, flow_cfs)) +
      geom_line() +
      facet_wrap(vars(station), ncol = 1, scales = "free_y")
  })
)