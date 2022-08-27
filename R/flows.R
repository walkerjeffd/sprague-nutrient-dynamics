tar_option_set(packages = c("tidyverse", "lubridate", "sf", "here", "janitor", "glue", "units", "patchwork"))


# functions ---------------------------------------------------------------

plot_flow_diagnostics <- function(x, station, ref_station_id, flows_ratio_mon, por) {
  p_ts <- x %>%
    ggplot(aes(date)) +
    geom_line(aes(y = ref_flow_cfs, color = "Reference"), size = 0.5) +
    geom_line(aes(y = pred_resid, color = "Interpolated"), size = 0.5) +
    geom_point(aes(y = flow_cfs, color = "Biweekly"), size = 1.5) +
    scale_x_date(expand = expansion(), breaks = por[1] + years(seq(0, 20, by = 2)), date_labels = "%m/%Y") +
    scale_y_log10_() +
    scale_color_manual(NULL, values = c(
      "Reference" = "grey50",
      "Biweekly" = "orangered",
      "Interpolated" = "orangered"
    )) +
    labs(x = "Date", y = "Flow (cfs)") +
    guides(
      colour = guide_legend(
        override.aes = list(
          linetype = c("solid", "blank", "solid"),
          shape = c(NA, 16, NA)
        )
      )
    ) +
    theme(legend.position = "bottom")
  
  p_splot <- x %>%
    ggplot(aes(ref_flow_cfs, flow_cfs)) +
    geom_point(size = 1) +
    scale_y_log10_() +
    scale_x_log10_() +
    geom_abline(linetype = "dashed", color = "orangered") +
    scale_color_manual("", values = "orangered") +
    labs(
      x = "Flow @ Reference Site (cfs)",
      y = glue("Flow @ {station} (cfs)")
    ) +
    theme(legend.position = "bottom")
  
  p_resid_ts <- x %>%
    filter(!is.na(pred_resid)) %>%
    ggplot(aes(date, ln_resid)) +
    geom_hline(yintercept = 0, color = "gray50") +
    geom_point(size = 1) +
    labs(
      x = "Date",
      y = "Log Flow Residual\nln[Measured/Scaled Reference]"
    )
  
  p_resid_splot <- x %>%
    filter(!is.na(pred_resid)) %>%
    ggplot(aes(flow_cfs, ln_resid)) +
    geom_hline(yintercept = 0, color = "gray50") +
    geom_point(size = 1) +
    xlim(0, NA) +
    labs(
      x = "Measured Biweekly Flow (cfs)",
      y = "Log Flow Residual\nln[Measured/Scaled Reference]"
    ) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 12)
    )
  
  p_ratio <- x %>% 
    filter(!is.na(flow_cfs)) %>%
    mutate(month = ordered(month, levels = c(10:12, 1:9))) %>%
    ggplot(aes(month, ratio)) +
    geom_boxplot(outlier.size = 1.5) +
    geom_point(
      data = flows_ratio_mon %>% 
        filter(station == !!station) %>% 
        mutate(month = ordered(month, levels = c(10:12, 1:9))),
      aes(month, ratio_month, color = "Mean")
    ) +
    scale_color_manual(NULL, values = "orangered") +
    labs(x = "Month", y = "Flow Ratio [Measured/Reference]")
  
  ((p_ts | p_splot) + plot_layout(widths = c(2, 1))) /
    (p_ratio | p_resid_ts | p_resid_splot) +
    plot_annotation(
      title = "Streamflow Model Diagnostics",
      subtitle = glue(
        "Site: {station}",
        "Reference Site: {ref_station_id}",
        .sep = " | "
      )
    )
}


# targets -----------------------------------------------------------------

targets_flows <- list(
  tar_target(flows_ref_day, {
    bind_rows(
      USGS = usgs_day %>% 
        filter(station == "Power"),
      OWRD = owrd_day %>% 
        filter(station %in% c("Beatty", "Sycan")) %>% 
        select(station, station_id, date, flow_cfs),
      .id = "source"
    ) %>% 
      filter(
        date >= por[1],
        date <= por[2]
      ) %>% 
      unite(station_id, c("source", "station_id"), sep = ":")
  }),
  tar_target(flows_ref_day_plot, {
    flows_ref_day %>% 
      ggplot(aes(date, flow_cfs)) +
      geom_line() +
      facet_wrap(vars(station, station_id), ncol = 1, scales = "free_y")
  }),
  
  tar_target(flows_ref_stations, {
    tribble(
      ~station, ~ref_station_id,
      "Power", "USGS:11501000",
      "Lone_Pine", "USGS:11501000",
      "Godowa", "OWRD:11497500",
      "Sycan", "OWRD:11499100",
      "SF", "OWRD:11497500",
      "NF", "OWRD:11497500",
      "SF_Ivory", "OWRD:11497500",
      "NF_Ivory", "OWRD:11497500"
    )
  }),
  
  tar_target(flows_ratio_day, {
    flows_ref_stations %>% 
      left_join(
        flows_ref_day %>% 
          select(ref_station_id = station_id, date, ref_flow_cfs = flow_cfs),
        by = "ref_station_id"
      ) %>% 
      left_join(kt_flow, by = c("station", "date")) %>% 
      mutate(ratio = flow_cfs / ref_flow_cfs) %>% 
      filter(
        date >= ymd(20001001),
        date <= por[2],
        (!str_ends(station, "_Ivory") | date >= ymd(20081001))
      )
  }),
  tar_target(flows_ratio_day_plot, {
    flows_ratio_day %>% 
      ggplot(aes(date)) +
      geom_line(aes(y = ref_flow_cfs)) +
      geom_point(aes(y = flow_cfs), size = 0.5, color = "deepskyblue") +
      facet_wrap(vars(station, ref_station_id), scales = "free_y", ncol = 2)
  }),
  tar_target(flows_ratio_day_plot_ratio, {
    flows_ratio_day %>% 
      ggplot(aes(date, ratio)) +
      geom_point(size = 0.5) +
      facet_wrap(vars(station, ref_station_id), scales = "free_y", ncol = 2)
  }),
  tar_target(flows_ratio_mon, {
    flows_ratio_day %>% 
      filter(!is.na(ref_flow_cfs), !is.na(flow_cfs)) %>% 
      group_by(station, month = month(date)) %>% 
      summarise(
        n = n(),
        ratio_month = mean(flow_cfs) / mean(ref_flow_cfs),
        .groups = "drop"
      )
  }),
  tar_target(flows_model, {
    flows_ratio_day %>% 
      mutate(month = month(date)) %>% 
      left_join(
        flows_ratio_mon %>% 
          select(station, month, ratio_month),
        by = c("station", "month")
      ) %>% 
      mutate(
        pred_ratio = ref_flow_cfs * ratio_month,
        ln_resid = log(flow_cfs / pred_ratio)
      ) %>% 
      arrange(station, date) %>% 
      group_by(station) %>% 
      mutate(
        ln_resid_interp = approx(date, ln_resid, xout = date, yleft = 0, yright = 0)$y,
        pred_resid = pred_ratio * exp(ln_resid_interp)
      ) %>% 
      ungroup()
  }),
  tar_target(flows_model_plot, {
    flows_model %>% 
      ggplot(aes(date)) +
      geom_line(aes(y = pred_ratio), alpha = 0.5) +
      geom_line(aes(y = pred_resid), color = "orangered") +
      geom_point(aes(y = flow_cfs), size = 1, color = "deepskyblue") +
      facet_wrap(vars(station), ncol = 2, scales = "free_y")
  }),
  tar_target(flows_day, {
    flows_model %>% 
      select(station, date, flow_cfs = pred_resid)
  }),

  tar_target(flows_validation_day, {
    x_obs <- owrd_day %>% 
      filter(station %in% c("SF", "NF", "Godowa", "Lone_Pine")) %>% 
      select(station, owrd_station_id = station_id, date, obs = flow_cfs)
    flows_day %>% 
      filter(
        date >= min(x_obs$date),
        station %in% c("SF", "NF", "Godowa", "Lone_Pine")
      ) %>% 
      select(station, date, pred = flow_cfs) %>% 
      inner_join(
        x_obs,
        by = c("station", "date")
      ) %>% 
      mutate(resid = pred - obs) %>% 
      arrange(station, date) %>% 
      nest_by(station, owrd_station_id) %>% 
      mutate(
        n = sum(!is.na(data$resid)),
        rmse = sqrt(mean(data$resid ^ 2, na.rm = TRUE)),
        r2 = cor(data$obs, data$pred, use = "complete.obs") ^ 2
      ) %>% 
      mutate(station = factor(station, levels = station_levels)) %>% 
      arrange(station)
  }),
  tar_target(flows_validation_mon, {
    flows_validation_day %>% 
      mutate(
        data = list({
          data %>% 
            group_by(date = floor_date(date, "month")) %>% 
            summarise(across(c(pred, obs), mean)) %>% 
            mutate(resid = pred - obs)
        }),
        n = sum(!is.na(data$resid)),
        rmse = sqrt(mean(data$resid ^ 2, na.rm = TRUE)),
        r2 = cor(data$obs, data$pred, use = "complete.obs") ^ 2
      )
  }),
  tar_target(flows_validation_day_plot_ts, {
    flows_validation_day %>% 
      select(station, data) %>% 
      unnest(data) %>% 
      ggplot(aes(date)) +
      geom_line(aes(y = pred)) +
      geom_point(aes(y = obs), size = 0.5, color = "deepskyblue") +
      facet_wrap(vars(station), ncol = 2, scales = "free_y")
  }),
  tar_target(flows_validation_day_plot_splot, {
    flows_validation_day %>% 
      select(station, data) %>% 
      unnest(data) %>% 
      ggplot(aes(pred, obs)) +
      geom_abline(linetype = "dashed") +
      geom_point(size = 0.5) +
      facet_wrap(vars(station), ncol = 2, scales = "free") +
      theme(aspect.ratio = 1)
  }),
  
  tar_target(flows_diagnostics, {
    flows_model %>% 
      nest_by(station, ref_station_id) %>% 
      mutate(
        plot = list(plot_flow_diagnostics(data, station, ref_station_id, flows_ratio_mon, por))
      ) %>% 
      mutate(station = factor(station, levels = station_levels)) %>% 
      arrange(station)
  }),
  
  tar_target(flows_gannett_file, "data/gannett/gannett.csv", format = "file"),
  tar_target(flows_gannett, {
    read_csv(flows_gannett_file, show_col_types = FALSE) %>% 
      clean_names() %>% 
      rename(basin = site_name)
  })
)