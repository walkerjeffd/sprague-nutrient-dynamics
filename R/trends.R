
# functions ---------------------------------------------------------------

scale_color_trend <- function (...) {
  scale_color_manual(
    "Trend Direction\n(Significance)", 
    values = c(
      "INCREASE_1" = "#D7191C",
      "INCREASE_2" = "#FDAE61",
      "NONE" = "gray80",
      "DECREASE_2" = "#ABD9E9",
      "DECREASE_1" = "#2C7BB6"
    ),
    labels = c(
      "INCREASE_1" = "Increase (p <= 0.05)",
      "INCREASE_2" = "Increase (0.05 < p <= 0.1)",
      "NONE" = "No Trend (p > 0.1)",
      "DECREASE_2" = "Decrease (0.05 < p <= 0.1)",
      "DECREASE_1" = "Decrease (p <= 0.05)"
    ),
    drop = FALSE,
    ...
  )
}

scale_fill_trend <- function (...) {
  scale_fill_manual(
    "Trend Direction\n(Significance)", 
    values = c(
      "INCREASE_1" = "#D7191C",
      "INCREASE_2" = "#FDAE61",
      "NONE" = "gray80",
      "DECREASE_2" = "#ABD9E9",
      "DECREASE_1" = "#2C7BB6"
    ),
    labels = c(
      "INCREASE_1" = "Increase (p < 0.05)",
      "INCREASE_2" = "Increase (p < 0.1)",
      "NONE" = "No Trend (p >= 0.1)",
      "DECREASE_2" = "Decrease (p < 0.1)",
      "DECREASE_1" = "Decrease (p < 0.05)"
    ),
    drop = FALSE,
    ...
  )
}

kendallTrendTest <- purrr::possibly(EnvStats::kendallTrendTest, NULL)
kendallSeasonalTrendTest <- purrr::possibly(EnvStats::kendallSeasonalTrendTest, NULL)

tidy.kendallTrendTest <- function (x) {
  if (is.null(x)) return(tibble())
  
  tibble(
    slope = x$estimate[["slope"]],
    intercept = x$estimate[["intercept"]],
    p.value = x$p.value[["z"]]
  )
}

tidy.kendallSeasonalTrendTest <- function (x) {
  if (is.null(x)) return(tibble())
  
  tibble(
    slope = x$estimate[["slope"]],
    intercept = x$estimate[["intercept"]],
    p.value = x$p.value[["z (Trend)"]]
  )
}

trend_significance <- function(slope, p.value) {
  if (is.na(p.value) | is.na(slope)) return(NA_character_)
  x <- "NONE"
  if (p.value <= 0.05) {
    if (slope < 0) {
      x <- "DECREASE_1"
    } else {
      x <- "INCREASE_1"
    }
  } else if (p.value <= 0.1) {
    if (slope < 0) {
      x <- "DECREASE_2"
    } else {
      x <- "INCREASE_2"
    }
  }
  x
}

create_trends <- function (x) {
  month_levels <- tolower(month.abb[c(10:12, 1:9)])
  
  df_seasons <- bind_rows(
    "oct-dec" = x$mon %>% 
      filter(month %in% tolower(month.abb[10:12])),
    "jan-mar" = x$mon %>% 
      filter(month %in% tolower(month.abb[1:3])),
    "apr-jun" = x$mon %>% 
      filter(month %in% tolower(month.abb[4:6])),
    "jul-sep" = x$mon %>% 
      filter(month %in% tolower(month.abb[7:9])),
    "oct-mar" = x$mon %>% 
      filter(month %in% tolower(month.abb[c(10:12, 1:3)])),
    "apr-sep" = x$mon %>% 
      filter(month %in% tolower(month.abb[c(4:9)])),
    "oct-sep" = x$mon,
    .id = "season"
  )
  
  # trend: kt annual
  df_trend_kt_annual <- x$wyr %>% 
    nest_by(dataset, basin, param, var, transform) %>% 
    mutate(
      method = "kt",
      season = "annual",
      test = list(kendallTrendTest(data$value, x = data$wyear)),
      tidy = list(tidy.kendallTrendTest(test))
    ) %>% 
    unnest_wider(tidy)
  
  # trend: kt monthly
  df_trend_kt_monthly <- x$mon %>% 
    nest_by(dataset, basin, param, var, transform, season = as.character(month)) %>% 
    rowwise() %>% 
    mutate(
      method = "kt",
      test = list(kendallTrendTest(data$value, x = data$wyear)),
      tidy = list(tidy.kendallTrendTest(test))
    ) %>% 
    unnest_wider(tidy)
  
  # trend: lm annual
  df_trend_lm_annual <- x$wyr %>% 
    nest_by(dataset, basin, param, var, transform) %>% 
    rowwise() %>% 
    mutate(
      method = "lm",
      season = "annual",
      test = list(lm(value ~ wyear, data = data)),
      slope = test$coefficients[["wyear"]],
      intercept = test$coefficients[["(Intercept)"]],
      p.value = summary(test)$coefficients[["wyear", "Pr(>|t|)"]]
    )
  
  # trend: skt
  df_trend_skt <- df_seasons %>% 
    nest_by(dataset, basin, param, var, transform, season) %>% 
    rowwise() %>% 
    mutate(
      method = "skt",
      test = list(kendallSeasonalTrendTest(data$value, season = data$month, year = data$wyear)),
      tidy = list(tidy.kendallSeasonalTrendTest(test))
    ) %>% 
    unnest_wider(tidy)
  
  # merge
  bind_rows(
    df_trend_kt_annual,
    df_trend_kt_monthly,
    df_trend_lm_annual,
    df_trend_skt
  ) %>% 
    mutate(
      method = ordered(method, levels = c("skt", "kt", "lm")),
      season = ordered(season, levels = c(
        # month
        month_levels,
        
        # 4 seasons
        "oct-dec",
        "jan-mar",
        "apr-jun",
        "jul-sep",
        
        # 2 seasons
        "oct-mar",
        "apr-sep",
        
        # annual
        "oct-sep",
        "annual"
      ))
    ) %>% 
    arrange(season, method) %>% 
    mutate(
      method_season = str_c(method, season, sep = ":"),
      method_season = ordered(method_season, levels = unique(method_season))
    ) %>% 
    rowwise() %>% 
    mutate(
      median_x = median(data$wyear, na.rm = TRUE),
      median_y = median(data$value, na.rm = TRUE),
      mean_y = mean(data$value, na.rm = TRUE),
      
      intercept_median = median_y - slope * median_x,
      slope_pct = if_else(
        transform == "log10", 
        10 ^ slope - 1,
        slope / mean_y
      ),
      
      significance = trend_significance(slope, p.value),
      
      var = factor(var, levels = c("precip_cm", "flow_hm3", "load_kg", "conc_ppb")),
      param = factor(param, levels = c("PRCP", "Q", "TP", "PO4", "PP", "TN", "NH4", "NO23", "TSS")),
      por = str_c(range(data$wyear), collapse = "-")
    )
}

create_rolling_trends <- function (trends) {
  x_trends_por <- trends %>% 
    filter(
      dataset == "POR",
      # param != "TSS",
      # !str_detect(basin, "Ivory"),
      method_season %in% c("kt:annual", "skt:oct-sep", "skt:oct-dec", "skt:jan-mar", "skt:apr-jun", "skt:jul-sep")
    ) %>% 
    select(basin, param, var, transform, data, method, season, method_season) %>%
    mutate(
      min_wyear = min(data$wyear)
    )
  
  x_trends_rolling_data <- x_trends_por %>% 
    crossing(
      start_wyear = 2002:2016
    ) %>% 
    filter(start_wyear >= min_wyear) %>%
    rowwise() %>% 
    mutate(
      data = list(filter(data, wyear >= start_wyear))
    )
  x_trends_rolling_skt <- x_trends_rolling_data %>% 
    filter(str_starts(method_season, "skt")) %>% 
    mutate(
      test = list(kendallSeasonalTrendTest(data$value, season = data$month, year = data$wyear)),
      tidy = list(tidy.kendallSeasonalTrendTest(test))
    )
  x_trends_rolling_kt <- x_trends_rolling_data %>% 
    filter(method_season == "kt:annual") %>% 
    mutate(
      test = list(kendallTrendTest(data$value, x = data$wyear)),
      tidy = list(tidy.kendallTrendTest(test))
    )
  x_trends_rolling <- bind_rows(x_trends_rolling_skt, x_trends_rolling_kt) %>% 
    unnest_wider(tidy) %>% 
    rowwise() %>% 
    mutate(
      median_x = median(data$wyear, na.rm = TRUE),
      median_y = median(data$value, na.rm = TRUE),
      mean_y = mean(data$value, na.rm = TRUE),
      
      intercept_median = median_y - slope * median_x,
      slope_pct = if_else(
        transform == "log10", 
        10 ^ slope - 1,
        slope / mean_y
      ),
      
      significance = trend_significance(slope, p.value)
    )
  
  x_trends_rolling
}

create_trend_diagnostic <- function (x, basin, param, var, transform, por, labels) {
  labels <- list(
    units = c(
      "precip_cm" = "cm",
      "conc_ppb" = "ppb",
      "load_kg" = "kg",
      "flow_hm3" = "hm3"
    ),
    var = c(
      "precip_cm" = "Precip",
      "flow_hm3" = "Flow",
      "load_kg" = "Load",
      "conc_ppb" = "Conc"
    )
  )
  
  basin_label <- basin
  param_label <- as.character(param)
  var_label <- labels$var[[as.character(var)]]
  
  if (var %in% c("flow_hm3", "precip_cm")) {
    param_var_label <- var_label
  } else {
    param_var_label <- str_c(param_label, " ", var_label)
  }
  
  # param_label <- labels$param[[param]]
  # var_label <- labels$var[[var]]
  
  if (var == "conc_ppb") {
    units_label <- labels$units[[var]]
  } else {
    units_label <- glue("{labels$units[[var]]}/d")
  }
  var_units_label <- glue("{var_label} ({units_label})")
  title_label <- glue("{basin_label} | {param_var_label}")
  
  if (transform == "log10") {
    var_units_label <- glue("log10[{var_units_label}]")
  }
  
  x_trend_skt <- x %>% 
    filter(method == "skt", season == "oct-sep")
  
  x_trend_monthly <- x %>% 
    filter(method == "kt", season %in% month_levels) %>% 
    mutate(month = season)
  
  x_trend_annual_kt <- x %>% 
    filter(method == "kt", season == "annual")
  
  x_trend_annual_lm <- x %>% 
    filter(method == "lm", season == "annual")
  
  x_mon <- x_trend_skt %>% 
    select(data) %>%
    unnest(data) %>% 
    mutate(month = ordered(month, levels = month_levels))
  
  x_wyr <- x_trend_annual_kt %>% 
    select(data) %>% 
    unnest(data)
  
  p_ts_mon <- x_mon %>% 
    ggplot(aes(decimal_date(date), value)) +
    geom_line() +
    geom_abline(
      data = x_trend_skt,
      aes(intercept = intercept_median, slope = slope, color = significance),
      size = 1
    ) +
    scale_color_trend(guide = "none") +
    scale_x_continuous(
      breaks = decimal_date(ymd(20011001)) + seq(0, 30, by = if_else(min(x_wyr$wyear) < 2010, 4, 2)),
      labels = ~ str_c("Oct ", floor(.x)),
      expand = expansion()
    ) +
    labs(
      x = "Month/Year",
      y = var_units_label,
      title = "Monthly Timeseries",
      subtitle = "Trendline = Seasonal Kendall Test (Oct-Sep)"
    ) + 
    theme(
      plot.title = element_text(size = 10),
      plot.subtitle = element_text(size = 8)
    )
  
  p_ts_wyr <- x_wyr %>% 
    ggplot(aes(wyear, value)) +
    geom_line(color = "gray20") +
    geom_point(color = "gray20") +
    geom_abline(
      data = x_trend_annual_kt,
      aes(intercept = intercept_median, slope = slope, color = significance, linetype = method),
      size = 1,
      key_glyph = "path"
    ) +
    geom_abline(
      data = x_trend_annual_lm,
      aes(intercept = intercept, slope = slope, color = significance, linetype = method),
      size = 1,
      key_glyph = "path"
    ) +
    scale_x_continuous(
      breaks = seq(min(x_wyr$wyear), max(x_wyr$wyear), by = if_else(min(x_wyr$wyear) < 2010, 4, 2))
    ) +
    scale_linetype_manual(values = c("kt" = "solid", "lm" = "dashed"), guide = "none") +
    scale_color_trend(guide = "none") +
    labs(
      x = "Water Year",
      y = var_units_label,
      title = "Annual Timeseries",
      subtitle = "Trendlines = Mann Kendall Test (solid), Linear Regression (dashed)"
    ) +
    theme(
      plot.title = element_text(size = 10),
      plot.subtitle = element_text(size = 8)
    )
  
  p_skt_mon <- x_mon %>% 
    ggplot(aes(wyear, value)) +
    geom_point(color = "gray20", size = 1) +
    geom_abline(
      data = x_trend_monthly,
      aes(intercept = intercept_median, slope = slope, color = significance),
      size = 1
    ) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 4)) +
    scale_color_trend(guide = "none") +
    facet_wrap(vars(month), nrow = 1, labeller = labeller(
      month = set_names(month.abb, nm = tolower(month.abb))
    )) +
    labs(
      x = "Water Year",
      y = var_units_label,
      title = "Annual Timeseries by Month",
      subtitle = "Trendlines = Mann Kendall Test by Month"
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      plot.title = element_text(size = 10),
      plot.subtitle = element_text(size = 8)
    )
  
  p_bar_slope_pct <- x %>% 
    ggplot(aes(method_season, slope_pct)) +
    geom_col(aes(fill = significance)) +
    geom_hline(yintercept = 0) +
    scale_fill_trend() +
    scale_x_discrete(labels = method_season_labels) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    facet_grid(vars(), vars(method_season_group), space = "free_x", scales = "free_x") +
    labs(
      x = NULL,
      y = "Slope\n(%/yr)",
      title = "Trend Slopes"
    ) +
    theme(
      axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
      plot.title = element_text(size = 10),
      plot.subtitle = element_text(size = 8),
      strip.text = element_text(size = 8)
    )
  # p_bar_slope_pct
  
  # x, term, param, var, transform, por
  tbl <- c(
    "Station: "     = as.character(basin),
    "Variable: "  = param_var_label,
    "Transform: " = transform,
    "Period: "    = str_c("WY", as.character(por))
  )
  
  p_tbl_info <- gridExtra::tableGrob(
    data.frame(value = unname(tbl)),
    rows = names(tbl),
    cols = NULL,
    theme = gridExtra::ttheme_default(
      rowhead = list(
        fg_params = list(hjust = 1, x = 1)
      ),
      core = list(
        fg_params = list(hjust = 0, x = 0.05)
      ),
      padding = unit(c(5, 5), "mm")
    )
  )
  
  p_tbl_slopes <- x %>% 
    filter(method %in% c("kt", "skt"), season %in% c("oct-sep", "annual")) %>% 
    transmute(
      method_season = case_when(
        method_season == "skt:oct-sep" ~ "Seasonal Kendall (Oct-Sep)",
        method_season == "kt:annual" ~ "Mann Kendall (Annual)"
      ),
      slope_pct = str_c(scales::number(slope_pct * 100, accuracy = 0.01), " %/yr"),
      p.value = if_else(p.value < 0.0001, "< 0.0001", scales::number(p.value, accuracy = 0.0001))
    ) %>% gridExtra::tableGrob(
      rows = NULL,
      cols = c("Method", "Slope", "p-value"),
      theme = gridExtra::ttheme_default(
        colhead = list(
          fg_params = list(fontface = 3L, hjust = 1, x = 0.95),
          bg_params = list(fill = "white")
        ),
        core = list(
          fg_params = list(hjust = 1, x = 0.95)
        ),
        padding = unit(c(5, 5), "mm")
      )
    )
  
  patch_1 <- (p_ts_mon | p_ts_wyr)
  patch_12 <- (patch_1 / p_skt_mon)
  p_tbl <- gridExtra::arrangeGrob(
    p_tbl_info, p_tbl_slopes, 
    ncol = 1,
    vp = grid::viewport(
      x = unit(0, "npc"),
      just = c("left", "center")
    )
  )
  
  (patch_12 / (p_bar_slope_pct | p_tbl)) +
    plot_layout(guides = "collect", heights = c(2, 1, 2)) +
    plot_annotation(
      title = title_label,
      theme = theme(plot.margin = unit(rep(0.5, 4), "in"))
    )
}


# targets -----------------------------------------------------------------

targets_trends <- list(
  tar_target(trends_data, {
    var_min_values <- c(
      "flow_hm3" = 0.001,
      "load_kg" = 0.001,
      "conc_ppb" = 1
    )
    
    x_mon_flow_load_conc <- loads_mon %>%
      filter(basin %in% station_levels) %>% 
      select(-n_day) %>% 
      pivot_longer(c(flow_hm3, load_kg, conc_ppb), names_to = "var") %>% 
      filter(
        water_year(date, start_month = 10) >= por_wyr[1],
        water_year(date, start_month = 10) <= por_wyr[2],
        !is.na(value)
      ) %>% 
      mutate(
        min_value = map_dbl(var, ~ var_min_values[[as.character(.x)]]),
        transform = "log10",
        value = log10(pmax(value, min_value))
      ) %>% 
      select(-min_value)
    
    x_mon_flow <- x_mon_flow_load_conc %>% 
      filter(var == "flow_hm3") %>% 
      distinct(dataset, basin, date, var, value, transform) %>% 
      mutate(param = "Q")
    
    x_mon_load_conc <- x_mon_flow_load_conc %>% 
      filter(var != "flow_hm3")

    x_mon_precip_por <- prism$mon %>% 
      filter(
        basin %in% unique(x_mon_flow_load_conc$basin),
        water_year(date, start_month = 10) >= por_wyr[1],
        water_year(date, start_month = 10) <= por_wyr[2]
      ) %>% 
      transmute(
        dataset = "POR",
        basin = basin,
        param = "PRCP",
        var = "precip_cm",
        date,
        value = precip_cm,
        transform = "none"
      )
    x_mon_precip_recent <- prism$mon %>% 
      filter(
        basin %in% unique(x_mon_flow_load_conc$basin),
        water_year(date, start_month = 10) >= 2010,
        water_year(date, start_month = 10) <= por_wyr[2]
      ) %>% 
      transmute(
        dataset = "RECENT",
        basin = basin,
        param = "PRCP",
        var = "precip_cm",
        date,
        value = precip_cm,
        transform = "none"
      )
    
    x_mon <- bind_rows(x_mon_flow, x_mon_load_conc, x_mon_precip_por, x_mon_precip_recent) %>% 
      mutate(
        month = tolower(month(date, abbr = TRUE, label = TRUE)),
        year = year(date),
        wyear = water_year(date, 10),
        basin = fct_drop(factor(basin, levels = basin_levels))
      ) %>% 
      arrange(dataset, basin, param, var, date)

    x_wyr_flow_load_conc <- loads_wyr %>% 
      select(-n_day) %>% 
      pivot_longer(c(flow_hm3, load_kg, conc_ppb), names_to = "var") %>% 
      filter(
        basin %in% station_levels,
        season == "annual",
        wyear >= por_wyr[1],
        wyear <= por_wyr[2],
        !is.na(value)
      ) %>% 
      mutate(
        min_value = map_dbl(var, ~ var_min_values[[as.character(.x)]]),
        transform = "log10",
        value = log10(pmax(value, min_value))
      ) %>% 
      select(-season, -min_value)
    
    x_wyr_flow <- x_wyr_flow_load_conc %>% 
      filter(var == "flow_hm3") %>% 
      distinct(dataset, basin, wyear, var, value, transform) %>% 
      mutate(param = "Q")
    
    x_wyr_load_conc <- x_wyr_flow_load_conc %>% 
      filter(var != "flow_hm3")
    
    x_wyr_precip_por <- prism$wyr %>% 
      filter(
        basin %in% unique(x_wyr_flow_load_conc$basin),
        wyear >= por_wyr[1],
        wyear <= por_wyr[2]
      ) %>% 
      transmute(
        dataset = "POR",
        basin,
        param = "PRCP",
        var = "precip_cm",
        wyear,
        value = precip_cm,
        transform = "none"
      )
    x_wyr_precip_recent <- prism$wyr %>% 
      filter(
        basin %in% unique(x_wyr_flow_load_conc$basin),
        wyear >= 2010,
        wyear <= por_wyr[2]
      ) %>% 
      transmute(
        dataset = "RECENT",
        basin,
        param = "PRCP",
        var = "precip_cm",
        wyear,
        value = precip_cm,
        transform = "none"
      )
    
    x_wyr <- bind_rows(x_wyr_flow, x_wyr_load_conc, x_wyr_precip_por, x_wyr_precip_recent) %>% 
      mutate(
        basin = factor(basin, levels = basin_levels)
      ) %>% 
      arrange(dataset, basin, param, var, wyear)
    
    list(
      mon = x_mon,
      wyr = x_wyr
    )
  }),
  tar_target(trends, create_trends(trends_data)),
  tar_target(trends_season_labels, {
    c(
      "oct-sep" = "All Months\n(Oct-Dec)",
      "oct-dec" = "Fall\n(Oct-Dec)",
      "jan-mar" = "Winter\n(Jan-Mar)",
      "apr-jun" = "Spring\n(Apr-Jun)",
      "jul-sep" = "Summer\n(Jul-Sep)"
    )
  }),
  tar_target(trends_param_var_labels, {
    c(
      "PRCP_precip_cm" = "Precip",
      "Q_flow_hm3" = "Flow",
      "TP_load_kg" = "TP Load",
      "TP_conc_ppb" = "TP FWM Conc.",
      "TN_load_kg" = "TN Load",
      "TN_conc_ppb" = "TN FWM Conc."
    )
  }),
  tar_target(trends_plot_season_param, {
    x <- trends %>% 
      filter(
        (dataset == "POR" & por == "2002-2020" & !str_detect(basin, "Ivory")) | 
          (dataset == "RECENT" & por ==  "2010-2020"),
        param %in% c("PRCP", "Q", "TP", "TN"),
        method == "skt",
        season %in% c("oct-sep", "oct-dec", "jan-mar", "apr-jun", "jul-sep")
      ) %>% 
      select(dataset, por, basin, param, var, season, slope_pct, significance) %>% 
      arrange(por, param, var) %>% 
      unite(param_var, c("param", "var")) %>% 
      mutate(
        param_var = fct_inorder(param_var),
        season = factor(season, levels = c("oct-sep", "oct-dec", "jan-mar", "apr-jun", "jul-sep"))
      )
    x %>% 
      nest_by(dataset, por) %>% 
      mutate(
        plot = list({
          data %>% 
            mutate(basin = fct_rev(basin)) %>% 
            ggplot(aes(basin, slope_pct)) +
            geom_hline(yintercept = 0, alpha = 0.5) +
            geom_segment(aes(x = basin, xend = basin, y = 0, yend = slope_pct), alpha = 0.5) +
            geom_point(aes(color = significance), size = 4, alpha = 1) +
            geom_point(shape = 21, fill = NA, color = "gray50", size = 4, alpha = 1) +
            scale_color_trend() +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 4), labels = scales::percent_format(accuracy = 1)) +
            facet_grid(
              vars(param_var), vars(season), 
              labeller = labeller(param_var = trends_param_var_labels, season = trends_season_labels)
            ) +
            coord_flip() +
            labs(y = "Trend Slope (%/yr)", x = NULL)
        })
      )
  }),
  tar_target(trends_plot_season_station, {
    x <- trends %>% 
      filter(
        (dataset == "POR" & por == "2002-2020" & !str_detect(basin, "Ivory")) | 
          (dataset == "RECENT" & por ==  "2010-2020"),
        param %in% c("PRCP", "Q", "TP", "TN"),
        method == "skt",
        season %in% c("oct-sep", "oct-dec", "jan-mar", "apr-jun", "jul-sep")
      ) %>% 
      select(dataset, por, basin, param, var, season, slope_pct, significance) %>% 
      arrange(por, param, var) %>% 
      unite(param_var, c("param", "var")) %>% 
      mutate(
        param_var = fct_inorder(param_var),
        season = factor(season, levels = c("oct-sep", "oct-dec", "jan-mar", "apr-jun", "jul-sep"))
      )
    x %>% 
      nest_by(dataset, por) %>% 
      mutate(
        plot = list({
          data %>% 
            mutate(basin = fct_rev(basin)) %>% 
            ggplot(aes(fct_rev(param_var), slope_pct)) +
            geom_hline(yintercept = 0, alpha = 0.5) +
            geom_segment(aes(x = param_var, xend = param_var, y = 0, yend = slope_pct), alpha = 0.5) +
            geom_point(aes(color = significance), size = 4, alpha = 1) +
            geom_point(shape = 21, fill = NA, color = "gray50", size = 4, alpha = 1) +
            scale_color_trend() +
            scale_x_discrete(labels = trends_param_var_labels) +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 4), labels = scales::percent_format(accuracy = 1)) +
            facet_grid(
              vars(fct_rev(basin)), vars(season), 
              labeller = labeller(season = trends_season_labels)
            ) +
            coord_flip() +
            labs(y = "Trend Slope (%/yr)", x = NULL)
        })
      )
  }),
  tar_target(trends_plot_season_conc, {
    x <- trends %>% 
      mutate(por = if_else(param == "TSS", "2010-2020", por)) %>% 
      filter(
        (dataset == "POR" & por == "2002-2020" & !str_detect(basin, "Ivory")) | 
          (dataset == "RECENT" & por ==  "2010-2020"),
        var == "conc_ppb",
        method == "skt",
        season %in% c("oct-sep", "oct-dec", "jan-mar", "apr-jun", "jul-sep")
      ) %>% 
      select(dataset, por, basin, param, var, season, slope_pct, significance) %>% 
      mutate(
        season = factor(season, levels = c("oct-sep", "oct-dec", "jan-mar", "apr-jun", "jul-sep"))
      )
    x %>% 
      nest_by(dataset, por) %>% 
      mutate(
        plot = list({
          data %>% 
            mutate(basin = fct_rev(basin)) %>% 
            ggplot(aes(basin, slope_pct)) +
            geom_hline(yintercept = 0, alpha = 0.5) +
            geom_segment(aes(x = basin, xend = basin, y = 0, yend = slope_pct), alpha = 0.5) +
            geom_point(aes(color = significance), size = 4, alpha = 1) +
            geom_point(shape = 21, fill = NA, color = "gray50", size = 4, alpha = 1) +
            scale_color_trend() +
            scale_y_continuous(breaks = scales::pretty_breaks(n = 4), labels = scales::percent_format(accuracy = 1)) +
            facet_grid(vars(param), vars(season), labeller = labeller(season = trends_season_labels)) +
            coord_flip() +
            labs(y = "Trend Slope (%/yr)", x = NULL)
        })
      )
  }),
  
  tar_target(trends_rolling, create_rolling_trends(trends)),
  tar_target(trends_rolling_plots, {
    x_labels <- trends_rolling %>% 
      unite(param_var, c("param", "var"), remove = FALSE) %>% 
      distinct(var, param, param_var) %>% 
      arrange(desc(var), param) %>% 
      mutate(
        param_var_label = case_when(
          param == "PRCP" ~ "Precip",
          param == "Q" ~ "Flow",
          var == "load_kg" ~ str_c(param, " Load"),
          var == "conc_ppb" ~ str_c(param, " Conc"),
          TRUE ~ NA_character_
        ),
        param_var_label = fct_inorder(param_var_label)
      )
    trends_rolling %>%
      filter(start_wyear <= 2016) %>% 
      select(method_season, start_wyear, basin, param, var, slope, slope_pct, p.value, significance) %>%
      unite(param_var, c("param", "var"), remove = FALSE) %>% 
      nest_by(method_season) %>% 
      mutate(
        plot = list({
          data %>%
            left_join(select(x_labels, param_var, param_var_label), by = "param_var") %>% 
            ggplot(aes(start_wyear, fct_rev(basin))) +
            geom_tile(aes(fill = significance), color = "white", alpha = 0.9) +
            scale_fill_manual(
              "Trend Direction\n(Significance)",
              values = c(
                "INCREASE_1" = "#D7191C",
                "INCREASE_2" = "#FDAE61",
                "NONE" = "gray80",
                "DECREASE_2" = "#ABD9E9",
                "DECREASE_1" = "#2C7BB6"
              ),
              labels = c(
                "INCREASE_1" = "Increase (p < 0.05)",
                "INCREASE_2" = "Increase (p < 0.1)",
                "NONE" = "No Trend (p >= 0.1)",
                "DECREASE_2" = "Decrease (p < 0.1)",
                "DECREASE_1" = "Decrease (p < 0.05)"
              ),
              drop = FALSE
            ) +
            scale_y_discrete(expand = expansion()) +
            scale_x_continuous(
              breaks = seq(min(trends_rolling$start_wyear), max(trends_rolling$start_wyear), by = 2),
              expand = expansion()
            ) +
            facet_wrap(vars(param_var_label), ncol = 7) +
            labs(
              x = "Starting Water Year of Trend Period",
              y = NULL
            ) +
            theme(
              aspect.ratio = 1,
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7)
            )
        })
      )
  }),
  
  tar_target(trends_diagnostics, {
    trends %>%
      filter(
        (dataset == "POR" & !str_detect(basin, "Ivory") & param != "TSS") |
        (dataset == "RECENT" & (str_detect(basin, "Ivory") | param == "TSS")),
        basin %in% station_levels
      ) %>%
      mutate(
        basin = factor(basin, levels = station_levels),
        param = factor(param, levels = c("PRCP", "Q", names(param_labels))),
        var = factor(var, levels = c("precip_cm", "flow_hm3", "load_kg", "conc_ppb")),
        method_season_group = case_when(
          season %in% c("oct-sep", "annual") ~ "All\nMonths",
          season %in% month_levels ~ "Monthly",
          season %in% c("oct-mar", "apr-sep") ~ "6-mon\nSeas.",
          TRUE ~ "3-month\nSeasons"
        ),
        method_season_group = ordered(method_season_group, levels = c(
          "Monthly",
          "3-month\nSeasons",
          "6-mon\nSeas.",
          "All\nMonths"
        )),
        method = ordered(method, levels = c("skt", "kt", "lm"))
      ) %>% 
      nest_by(dataset, basin, param, var, transform, por) %>%
      mutate(
        plot = list(create_trend_diagnostic(data, basin, param, var, transform, por))
      ) %>% 
      arrange(param, var, basin)
  }),
  
  tar_target(trends_chewaucan_data, {
    x_mon <- reg_chewacuan_mon %>%
      filter(station == "Chewaucan", wyear >= 2002) %>% 
      transmute(
        dataset = "POR",
        date,
        basin = station,
        var = "flow_hm3",
        value = log10(pmax(change_units(flow_cfs, "ft3/sec", "hm3/d"), 0.001)),
        transform = "log10",
        param = "Q",
        month = tolower(month(date, label = TRUE)),
        year = year(date),
        wyear
      )
    x_wyr <- reg_chewacuan_day %>%
      filter(station == "Chewaucan", wyear >= 2002) %>% 
      group_by(station, wyear) %>% 
      summarise(flow_cfs = mean(flow_cfs), .groups = "drop") %>% 
      transmute(
        dataset = "POR",
        wyear,
        basin = station,
        var = "flow_hm3",
        value = log10(pmax(change_units(flow_cfs, "ft3/sec", "hm3/d"), 0.001)),
        transform = "log10",
        param = "Q"
      )
      
    list(
      mon = x_mon,
      wyr = x_wyr
    )    
  }),
  tar_target(trends_chewaucan, create_trends(trends_chewaucan_data)),
  tar_target(trends_chewaucan_rolling, create_rolling_trends(trends_chewaucan)),
  tar_target(trends_chewaucan_rolling_plot, {
    x <- trends_chewaucan_rolling %>%
      filter(method_season == "skt:oct-sep") %>%
      select(start_wyear, basin, param, var, slope, slope_pct, p.value, significance) %>%
      unite(param_var, c("param", "var"), remove = FALSE)
    x_labels <- distinct(x, var, param, param_var) %>% 
      mutate(
        param_var_label = case_when(
          param == "PRCP" ~ "Precip",
          param == "Q" ~ "Flow",
          var == "load_kg" ~ str_c(param, " Load"),
          var == "conc_ppb" ~ str_c(param, " Conc"),
          TRUE ~ NA_character_
        ),
        param_var_label = fct_inorder(param_var_label)
      )
    x %>%
      left_join(select(x_labels, param_var, param_var_label), by = "param_var") %>% 
      ggplot(aes(start_wyear, fct_rev(basin))) +
      geom_tile(aes(fill = significance), color = "white", alpha = 0.9) +
      scale_fill_manual(
        "Trend Direction\n(Significance)",
        values = c(
          "INCREASE_1" = "#D7191C",
          "INCREASE_2" = "#FDAE61",
          "NONE" = "gray80",
          "DECREASE_2" = "#ABD9E9",
          "DECREASE_1" = "#2C7BB6"
        ),
        labels = c(
          "INCREASE_1" = "Increase (p < 0.05)",
          "INCREASE_2" = "Increase (p < 0.1)",
          "NONE" = "No Trend (p >= 0.1)",
          "DECREASE_2" = "Decrease (p < 0.1)",
          "DECREASE_1" = "Decrease (p < 0.05)"
        ),
        drop = FALSE
      ) +
      scale_y_discrete(expand = expansion()) +
      scale_x_continuous(
        breaks = seq(min(trends_chewaucan_rolling$start_wyear), max(trends_chewaucan_rolling$start_wyear), by = 2),
        expand = expansion()
      ) +
      facet_wrap(vars(param_var_label), ncol = 6) +
      labs(
        x = "Starting Water Year of Trend Period",
        y = NULL
      ) +
      theme(
        aspect.ratio = 1,
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7)
      )
  }),
  tar_target(trends_chewaucan_diagnostics, {
    p <- trends_chewaucan %>%
      mutate(
        # basin = factor(basin, levels = station_levels),
        # param = factor(param, levels = c("PRCP", "Q", names(param_labels))),
        # var = factor(var, levels = c("precip_cm", "flow_hm3", "load_kg", "conc_ppb")),
        method_season_group = case_when(
          season %in% c("oct-sep", "annual") ~ "All\nMonths",
          season %in% month_levels ~ "Monthly",
          season %in% c("oct-mar", "apr-sep") ~ "6-mon\nSeas.",
          TRUE ~ "3-month\nSeasons"
        ),
        method_season_group = ordered(method_season_group, levels = c(
          "Monthly",
          "3-month\nSeasons",
          "6-mon\nSeas.",
          "All\nMonths"
        )),
        method = ordered(method, levels = c("skt", "kt", "lm"))
      ) %>% 
      nest_by(dataset, basin, param, var, transform, por) %>%
      mutate(
        plot = list(create_trend_diagnostic(data, basin, param, var, transform, por))
      ) %>% 
      arrange(param, var, basin)
    p$plot[[1]]
  })
) 
