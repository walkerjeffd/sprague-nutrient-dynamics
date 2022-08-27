targets_reg <- list(
  tar_target(reg_net_data, {
    x <- loads_wyr %>% 
      filter(
        basin %in% names(incbasin_labels),
        # basin != "Godowa-SF-NF",
        dataset == "POR"
      ) %>% 
      mutate(across(c(flow_hm3, load_kg), ~ .x / n_day))
    x_flow <- x %>% 
      filter(param == "TP") %>% 
      mutate(param = "FLOW") %>% 
      select(-load_kg, -conc_ppb) %>% 
      mutate(flow_cfs = change_units(flow_hm3, "hm3/day", "ft3/sec")) %>% 
      select(-flow_hm3) %>% 
      pivot_longer(flow_cfs, names_to = "var") %>% 
      mutate(param_var = var, param = "FLOW")
    x_wq <- x %>% 
      select(-flow_hm3) %>% 
      mutate(
        # conc_ppb = if_else(str_detect(basin, "-"), conc_ppb, NA_real_)
      ) %>% 
      pivot_longer(c(load_kg, conc_ppb), names_to = "var", values_drop_na = TRUE) %>% 
      unite(param_var, c("param", "var"))
    
    bind_rows(
      x_flow,
      x_wq
    ) %>% 
      mutate(
        basin = factor(basin, levels = names(incbasin_labels)),
        season = factor(season, levels = names(season_labels)),
        param_var = factor(param_var, levels = names(param_var_labels))
      )
  }),
  tar_target(reg_net_plots, {
    reg_net_data %>%
      nest_by(param_var) %>% 
      arrange(param_var) %>% 
      mutate(
        plot = list({
          y_label <- param_var_labels[[as.character(param_var)]]
          data %>% 
            ggplot(aes(wyear, value)) +
            geom_rect(
              data = tibble(xmin = 2013, xmax = Inf, ymin = -Inf, ymax = Inf),
              aes(
                xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                fill = "Regulation\nPeriod"
              ), inherit.aes = FALSE,
              alpha = 0.5
            ) +
            geom_hline(yintercept = 0) +
            geom_line() +
            geom_point(aes(color = value > 0)) +
            scale_x_continuous(breaks = seq(2002, 2020, by = 4)) +
            scale_fill_manual(NULL, values = "goldenrod") +
            scale_color_brewer("Direction", palette = "Set1", labels = c(
              "TRUE" = "Increasing", "FALSE" = "Decreasing"
            ), breaks = c("TRUE", "FALSE")) +
            facet_grid(vars(season), vars(basin), scales = "free_y", labeller = labeller(
              basin = set_names(str_wrap(incbasin_labels, width = 20), names(incbasin_labels)),
              season = season_labels
            )) +
            guides(
              color = guide_legend(order = 1),
              fill = guide_legend(order = 2)
            ) +
            labs(
              x = "Water Year",
              y = glue("Net Change in {y_label}")
            ) +
            theme(
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
              strip.text = element_text(size = 8)
            )
        })
      )
  }),
  
  tar_target(reg_seas_data, {
    report_splots_data %>% 
      mutate(
        period = if_else(
          water_year(date) < 2013,
          "Pre-Regulation\n(WY 2002-2012)",
          "Regulation\n(WY 2013-2020)",
        ),
        period = factor(period, levels = rev(unique(period)))
      ) %>% 
      select(station, period, date, flow_cfs, TP, PO4, PP, TN, NH4, NO23, TSS) %>% 
      mutate(`PP/TP` = PP / TP, .after = "PP") %>% 
      pivot_longer(-c(station, period, date)) %>% 
      mutate(name = fct_inorder(name))
  }),
  tar_target(reg_seas_plots, {
    reg_seas_data %>% 
      nest_by(name) %>% 
      mutate(
        plot = list({
          if (name == "flow_cfs") {
            y_label <- "Flow (cfs)"
          } else if (name == "PP/TP") {
            y_label <- "% Total P as Particulate"
          } else {
            y_label <- glue("{name} Conc (ppb)")
          }
          p <- data %>% 
            ggplot(aes(same_wyear(date), value, color = period)) +
            geom_point(size = 1, alpha = 0.25) +
            geom_smooth(method = "loess", formula = y ~ x, se = FALSE, span = 0.5) +
            scale_x_date(date_breaks = "3 months", date_labels = "%b", expand = expansion()) +
            scale_color_brewer("Period", palette = "Set1", breaks = rev(levels(data$period))) +
            facet_wrap(vars(station), nrow = 2, scales = "free") +
            labs(
              x = "Day of Water Year",
              y = y_label
            ) +
            theme(
              aspect.ratio = 1,
              legend.key.height = unit(10, "mm")
            )
          if (name == "PP/TP") {
            p <- p + scale_y_continuous(limits = c(0, 1), labels = scales::percent)
          } else if (name == "flow_cfs") {
            p <- p + scale_y_log10_(magnitudes = c(0.1, 1, 10, 100, 1000, 10000))
          } else {
            p <- p + ylim(0, NA)
          }
          p
        })
      )
  }),
  
  tar_target(reg_chewacuan_day, {
    bind_rows(
      owrd_day %>% 
        filter(station %in% c("Chewaucan", "Sycan", "Beatty")) %>% 
        select(station, date, flow_cfs),
      usgs_day %>% 
        filter(!station %in% c("Beatty"))
    ) %>% 
      filter(!is.na(flow_cfs)) %>% 
      mutate(wyear = water_year(date))
  }),
  tar_target(reg_chewacuan_mon, {
    reg_chewacuan_day %>% 
      group_by(station, wyear, date = floor_date(date, unit = "month")) %>% 
      summarise(
        n = n(),
        flow_cfs = mean(flow_cfs),
        .groups = "drop"
      ) %>% 
      filter(n == days_in_month(date)) %>% 
      select(-n)
  }),
  tar_target(reg_chewacuan_splots, {
    reg_chewacuan_mon %>% 
      filter(year(date) >= 2002) %>%
      pivot_wider(names_from = "station", values_from = "flow_cfs") %>% 
      pivot_longer(-c(wyear, date, Chewaucan), names_to = "station", values_drop_na = TRUE) %>% 
      mutate(month = factor(month(date, label = TRUE), levels = month.abb)) %>%
      mutate(period = if_else(
        wyear >= 2013,
        "Regulation\n(WY 2013-2020)",
        "Pre-Regulation\n(WY 2002-2012)"
      )) %>% 
      nest_by(station) %>% 
      mutate(
        plot = list({
          data %>% 
            arrange(period) %>% 
            ggplot(aes(Chewaucan, value, color = period)) +
            geom_point(alpha = 0.5) +
            geom_smooth(method = "lm", se = FALSE, aes(), formula = y ~ x) +
            scale_alpha_manual(values = c(0.5, 1)) +
            scale_color_manual("Period", values = c("gray50", "orangered")) +
            facet_wrap(vars(month), scales = "free") +
            xlim(0, NA) +
            ylim(0, NA) +
            labs(
              x = "Monthly Flow (cfs)\nChewacuan",
              y = glue("Monthly Flow (cfs)\n{station}")
            ) +
            theme(legend.key.height = unit(10, "mm"))
        })
      )
  }),
  tar_target(reg_chewacuan_box, {
    reg_chewacuan_mon %>% 
      filter(
        year(date) >= 2002,
        month(date) %in% c(7:10),
        station != "NF"
      ) %>%
      mutate(month = factor(month(date, label = TRUE), levels = month.abb[7:10])) %>%
      mutate(period = wyear >= 2013) %>% 
      ggplot(aes(month, flow_cfs)) +
      geom_boxplot(aes(fill = period), position = "dodge") +
      scale_alpha_manual(values = c(0.5, 1)) +
      scale_fill_manual(values = c("gray50", "orangered")) +
      ylim(0, NA) +
      facet_wrap(vars(station), scales = "free_y")
  }),
  tar_target(reg_chewacuan_seas, {
    reg_chewacuan_day %>% 
      filter(
        station %in% c("Beatty", "Chewaucan", "Power", "Sycan"),
        wyear >= 2002
      ) %>% 
      mutate(
        period = if_else(
          wyear >= 2013,
          "Regulation\n(WY 2013-2020)",
          "Pre-Regulation\n(WY 2002-2012)"
        ),
        station = factor(station, levels = c("Power", "Beatty", "Sycan", "Chewaucan"))
      ) %>% 
      ggplot(aes(same_wyear(date), flow_cfs)) +
      geom_line(aes(group = wyear, color = period, alpha = period)) +
      scale_x_date(expand = expansion(), date_breaks = "3 months", date_labels = "%b") +
      scale_y_log10_(magnitudes = c(1, 10, 100, 1000, 10000)) +
      scale_color_manual(
        "Period",
        values = c("gray50", "orangered")
      ) +
      scale_alpha_manual(values = c(0.5, 1), guide = "none") +
      facet_wrap(vars(station), scales = "free_y", ncol = 2, labeller = labeller(station = function (x) {
        case_when(
          x == "Chewaucan" ~ "Chewaucan @ Paisley",
          x == "Sycan" ~ "Sycan",
          TRUE ~ as.character(glue("Sprague @ {x}"))
        )
      })) +
      labs(x = "Day of Water Year", y = "Flow (cfs)") +
      theme(legend.key.height = unit(10, "mm"))
  })
)