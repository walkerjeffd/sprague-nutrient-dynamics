targets_loads <- list(
  tar_target(loads_model, {
    crossing(
      dataset = c("POR", "RECENT"),
      station = station_levels,
      param = setdiff(names(param_labels), "PP")
    ) %>%
      mutate(
        start = case_when(
          param == "TSS" ~ 20101001,
          str_ends(station, "_Ivory") ~ 20091001,
          dataset == "RECENT" ~ 20091001,
          dataset == "POR" ~ 20011001,
          TRUE ~ NA_real_
        ),
        start = ymd(start),
        end = ymd(20200930)
      ) %>%
      left_join(
        flows_day %>% 
          mutate(flow_hm3 = change_units(flow_cfs, "ft3/sec", "hm3/d")) %>% 
          nest_by(station, .key = "flows"),
        by = "station"
      ) %>% 
      left_join(
        kt_wq %>% 
          nest_by(dataset, station, param, .key = "wq"),
        by = c("dataset", "station", "param")
      ) %>% 
      rowwise() %>% 
      mutate(
        data = list(
          left_join(flows, wq, by = "date") %>% 
            transmute(date, flow_hm3, conc_ppb = value * 1000)
        ),
        results = list(estimate_loads(data, period = c(start, end)))
      ) %>% 
      unnest(results)
  }),
  tar_target(loads_day, {
    x <- loads_model %>% 
      select(dataset, station, param, pred) %>% 
      unnest(pred) %>% 
      select(dataset, station, param, date, flow_hm3, load_kg, conc_ppb)
    x_pp <- x %>% 
      filter(param %in% c("TP", "PO4")) %>% 
      select(-load_kg) %>% 
      pivot_wider(names_from = "param", values_from = "conc_ppb") %>% 
      mutate(PP = pmax(TP - PO4, 1)) %>% 
      pivot_longer(c(TP, PO4, PP), names_to = "param", values_to = "conc_ppb") %>% 
      mutate(load_kg = flow_hm3 * conc_ppb) %>% 
      filter(param == "PP")
    
    bind_rows(x, x_pp) %>% 
      select(-conc_ppb) %>% 
      pivot_longer(c(flow_hm3, load_kg), names_to = "var") %>% 
      pivot_wider(names_from = "station") %>% 
      mutate(
        `Godowa+Sycan` = Godowa + Sycan,
        `SF_Ivory+NF_Ivory` = SF_Ivory + NF_Ivory,
        `SF+NF` = SF + NF
      ) %>% 
      pivot_longer(-c(dataset, param, date, var), names_to = "basin", values_drop_na = TRUE) %>% 
      pivot_wider(names_from = "var") %>% 
      mutate(conc_ppb = load_kg / flow_hm3) %>% 
      pivot_longer(c(flow_hm3, load_kg, conc_ppb), names_to = "var") %>% 
      pivot_wider(names_from = "basin") %>% 
      mutate(
        `Power-Lone_Pine` = Power - Lone_Pine,
        `Lone_Pine-Godowa-Sycan` = Lone_Pine - `Godowa+Sycan`,
        `Godowa-SF-NF` = Godowa - `SF+NF`,
        `Godowa-SF_Ivory-NF_Ivory` = Godowa - `SF_Ivory+NF_Ivory`,
        `SF_Ivory-SF` = SF_Ivory - SF,
        `NF_Ivory-NF` = NF_Ivory - NF,
      ) %>% 
      pivot_longer(-c(dataset, param, date, var), names_to = "basin", values_drop_na = TRUE) %>% 
      pivot_wider(names_from = "var") %>%
      mutate(n_day = 1)
  }),
  tar_target(loads_mon, {
    x <- loads_day %>% 
      filter(basin %in% station_levels) %>% 
      group_by(dataset, basin, param, date = floor_date(date, unit = "month")) %>% 
      summarise(
        n_day = n(),
        flow_hm3 = sum(flow_hm3),
        load_kg = sum(load_kg),
        conc_ppb = load_kg / flow_hm3,
        .groups = "drop"
      )
    
    x %>% 
      select(-conc_ppb) %>% 
      pivot_longer(c(flow_hm3, load_kg), names_to = "var") %>% 
      pivot_wider(names_from = "basin") %>% 
      mutate(
        `Godowa+Sycan` = Godowa + Sycan,
        `SF_Ivory+NF_Ivory` = SF_Ivory + NF_Ivory,
        `SF+NF` = SF + NF
      ) %>% 
      pivot_longer(-c(dataset, param, date, var, n_day), names_to = "basin", values_drop_na = TRUE) %>% 
      pivot_wider(names_from = "var") %>% 
      mutate(conc_ppb = load_kg / flow_hm3) %>% 
      pivot_longer(c(flow_hm3, load_kg, conc_ppb), names_to = "var") %>% 
      pivot_wider(names_from = "basin") %>% 
      mutate(
        `Power-Lone_Pine` = Power - Lone_Pine,
        `Lone_Pine-Godowa-Sycan` = Lone_Pine - `Godowa+Sycan`,
        `Godowa-SF-NF` = Godowa - `SF+NF`,
        `Godowa-SF_Ivory-NF_Ivory` = Godowa - `SF_Ivory+NF_Ivory`,
        `SF_Ivory-SF` = SF_Ivory - SF,
        `NF_Ivory-NF` = NF_Ivory - NF,
      ) %>% 
      pivot_longer(-c(dataset, param, date, var, n_day), names_to = "basin", values_drop_na = TRUE) %>% 
      pivot_wider(names_from = "var")
  }),
  tar_target(loads_wyr, {
    x <- bind_rows(
      tibble(month = 1:12, season = "annual"),
      tibble(
        month = 1:12,
        season = case_when(
          month %in% 10:12 ~ "fall",
          month %in% 1:3 ~ "winter",
          month %in% 4:6 ~ "spring",
          month %in% 7:9 ~ "summer"
        )
      )
    ) %>% 
      left_join(
        loads_mon %>% 
          mutate(month = month(date), wyear = water_year(date)),
        by = "month"
      ) %>%
      filter(basin %in% station_levels) %>% 
      group_by(dataset, basin, param, wyear, season) %>% 
      summarise(
        n_day = sum(n_day),
        flow_hm3 = sum(flow_hm3),
        load_kg = sum(load_kg),
        conc_ppb = load_kg / flow_hm3,
        .groups = "drop"
      )
    
    x %>% 
      select(-conc_ppb) %>% 
      pivot_longer(c(flow_hm3, load_kg), names_to = "var") %>% 
      pivot_wider(names_from = "basin") %>% 
      mutate(
        `Godowa+Sycan` = Godowa + Sycan,
        `SF_Ivory+NF_Ivory` = SF_Ivory + NF_Ivory,
        `SF+NF` = SF + NF
      ) %>% 
      pivot_longer(-c(dataset, param, wyear, season, var, n_day), names_to = "basin", values_drop_na = TRUE) %>% 
      pivot_wider(names_from = "var") %>% 
      mutate(conc_ppb = load_kg / flow_hm3) %>% 
      pivot_longer(c(flow_hm3, load_kg, conc_ppb), names_to = "var") %>% 
      pivot_wider(names_from = "basin") %>% 
      mutate(
        `Power-Lone_Pine` = Power - Lone_Pine,
        `Lone_Pine-Godowa-Sycan` = Lone_Pine - `Godowa+Sycan`,
        `Godowa-SF-NF` = Godowa - `SF+NF`,
        `Godowa-SF_Ivory-NF_Ivory` = Godowa - `SF_Ivory+NF_Ivory`,
        `SF_Ivory-SF` = SF_Ivory - SF,
        `NF_Ivory-NF` = NF_Ivory - NF,
      ) %>% 
      pivot_longer(-c(dataset, param, wyear, season, var, n_day), names_to = "basin", values_drop_na = TRUE) %>% 
      pivot_wider(names_from = "var")
  }),
  tar_target(loads_basin, {
    x <- loads_wyr %>% 
      filter(basin %in% station_levels) %>% 
      group_by(dataset, season, basin, param) %>% 
      summarise(
        period = glue("{min(wyear)}-{max(wyear)}"),
        n_day = mean(n_day),
        n_wyear = n(),
        flow_hm3 = mean(flow_hm3),
        load_kg = mean(load_kg),
        conc_ppb = load_kg / flow_hm3,
        .groups = "drop"
      )
    x_por_2010 <- loads_wyr %>% 
      filter(basin %in% station_levels) %>% 
      filter(
        dataset == "POR", 
        param != "TSS",
        !str_detect(basin, "Ivory"), 
        wyear >= 2010
      ) %>% 
      group_by(dataset, season, basin, param) %>% 
      summarise(
        period = glue("{min(wyear)}-{max(wyear)}"),
        n_day = mean(n_day),
        n_wyear = n(),
        flow_hm3 = mean(flow_hm3),
        load_kg = mean(load_kg),
        conc_ppb = load_kg / flow_hm3,
        .groups = "drop"
      )
    bind_rows(x, x_por_2010) %>%
      select(-conc_ppb) %>% 
      pivot_longer(c(flow_hm3, load_kg), names_to = "var") %>% 
      pivot_wider(names_from = "basin") %>% 
      mutate(
        `Godowa+Sycan` = Godowa + Sycan,
        `SF_Ivory+NF_Ivory` = SF_Ivory + NF_Ivory,
        `SF+NF` = SF + NF
      ) %>% 
      pivot_longer(-c(dataset, param, period, season, var, n_day, n_wyear), names_to = "basin", values_drop_na = TRUE) %>% 
      pivot_wider(names_from = "var") %>% 
      mutate(conc_ppb = load_kg / flow_hm3) %>% 
      pivot_longer(c(flow_hm3, load_kg, conc_ppb), names_to = "var") %>% 
      pivot_wider(names_from = "basin") %>% 
      mutate(
        `Power-Lone_Pine` = Power - Lone_Pine,
        `Lone_Pine-Godowa-Sycan` = Lone_Pine - `Godowa+Sycan`,
        `Godowa-SF-NF` = Godowa - `SF+NF`,
        `Godowa-SF_Ivory-NF_Ivory` = Godowa - `SF_Ivory+NF_Ivory`,
        `SF_Ivory-SF` = SF_Ivory - SF,
        `NF_Ivory-NF` = NF_Ivory - NF,
      ) %>% 
      pivot_longer(-c(dataset, param, period, season, var, n_day, n_wyear), names_to = "basin", values_drop_na = TRUE) %>% 
      pivot_wider(names_from = "var")
  }),
  tar_target(loads, {
    list(
      day = loads_day,
      mon = loads_mon,
      wyr = loads_wyr,
      basin = loads_basin
    )
  }),
  
  tar_target(loads_diagnostics, {
    x_mon <- loads$mon %>% 
      filter(
        basin %in% station_levels,
        dataset == "POR"
      ) %>% 
      mutate(
        station = factor(basin, levels = station_levels),
        param = factor(param, levels = names(param_labels))
      ) %>% 
      nest_by(station, param, .key = "data_mon")
    x_wyr <- loads$wyr %>% 
      filter(
        basin %in% station_levels,
        dataset == "POR",
        season == "annual"
      ) %>% 
      mutate(
        station = factor(basin, levels = station_levels),
        param = factor(param, levels = names(param_labels))
      ) %>% 
      nest_by(station, param, .key = "data_wyr")
    x_model <- loads_model %>% 
      filter(dataset == "POR") %>% 
      mutate(
        station = factor(station, levels = station_levels),
        param = factor(param, levels = names(param_labels))
      ) %>% 
      nest_by(station, param, .key = "data_model")
    x <- x_mon %>%
      filter(param != "PP") %>% 
      left_join(x_wyr, by = c("station", "param")) %>%
      left_join(x_model, by = c("station", "param")) %>% 
      mutate(
        pred = list(data_model$pred[[1]]),
        model = list(data_model$model[[1]])
      ) %>% 
      arrange(param, station)
    
    x %>% 
      mutate(
        page_ts = list({
          x_day <- pred %>% 
            select(date, conc_ppb, flow_hm3, load_kg)
          x_wyr <- data_wyr %>% 
            select(wyear, conc_ppb, flow_hm3, load_kg)
          
          p_ts <- x_day %>% 
            pivot_longer(-date) %>% 
            mutate(name = fct_inorder(name)) %>% 
            ggplot(aes(date, color = name)) +
            geom_area(aes(y = if_else(name == "conc_ppb", NA_real_, value), fill = name)) +
            geom_line(
              aes(y = if_else(name == "conc_ppb", value, NA_real_), color = name, linetype = "Interpolated"),
              color = "orangered"
            ) +
            geom_line(
              data = pred %>% 
                mutate(name = "conc_ppb"),
              aes(y = pred_conc_ppb, linetype = "Regression Pred."),
              color = "gray50"
            ) +
            geom_point(
              data = pred %>%
                filter(!is.na(obs_conc_ppb)) %>% 
                select(date, conc_ppb = obs_conc_ppb, flow_hm3, load_kg = obs_load_kg) %>% 
                pivot_longer(-date),
              aes(y = value), size = 1
            ) +
            scale_linetype_manual(NULL, values = c("solid", "solid")) +
            scale_fill_manual(NULL, values = c(
              "flow_hm3" = "steelblue2",
              "load_kg" = "olivedrab3",
              "conc_ppb" = "orangered"
            ), guide = "none") +
            scale_color_manual(NULL, values = c(
              "flow_hm3" = "steelblue4",
              "load_kg" = "olivedrab4",
              "conc_ppb" = "orangered"
            ), guide = "none") +
            scale_x_date(expand = expansion(), breaks = min(pred$date) + years(0:30), date_labels = "%m/%Y") +
            scale_y_continuous(expand = expansion(c(0, 0.3)), limits = c(0, NA), labels = scales::comma) +
            guides(
              linetype = guide_legend(nrow = 1, override.aes = list(color = c("orangered", "gray50")))
            ) +
            facet_wrap(vars(name), ncol = 1, scales = "free_y", strip.position = "left", labeller = labeller(
              name = c(
                "conc_ppb" = "Conc (ppb)",
                "flow_hm3" = "Flow (hm3/d)",
                "load_kg" = "Load (kg/d)"
              )
            )) +
            labs(x = "Date", y = NULL, title = "Daily Timeseries", caption = "Points = Biweekly sampling event") +
            theme(
              axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1),
              strip.placement = "outside",
              legend.position = c(1, 1),
              legend.justification = c(1, 1),
              legend.background = element_blank()
            )
          
          is_even <- function (x) {
            as.numeric(x) %% 2 == 0
          }
          p_wyr <- x_wyr %>% 
            pivot_longer(-wyear) %>% 
            mutate(name = fct_inorder(name)) %>% 
            ggplot(aes(factor(wyear), value)) +
            geom_col(aes(fill = name)) +
            scale_x_discrete(
              expand = expansion(mult = 0.01),
              labels = ~ if_else(is_even(.x), .x, "")
            ) +
            scale_y_continuous(labels = scales::comma, expand = expansion(c(0, 0.05))) +
            scale_fill_manual(NULL, values = var_colors, guide = "none") +
            facet_wrap(vars(name), ncol = 1, scales = "free_y", strip.position = "left", labeller = labeller(
              name = c(
                "conc_ppb" = "FWM Conc (ppb)",
                "flow_hm3" = "Flow (hm3/yr)",
                "load_kg" = "Load (kg/yr)"
              )
            )) +
            labs(x = "Water Year", y = NULL, title = "Annual Timeseries") +
            theme(
              axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1),
              strip.placement = "outside",
              legend.position = c(1, 1),
              legend.justification = c(1, 1),
              legend.background = element_blank()
            )
          (p_ts | p_wyr) +
            plot_layout(widths = c(3, 1)) +
            plot_annotation(
              title = "Daily and Annual Flows, Loads, and Concentrations",
              subtitle = glue("Station: {station} | Parameter: {param}"),
              theme = theme(plot.margin = unit(rep(0.5, 4), "in"))
            )
        }),
        page_mon = list({
          x_mon <- data_mon %>% 
            select(date, conc_ppb, flow_hm3, load_kg)
          
          x_mon_mean <- x_mon %>% 
            pivot_longer(-date) %>% 
            group_by(name, date = same_wyear(date, wyr = 2001)) %>% 
            summarise(mean = mean(value), .groups = "drop")
          
          p_ts <- x_mon %>% 
            pivot_longer(-date) %>% 
            mutate(name = fct_inorder(name)) %>% 
            ggplot(aes(date, color = name)) +
            geom_area(aes(y = if_else(name == "conc_ppb", NA_real_, value), fill = name)) +
            geom_line(
              aes(y = if_else(name == "conc_ppb", value, NA_real_), color = name)
            ) +
            scale_linetype_manual(NULL, values = c("solid", "solid")) +
            scale_fill_manual(NULL, values = c(
              "flow_hm3" = "steelblue2",
              "load_kg" = "olivedrab3",
              "conc_ppb" = "orangered"
            ), guide = "none") +
            scale_color_manual(NULL, values = c(
              "flow_hm3" = "steelblue4",
              "load_kg" = "olivedrab4",
              "conc_ppb" = "orangered"
            ), guide = "none") +
            scale_x_date(expand = expansion(), breaks = min(x_mon$date) + years(0:30), date_labels = "%m/%Y") +
            scale_y_continuous(expand = expansion(c(0, 0.05)), limits = c(0, NA), labels = scales::comma) +
            guides(
              linetype = guide_legend(nrow = 1, override.aes = list(color = c("orangered", "gray50")))
            ) +
            facet_wrap(vars(name), ncol = 1, scales = "free_y", strip.position = "left", labeller = labeller(
              name = c(
                "conc_ppb" = "FWM Conc (ppb)",
                "flow_hm3" = "Flow (hm3/mon)",
                "load_kg" = "Load (kg/mon)"
              )
            )) +
            labs(x = "Date", y = NULL, title = "Monthly Timeseries") +
            theme(
              axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1),
              strip.placement = "outside",
              legend.position = c(1, 1),
              legend.justification = c(1, 1),
              legend.background = element_blank()
            )
          
          p_seas <- x_mon %>% 
            pivot_longer(-date) %>% 
            mutate(name = fct_inorder(name)) %>% 
            ggplot(aes(same_wyear(date, wyr = 2001), value, color = name)) +
            geom_line(aes(group = water_year(date)), alpha = 0.5) +
            geom_line(
              data = x_mon_mean,
              aes(date, mean, color = str_c("mean_", name)), inherit.aes = FALSE, size = 1
            ) +
            geom_point(
              data = x_mon_mean,
              aes(date, mean, color = str_c("mean_", name)), inherit.aes = FALSE, size = 3
            ) +
            scale_linetype_manual(NULL, values = c("solid", "solid")) +
            scale_color_manual(NULL, values = c(
              "flow_hm3" = "steelblue3",
              "load_kg" = "olivedrab3",
              "conc_ppb" = "orangered",
              "mean_flow_hm3" = "steelblue4",
              "mean_load_kg" = "olivedrab4",
              "mean_conc_ppb" = "orangered3"
            ), guide = "none") +
            scale_x_date(expand = expansion(), date_breaks = "2 months", date_labels = "%b %d") +
            scale_y_continuous(expand = expansion(c(0, 0.05)), limits = c(0, NA), labels = scales::comma) +
            guides(
              linetype = guide_legend(nrow = 1, override.aes = list(color = c("orangered", "gray50")))
            ) +
            facet_wrap(vars(name), ncol = 1, scales = "free_y", strip.position = "left", labeller = labeller(
              name = c(
                "conc_ppb" = "FWM Conc (ppb)",
                "flow_hm3" = "Flow (hm3/mon)",
                "load_kg" = "Load (kg/mon)"
              )
            )) +
            labs(x = "Day of Water Year", y = NULL, title = "Seasonal Variability", caption = "Points = Mean Values\nLines = Monthly Values of Each Water Year") +
            theme(
              axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1),
              strip.placement = "outside",
              legend.position = c(1, 1),
              legend.justification = c(1, 1),
              legend.background = element_blank()
            )
          (p_ts | p_seas) +
            plot_layout(widths = c(3, 1)) +
            plot_annotation(
              title = "Monthly Flows, Loads, and Concentrations",
              subtitle = glue("Station: {station} | Parameter: {param}"),
              theme = theme(plot.margin = unit(rep(0.5, 4), "in"))
            )
        }),
        page_reg = list({
          x_coef <- broom::tidy(model) %>% 
            transmute(
              term, 
              estimate = sprintf("%g", signif(estimate, 4)),
              p.value = if_else(p.value < 0.0001, "< 0.0001", sprintf("%.4f", p.value))
            ) %>% 
            column_to_rownames("term")
          t_coef_1 <- x_coef[1:6,] %>% 
            gridExtra::tableGrob(
              theme = gridExtra::ttheme_minimal(
                base_size = 8,
                rowhead = list(
                  fg_params = list(fontface = 2L, hjust = 1, x = 0.9),
                  padding = grid::unit(c(3, 3), 'mm')
                ),
                colhead = list(
                  fg_params = list(fontface = 2L, hjust = 1, x = 0.95)
                ),
                core = list(
                  fg_params = list(hjust = 1, x = 0.95),
                  padding = grid::unit(c(3, 3),'mm')
                )
              )
            )
          t_coef_2 <- x_coef[7:11,] %>% 
            gridExtra::tableGrob(
              theme = gridExtra::ttheme_minimal(
                base_size = 8,
                rowhead = list(
                  fg_params = list(fontface = 2L, hjust = 1, x = 0.9),
                  padding = grid::unit(c(3, 3), 'mm')
                ),
                colhead = list(
                  fg_params = list(fontface = 2L, hjust = 1, x = 0.95)
                ),
                core = list(
                  fg_params = list(hjust = 1, x = 0.95),
                  padding = grid::unit(c(3, 3),'mm')
                )
              )
            )
          t_gof <- broom::glance(model) %>% 
            transmute(
              Station = station,
              Parameter = toupper(param),
              `# Samples` = nobs,
              `R^2` = sprintf("%.3f", r.squared),
              `Residual Std. Err.` = sprintf("%.3f", sigma),
              `Residual DOF` = df.residual
            ) %>% 
            t() %>% 
            gridExtra::tableGrob(
              theme = gridExtra::ttheme_minimal(
                base_size = 8,
                rowhead = list(
                  fg_params = list(fontface = 2L, hjust = 1),
                  padding = grid::unit(c(3, 3), 'mm')
                ),
                colhead = list(
                  fg_params = list(fontface = 2L, hjust = 0, x = 0),
                  padding = grid::unit(c(3, 3), 'mm')
                ),
                core = list(
                  fg_params = list(hjust = 0, x = 0.0),
                  padding = grid::unit(c(3, 3),'mm')
                )
              )
            )
          p_resid_ts <- pred %>% 
            ggplot(aes(date, log_c_resid)) +
            geom_hline(yintercept = 0, color = "gray50") +
            geom_point(size = 1, alpha = 0.75) +
            scale_x_date(breaks = min(pred$date) + years(seq(0, 30, by = 4)), date_labels = "%m/%Y", expand = expansion()) +
            labs(
              x = "Date",
              y = "log[Conc. Residual (ppb)]"
            ) +
            theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1))
          p_resid_jday <- pred %>% 
            ggplot(aes(same_wyear(date, wyr = 2001), log_c_resid)) +
            geom_hline(yintercept = 0, color = "gray50") +
            geom_point(size = 1, alpha = 0.75) +
            scale_x_date(breaks = ymd(20001001) + months(seq(0, 12, by = 2)), date_labels = "%b", expand = expansion(mult = c(0.01, 0.01))) +
            labs(x = "Day of Year", y = "log[Conc. Residual (ppb)]")
          p_resid_pred <- pred %>%
            ggplot(aes(pred_conc_ppb, log_c_resid)) +
            geom_hline(yintercept = 0, color = "gray50") +
            geom_point(size = 1, alpha = 0.75) +
            xlim(0, NA) +
            labs(x = "Predicted Conc. (ppb)", y = "log[Conc. Residual (ppb)]")
          p_obs_pred <- pred %>%
            ggplot(aes(pred_conc_ppb, obs_conc_ppb)) +
            geom_abline(linetype = "dashed", alpha = 0.5) +
            geom_point(size = 1, alpha = 0.75) +
            geom_smooth(method = "lm", formula = y ~ x) +
            xlim(0, max(select(pred, pred_conc_ppb, obs_conc_ppb), na.rm = TRUE)) +
            ylim(0, max(select(pred, pred_conc_ppb, obs_conc_ppb), na.rm = TRUE)) +
            labs(x = "Predicted Conc. (ppb)", y = "Observed Conc. (ppb)")
          p_conc_flow <- pred %>%
            select(date, flow_hm3, pred_conc_ppb, obs_conc_ppb) %>% 
            filter(!is.na(obs_conc_ppb)) %>% 
            ggplot(aes(flow_hm3)) +
            geom_point(aes(y = obs_conc_ppb, color = "obs"), size = 1, alpha = 0.5) +
            geom_point(aes(y = pred_conc_ppb, color = "pred"), size = 1, alpha = 0.5) +
            scale_x_log10_(magnitudes = c(0.01, 0.1, 1, 10, 100, 1000)) +
            scale_color_brewer(NULL, palette = "Set1", labels = c("obs" = "Observed", "pred" = "Predicted")) +
            labs(x = "Flow (hm3/d)", y = "Conc. (ppb)")
          p_conc_jday <- pred %>%
            select(date, flow_hm3, pred_conc_ppb, obs_conc_ppb) %>% 
            filter(!is.na(obs_conc_ppb)) %>% 
            ggplot(aes(same_wyear(date, wyr = 2001), value)) +
            geom_point(aes(y = obs_conc_ppb, color = "obs"), size = 1, alpha = 0.5) +
            geom_point(aes(y = pred_conc_ppb, color = "pred"), size = 1, alpha = 0.5) +
            scale_color_brewer(NULL, palette = "Set1", labels = c("obs" = "Observed", "pred" = "Predicted")) +
            scale_x_date(breaks = ymd(20001001) + months(seq(0, 12, by = 2)), date_labels = "%b", expand = expansion(mult = c(0.01, 0.01))) +
            labs(x = "Day of Year", y = "Conc. (ppb)")
          
          p_plots <- ((p_obs_pred | p_conc_flow | p_conc_jday) / (p_resid_ts | p_resid_pred | p_resid_jday)) +
            plot_layout(guides = "collect")
          p_tbls <- wrap_plots(t_gof, t_coef_1, t_coef_2)
          (p_plots / p_tbls) +
            plot_annotation(
              title = "Regression Model Diagnostics",
              subtitle = glue("Station: {station} | Parameter: {param}"),
              theme = theme(plot.margin = unit(rep(0.5, 4), "in"))
            )
        })
      )
  }),
  
  tar_target(loads_maps_subbasins, {
    x_basin_long <- loads_basin %>% 
      filter(
        season == "annual",
        basin %in% station_levels
      ) %>% 
      left_join(
        st_drop_geometry(gis_basins),
        by = "basin"
      ) %>% 
      mutate(
        runoff_cmyr = change_units(flow_hm3 / area_km2, "hm3 / km2", "cm"),
        export_kgkm2yr = load_kg / area_km2
      ) %>% 
      filter(
        !(dataset == "POR" & period == "2010-2020"),
        !(dataset == "POR" & param == "TSS")
      ) %>% 
      select(dataset, basin, param, runoff_cmyr, export_kgkm2yr, conc_ppb)
    x_basin_flow <- x_basin_long %>% 
      filter(param == "TP") %>% 
      distinct(dataset, basin, runoff_cmyr) %>% 
      pivot_longer(-c(dataset, basin), names_to = "name") %>% 
      mutate(param = "FLOW", param_name = name)
    x_basin_wq <- x_basin_long %>% 
      select(-runoff_cmyr) %>% 
      pivot_longer(-c(dataset, basin, param)) %>% 
      unite(param_name, c("param", "name"), remove = FALSE)
    x_basin <- bind_rows(x_basin_flow, x_basin_wq) %>% 
      mutate(
        basin = factor(basin, levels = station_levels),
        param_name = fct_drop(factor(param_name, levels = names(param_var_labels)))
      )
    
    x_wyr_long <- loads_wyr %>% 
      filter(
        season == "annual",
        basin %in% station_levels
      ) %>% 
      left_join(
        st_drop_geometry(gis_basins),
        by = "basin"
      ) %>% 
      mutate(
        runoff_cmyr = change_units(flow_hm3 / area_km2, "hm3 / km2", "cm"),
        export_kgkm2yr = load_kg / area_km2
      ) %>% 
      filter(
        !(dataset == "POR" & param == "TSS"),
        !(dataset == "POR" & str_detect(basin, "_Ivory"))
      ) %>% 
      select(dataset, basin, param, wyear, runoff_cmyr, export_kgkm2yr, conc_ppb)
      
    x_wyr_flow <- x_wyr_long %>%
      filter(param == "TP") %>% 
      distinct(dataset, basin, wyear, runoff_cmyr) %>% 
      pivot_longer(-c(dataset, basin, wyear), names_to = "name") %>% 
      mutate(param = "FLOW", param_name = name)
    x_wyr_wq <- x_wyr_long %>% 
      select(-runoff_cmyr) %>% 
      pivot_longer(-c(dataset, basin, param, wyear)) %>% 
      unite(param_name, c("param", "name"), remove = FALSE)
    x_wyr <- bind_rows(x_wyr_flow, x_wyr_wq) %>% 
      mutate(
        basin = factor(basin, levels = station_levels),
        param_name = fct_drop(factor(param_name, levels = names(param_var_labels)))
      )
    
    x_sf <- gis_basins %>% 
      st_simplify(dTolerance = 100) %>% 
      filter(basin %in% station_levels) %>% 
      mutate(basin = factor(basin, levels = station_levels))
    
    x_stations <- gis_stations %>%
      filter(station %in% unique(x_basin$basin)) %>%
      rename(basin = station)
    
    var_colors <- c(
      "runoff_cmyr" = RColorBrewer::brewer.pal(3, "Blues")[3],
      "export_kgkm2yr" = RColorBrewer::brewer.pal(3, "Greens")[3],
      "conc_ppb" = RColorBrewer::brewer.pal(3, "Reds")[3]
    )
    
    x_basin %>% 
      nest_by(dataset, param, name, param_name, .key = "data_basin") %>% 
      left_join(
        x_wyr %>% 
          nest_by(dataset, param, name, param_name, .key = "data_wyr"),
        by = c("dataset", "param", "name", "param_name")
      ) %>% 
      arrange(param_name) %>% 
      mutate(
        max_value = max(data_wyr$value),
        plot_map = list({
          gis_basin %>% 
            ggplot() +
            geom_sf(
              fill = NA, color = "gray50", alpha = 0.5
            ) +
            geom_sf(
              data = inner_join(x_sf, data_basin, by = "basin") %>% 
                rename(ignore = basin),
              fill = NA, alpha = 0.25, size = 0.5
            ) +
            geom_sf(
              data = inner_join(x_sf, data_basin, by = "basin"),
              aes(fill = value)
            ) +
            geom_sf(
              data = filter(x_stations, basin %in% data_basin$basin),
              fill = "deepskyblue", size = 2, shape = 21
            ) +
            facet_wrap(vars(basin), nrow = 2) +
            scale_fill_gradient2(
              low = "#FFFFFF",
              high = var_colors[[name]],
              na.value = NA,
              limits = c(0, NA)
            ) +
            labs(
              x = NULL, 
              y = NULL,
              fill = str_replace(param_var_labels[[as.character(param_name)]], " \\(", "\n\\(")
            ) +
            theme(
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              panel.grid = element_blank()
            )
        }),
        plot_bar = list({
          data_basin %>% 
            ggplot(aes(value, fct_rev(basin))) +
            geom_col(aes(fill = value), color = "gray50") +
            scale_x_continuous(expand = expansion(c(0, 0.05))) +
            scale_fill_gradient2(
              low = "#FFFFFF",
              high = var_colors[[name]],
              na.value = NA,
              limits = c(0, NA)
            ) +
            labs(
              x = param_var_labels[[as.character(param_name)]], 
              y = NULL, 
              fill = str_replace(param_var_labels[[as.character(param_name)]], " \\(", "\n\\("),
              subtitle = "Long-term Mean"
            )
        }),
        plot_tile = list({
          data_wyr %>% 
            ggplot(aes(wyear, fct_rev(basin))) +
            geom_tile(aes(fill = value)) +
            scale_x_continuous(breaks = scales::pretty_breaks(), expand = expansion()) +
            scale_y_discrete(expand = expansion()) +
            scale_fill_gradient2(
              low = "#FFFFFF",
              high = var_colors[[name]],
              na.value = NA,
              limits = c(0, NA)
            ) +
            labs(
              x = "Water Year",
              y = NULL,
              fill = str_replace(param_var_labels[[as.character(param_name)]], " \\(", "\n\\("),
              subtitle = "Annual Values"
            )
        }),
        plot = list({
          plot_map / (plot_bar | plot_tile) +
            plot_layout(guides = "keep", heights = c(2, 1))
        })
      )
  }),
  tar_target(loads_maps_incbasins, {
    x_basin_long <- loads_basin %>% 
      filter(
        season == "annual",
        basin %in% names(incbasin_labels),
        !(dataset == "POR" & str_detect(basin, "_Ivory")),
        !(dataset == "RECENT" & basin == c("Godowa-SF-NF"))
      ) %>% 
      left_join(
        st_drop_geometry(gis_basins),
        by = "basin"
      ) %>% 
      mutate(
        runoff_cmyr = change_units(flow_hm3 / area_km2, "hm3 / km2", "cm"),
        export_kgkm2yr = load_kg / area_km2
      ) %>% 
      filter(
        !(dataset == "POR" & period == "2010-2020"),
        !(dataset == "POR" & param == "TSS")
      ) %>% 
      select(dataset, basin, param, runoff_cmyr, export_kgkm2yr, conc_ppb)
    x_basin_flow <- x_basin_long %>%
      filter(param == "TP") %>% 
      distinct(dataset, basin, runoff_cmyr) %>% 
      pivot_longer(-c(dataset, basin), names_to = "name") %>% 
      mutate(param = "FLOW", param_name = name)
    x_basin_wq <- x_basin_long %>% 
      select(-runoff_cmyr) %>% 
      pivot_longer(-c(dataset, basin, param)) %>% 
      unite(param_name, c("param", "name"), remove = FALSE)
    x_basin <- bind_rows(x_basin_flow, x_basin_wq) %>% 
      mutate(
        basin = factor(basin, levels = names(incbasin_labels)),
        param_name = fct_drop(factor(param_name, levels = names(param_var_labels)))
      )
    
    x_wyr_long <- loads_wyr %>% 
      filter(
        season == "annual",
        basin %in% names(incbasin_labels),
        !(dataset == "POR" & str_detect(basin, "_Ivory")),
        !(dataset == "RECENT" & basin == c("Godowa-SF-NF"))
      ) %>% 
      left_join(
        st_drop_geometry(gis_basins),
        by = "basin"
      ) %>% 
      mutate(
        runoff_cmyr = change_units(flow_hm3 / area_km2, "hm3 / km2", "cm"),
        export_kgkm2yr = load_kg / area_km2
      ) %>% 
      filter(
        !(dataset == "POR" & param == "TSS"),
        !(dataset == "POR" & str_detect(basin, "_Ivory"))
      ) %>% 
      select(dataset, basin, param, wyear, runoff_cmyr, export_kgkm2yr, conc_ppb)
    x_wyr_flow <- x_wyr_long %>%
      filter(param == "TP") %>% 
      distinct(dataset, basin, wyear, runoff_cmyr) %>% 
      pivot_longer(-c(dataset, basin, wyear), names_to = "name") %>% 
      mutate(param = "FLOW", param_name = name)
    x_wyr_wq <- x_wyr_long %>% 
      select(-runoff_cmyr) %>% 
      pivot_longer(-c(dataset, basin, param, wyear)) %>% 
      unite(param_name, c("param", "name"), remove = FALSE)
    x_wyr <- bind_rows(x_wyr_flow, x_wyr_wq) %>% 
      mutate(
        basin = factor(basin, levels = names(incbasin_labels)),
        param_name = fct_drop(factor(param_name, levels = names(param_var_labels)))
      )
    
    x_sf <- gis_basins %>% 
      st_simplify(dTolerance = 100) %>% 
      right_join(distinct(x_basin, dataset, basin), by = c("basin")) %>% 
      mutate(basin = factor(basin, levels = names(incbasin_labels)))
    
    x_stations <- gis_stations %>% 
      left_join(
        st_drop_geometry(x_sf) %>% 
        distinct(basin) %>% 
        mutate(
          station = map_chr(basin, ~ str_split(.x, "-")[[1]][1])
        ),
        by = "station"
      )
    
    var_colors <- c(
      "runoff_cmyr" = RColorBrewer::brewer.pal(3, "Blues")[3],
      "export_kgkm2yr" = RColorBrewer::brewer.pal(3, "Greens")[3],
      "conc_ppb" = RColorBrewer::brewer.pal(3, "Reds")[3]
    )
    
    x_basin %>% 
      nest_by(dataset, param, name, param_name, .key = "data_basin") %>% 
      left_join(
        x_wyr %>% 
          nest_by(dataset, param, name, param_name, .key = "data_wyr"), 
        by = c("dataset", "param", "name", "param_name")
      ) %>% 
      arrange(dataset, param_name) %>% 
      mutate(
        data_basin = list({
          data_basin %>% 
            mutate(
              value = if_else(
                name == "conc_ppb" & !str_detect(basin, "-"),
                NA_real_,
                value
              )
            )
        }),
        data_wyr = list({
          data_wyr %>% 
            mutate(
              value = if_else(
                name == "conc_ppb" & !str_detect(basin, "-"),
                NA_real_,
                value
              )
            )
        }),
        plot_map = list({
          gis_basin %>% 
            ggplot() +
            geom_sf(
              fill = NA, color = "gray50", alpha = 0.5
            ) +
            geom_sf(
              data = inner_join(x_sf, data_basin, by = "basin"),
              aes(fill = value)
            ) +
            geom_sf(
              data = filter(x_stations, basin %in% data_basin$basin),
              fill = "deepskyblue", size = 2, shape = 21
            ) +
            geom_sf_text(
              data = gis_basins %>% 
                st_centroid() %>% 
                filter(basin %in% unique(data_basin$basin)) %>% 
                mutate(basin_label = map_chr(basin, ~ str_wrap(incbasin_labels[[.x]], width = 6))),
              aes(label = basin_label), hjust = 0.5, vjust = 0.5, size = 3
            ) +
            scale_fill_gradient2(
              low = "#999999",
              mid = "#FFFFFF",
              high = var_colors[[name]],
              na.value = NA,
              limits = c(min(min(data_basin$value), 0), NA)
            ) +
            labs(
              x = NULL, y = NULL,
              fill = str_replace(str_c("Net Change\n", param_var_labels[[as.character(param_name)]]), " \\(", "\n\\(")
            ) +
            theme(
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              panel.grid = element_blank()
            )
        }),
        plot_bar = list({
          data_basin %>%
            filter(!is.na(value)) %>% 
            ggplot(aes(value, fct_rev(basin))) +
            geom_vline(xintercept = 0, alpha = 0.5) +
            geom_col(aes(fill = value), color = "gray50") +
            scale_x_continuous(expand = expansion(c(0.05, 0.05))) +
            scale_y_discrete(labels = incbasin_labels) +
            scale_fill_gradient2(
              low = "#999999",
              mid = "#FFFFFF",
              high = var_colors[[name]],
              na.value = NA,
              limits = c(min(min(data_basin$value), 0), NA)
            ) +
            labs(
              x = param_var_labels[[as.character(param_name)]], 
              y = NULL, 
              subtitle = "Long-term Mean",
              fill = str_replace(str_c("Net Change\n", param_var_labels[[as.character(param_name)]]), " \\(", "\n\\(")
            )
        }),
        plot_tile = list({
          max_data_wyr_value <- max(data_wyr$value, na.rm = TRUE)
          data_wyr %>%
            filter(!is.na(value)) %>% 
            # mutate(value = pmin(max_value, value)) %>% 
            ggplot(aes(wyear, fct_rev(basin))) +
            geom_tile(aes(fill = value)) +
            geom_tile(aes(fill = value)) +
            scale_x_continuous(breaks = scales::pretty_breaks(), expand = expansion()) +
            scale_y_discrete(expand = expansion(), labels = incbasin_labels) +
            scale_fill_gradient2(
              low = "#999999",
              mid = "#FFFFFF",
              high = var_colors[[name]],
              na.value = NA,
              limits = c(min(min(data_wyr$value), 0), NA)
            ) +
            labs(
              x = "Water Year",
              y = NULL,
              fill = str_replace(str_c("Net Change\n", param_var_labels[[as.character(param_name)]]), " \\(", "\n\\("),
              subtitle = "Annual Values",
              caption = "Note: gray tiles indicate negative values"
            )
        }),
        plot = list({
          plot_map / (plot_bar | plot_tile) +
            plot_layout(heights = c(2, 1), guides = "keep")
        })
      )
  }),
  
  tar_target(loads_dataset_nitrogen_limits, {
    kt_wq_detection_limits %>% 
      select(param, POR = dl_upper, RECENT = dl_lower) %>% 
      mutate(across(-param, ~ .x * 1000)) %>% 
      pivot_longer(-param, names_to = "dataset") %>% 
      filter(param %in% c("TN", "NH4", "NO23")) %>% 
      mutate(
        param = factor(param, levels = c("TN", "NH4", "NO23"))
      )
  }),
  tar_target(loads_dataset_nitrogen_basin, {
    loads_basin %>% 
      filter(
        basin %in% station_levels,
        param %in% c("TN", "NH4", "NO23"),
        season == "annual",
        period == "2010-2020"
      ) %>% 
      mutate(
        param = factor(param, levels = c("TN", "NH4", "NO23"))
      ) %>% 
      ggplot(aes(basin, conc_ppb)) +
      geom_col(aes(fill = dataset), position = "dodge") +
      geom_hline(
        data = loads_dataset_nitrogen_limits,
        aes(yintercept = value, linetype = dataset)
      ) +
      scale_fill_brewer("Results", palette = "Set1", labels = c(
        "POR" = "Higher Limit",
        "RECENT" = "Lower Limit"
      )) +
      scale_linetype_discrete("Detection Limit ", labels = c(
        "POR" = "Higher Limit",
        "RECENT" = "Lower Limit"
      )) +
      scale_y_continuous(expand = expansion(c(0, 0.05))) +
      facet_wrap(vars(param), scales = "free_y") +
      labs(
        x = "Station",
        y = "FWM Concentration (ppb)"
      ) +
      theme(
        aspect.ratio = 1,
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
      )
  }),
  tar_target(loads_dataset_nitrogen_wyr, {
    loads_wyr %>% 
      filter(
        basin %in% station_levels,
        param %in% c("TN", "NH4", "NO23"),
        season == "annual",
        wyear >= 2010
      ) %>% 
      mutate(
        param = factor(param, levels = c("TN", "NH4", "NO23"))
      ) %>% 
      ggplot(aes(wyear, conc_ppb)) +
      geom_line(aes(color = dataset)) +
      geom_point(aes(color = dataset), size = 1) +
      geom_hline(
        data = loads_dataset_nitrogen_limits,
        aes(yintercept = value, linetype = dataset)
      ) +
      scale_color_brewer("Results", palette = "Set1", labels = c(
        "POR" = "Higher Limit",
        "RECENT" = "Lower Limit"
      )) +
      scale_linetype_discrete("Detection Limit ", labels = c(
        "POR" = "Higher Limit",
        "RECENT" = "Lower Limit"
      )) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 3)) +
      scale_y_continuous(expand = expansion(c(0, 0.05)), limits = c(0, NA)) +
      facet_grid(vars(param), vars(basin), scales = "free_y") +
      guides(
        color = guide_legend(),
        linetype = guide_legend()
      ) +
      labs(
        x = "Water Year",
        y = "FWM Concentration (ppb)"
      ) +
      theme(
        aspect.ratio = 0.75,
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
      )
  })
)