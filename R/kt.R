tar_option_set(packages = c("tidyverse", "lubridate", "here", "janitor", "glue", "units", "patchwork"))

targets_kt <- list(
  tar_target(kt_wq_detection_limits_file, here("data", "kt", "detection-limits.csv"), format = "file"),
  tar_target(kt_wq_detection_limits, read_csv(kt_wq_detection_limits_file, show_col_types = FALSE)),
  tar_target(kt_wq_outliers_file, here("data", "kt", "outliers.csv"), format = "file"),
  tar_target(kt_wq_outliers, read_csv(kt_wq_outliers_file, show_col_types = FALSE)),
  
  tar_target(kt_stations_file, here("data", "kt", "kt_stations.csv"), format = "file"),
  tar_target(kt_stations, {
    read_csv(kt_stations_file, show_col_types = FALSE) %>% 
      mutate(station = fct_inorder(station))
  }),
  
  tar_target(kt_raw_2001_2014_file, here("data", "kt", "Sprague River--Water Quality Dataset 2001_2014_revised_20150127.csv"), format = "file"),
  tar_target(kt_raw_2001_2014, {
    read_csv(kt_raw_2001_2014_file, col_types = cols(
      .default = col_character()
    )) %>% 
      select(
        date = DATE, site_code = SITE,
        flow_cfs = Q_CFS,
        TEMP_degC = TEMP_C,
        SPCOND_uScm = COND_US_CM,
        DO_ppm = DO_mgl,
        PSAT_pct = PSAT_perc,
        PH_su = PH,
        TP_ppm_value = TP_MGL,
        TP_ppm_dup = TP_DUP,
        PO4_ppm_value = PO4_MGL,
        PO4_ppm_dup = PO4_DUP,
        TN_ppm_value = TN_MGL,
        TN_ppm_dup = TN_DUP,
        NH4_ppm_value = NH4_MGL,
        NH4_ppm_dup = NH4_DUP,
        NO23_ppm_value = NO3NO2_MGL,
        NO23_ppm_dup = NO3NO2_DUP,
        TSS_ppm_value = TSS_MGL,
        TSS_ppm_dup = TSS_DUP
      ) %>% 
      mutate(date = mdy(date))
  }),
  tar_target(kt_raw_2014_2020_file, here("data", "kt", "SR_2014_2020.csv"), format = "file"),
  tar_target(kt_raw_2014_2020, {
    read_csv(kt_raw_2014_2020_file, col_types = cols(
      .default = col_character()
    )) %>% 
      select(
        date = DATE, site_code = SITE,
        flow_cfs = FLOW_cfs,
        TEMP_degC = TEMP_degC,
        SPCOND_uScm = COND_uScm,
        DO_ppm = DO_ppm,
        PSAT_pct = PSAT_pct,
        PH_su = PH_su,
        TURBIDITY_NTU = TURBIDITY_NTU,
        TP_ppm_value = TP_ppm,
        TP_ppm_dup = TP_DUP,
        PO4_ppm_value = PO4_ppm,
        PO4_ppm_dup = PO4_DUP,
        TN_ppm_value = TN_ppm,
        TN_ppm_dup = TN_DUP,
        NH4_ppm_value = NH4_ppm,
        NH4_ppm_dup = NH4_DUP,
        NO23_ppm_value = NO23_ppm,
        NO23_ppm_dup = NO23_DUP,
        TSS_ppm_value = TSS_ppm,
        TSS_ppm_dup = TSS_DUP
      ) %>% 
      filter(!is.na(date)) %>% 
      mutate(date = ymd(date))
  }),
  tar_target(kt_raw, {
    bind_rows(kt_raw_2001_2014, kt_raw_2014_2020) %>% 
      filter(
        !(site_code == "SR0140" & year(date) <= 2008)
      ) %>% 
      inner_join(
        select(kt_stations, station, site_code),
        by = c("site_code")
      ) %>% 
      select(-site_code)
  }),
  
  tar_target(kt_flow, {
    x <- kt_raw %>% 
      filter(!is.na(flow_cfs)) %>% 
      transmute(date, station, flow_cfs = parse_number(flow_cfs))
    stopifnot(
      all(count(x, date, station)$n == 1),
      all(x$flow_cfs > 0)
    )
    x
  }),
  tar_target(kt_flow_plot, {
    kt_flow %>% 
      ggplot(aes(date, flow_cfs)) +
      geom_point(size = 1) +
      facet_wrap(vars(station), scales = "free_y")
  }),
  tar_target(kt_flow_export, {
    filename <- "export/flows-kt-obs.csv"
    kt_flow %>% 
      arrange(station, date, flow_cfs) %>% 
      relocate(station) %>% 
      write_csv(filename)
    filename
  }, format = "file"),
  
  tar_target(kt_field_raw, {
    x <- kt_raw %>% 
      select(date, station, TEMP_degC, SPCOND_uScm, DO_ppm, PSAT_pct, PH_su, TURBIDITY_NTU) %>% 
      pivot_longer(-c(date, station), names_to = c("param", "units"), names_sep = "_", values_drop_na = TRUE) %>% 
      mutate(value = parse_number(value))
    stopifnot(
      all(!is.na(x$value)),
      all(count(x, date, station, param)$n == 1)
    )
    x
  }),
  tar_target(kt_field_raw_plot, {
    kt_field_raw %>% 
      ggplot(aes(date, value)) +
      geom_point(size = 1) +
      facet_grid(vars(param), vars(station), scales = "free_y")
  }),
  
  tar_target(kt_wq_raw, {
    x <- kt_raw %>% 
      select(date, station, starts_with(c("TP_", "PO4_", "TN_", "NH4_", "NO23_", "TSS_"))) %>% 
      pivot_longer(-c(date, station), names_to = c("param", "units", "type"), names_sep = "_", values_drop_na = TRUE) %>% 
      pivot_wider(names_from = "type") %>% 
      mutate(
        across(c(value, dup), parse_number),
        param = fct_inorder(param)
      )
    stopifnot(
      all(!is.na(x$value)),
      all(count(x, date, station, param)$n == 1)
    )
    x
  }),
  tar_target(kt_wq_raw_plot, {
    kt_wq_raw %>% 
      ggplot(aes(date, value)) +
      geom_point(size = 1) +
      facet_grid(vars(param), vars(station), scales = "free_y")
  }),
  
  tar_target(kt_wq_rpd, {
    kt_wq_raw %>% 
      filter(!is.na(dup)) %>% 
      left_join(kt_wq_detection_limits, by = "param") %>% 
      mutate(
        dl = if_else(
          date < ymd(20080401),
          dl_upper,
          dl_lower
        ),
        abs_diff = abs(value - dup),
        rpd = abs_diff / ((value + dup) / 2),
        rpd_type = if_else(
          value > 5 * dl & dup > 5 * dl,
          "HIGH",
          "LOW"
        ),
        rpd_pass = if_else(
          rpd_type == "HIGH",
          rpd <= 0.2,
          abs_diff <= dl
        )
      )
  }),
  tar_target(kt_wq_rpd_plot_ts, {
    kt_wq_rpd %>% 
      ggplot(aes(date, value)) +
      geom_line(aes(y = dl / 2)) +
      geom_point(size = 1, alpha = 0.5, color = "gray50") +
      geom_point(aes(y = if_else(!rpd_pass, value, NA_real_), color = "RPD Fail"), size = 1) +
      scale_y_log10() +
      scale_color_manual(NULL, values = c("orangered")) +
      facet_wrap(vars(param), scales = "free", ncol = 2) +
      theme_bw()
  }),
  tar_target(kt_wq_rpd_plot_splot, {
    kt_wq_rpd %>% 
      ggplot(aes(value, dup)) +
      geom_abline(linetype = "dashed") +
      geom_point(size = 1, alpha = 0.5, color = "gray50") +
      geom_point(aes(y = if_else(!rpd_pass, dup, NA_real_), color = "RPD Fail"), size = 1) +
      scale_x_log10() +
      scale_y_log10() +
      scale_color_manual(NULL, values = c("orangered")) +
      facet_wrap(vars(param), scales = "free", ncol = 2) +
      theme_bw() +
      theme(aspect.ratio = 1)
  }),
  
  tar_target(kt_wq_qaqc, {
    kt_wq_raw %>%
      select(-dup) %>% 
      left_join(
        select(kt_wq_rpd, date, station, param, rpd_pass),
        by = c("date", "station", "param")
      ) %>% 
      left_join(
        mutate(kt_wq_outliers, outlier = TRUE),
        by = c("date", "station", "param")
      ) %>% 
      mutate(
        outlier = coalesce(outlier, FALSE),
        qaqc = case_when(
          outlier ~ "OUTLIER",
          station == "SF" & date == ymd(20080716) ~ "BLANK",
          param == "TP" & value <= 0.009 ~ "LOW",
          param == "TN" & value <= 0.02 ~ "LOW",
          param == "NH4" & value <= 0.001 ~ "LOW",
          param == "TSS" & value < 0 ~ "NEGATIVE",
          !rpd_pass ~ "RPD",
          TRUE ~ "PASS"
        ),
        qaqc = factor(qaqc, levels = c("PASS", "RPD", "BLANK", "LOW", "NEGATIVE", "OUTLIER"))
      ) %>% 
      left_join(kt_wq_detection_limits, by = "param") %>% 
      mutate(
        dl = if_else(
          date < ymd(20080401),
          dl_upper,
          dl_lower
        ),
        station = factor(station, levels = levels(kt_wq_raw$station)),
        param = factor(param, levels = levels(kt_wq_raw$param))
      )
  }),
  tar_target(kt_wq_qaqc_plot, {
    x <- kt_wq_qaqc %>% 
      mutate(
        qaqc = factor(qaqc, levels = c(
          "PASS",
          "RPD",
          "BLANK",
          "LOW",
          "OUTLIER"
        ))
      )
    x %>% 
      filter(qaqc == "PASS") %>%
      ggplot(aes(date, value)) +
      geom_line(aes(y = dl / 2, linetype = "Detection\nLimit")) +
      geom_point(aes(color = qaqc), size = 1, alpha = 0.5) +
      geom_point(
        data = filter(x, qaqc == "RPD"),
        aes(color = qaqc), size = 1
      ) +
      geom_point(
        data = filter(x, !qaqc %in% c("PASS", "RPD")),
        aes(color = qaqc),
        size = 2
      ) +
      scale_color_manual(
        "QAQC",
        values = c(
          "PASS" = "gray50",
          "RPD" = "black",
          "BLANK" = "deepskyblue",
          "LOW" = "chartreuse3",
          "OUTLIER" = "orangered"
        )
      ) +
      scale_y_log10_(breaks = c(1, 2, 5), magnitudes = c(0.001, 0.01, 0.1, 1, 10, 100)) +
      facet_grid(vars(param), vars(station), scales = "free_y") +
      labs(
        x = "Date",
        y = "Concentration (ppm)",
        linetype = NULL
      ) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold")
      )
  }),
  
  tar_target(kt_wq_removed, {
    kt_wq_qaqc %>% 
      filter(!(qaqc %in% c("PASS", "RPD"))) %>% 
      count(station, param) %>% 
      pivot_wider(names_from = "param", values_from = "n", values_fill = 0) %>% 
      select(station, TP, PO4, TN, NH4, NO23, TSS) %>%
      adorn_totals(name = "Total Removed") %>%
      bind_rows(
        kt_wq_qaqc %>%
          filter(!is.na(value)) %>%
          count(param) %>%
          pivot_wider(names_from = "param", values_from = "n", values_fill = 0) %>% 
          mutate(station = "Total Collected") %>%
          select(station, TP, PO4, TN, NH4, NO23, TSS)
      )
  }),
  tar_target(kt_wq_removed_csv, {
    filename <- "report/csv/kt-wq-removed.csv"
    kt_wq_removed %>% 
      write_csv(filename)
    filename
  }, format = "file"),
  
  tar_target(kt_wq_por, {
    kt_wq_qaqc %>% 
      filter(qaqc %in% c("PASS", "RPD")) %>% 
      mutate(
        value = pmax(value, dl_upper)
      ) %>% 
      select(date, station, param, value, dl = dl_upper)
  }),
  tar_target(kt_wq_por_plot, {
    kt_wq_por %>% 
      ggplot(aes(date, value)) +
      geom_line(aes(y = dl)) +
      geom_point(size = 1, color = "gray50") +
      scale_y_log10() +
      scale_color_brewer(palette = "Set1") +
      facet_grid(vars(param), vars(station), scales = "free_y") +
      ggtitle("dataset: POR") +
      theme_bw()
  }),
  
  tar_target(kt_wq_recent, {
    kt_wq_qaqc %>% 
      filter(
        qaqc %in% c("PASS", "RPD"),
        water_year(date) >= 2010
      ) %>% 
      mutate(
        value = pmax(value, dl_lower)
      ) %>% 
      select(date, station, param, value, dl = dl_lower)
  }),
  tar_target(kt_wq_recent_plot, {
    kt_wq_recent %>% 
      ggplot(aes(date, value)) +
      geom_line(aes(y = dl)) +
      geom_point(size = 1, color = "gray50") +
      scale_y_log10() +
      scale_color_brewer(palette = "Set1") +
      facet_grid(vars(param), vars(station), scales = "free_y") +
      ggtitle("dataset: RECENT") +
      theme_bw()
  }),

  tar_target(kt_wq, {
    bind_rows(
      POR = kt_wq_por,
      RECENT = kt_wq_recent,
      .id = "dataset"
    )
  }),
  tar_target(kt_wq_plot, {
    kt_wq %>% 
      ggplot(aes(date, value, color = dataset)) +
      geom_line(aes(y = dl)) +
      geom_point(size = 1, alpha = 0.5) +
      scale_y_log10() +
      scale_color_brewer(palette = "Set1") +
      facet_grid(vars(param), vars(station), scales = "free_y") +
      ggtitle("dataset: RECENT") +
      theme_bw()
  }),
  
  tar_target(kt_synoptic_stations_file, here("data", "kt", "synoptic-stations.csv"), format = "file"),
  tar_target(kt_synoptic_stations, {
    read_csv(kt_synoptic_stations_file, show_col_types = FALSE) %>% 
      rename(station = site_description)
  }),
  
  tar_target(kt_synoptic_files, {
    c(
      here("data", "kt", "Sprague River--Water Quality Dataset 2001_2013_imp_synoptic_20141008.csv"),
      here("data", "kt", "Sprague River--Water Quality Dataset 2001_2013_imp_springs_20141008.csv")
    )
  }, format = "file"),
  tar_target(kt_synoptic_raw, {
    read_csv(kt_synoptic_files, col_types = cols(.default = col_character())) %>% 
      select(
        date = DATE,
        time = TIME,
        site_code = SITE,
        flow_cfs = Q_CFS,
        TEMP_degC = TEMP_C,
        SPCOND_uScm = COND_US_CM,
        DO_ppm = DO_MGL,
        PSAT_pct = PSAT_PERC,
        PH_su = PH,
        TP_ppm = TP_MGL,
        PO4_ppm = PO4_MGL,
        TN_ppm = TN_MGL,
        NH4_ppm = NH4_MGL,
        NO23_ppm = NO3NO2_MGL,
        TSS_ppm = TSS_MGL
      ) %>% 
      mutate(date = mdy(date)) %>% 
      left_join(select(kt_synoptic_stations, station, site_code), by = "site_code")
  }),
  tar_target(kt_synoptic_flow, {
    kt_synoptic_raw %>% 
      select(date, time, station, site_code, flow_cfs) %>% 
      filter(!is.na(flow_cfs), !is.na(station)) %>% 
      mutate(flow_cfs = parse_number(flow_cfs))
  }),
  tar_target(kt_synoptic_field, {
    x <- kt_synoptic_raw %>% 
      filter(!is.na(station)) %>% 
      select(date, time, site_code, station, TEMP_degC, SPCOND_uScm, DO_ppm, PSAT_pct, PH_su) %>% 
      pivot_longer(-c(date, time, site_code, station), names_to = c("param", "units"), names_sep = "_", values_drop_na = TRUE) %>% 
      mutate(value = parse_number(value))
    stopifnot(
      all(!is.na(x$value)),
      all(count(x, date, time, site_code, param)$n == 1)
    )
    x
  }),
  tar_target(kt_synoptic_wq, {
    x <- kt_synoptic_raw %>% 
      filter(!is.na(station)) %>% 
      select(date, time, site_code, station, TP_ppm, PO4_ppm, TN_ppm, NH4_ppm, NO23_ppm, TSS_ppm) %>% 
      pivot_longer(-c(date, time, site_code, station), names_to = c("param", "units"), names_sep = "_", values_drop_na = TRUE) %>% 
      mutate(value = parse_number(value))
    stopifnot(
      all(!is.na(x$value)),
      all(count(x, date, time, site_code, param)$n == 1)
    )
    x
  }),
  tar_target(kt_synoptic_wq_plot, {
    kt_synoptic_wq %>% 
      ggplot(aes(date, value)) +
      geom_point(size = 1) +
      facet_grid(vars(param), vars(station), scales = "free_y")
  })
)
