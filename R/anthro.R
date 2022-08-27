tar_option_set(packages = c("tidyverse", "lubridate", "sf", "here", "janitor", "glue", "units", "patchwork"))

compute_anthro <- function (loads_basin, flows_gannett, background_gw_tp_ppb = 60) {
  total <- loads_basin %>% 
    filter(
      dataset == "RECENT",
      param == "TP",
      basin %in% c("Power", "Lone_Pine", "Godowa", "Sycan", "SF_Ivory", "SF", "NF_Ivory", "NF"),
      season == "annual"
    ) %>% 
    transmute(basin, flow_hm3, load_mton = load_kg / 1000)
  
  gw <- flows_gannett %>% 
    transmute(
      basin,
      flow_hm3 = change_units(flow_cfs, "ft3/sec", "hm3/yr")
    ) %>% 
    pivot_wider(names_from = "basin", values_from = "flow_hm3") %>% 
    transmute(
      SF,
      SF_Ivory = `SF_Ivory-SF` + SF,
      NF,
      NF_Ivory = `NF_Ivory-NF` + NF,
      Sycan,
      Godowa = `Godowa-SF_Ivory-NF_Ivory` + SF_Ivory + NF_Ivory,
      Lone_Pine = `Lone_Pine-Godowa-Sycan` + Godowa + Sycan,
      Power = `Power-Lone_Pine` + Lone_Pine
    ) %>% 
    pivot_longer(everything(), names_to = "basin", values_to = "flow_hm3") %>% 
    mutate(load_mton = flow_hm3 * background_gw_tp_ppb / 1000)

  background_runoff_unimpacted_basins <- bind_rows(
    total = total,
    gw = gw,
    .id = "term"
  ) %>% 
    filter(basin %in% c("SF", "NF")) %>% 
    pivot_longer(c(flow_hm3, load_mton)) %>% 
    pivot_wider(names_from = "term") %>% 
    mutate(
      gw = pmin(total, gw),
      runoff = pmax(total - gw, 0)
    ) %>% 
    pivot_longer(c(total, gw, runoff), names_to = "term") %>% 
    pivot_wider() %>% 
    mutate(conc_ppb = load_mton * 1000 / flow_hm3) %>% 
    pivot_longer(-c(basin, term)) %>% 
    unite(name, c("term", "name")) %>% 
    pivot_wider() %>% 
    arrange(desc(basin))
  
  background_runoff_tp_ppb <- sum(background_runoff_unimpacted_basins$runoff_load_mton) / 
    sum(background_runoff_unimpacted_basins$runoff_flow_hm3) *
    1000
  
  runoff <- bind_rows(
    total = total,
    gw = gw,
    .id = "term"
  ) %>% 
    select(-load_mton) %>% 
    pivot_wider(names_from = "term", values_from = "flow_hm3") %>%
    transmute(basin, flow_hm3 = total - gw) %>% 
    left_join(
      background_runoff_unimpacted_basins %>% 
        select(basin, runoff_conc_ppb),
      by = "basin"
    ) %>% 
    mutate(
      conc_ppb = pmin(coalesce(runoff_conc_ppb, background_runoff_tp_ppb), background_runoff_tp_ppb),
      load_mton = flow_hm3 * conc_ppb / 1000
    ) %>%
    select(basin, flow_hm3, load_mton)
  
  anthro <- bind_rows(
    total = total,
    gw = gw,
    runoff = runoff,
    .id = "term"
  ) %>% 
    pivot_longer(c(flow_hm3, load_mton), names_to = "var") %>% 
    pivot_wider(names_from = "term") %>% 
    mutate(
      runoff = if_else(gw > total, 0, runoff),
      gw = pmin(total, gw),
      background = gw + runoff,
      anthro = if_else(
        var == "flow_hm3",
        background,
        total - background
      )
    ) %>%
    pivot_longer(-c(basin, var), names_to = "term") %>% 
    pivot_wider(names_from = "var") %>% 
    mutate(conc_ppb = load_mton / flow_hm3 * 1000)

  list(
    background_runoff_unimpacted_basins = background_runoff_unimpacted_basins,
    background_gw_tp_ppb = background_gw_tp_ppb,
    background_runoff_tp_ppb = background_runoff_tp_ppb,
    anthro = anthro
  )
}

targets_anthro <- list(
  tar_target(anthro_synoptic_samples, {
    kt_synoptic_wq %>% 
      filter(param == "TP") %>% 
      mutate(value = value * 1000)
  }),
  tar_target(anthro_synoptic_stations, {
    anthro_synoptic_samples %>% 
      group_by(site_code, station) %>% 
      summarise(
        n_sample = n(),
        median = median(value),
        .groups = "drop"
      )
  }),
  tar_target(anthro_synoptic_median, round(median(anthro_synoptic_stations$median), 0)),
  tar_target(anthro_loads, compute_anthro(loads_basin, flows_gannett, background_gw_tp_ppb = anthro_synoptic_median)),
  
  tar_target(anthro_sensitivity, {
    crossing(
      gw_tp_ppb = 50:70,
      gw_flow_adjust = c(0.8, 1, 1.2)
    ) %>% 
      rowwise() %>% 
      mutate(
        flows_gannett_adjust = list({
          flows_gannett %>% 
            mutate(flow_cfs = flow_cfs * gw_flow_adjust)
        }),
        mean_flows_gannett_adjust = mean(flows_gannett_adjust$flow_cfs),
        anthro = list(compute_anthro(loads_basin, flows_gannett_adjust, gw_tp_ppb))
      ) %>% 
      unnest_wider(anthro)
  }),
  tar_target(anthro_sensitivity_plot_runoff_conc, {
    bind_rows(
      anthro_sensitivity %>%
        select(gw_tp_ppb, gw_flow_adjust, runoff_conc_ppb = background_runoff_tp_ppb) %>% 
        mutate(basin = "SF + NF"),
      anthro_sensitivity %>%
        select(gw_tp_ppb, gw_flow_adjust, background_runoff_unimpacted_basins) %>%
        unnest(background_runoff_unimpacted_basins) %>% 
        select(gw_tp_ppb, gw_flow_adjust, basin, runoff_conc_ppb)
    ) %>%
      mutate(basin = fct_inorder(basin)) %>% 
      ggplot(aes(gw_tp_ppb, runoff_conc_ppb, color = factor(gw_flow_adjust, levels = c("1.2", "1", "0.8")))) +
      geom_line() +
      scale_color_manual("GW Adjustment", values = c(
        "0.8" = "deepskyblue",
        "1" = "black",
        "1.2" = "orangered"
      ), labels = c(
        "0.8" = "-20%",
        "1" = "No Change",
        "1.2" = "+20%"
      )) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(c(0, 0.05))) +
      facet_wrap(vars(basin), nrow = 1) +
      labs(
        x = "Assumed Groundwater TP Concentration (ppb)",
        y = "Runoff TP Concentration (ppb)"
      )
  }),
  tar_target(anthro_sensitivity_plot_flow, {
    anthro_sensitivity %>%
      select(gw_tp_ppb, gw_flow_adjust, anthro) %>% 
      unnest(anthro) %>% 
      distinct(gw_flow_adjust, basin, term, flow_hm3) %>% 
      filter(term %in% c("gw", "runoff")) %>% 
      mutate(
        basin = factor(basin, levels = station_levels),
        term = factor(term, levels = c("runoff", "gw"))
      ) %>% 
      ggplot(aes(factor(gw_flow_adjust), flow_hm3)) +
      geom_col(aes(fill = term), position = "stack") +
      scale_fill_brewer(NULL, palette = "Dark2", labels = c(
        "runoff" = "Runoff",
        "gw" = "GW"
      )) +
      scale_x_discrete(labels = c(
        "0.8" = "-20%",
        "1" = "No\nChange",
        "1.2" = "+20%"
      )) +
      scale_y_continuous(expand = expansion(c(0, 0.05))) +
      facet_wrap(vars(basin), nrow = 2) +
      labs(
        x = "GW Flow Adjustment",
        y = "Flow (hm3)"
      )
  }),
  tar_target(anthro_sensitivity_plot_load, {
    anthro_sensitivity %>%
      select(gw_tp_ppb, gw_flow_adjust, anthro) %>% 
      unnest(anthro) %>% 
      filter(term %in% c("gw", "runoff", "anthro")) %>% 
      mutate(
        basin = factor(basin, levels = station_levels),
        term = factor(term, levels = c("anthro", "runoff", "gw")),
        gw_flow_adjust = fct_rev(factor(gw_flow_adjust))
      ) %>% 
      ggplot(aes(gw_tp_ppb, load_mton)) +
      geom_area(aes(fill = term), position = "stack") +
      scale_fill_brewer(NULL, palette = "Set1", labels = c(
        "anthro" = "Anthropogenic",
        "runoff" = "Runoff",
        "gw" = "GW"
      )) +
      facet_grid(vars(gw_flow_adjust), vars(basin), labeller = labeller(gw_flow_adjust = c(
        "0.8" = "GW Flow -20%",
        "1" = "No Change",
        "1.2" = "GW Flow +20%"
      ))) +
      labs(
        x = "GW Background TP Concentration (ppb)",
        y = "TP Load (mton)"
      )
  }),
  tar_target(anthro_sensitivity_plot_conc, {
    anthro_sensitivity %>%
      select(gw_tp_ppb, gw_flow_adjust, anthro) %>% 
      unnest(anthro) %>% 
      filter(term %in% c("gw", "runoff", "anthro", "background", "total")) %>% 
      mutate(
        basin = factor(basin, levels = station_levels),
        term = factor(term, levels = c("anthro", "runoff", "gw", "background", "total")),
        gw_flow_adjust = fct_rev(factor(gw_flow_adjust))
      ) %>% 
      ggplot(aes(gw_tp_ppb, conc_ppb)) +
      geom_line(aes(color = term)) +
      scale_color_brewer(NULL, palette = "Set1", labels = c(
        "anthro" = "Anthropogenic",
        "runoff" = "Runoff",
        "background" = "Background",
        "gw" = "GW",
        "total" = "Total"
      )) +
      facet_grid(vars(gw_flow_adjust), vars(basin), labeller = labeller(gw_flow_adjust = c(
        "0.8" = "GW Flow -20%",
        "1" = "No Change",
        "1.2" = "GW Flow +20%"
      ))) +
      labs(
        x = "GW Background TP Concentration (ppb)",
        y = "TP FWM Conc (ppb)"
      )
  }),
  tar_target(anthro_sensitivity_plot_pct_anthro, {
    anthro_sensitivity %>%
      unnest(anthro) %>% 
      select(gw_tp_ppb, gw_flow_adjust, basin, term, load_mton) %>% 
      pivot_wider(names_from = "term", values_from = "load_mton") %>% 
      mutate(
        basin = factor(basin, levels = station_levels),
        pct_anthro = pmax(anthro / total, 0)
      ) %>% 
      ggplot(aes(gw_tp_ppb, pct_anthro, color = factor(gw_flow_adjust, levels = c("1.2", "1", "0.8")))) +
      geom_line() +
      scale_color_manual("GW Flow\nAdjustment", values = c(
        "1.2" = "orangered",
        "1" = "black",
        "0.8" = "deepskyblue"
      ), labels = c(
        "1.2" = "+20%",
        "1" = "No Change",
        "0.8" = "-20%"
      )) +
      scale_y_continuous(expand = expansion(), limits = c(0, 1), labels = scales::percent, breaks = scales::pretty_breaks()) +
      facet_wrap(vars(basin), nrow = 2) +
      labs(
        x = "Background Groundwater TP Concentration (ppb)",
        y = "% Total TP Load\nfrom Anthropogenic Sources"
      )
  })
)
