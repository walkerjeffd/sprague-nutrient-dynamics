targets_labels <- list(
  tar_target(basin_levels, {
    c(
      "Power", "Power-Lone_Pine",
      "Lone_Pine", "Lone_Pine-Godowa-Sycan", 
      "Godowa+Sycan",
      "Godowa", "Godowa-SF-NF", "Godowa-SF_Ivory-NF_Ivory", 
      "Sycan", 
      "SF_Ivory+NF_Ivory",
      "SF+NF",
      "SF_Ivory", "SF_Ivory-SF",
      "SF",
      "NF_Ivory", "NF_Ivory-NF",
      "NF"
    )
  }),
  tar_target(station_levels, {
    c(
      "Power",
      "Lone_Pine",
      "Godowa",
      "Sycan", 
      "SF_Ivory",
      "SF", 
      "NF_Ivory",
      "NF"
    )
  }),
  tar_target(season_labels, {
    c(
      "annual" = "Annual",
      "fall" = "Fall (Oct-Dec)",
      "winter" = "Winter (Jan-Mar)",
      "spring" = "Spring (Apr-Jun)", 
      "summer" = "Summer (Jul-Sep)"
    )
  }),
  tar_target(incbasin_labels, {
    c(
      "Power-Lone_Pine" = "Lower Sprague",
      "Lone_Pine-Godowa-Sycan" = "Middle Sprague",
      "Godowa-SF-NF" = "Upper Sprague + Lower SF/NF",
      "Godowa-SF_Ivory-NF_Ivory" = "Upper Sprague",
      "Sycan" = "Sycan",
      "SF_Ivory-SF" = "Lower SF",
      "SF" = "Upper SF",
      "NF_Ivory-NF" = "Lower NF",
      "NF" = "Upper NF"
    )
  }),
  tar_target(param_labels, {
    c(
      "TP" = "Total Phosphorus",
      "PO4" = "Dissolved Phosphorus",
      "PP" = "Particulate Phosphorus",
      "TN" = "Total Nitrogen",
      "NH4" = "Ammonia",
      "NO23" = "Nirate+Nitrite",
      "TSS" = "Total Suspended Solids"
    )
  }),
  tar_target(station_colors, {
    color_site <- c(
      "Power" = "#000000",             # black
      "Lone_Pine" = "#aaaaaa",         # gray
      "Godowa+Sycan" = "#b15928",      # brown
      "Godowa" = "#ff7f00",            # orange-dark
      "Sycan" = "#33a02c",             # green-dark
      "SF_Ivory+NF_Ivory" = "#cab2d6", # purple-light
      "SF+NF" = "#6a3d9a",             # purple-dark
      "SF_Ivory" = "#a6cee3",          # blue-light
      "SF" = "#1f78b4",                # blue-dark
      "NF_Ivory" = "#fb9a99",          # gray
      "NF" = "#e31a1c"                 # red-dark
    )
  }),
  tar_target(var_colors, {
    c(
      "flow_hm3" = "steelblue3",
      "load_kg" = "olivedrab3",
      "conc_ppb" = "orangered"
    )
  }),
  tar_target(param_var_labels, {
    var_labels <- tribble(
      ~var, ~var_label,
      "load_kg", "Load (kg/d)", 
      "load_kgyr", "Load (kg/yr)", 
      "export_kgkm2", "Export (kg/km2/d)", 
      "export_kgkm2yr", "Export (kg/km2/yr)", 
      "conc_ppb", "Conc (ppb)"
    )
    x <- crossing(
      var = fct_inorder(var_labels$var),
      param = fct_inorder(names(param_labels))
    ) %>% 
      left_join(var_labels, by = "var") %>% 
      mutate(labels = str_c(param, var_label, sep = " ")) %>% 
      unite(param_var, c("param", "var"), remove = FALSE) %>% 
      mutate(var = factor(var, levels = var_labels$var)) %>% 
      arrange(param, var)
    c(
      "flow_cfs" = "Flow (cfs)",
      "flow_hm3" = "Flow (hm3/d)",
      "flow_hm3yr" = "Flow (hm3/yr)",
      "runoff_cm" = "Runoff (cm/d)",
      "runoff_cmyr" = "Runoff (cm/yr)",
      set_names(x$labels, nm = x$param_var)
    )
  }),
  tar_target(scale_color_season, {
    function (...) {
      scale_color_manual(
        "Season",
        values = c(
          "fall" = "grey50",
          "winter" = "orangered",
          "spring" = "deepskyblue",
          "summer" = "chartreuse3"
        ),
        labels = season_labels,
        ...
      )
    }
  })
)
