targets_network <- list(
  tar_target(network, {
    network_por <- tribble(
      ~from_basin,    ~to_basin,      ~mainstem,
      "Lone_Pine",    "Power",        TRUE,
      "Godowa+Sycan", "Lone_Pine",    TRUE,
      "Sycan",        "Godowa+Sycan", FALSE,
      "Godowa",       "Godowa+Sycan", TRUE,
      "SF+NF",        "Godowa",       TRUE,
      "NF",           "SF+NF",        FALSE,
      "SF",           "SF+NF",        FALSE
    )

    network_recent <- tribble(
      ~from_basin,        ~to_basin,          ~mainstem,
      "Lone_Pine",        "Power",            TRUE,
      "Godowa+Sycan",     "Lone_Pine",        TRUE,
      "Sycan",            "Godowa+Sycan",     FALSE,
      "Godowa",           "Godowa+Sycan",     TRUE,
      "SF_Ivory+NF_Ivory","Godowa",           TRUE,
      "NF_Ivory",         "SF_Ivory+NF_Ivory",FALSE,
      "SF_Ivory",         "SF_Ivory+NF_Ivory",FALSE,
      "NF",               "NF_Ivory",         FALSE,
      "SF",               "SF_Ivory",         FALSE
    )
    
    bind_rows(
      POR = network_por,
      RECENT = network_recent,
      .id = "dataset"
    )
  })
)