source("_targets.R")

# TP vs TSS relationship @ Sycan for Megan Skinner

tar_read(c(report_splots_data, report_splots_labels, scale_color_season))
create_report_splot(report_splots_data, "TSS", "TP", report_splots_labels, scale_color_season)

sycan <- report_splots_data %>% 
  filter(station == "Sycan") %>% 
  mutate(log_TP = log10(TP), log_TSS = log10(TSS)) %>% 
  filter(!is.na(TP), !is.na(TSS))

summary(lm(log_TP ~ log_TSS, data = sycan))
x_lm <- summary(lm(log_TP ~ poly(log_TSS, 2), data = sycan))
eqn <- glue("log10(TP) = {round(coef(x_lm)[1, 1], 2)} + {round(coef(x_lm)[2, 1], 3)} * log10(TSS) + {round(coef(x_lm)[3, 1], 3)} * log10(TSS) ^ 2")

sycan %>% 
  ggplot(aes(TSS, TP)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  scale_x_log10_(magnitudes = c(0.1, 1, 10, 100, 1000)) +
  scale_y_log10_(magnitudes = c(0.1, 1, 10, 100, 1000)) +
  labs(
    x = "TSS (ppb)",
    y = "TP (ppb)",
    title = "TP vs TSS @ Sycan, WY 2011-2020",
    subtitle = glue(
      eqn,
      "R^2 = {round(x_lm$r.squared, 3)}",
      .sep = "\n"
    )
  )
