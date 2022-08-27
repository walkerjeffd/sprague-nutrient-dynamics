source("_targets.R")


# regulation: wq vs flow --------------------------------------------------

x <- tar_read(report_splots_data) %>% 
  mutate(
    reg = if_else(
      water_year(date) < 2013,
      "pre-regulation",
      "post-regulation"
    )
  )

x %>% 
  nest_by(season) %>% 
  mutate(
    plot = list({
      data %>% 
        ggplot(aes(flow_cfs, TP, color = reg)) +
        geom_point() +
        geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
        scale_color_brewer(palette = "Set1") +
        scale_x_log10() +
        scale_y_log10() +
        facet_wrap(vars(station), nrow = 2, scales = "free") +
        labs(title = season) +
        theme(aspect.ratio = 1)
    })
  ) %>% 
  pull(plot) %>% 
  wrap_plots() +
  plot_layout(guides = "collect")

x %>% 
  ggplot(aes(flow_cfs, `PP/TP`, color = reg)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
  scale_color_brewer(palette = "Set1") +
  scale_x_log10() +
  # scale_y_log10() +
  scale_y_continuous(limits = c(0, 1)) +
  facet_wrap(vars(station), nrow = 2, scales = "free") +
  theme(aspect.ratio = 1)

x %>% 
  ggplot(aes(flow_cfs, PP, color = reg)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
  scale_color_brewer(palette = "Set1") +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(vars(station), nrow = 2, scales = "free") +
  theme(aspect.ratio = 1)

x %>% 
  ggplot(aes(flow_cfs, PO4, color = reg)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
  scale_color_brewer(palette = "Set1") +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(vars(station), nrow = 2, scales = "free") +
  theme(aspect.ratio = 1)




x %>% 
  filter(month(date) %in% 4:9) %>% 
  ggplot(aes(flow_cfs, PP/TP, color = reg)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
  scale_color_brewer(palette = "Set1") +
  scale_x_log10() +
  # scale_y_log10() +
  facet_wrap(vars(station), nrow = 2, scales = "free") +
  theme(aspect.ratio = 1)

x %>% 
  filter(month(date) %in% 4:9) %>% 
  ggplot(aes(same_wyear(date), flow_cfs, color = reg)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
  scale_color_brewer(palette = "Set1") +
  scale_y_log10() +
  facet_wrap(vars(station), nrow = 2, scales = "free") +
  theme(aspect.ratio = 1)


x %>% 
  filter(month(date) %in% 4:9) %>% 
  ggplot(aes(same_wyear(date), PP / TP, color = reg)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
  scale_color_brewer(palette = "Set1") +
  # scale_y_log10() +
  facet_wrap(vars(station), nrow = 2, scales = "free") +
  theme(aspect.ratio = 1)

x %>% 
  filter(month(date) %in% 4:9) %>% 
  ggplot(aes(same_wyear(date), TP, color = reg)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
  scale_color_brewer(palette = "Set1") +
  # scale_y_log10() +
  facet_wrap(vars(station), nrow = 2, scales = "free") +
  theme(aspect.ratio = 1)


x %>% 
  filter(month(date) %in% 4:9) %>% 
  ggplot(aes(flow_cfs, TN, color = reg)) +
  geom_point(size = 1, alpha = 0.5) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE) +
  scale_color_brewer(palette = "Set1") +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(vars(station), nrow = 2, scales = "free") +
  theme(aspect.ratio = 1)


