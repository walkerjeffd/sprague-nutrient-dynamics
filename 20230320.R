# walkthrough of flow estimation algorithm for USBOR

source("_targets.R")

flows_model <- tar_read(flows_model)

x <- flows_model %>% 
  filter(station == "SF", water_year(date) == 2010)

x %>% 
  ggplot(aes(date, flow_cfs)) +
  geom_point() +
  labs(title = "flow data @ SF, WY 2010")

x %>% 
  ggplot(aes(date, flow_cfs)) +
  geom_line(data = filter(x, !is.na(flow_cfs)), aes(color = "0_linear")) +
  # geom_line(aes(y = ref_flow_cfs, color = "1_ref")) +
  # geom_line(aes(y = pred_ratio, color = "2_ratio")) +
  # geom_point(aes(y = if_else(is.na(flow_cfs), NA_real_, pred_ratio), color = "2_ratio")) +
  # geom_line(aes(y = pred_resid, color = "3_est")) +
  geom_point() +
  scale_color_brewer(NULL, palette = "Set1", labels = c(
    "0_linear" = "linear interp",
    "1_ref" = "obs @ ref gage",
    "2_ratio" = "model predict",
    "3_est" = "estimated (adj. residual)"
  )) +
  labs(title = "observed flow data @ SF, WY 2010", subtitle = "linear interpolation")

x %>% 
  ggplot(aes(date, flow_cfs)) +
  geom_line(data = filter(x, !is.na(flow_cfs)), aes(color = "0_linear")) +
  geom_line(aes(y = ref_flow_cfs, color = "1_ref")) +
  # geom_line(aes(y = pred_ratio, color = "2_ratio")) +
  # geom_point(aes(y = if_else(is.na(flow_cfs), NA_real_, pred_ratio), color = "2_ratio")) +
  # geom_line(aes(y = pred_resid, color = "3_est")) +
  geom_point() +
  scale_color_brewer(NULL, palette = "Set1", labels = c(
    "0_linear" = "linear interp",
    "1_ref" = "obs @ ref gage",
    "2_ratio" = "model predict",
    "3_est" = "estimated (adj. residual)"
  )) +
  labs(title = "observed flow data @ SF, WY 2010", subtitle = "reference gage")

x %>% 
  ggplot(aes(date, flow_cfs)) +
  geom_line(data = filter(x, !is.na(flow_cfs)), aes(color = "0_linear")) +
  geom_line(aes(y = ref_flow_cfs, color = "1_ref")) +
  geom_line(aes(y = pred_ratio, color = "2_ratio")) +
  # geom_point(aes(y = if_else(is.na(flow_cfs), NA_real_, pred_ratio), color = "2_ratio")) +
  # geom_line(aes(y = pred_resid, color = "3_est")) +
  geom_point() +
  scale_color_brewer(NULL, palette = "Set1", labels = c(
    "0_linear" = "linear interp",
    "1_ref" = "obs @ ref gage",
    "2_ratio" = "model predict",
    "3_est" = "estimated (adj. residual)"
  )) +
  labs(title = "observed flow data @ SF, WY 2010", subtitle = "model prediction")

x %>% 
  ggplot(aes(date, flow_cfs)) +
  geom_line(data = filter(x, !is.na(flow_cfs)), aes(color = "0_linear")) +
  geom_line(aes(y = ref_flow_cfs, color = "1_ref"), alpha = 0.5) +
  geom_line(aes(y = pred_ratio, color = "2_ratio")) +
  geom_point(aes(y = if_else(is.na(flow_cfs), NA_real_, pred_ratio), color = "2_ratio")) +
  # geom_line(aes(y = pred_resid, color = "3_est")) +
  geom_point() +
  coord_cartesian(ylim = c(0, 210)) +
  scale_color_brewer(NULL, palette = "Set1", labels = c(
    "0_linear" = "linear interp",
    "1_ref" = "obs @ ref gage",
    "2_ratio" = "model predict",
    "3_est" = "estimated (adj. residual)"
  )) +
  labs(title = "observed flow data @ SF, WY 2010", subtitle = "model prediction")






x %>% 
  ggplot(aes(date, ln_resid)) +
  geom_point() +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_line(aes(y = ln_resid_interp)) +
  labs(title = "observed flow data @ SF, WY 2010", subtitle = "interpolated residuals (log-space)")

x %>% 
  ggplot(aes(date, flow_cfs)) +
  geom_line(data = filter(x, !is.na(flow_cfs)), aes(color = "0_linear"), alpha = 0.5) +
  geom_line(aes(y = ref_flow_cfs, color = "1_ref"), alpha = 0.5) +
  geom_line(aes(y = pred_ratio, color = "2_ratio"), alpha = 0.5) +
  geom_point(aes(y = if_else(is.na(flow_cfs), NA_real_, pred_ratio), color = "2_ratio")) +
  geom_line(aes(y = pred_resid, color = "3_est")) +
  geom_point() +
  coord_cartesian(ylim = c(0, 210)) +
  scale_color_brewer(NULL, palette = "Set1", labels = c(
    "0_linear" = "linear interp",
    "1_ref" = "obs @ ref gage",
    "2_ratio" = "model predict",
    "3_est" = "final estimated"
  )) +
  labs(title = "observed flow data @ SF, WY 2010", subtitle = "estimated daily via residual interpolation")
