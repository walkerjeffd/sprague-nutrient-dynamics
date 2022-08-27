# units -------------------------------------------------------------------

change_units <- function(x, from, to) {
  units::drop_units(units::set_units(units::set_units(x, from, mode = "standard"), to, mode = "standard"))
}

# ggplot ------------------------------------------------------------------

# library(ggplot2)
ggplot2::theme_set(ggplot2::theme_bw())

ggplot2::theme_update(
  strip.background = ggplot2::element_blank(),
  strip.text = ggplot2::element_text(size = 10, face = "bold")
)



scale_y_log10_ <- function (breaks = c(1, 2, 5), magnitudes = c(10, 100, 1000), ...) {
  log10_breaks <- as.numeric(outer(1:9, magnitudes))
  log10_labels <- as.character(log10_breaks)
  log10_labels[!str_sub(sprintf("%e", log10_breaks), 1, 1) %in% as.character(breaks)] <- ""
  
  scale_y_log10(
    breaks = log10_breaks,
    minor_breaks = c(),
    labels = log10_labels,
    ...
  )
}

scale_x_log10_ <- function (breaks = c(1, 2, 5), magnitudes = c(10, 100, 1000), ...) {
  log10_breaks <- as.numeric(outer(1:9, magnitudes))
  log10_labels <- as.character(log10_breaks)
  log10_labels[!str_sub(sprintf("%e", log10_breaks), 1, 1) %in% as.character(breaks)] <- ""
  
  scale_x_log10(
    breaks = log10_breaks,
    minor_breaks = c(),
    labels = log10_labels,
    ...
  )
}


# time --------------------------------------------------------------------

month_levels <- tolower(month.abb[c(10:12, 1:9)])

method_season_labels <- c(
  "kt:oct" = "Oct",
  "kt:nov" = "Nov",
  "kt:dec" = "Dec",
  "kt:jan" = "Jan",
  "kt:feb" = "Feb",
  "kt:mar" = "Mar",
  "kt:apr" = "Apr",
  "kt:may" = "May",
  "kt:jun" = "Jun",
  "kt:jul" = "Jul",
  "kt:aug" = "Aug",
  "kt:sep" = "Sep", 
  
  "skt:oct-dec" = "Oct-Dec",
  "skt:jan-mar" = "Jan-Mar",
  "skt:apr-jun" = "Apr-Jun",
  "skt:jul-sep" = "Jul-Sep",
  
  "skt:oct-mar" = "Oct-Mar",
  "skt:apr-sep" = "Apr-Sep",
  
  "skt:oct-sep" = "Oct-Sep (SK)",
  "kt:annual" = "Annual (MK)",
  "lm:annual" = "Annual (LR)"
)

water_year <- function (x, start_month = 10) {
  if (start_month == 1) {
    return(lubridate::year(x))
  } else {
    return(
      ifelse(
        lubridate::month(x) >= start_month,
        lubridate::year(x) + 1,
        lubridate::year(x)
      )
    )
  }
}

#' Get water day component of datetime(s)
#'
#' A water day is the number of days past the start of a water year beginning with 1.
#' For example, if a water year is defined as starting in October, then the water day for
#' 2007-09-30 is 365, 2007-10-01 is 1, 2007-10-02 is 2, etc.
#'
#' @param x vector of datetime objects
#' @param start_month integer specifying first month of water year (default = 10 for October)
#'
#' @return vector of water days as decimal numbers
#' @export
#'
#' @examples
#' x <- seq.Date(from = as.Date("2007-09-30"), to = as.Date("2007-10-02"), by = "day")
#' water_day(x)
water_day <- function(x, start_month = 10) {
  jd <- lubridate::yday(x)
  
  if (start_month == 1) {
    # same as julian day
    return(jd)
  } else {
    wy <- water_year(x, start_month = start_month)
    wy_start_date <- as.Date(paste(wy - 1, start_month, 1, sep = "-"))
    wd <- as.numeric(difftime(x, wy_start_date, units = "day")) + 1
    return(wd)
  }
}

#' Assign same year to all dates
#' 
#' Useful for plotting seasonal patterns
#'
#' @param x vector of dates
#' @param yr year to assign (default = min(year(x)))
#'
#' @return vector of dates with same year
same_year <- function (x, yr = min(year(x))) {
  year(x) <- yr
  x
}

#' Assign same water year to all dates
#' 
#' Useful for plotting seasonal patterns
#'
#' @param x vector of dates
#' @param wyr water year to assign (default = min(water_year(x)))
#'
#' @return vector of dates with same year
same_wyear <- function (x, start_month = 10, wyr = min(water_year(x, start_month = start_month))) {
  x_wyr <- water_year(x, start_month)
  x_yr <- year(x)
  year(x) <- x_yr - (x_wyr - wyr)
  x
}


# loading -----------------------------------------------------------------

#' Estimate daily loads from flow and concentration data
#' using residual interpolation of regression model
#'
#' @param x data frame containing dates (`date`), flow (`flow_hm3`), and sampled concentration (`conc_ppb`)
#' @param interp_resid boolean flag to add interpolated residuals to predicted concentrations (default=TRUE)
#' @param extrap_flow boolean flag to extrapolate model predictions outside sampled flow range
#'
#' @return list containing the model input data frame (`inp`), linear model (`model`),
#' summary statistics (`stats`), and predicted concentration and load timeseries (`pred`)
#' 
#' @export
#'
#' @examples
estimate_loads <- function(x, period = range(x$date), interp_resid = TRUE, extrap_flow = FALSE) {
  stopifnot(!tsibble::has_gaps(tsibble::tsibble(x, index = date)))
  stopifnot(all(!is.na(x[["flow_hm3"]])))
  stopifnot(sum(!is.na(x[["conc_ppb"]])) > 10)
  
  n <- nrow(x)
  
  dates <- x$date
  wyr <- water_year(dates, start_month = 10)
  mon <- month(dates)
  jday <- yday(dates)
  dyear <- year(dates) + jday / 365.25
  dyear2 <- dyear * dyear
  
  c_obs <- x$conc_ppb
  q_obs <- x$flow_hm3
  l_obs <- q_obs * c_obs # kg/d
  sampled <- !is.na(c_obs)
  
  log_c_obs <- log(c_obs)
  q_deriv <- coalesce(log(q_obs / lag(q_obs)), 0)
  cos_2t <- cos(2 * 2 * pi * jday / 365.25)
  sin_2t <- sin(2 * 2 * pi * jday / 365.25)
  cos_t <- cos(2 * pi * jday / 365.25)
  sin_t <- sin(2 * pi * jday / 365.25)
  
  if (!extrap_flow) {
    # limit range of deriv and observed flow to sampled flows
    q_deriv <- pmin(max(q_deriv[sampled]), pmax(min(q_deriv[sampled]), q_deriv))
    q_inp <- pmin(max(q_obs[sampled]), pmax(min(q_obs[sampled]), q_obs))
  } else {
    q_inp <- q_obs
  }
  
  log_q <- log(q_inp)
  log_q2 <- log_q ^ 2
  log_q3 <- log_q ^ 3
  
  x_model <- tibble(
    log_c_obs,
    q_deriv,
    dyear,
    dyear2,
    cos_2t,
    sin_2t,
    cos_t,
    sin_t,
    log_q,
    log_q2,
    log_q3
  )
  
  lm_conc <- lm(
    log_c_obs ~ q_deriv + dyear + dyear2 + cos_2t + sin_2t + cos_t + sin_t + log_q3 + log_q2 + log_q,
    data = x_model[sampled, ]
  )
  
  log_c_pred_biased <- predict(lm_conc, newdata = x_model)
  bias_correction <- log(sum(c_obs[sampled]/exp(log_c_pred_biased[sampled])) / sum(sampled))
  c_pred <- exp(log_c_pred_biased + bias_correction)
  
  log_c_resid <- log(c_obs / c_pred)
  log_c_resid_interp <- approx(1:n, log_c_resid, 1:n, rule = 1, yleft = 0, yright = 0)$y
  
  c_est <- c_pred * exp(log_c_resid_interp)
  
  l_obs <- c_obs * q_obs
  l_pred <- c_pred * q_obs
  l_est <- c_est * q_obs
  l_resid <- l_obs - l_pred
  
  lm_conc_stats <- broom::glance(lm_conc)
  
  l_rss <- sum(l_resid[sampled] ^ 2)
  l_resid_se <- sqrt(l_rss / lm_conc_stats$df.residual)
  l_se <- l_resid_se / sqrt(sum(sampled))
  l_est_mean <- mean(l_est[sampled])
  l_est_sd <- sd(l_est[sampled])
  l_obs_mean <- mean(l_obs[sampled])
  l_obs_sd <- sd(l_obs[sampled])
  l_relative_se <- l_se / l_est_mean
  l_resid_cv <- l_resid_se / l_est_mean
  
  c_obs_mean <- mean(c_obs[sampled])
  c_obs_sd <- sd(c_obs[sampled])
  c_se <- l_relative_se * c_obs_mean
  
  q_obs_mean <- mean(q_obs)
  
  lm_stats <- tibble(
    q_obs_mean = q_obs_mean,
    
    c_obs_mean = c_obs_mean,
    c_obs_sd = c_obs_sd,
    c_se = c_se,
    
    l_rss = l_rss,
    l_resid_se = l_resid_se,
    l_se = l_se,
    l_est_mean = l_est_mean,
    l_est_sd = l_est_sd,
    l_obs_mean = l_obs_mean,
    l_obs_sd = l_obs_sd,
    l_relative_se = l_relative_se,
    l_resid_cv = l_resid_cv
  ) %>% 
    bind_cols(broom::glance(lm_conc))
  
  x_pred <- tibble(
    date = dates,
    flow_hm3 = q_obs,
    load_kg = l_est,
    obs_load_kg = l_obs,
    obs_conc_ppb = c_obs,
    log_c_resid = log_c_resid,
    log_c_resid_interp = log_c_resid_interp,
    pred_conc_ppb = c_pred,
    conc_ppb = c_est
  ) %>% 
    filter(date >= period[1], date <= period[2])
  
  tibble(
    inp = list(x_model),
    model = list(lm_conc),
    stats = list(lm_stats),
    pred = list(x_pred)
  )
}

#' Estimate daily loads from flow and concentration data
#' using linear interpolation
#'
#' @param x data frame containing dates (`date`), flow (`flow_hm3`), and sampled concentration (`conc_ppb`)
#'
#' @return list containing the model input data frame (`inp`), and
#' predicted concentration and load timeseries (`pred`)
#' 
#' @export
#'
#' @examples
estimate_loads_linear <- function(x) {
  stopifnot(!tsibble::has_gaps(tsibble::tsibble(x, index = date)))
  stopifnot(all(!is.na(x[["flow_hm3"]])))
  stopifnot(sum(!is.na(x[["conc_ppb"]])) > 10)
  
  n <- nrow(x)
  
  dates <- x$date
  c_obs <- x$conc_ppb
  q_obs <- x$flow_hm3
  l_obs <- q_obs * c_obs # kg/day
  
  sampled <- !is.na(c_obs)
  
  c_pred <- approx(dates, c_obs, xout = dates, rule = 2)$y
  l_pred <- c_pred * q_obs
  
  tibble(
    inp = list(x),
    pred = list(tibble(
      date = dates,
      flow_hm3 = q_obs,
      load_kg = l_pred,
      conc_ppb = c_pred,
      obs_load_kg = l_obs,
      obs_conc_ppb = c_obs
    ))
  )
}

# misc --------------------------------------------------------------------

#' Save plot to file
#'
#' @param plot ggplot object
#' @param filename filename
#' @param ... other arguments passed to ggsave()
#'
#' @return filename
save_plot <- function (plot, filename, ...) {
  ggsave(filename, plot = plot, ...)
  filename
}
