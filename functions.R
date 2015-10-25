makeFootnote <- function(footnoteText='Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under CC BY SA.',
                         size=0.7, color=grey(0.5)) {
  #   http://ryouready.wordpress.com/2009/02/17/r-good-practice-adding-footnotes-to-graphics/
  require(grid)
  pushViewport(viewport())
  grid.text(label= footnoteText ,
            x = unit(1,"npc") - unit(2, "mm"),
            y= unit(2, "mm"),
            just=c("right", "bottom"),
            gp=gpar(cex= size, col=color))
  popViewport()
}

water_day <- function(x) {
  # returns water day (1 = Oct 1)
  ifelse(lubridate::yday(x)-lubridate::leap_year(x)>=274,
         lubridate::yday(x)-lubridate::leap_year(x)-274,
         lubridate::yday(x)+(365-274))
}
# water_day(seq.Date(from=as.Date("2000-01-01"), to=as.Date("2000-01-05"), by="day"))
#
# water_day(c(as.Date("2000-10-01"), as.Date("2001-10-01")))
# water_day(c(as.Date("2000-12-31"), as.Date("2001-12-31"), as.Date("2002-12-31")))
# water_day(c(as.Date("2000-01-01"), as.Date("2001-01-01"), as.Date("2002-01-01")))
# water_day(c(as.Date("2000-09-30"), as.Date("2001-09-30"), as.Date("2002-09-30")))
#
# plot(water_day(seq(as.Date("2000-10-01"), as.Date("2001-10-01"), by='day')))


log_breaks <- function(x, y) {
  # log breaks
  as.vector(outer(x, y, '*'))
}
log_labels <- function(x, y) {
  # labels for log scales with gaps
  x_na <- seq(1, 9)
  x_na[which(!(x_na %in% x))] <- NA
  x <- log_breaks(x_na, y)
  x <- as.character(x)
  x <- ifelse(is.na(x), "", x)
  x
}
# example:
# scale_y_log10(breaks=log_breaks(seq(1, 9), 10^seq(-3, 3)),
#               labels=log_labels(c(1, 5), 10^seq(-3, 3)))

get_units <- function(variable, ppm=TRUE) {
  if (variable %in% c('TP', 'PO4', 'PP', 'TN', 'NH4', 'NO23', 'TSS')) {
    if (ppm) {
      units <- 'mg/L'
    } else {
      units <- 'ug/L'
    }
  } else if (variable == 'TURBIDITY') {
    units <- 'NTU'
  } else if (variable == 'FLOW') {
    units <- 'cfs'
  } else {
    units <- '?'
  }
  units
}

hm3d_cfs <- function(x) {
  return(x / (24 * 3600/(3.28^3)/1e+06))
}

incbasin_names <- c("Power-Lone_Pine"="Lower Sprague",
                    "Lone_Pine-Godowa-Sycan"="Middle Sprague",
                    "Godowa-SF_Ivory-NF_Ivory"="Upper Sprague",
                    "Godowa-SF-NF"="Upper Sprague + Lower SF/NF",
                    "Sycan"="Sycan",
                    "SF_Ivory-SF"="Lower SF",
                    "SF"="Upper SF",
                    "NF_Ivory-NF"="Lower NF",
                    "NF"="Upper NF")