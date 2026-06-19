#' @title Precipitation Component of the ACI
#' @description Computes the precipitation component of the Actuarial Climate
#'   Index.
#' @name precipitation
NULL

#' Calculate maximum precipitation over a rolling window
#'
#' Computes the rolling \code{window_size}-day sum of precipitation, then
#' takes the monthly (or seasonal) maximum.
#'
#' @param dataset     List returned by \code{load_component()} for \code{tp}.
#' @param var_name    Variable name in \code{dataset} (default \code{"tp"}).
#' @param window_size Rolling window in days. Default 5.
#' @param season      Logical. If \code{TRUE} aggregate by season (DJF, MAM,
#'   JJA, SON) instead of by month. Default \code{FALSE}.
#' @return A list with \code{data} [lon × lat × periods] and \code{time}.
#' @export
calculate_maximum_precipitation_over_window <- function(dataset,
                                                         var_name    = "tp",
                                                         window_size = 5L,
                                                         season      = FALSE) {
  rolling <- calculate_rolling_sum(dataset, var_name, window_size)

  data  <- rolling$data
  time  <- rolling$time
  dims  <- dim(data)
  nl    <- dims[1]; nw <- dims[2]; nt <- dims[3]

  if (season) {
    # Define meteorological seasons (DJF starts in Dec of previous year)
    season_key <- dplyr::case_when(
      as.integer(format(time, "%m")) %in% c(12, 1, 2)  ~ paste0(
        ifelse(as.integer(format(time, "%m")) == 12,
               as.integer(format(time, "%Y")),
               as.integer(format(time, "%Y")) - 1L),
        "-DJF"),
      as.integer(format(time, "%m")) %in% 3:5  ~ paste0(format(time, "%Y"), "-MAM"),
      as.integer(format(time, "%m")) %in% 6:8  ~ paste0(format(time, "%Y"), "-JJA"),
      TRUE                                      ~ paste0(format(time, "%Y"), "-SON")
    )
  } else {
    season_key <- format(time, "%Y-%m")
  }

  periods <- unique(season_key)
  out     <- array(NA_real_, c(nl, nw, length(periods)))

  for (k in seq_along(periods)) {
    idx <- which(season_key == periods[k])
    out[, , k] <- apply(data[, , idx, drop = FALSE], c(1, 2),
                         max, na.rm = TRUE)
  }

  period_dates <- if (season) {
    # Use first day of each season label
    as.POSIXct(paste0(sub("-.*", "", periods), "-",
                      dplyr::case_when(
                        grepl("DJF", periods) ~ "12",
                        grepl("MAM", periods) ~ "03",
                        grepl("JJA", periods) ~ "06",
                        TRUE                  ~ "09"),
                      "-01"), format = "%Y-%m-%d", tz = "UTC")
  } else {
    as.POSIXct(paste0(periods, "-01"), format = "%Y-%m-%d", tz = "UTC")
  }

  list(data = out, time = period_dates, lon = dataset$lon, lat = dataset$lat)
}

#' Calculate the precipitation component of the ACI
#'
#' Computes the standardised anomaly of maximum monthly precipitation over a
#' rolling 5-day window.
#'
#' @param precipitation_data_path Path to the precipitation NetCDF file.
#' @param mask_path               Path to the country mask NetCDF file, or \code{NULL}.
#' @param reference_period        Character vector \code{c("start", "end")}.
#' @param var_name                Variable name in the NetCDF (default \code{"tp"}).
#' @param window_size             Rolling window in days. Default 5.
#' @param season                  Logical. Seasonal aggregation. Default \code{FALSE}.
#' @param area                    Logical. Spatial mean before standardising.
#'   Default \code{FALSE}.
#' @return Standardised precipitation metric (list or named vector depending on
#'   \code{area}).
#' @export
precipitation_component <- function(precipitation_data_path,
                                     mask_path        = NULL,
                                     reference_period,
                                     var_name         = "tp",
                                     window_size      = 5L,
                                     season           = FALSE,
                                     area             = FALSE) {
  dataset     <- load_component(precipitation_data_path, var_name, mask_path)
  period_max  <- calculate_maximum_precipitation_over_window(dataset, var_name,
                                                              window_size, season)
  standardize_metric(period_max, reference_period, area)
}
