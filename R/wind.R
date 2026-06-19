#' @title Wind Component of the ACI
#' @description Computes the wind component of the Actuarial Climate Index.
#' @name wind
NULL

#' Calculate daily wind power from u10 and v10 components
#'
#' Wind speed = sqrt(u10^2 + v10^2), wind power = 0.5 * rho * ws^3.
#'
#' @param u10_dataset List returned by \code{load_component()} for \code{u10}.
#' @param v10_dataset List returned by \code{load_component()} for \code{v10}.
#' @param reference_period Optional character vector \code{c("start","end")}.
#'   If provided, only the reference sub-period is returned.
#' @return A list with \code{data} [lon x lat x days] (wind power in W/m²)
#'   and daily \code{time}.
#' @export
wind_power <- function(u10_dataset, v10_dataset, reference_period = NULL) {
  rho <- 1.23  # air density kg/m³

  u <- u10_dataset$data
  v <- v10_dataset$data
  ws <- sqrt(u^2 + v^2)

  # Resample u and v separately to daily means
  u_list <- resample_daily(list(data = u, time = u10_dataset$time,
                                lon = u10_dataset$lon, lat = u10_dataset$lat),
                           FUN = mean)
  v_list <- resample_daily(list(data = v, time = v10_dataset$time,
                                lon = v10_dataset$lon, lat = v10_dataset$lat),
                           FUN = mean)

  ws_daily <- sqrt(u_list$data^2 + v_list$data^2)
  wp       <- 0.5 * rho * ws_daily^3

  result <- list(data = wp, time = u_list$time,
                 lon = u10_dataset$lon, lat = u10_dataset$lat)

  if (!is.null(reference_period)) {
    ref_start <- as.POSIXct(reference_period[1], tz = "UTC")
    ref_end   <- as.POSIXct(reference_period[2], tz = "UTC")
    keep      <- result$time >= ref_start & result$time <= ref_end
    result$data <- result$data[, , keep, drop = FALSE]
    result$time <- result$time[keep]
  }
  result
}

#' Calculate wind power thresholds (90th percentile per day-of-year)
#'
#' @param u10_dataset      List for \code{u10} variable.
#' @param v10_dataset      List for \code{v10} variable.
#' @param reference_period Character vector \code{c("start", "end")}.
#' @return A list with \code{data} [lon x lat x all_days] giving the threshold
#'   for each day of the full series, and \code{time}.
#' @export
wind_thresholds <- function(u10_dataset, v10_dataset, reference_period) {
  wp_full <- wind_power(u10_dataset, v10_dataset)
  ref_start <- as.POSIXct(reference_period[1], tz = "UTC")
  ref_end   <- as.POSIXct(reference_period[2], tz = "UTC")

  time     <- wp_full$time
  data     <- wp_full$data
  ref_mask <- time >= ref_start & time <= ref_end
  dims     <- dim(data)
  nl <- dims[1]; nw <- dims[2]; nt <- dims[3]

  doy_all <- as.integer(format(as.Date(time), "%j"))
  doy_ref <- doy_all[ref_mask]

  # Per-day mean and sd over reference period
  thresh <- array(NA_real_, c(nl, nw, nt))
  for (i in seq_len(nl)) {
    for (j in seq_len(nw)) {
      series_ref <- data[i, j, ref_mask]
      for (t in seq_len(nt)) {
        d   <- doy_all[t]
        idx <- which(doy_ref == d)
        if (length(idx) == 0) next
        m <- mean(series_ref[idx], na.rm = TRUE)
        s <- sd(series_ref[idx],   na.rm = TRUE)
        thresh[i, j, t] <- m + 1.28 * s
      }
    }
  }
  list(data = thresh, time = time, lon = u10_dataset$lon, lat = u10_dataset$lat)
}

#' Calculate days with wind power above the 90th-percentile threshold
#'
#' @param u10_dataset      List for \code{u10}.
#' @param v10_dataset      List for \code{v10}.
#' @param reference_period Character vector \code{c("start", "end")}.
#' @return A list with binary \code{data} [lon x lat x days] and \code{time}.
#' @export
days_above_wind_thresholds <- function(u10_dataset, v10_dataset,
                                       reference_period) {
  wp   <- wind_power(u10_dataset, v10_dataset)
  thr  <- wind_thresholds(u10_dataset, v10_dataset, reference_period)
  above <- ifelse(wp$data > thr$data, 1L, 0L)
  list(data = above, time = wp$time,
       lon = u10_dataset$lon, lat = u10_dataset$lat)
}

#' Calculate monthly wind exceedance frequency
#'
#' @param u10_dataset      List for \code{u10}.
#' @param v10_dataset      List for \code{v10}.
#' @param reference_period Character vector \code{c("start", "end")}.
#' @param season           Logical. Seasonal aggregation. Default \code{FALSE}.
#' @return A list with \code{data} [lon x lat x periods] and \code{time}.
#' @export
calculate_period_wind_exceedance_frequency <- function(u10_dataset, v10_dataset,
                                                        reference_period,
                                                        season = FALSE) {
  above <- days_above_wind_thresholds(u10_dataset, v10_dataset, reference_period)
  data  <- above$data
  time  <- above$time

  if (season) {
    key <- dplyr::case_when(
      as.integer(format(time, "%m")) %in% c(12, 1, 2) ~ paste0(
        ifelse(as.integer(format(time, "%m")) == 12,
               format(time, "%Y"),
               as.integer(format(time, "%Y")) - 1L), "-DJF"),
      as.integer(format(time, "%m")) %in% 3:5  ~ paste0(format(time, "%Y"), "-MAM"),
      as.integer(format(time, "%m")) %in% 6:8  ~ paste0(format(time, "%Y"), "-JJA"),
      TRUE                                      ~ paste0(format(time, "%Y"), "-SON")
    )
  } else {
    key <- format(time, "%Y-%m")
  }

  periods <- unique(key)
  dims    <- dim(data)
  nl <- dims[1]; nw <- dims[2]
  out <- array(NA_real_, c(nl, nw, length(periods)))

  for (k in seq_along(periods)) {
    idx <- which(key == periods[k])
    s   <- apply(data[, , idx, drop = FALSE], c(1, 2), sum,   na.rm = TRUE)
    n   <- apply(data[, , idx, drop = FALSE], c(1, 2), length)
    out[, , k] <- s / n
  }

  period_dates <- as.POSIXct(paste0(sub("-.*", "", periods), "-",
    dplyr::case_when(grepl("DJF", periods) ~ "12",
                     grepl("MAM", periods) ~ "03",
                     grepl("JJA", periods) ~ "06",
                     grepl("SON", periods) ~ "09",
                     TRUE ~ sub(".*-", "", periods)),
    "-01"), format = "%Y-%m-%d", tz = "UTC")

  list(data = out, time = period_dates,
       lon = u10_dataset$lon, lat = u10_dataset$lat)
}

#' Calculate the wind component of the ACI
#'
#' @param wind_u10_data_path Path to the u10 NetCDF file.
#' @param wind_v10_data_path Path to the v10 NetCDF file.
#' @param mask_path          Path to the country mask NetCDF file, or \code{NULL}.
#' @param reference_period   Character vector \code{c("start", "end")}.
#' @param area               Logical. Spatial mean. Default \code{FALSE}.
#' @param season             Logical. Seasonal aggregation. Default \code{FALSE}.
#' @return Standardised wind metric (list or named vector).
#' @export
wind_component <- function(wind_u10_data_path, wind_v10_data_path,
                            mask_path = NULL, reference_period,
                            area = FALSE, season = FALSE) {
  u10 <- load_component(wind_u10_data_path, "u10", mask_path)
  v10 <- load_component(wind_v10_data_path, "v10", mask_path)

  freq <- calculate_period_wind_exceedance_frequency(u10, v10,
                                                      reference_period, season)
  standardize_metric(freq, reference_period, area)
}
