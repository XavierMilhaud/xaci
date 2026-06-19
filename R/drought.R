#' @title Drought Component of the ACI
#' @description Computes the drought component of the Actuarial Climate Index
#'   via maximum consecutive dry days (CDD).
#' @name drought
NULL

#' Calculate maximum consecutive dry days (CDD) per year
#'
#' A dry day is defined as total daily precipitation < 1 mm.  The function
#' returns the annual maximum number of consecutive dry days for each grid
#' cell.
#'
#' @param dataset List returned by \code{load_component()} for \code{tp}
#'   (total precipitation), with at least daily time resolution.
#' @return A list with \code{data} [lon × lat × years] and annual \code{time}.
#' @export
max_consecutive_dry_days <- function(dataset) {
  # Resample to daily totals first (in case sub-daily data provided)
  daily   <- resample_daily(dataset, FUN = sum)
  data    <- daily$data
  time    <- as.Date(daily$time)
  dims    <- dim(data)
  nl      <- dims[1]; nw <- dims[2]; nt <- dims[3]

  years      <- unique(as.integer(format(time, "%Y")))
  year_index <- as.integer(format(time, "%Y"))

  out <- array(NA_real_, c(nl, nw, length(years)))

  for (i in seq_len(nl)) {
    for (j in seq_len(nw)) {
      series <- data[i, j, ]
      # Dry day indicator (< 1 mm = 0.001 m)
      dry <- ifelse(series < 0.001, 1L, 0L)
      dry[is.na(series)] <- NA_integer_

      # Cumulative consecutive dry days (reset on wet days)
      cdd_series <- integer(nt)
      for (t in seq_len(nt)) {
        if (is.na(dry[t])) {
          cdd_series[t] <- NA_integer_
        } else if (dry[t] == 0L) {
          cdd_series[t] <- 0L
        } else {
          cdd_series[t] <- (if (t == 1L) 0L else
            ifelse(is.na(cdd_series[t - 1L]), 0L, cdd_series[t - 1L])) + 1L
        }
      }

      for (k in seq_along(years)) {
        idx_yr <- which(year_index == years[k])
        out[i, j, k] <- max(cdd_series[idx_yr], na.rm = TRUE)
      }
    }
  }

  annual_time <- as.POSIXct(paste0(years, "-12-31"), format = "%Y-%m-%d",
                             tz = "UTC")
  list(data = out, time = annual_time, lon = dataset$lon, lat = dataset$lat)
}

#' Linearly interpolate annual CDD to monthly resolution
#'
#' For each pair of consecutive years \eqn{k} and \eqn{k+1} the monthly CDD
#' for month \eqn{m \in 1..12} of year \eqn{k} is:
#' \deqn{CDD_m = \frac{12-m}{12} \cdot CDD_k + \frac{m}{12} \cdot CDD_{k+1}}
#' The last year repeats its own value for all 12 months.
#'
#' @param cdd_annual List returned by \code{max_consecutive_dry_days()}.
#' @return A list with \code{data} [lon × lat × months] and monthly \code{time}.
#' @export
drought_interpolate <- function(cdd_annual) {
  data  <- cdd_annual$data
  time  <- cdd_annual$time
  years <- as.integer(format(time, "%Y"))
  dims  <- dim(data)
  nl    <- dims[1]; nw <- dims[2]; ny <- dims[3]

  monthly_times <- c()
  monthly_list  <- list()

  for (k in seq_len(ny - 1L)) {
    for (m in 1:12) {
      w1      <- (12 - m) / 12
      w2      <- m / 12
      interp  <- w1 * data[, , k] + w2 * data[, , k + 1L]
      monthly_list[[length(monthly_list) + 1L]] <- interp
      monthly_times <- c(monthly_times,
                         as.POSIXct(sprintf("%d-%02d-01", years[k], m),
                                    format = "%Y-%m-%d", tz = "UTC"))
    }
  }

  # Last year: repeat last annual value for all 12 months
  for (m in 1:12) {
    monthly_list[[length(monthly_list) + 1L]] <- data[, , ny]
    monthly_times <- c(monthly_times,
                       as.POSIXct(sprintf("%d-%02d-01", years[ny], m),
                                  format = "%Y-%m-%d", tz = "UTC"))
  }

  # Stack into [lon x lat x months]
  out_data <- array(NA_real_, c(nl, nw, length(monthly_list)))
  for (k in seq_along(monthly_list)) out_data[, , k] <- monthly_list[[k]]

  list(data     = out_data,
       time     = monthly_times,
       lon      = cdd_annual$lon,
       lat      = cdd_annual$lat)
}

#' Calculate the drought component of the ACI
#'
#' Computes standardised consecutive dry days.
#'
#' @param precipitation_data_path Path to the precipitation NetCDF file.
#' @param mask_path               Path to the country mask NetCDF file, or
#'   \code{NULL}.
#' @param reference_period        Character vector \code{c("start", "end")}.
#' @param area                    Logical. Spatial mean. Default \code{FALSE}.
#' @return Standardised drought metric (list or named vector).
#' @export
drought_component <- function(precipitation_data_path,
                               mask_path        = NULL,
                               reference_period,
                               area             = FALSE) {
  dataset     <- load_component(precipitation_data_path, "tp", mask_path)
  cdd_annual  <- max_consecutive_dry_days(dataset)
  cdd_monthly <- drought_interpolate(cdd_annual)
  standardize_metric(cdd_monthly, reference_period, area)
}
