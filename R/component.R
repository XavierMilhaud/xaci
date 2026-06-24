#' @title Base Climate Component Helpers
#' @description Low-level helpers shared by all ACI component functions.
#' @name component
NULL

#' Load a NetCDF dataset and optionally apply a country mask
#'
#' This function is a constructor.
#' It reads a NetCDF variable and, if \code{mask_path} is provided, zeroes out
#' (sets to \code{NA}) all grid cells where the mask value is below
#' \code{threshold}.
#'
#' @param data_path Path to the NetCDF data file.
#' @param var_name  Name of the primary variable to load.
#' @param mask_path Path to the mask NetCDF file, or \code{NULL} (default).
#' @param threshold Numeric threshold for the mask. Default \code{0.8}.
#' @return A list with elements \code{data} (3-D array [lon × lat × time]),
#'   \code{lon}, \code{lat}, \code{time} (POSIXct vector), and
#'   \code{var_name}.
#' @export
load_component <- function(data_path, var_name, mask_path = NULL,
                           threshold = 0.8) {
  dataset <- load_netcdf(data_path, var_name)
  if (!is.null(mask_path)) {
    dataset <- apply_mask(dataset, mask_path, var_name, threshold)
  }
  dataset
}

#' Calculate the rolling sum of a climate variable
#'
#' Computes a rolling (sliding-window) sum along the time dimension of a
#' 3-D climate array.
#'
#' @param dataset   List returned by \code{load_component()}.
#' @param var_name  Variable name (for documentation; the array in
#'   \code{dataset$data} is used directly).
#' @param window_size Integer number of time steps for the rolling window.
#' @return A list with the same structure as \code{dataset} but with
#'   \code{data} replaced by the rolling sum (leading incomplete windows are
#'   \code{NA}).
#' @export
#' @importFrom zoo rollsum
calculate_rolling_sum <- function(dataset, var_name, window_size) {
  data <- dataset$data
  dims <- dim(data)
  nl <- dims[1]; nw <- dims[2]; nt <- dims[3]

  out <- array(NA_real_, dims)
  for (i in seq_len(nl)) {
    for (j in seq_len(nw)) {
      series <- data[i, j, ]
      if (all(is.na(series))) next
      rs <- zoo::rollsum(series, k = window_size, fill = NA, align = "right")
      out[i, j, ] <- rs
    }
  }
  dataset$data <- out
  dataset
}

#' Resample a 3-D climate array to daily resolution
#'
#' Aggregates sub-daily data to daily values using the supplied function
#' (\code{sum} or \code{mean}).
#'
#' @param dataset  List returned by \code{load_component()} (may have
#'   sub-daily time steps).
#' @param FUN      Aggregation function: \code{sum} or \code{mean}.
#' @return A new list with daily \code{time} and aggregated \code{data}.
#' @export
resample_daily <- function(dataset, FUN = sum) {
  time  <- as.Date(dataset$time)
  data  <- dataset$data
  dims  <- dim(data)
  days  <- unique(time)

  out <- array(NA_real_, c(dims[1], dims[2], length(days)))
  for (k in seq_along(days)) {
    idx <- which(time == days[k])
    if (length(idx) == 1L) {
      out[, , k] <- data[, , idx]
    } else {
      out[, , k] <- apply(data[, , idx, drop = FALSE], c(1, 2), FUN,
                          na.rm = TRUE)
    }
  }
  out[out == -Inf] <- NA
  dataset$data <- out
  dataset$time <- as.POSIXct(days, tz = "UTC")
  dataset
}

#' Resample a 3-D or 1-D climate series to monthly resolution
#'
#' Applies \code{FUN} within each calendar month.
#'
#' @param data  Either a 3-D array \code{[lon × lat × time]} or a numeric
#'   vector of length \emph{T}.
#' @param time  POSIXct vector of length \emph{T}.
#' @param FUN   Aggregation function. Default \code{mean}.
#' @return A list with \code{data} (monthly aggregates) and \code{time}
#'   (first day of each month as POSIXct).
#' @export
resample_monthly <- function(data, time, FUN = mean) {
  month_key <- format(time, "%Y-%m")
  months    <- unique(month_key)
  is_3d     <- length(dim(data)) == 3

  if (is_3d) {
    dims <- dim(data)
    out  <- array(NA_real_, c(dims[1], dims[2], length(months)))
    for (k in seq_along(months)) {
      idx <- which(month_key == months[k])
      out[, , k] <- apply(data[, , idx, drop = FALSE], c(1, 2), FUN,
                          na.rm = TRUE)
    }
  } else {
    out <- vapply(months, function(m) FUN(data[month_key == m], na.rm = TRUE),
                  numeric(1))
  }
  out[out == -Inf] <- NA

  list(
    data = out,
    time = as.POSIXct(paste0(months, "-01"), format = "%Y-%m-%d", tz = "UTC")
  )
}
