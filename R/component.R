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
#' @keywords internal
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
  # is.infinite() attrape -Inf (ex. max() sur une journee entierement NA) ET
  # +Inf (ex. min() sur une journee entierement NA) -- l'ancienne version ne
  # convertissait que -Inf en NA, laissant filer un +Inf avec FUN = min.
  out[is.infinite(out)] <- NA
  dataset$data <- out
  dataset$time <- as.POSIXct(days, tz = "UTC")
  dataset
}

#' Resample a 3-D or 1-D climate series to monthly resolution
#'
#' Applies \code{FUN} within each calendar month.
#'
#' @param dataset List returned by \code{load_component()} or any component
#'   function, with fields \code{data} (3-D array \code{[lon x lat x time]}
#'   or numeric vector) and \code{time} (POSIXct vector).
#' @param FUN Aggregation function. Default \code{mean}.
#' @return A list with \code{data} (monthly aggregates) and \code{time}
#'   (first day of each month as POSIXct).
#' @keywords internal
resample_monthly <- function(dataset, FUN = mean) {
  data      <- dataset$data
  time      <- dataset$time
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
  # Voir la note equivalente dans resample_daily() : is.infinite() attrape
  # -Inf ET +Inf, contrairement a l'ancien test == -Inf.
  out[is.infinite(out)] <- NA

  list(
    data = out,
    time = as.POSIXct(paste0(months, "-01"), format = "%Y-%m-%d", tz = "UTC")
  )
}


#' Aggregate a 3-D climate array along the time dimension
#'
#' @param data  Numeric array \code{[nl x nw x nt]}.
#' @param time  POSIXct vector of length \code{nt}.
#' @param granularity One of \code{"month"}, \code{"season"},
#'   \code{"semester"}, \code{"year"}.
#' @param FUN  Aggregation function. Default \code{mean}.
#' @return A list with \code{data} (array \code{[nl x nw x nt_agg]}),
#'   and \code{time} (character vector of period labels, same convention
#'   as \code{aggregate_granularity()}).
#' @keywords internal
.aggregate_granularity_array <- function(data, time,
                                         granularity = "month",
                                         FUN = mean) {
  nl <- dim(data)[1]
  nw <- dim(data)[2]
  nt <- dim(data)[3]

  # Construire les clés de période (même logique que aggregate_granularity)
  keys <- switch(granularity,
                 month    = format(time, "%Y-%m"),
                 year     = format(time, "%Y"),
                 semester = {
                   yr  <- as.integer(format(time, "%Y"))
                   mo  <- as.integer(format(time, "%m"))
                   sem <- ifelse(mo <= 6L, 1L, 2L)
                   sprintf("%d-S%d", yr, sem)
                 },
                 season = {
                   yr  <- as.integer(format(time, "%Y"))
                   mo  <- as.integer(format(time, "%m"))
                   # Convention météorologique : DJF, MAM, JJA, SON
                   # Décembre est attribué à l'année suivante
                   season_key <- c("01" = "DJF", "02" = "DJF", "03" = "MAM",
                                   "04" = "MAM", "05" = "MAM", "06" = "JJA",
                                   "07" = "JJA", "08" = "JJA", "09" = "SON",
                                   "10" = "SON", "11" = "SON", "12" = "DJF")
                   seas <- season_key[sprintf("%02d", mo)]
                   yr_adj <- ifelse(mo == 12L, yr + 1L, yr)
                   sprintf("%d-%s", yr_adj, seas)
                 },
                 stop("'granularity' must be one of: 'month', 'year', 'semester', 'season'.")
  )

  periods <- unique(keys)
  nt_agg  <- length(periods)
  out     <- array(NA_real_, c(nl, nw, nt_agg))

  for (k in seq_along(periods)) {
    idx <- which(keys == periods[k])
    if (length(idx) == 1L) {
      out[, , k] <- data[, , idx]
    } else {
      out[, , k] <- apply(data[, , idx, drop = FALSE], c(1L, 2L),
                          FUN, na.rm = TRUE)
    }
  }

  list(data = out, time = periods)
}
