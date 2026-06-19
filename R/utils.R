#' @title Utility Functions for ACI Package
#' @description Helper functions to manipulate and merge climate data.
#' @name utils
NULL

#' Load a NetCDF variable as a 3D array [lon x lat x time]
#'
#' @param path Path to the NetCDF file.
#' @param var_name Name of the variable to extract.
#' @return A list with fields: \code{data} (3D array), \code{lon}, \code{lat}, \code{time}.
#' @export
#' @importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
load_netcdf <- function(path, var_name) {
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))

  data <- ncdf4::ncvar_get(nc, var_name)
  lon  <- ncdf4::ncvar_get(nc, "longitude")
  lat  <- ncdf4::ncvar_get(nc, "latitude")

  time_raw  <- ncdf4::ncvar_get(nc, "time")
  time_atts <- ncdf4::ncatt_get(nc, "time")
  time_unit <- time_atts$units

  # Parse time units: "hours since YYYY-MM-DD" or "days since ..."
  origin <- sub(".*(since )(.+)", "\\2", time_unit)
  unit   <- trimws(sub(" since.*", "", time_unit))

  if (grepl("hour", unit)) {
    time <- as.POSIXct(origin, tz = "UTC") + time_raw * 3600
  } else if (grepl("day", unit)) {
    time <- as.POSIXct(origin, tz = "UTC") + time_raw * 86400
  } else {
    stop("Unsupported time unit: ", time_unit)
  }

  list(data = data, lon = lon, lat = lat, time = time, var_name = var_name)
}

#' Apply a country mask to a NetCDF data array
#'
#' Sets grid cells to NA where the mask value is below \code{threshold}.
#'
#' @param dataset List returned by \code{load_netcdf()}.
#' @param mask_path Path to the mask NetCDF file (variable: \code{country}).
#' @param var_name Name of the variable inside \code{dataset}.
#' @param threshold Numeric threshold; cells with mask < threshold become NA. Default 0.8.
#' @return The dataset list with \code{data} masked in-place.
#' @export
apply_mask <- function(dataset, mask_path, var_name, threshold = 0.8) {
  mask_nc <- ncdf4::nc_open(mask_path)
  on.exit(ncdf4::nc_close(mask_nc))

  mask_vals <- ncdf4::ncvar_get(mask_nc, "country")  # [lon x lat]
  country_mask <- mask_vals >= threshold              # logical [lon x lat]

  data <- dataset$data
  # Broadcast mask over time dimension
  nt <- dim(data)[3]
  for (t in seq_len(nt)) {
    slice <- data[, , t]
    slice[!country_mask] <- NA
    data[, , t] <- slice
  }
  dataset$data <- data
  dataset
}

#' Standardise a monthly metric relative to a reference period
#'
#' For each calendar month, computes the mean and standard deviation over the
#' reference period, then returns (x - mean) / sd. Optionally averages over
#' the spatial dimensions first.
#'
#' @param metric A list with fields \code{data} [lon x lat x time] or [time],
#'   \code{time}, and optionally \code{lon}/\code{lat}.
#' @param reference_period Character vector of length 2: start and end dates
#'   (\code{"YYYY-MM-DD"}).
#' @param area Logical. If \code{TRUE} the spatial mean is computed before
#'   standardising and a named numeric vector is returned. Default \code{FALSE}.
#' @return If \code{area = TRUE}: a named numeric vector (names = dates).
#'   Otherwise: a list with the same structure as \code{metric} but standardised
#'   values.
#' @export
standardize_metric <- function(metric, reference_period, area = FALSE) {
  ref_start <- as.POSIXct(reference_period[1], tz = "UTC")
  ref_end   <- as.POSIXct(reference_period[2], tz = "UTC")

  time   <- metric$time
  data   <- metric$data
  is_3d  <- length(dim(data)) == 3

  # Optional spatial averaging
  if (area && is_3d) {
    # Collapse lon x lat -> mean over non-NA cells
    nt   <- dim(data)[3]
    data <- vapply(seq_len(nt), function(t) mean(data[, , t], na.rm = TRUE),
                   numeric(1))
    is_3d <- FALSE
  }

  months_all <- as.integer(format(time, "%m"))
  ref_mask   <- time >= ref_start & time <= ref_end
  ref_months <- months_all[ref_mask]

  # Compute reference monthly stats
  if (is_3d) {
    nl <- dim(data)[1]; nw <- dim(data)[2]
    ref_mean <- array(NA_real_, c(nl, nw, 12))
    ref_sd   <- array(NA_real_, c(nl, nw, 12))
    for (m in 1:12) {
      idx <- which(ref_mask & months_all == m)
      if (length(idx) == 0) next
      slices       <- data[, , idx, drop = FALSE]
      ref_mean[, , m] <- apply(slices, c(1, 2), mean, na.rm = TRUE)
      ref_sd[, , m]   <- apply(slices, c(1, 2), sd,   na.rm = TRUE)
    }
    # Standardise
    out <- array(NA_real_, dim(data))
    for (t in seq_along(time)) {
      m         <- months_all[t]
      out[, , t] <- (data[, , t] - ref_mean[, , m]) / ref_sd[, , m]
    }
    metric$data <- out
    return(metric)
  } else {
    ref_mean_v <- tapply(data[ref_mask], ref_months, mean, na.rm = TRUE)
    ref_sd_v   <- tapply(data[ref_mask], ref_months, sd,   na.rm = TRUE)
    out <- (data - ref_mean_v[months_all]) / ref_sd_v[months_all]
    names(out) <- format(time, "%Y-%m-%d")
    return(out)
  }
}

#' Reduce a standardised spatial metric to a data frame
#'
#' Averages over lon/lat and returns a one-column \code{data.frame} with the
#' month-end date as row names.
#'
#' @param metric List with fields \code{data} [lon x lat x time] or numeric
#'   vector, and \code{time}.
#' @param column_name Character. Name to give the column. Default \code{"value"}.
#' @return A \code{data.frame} with one column and \code{Date} row names aligned
#'   to month-end.
#' @export
#' @importFrom lubridate ceiling_date as_date
reduce_dataarray_to_dataframe <- function(metric, column_name = "value") {
  data <- metric$data
  time <- metric$time

  if (length(dim(data)) == 3) {
    nt   <- dim(data)[3]
    vals <- vapply(seq_len(nt), function(t) mean(data[, , t], na.rm = TRUE),
                   numeric(1))
  } else {
    vals <- as.numeric(data)
  }

  # Align to month-end (equivalent to pandas MonthEnd offset)
  dates <- as.Date(time)
  month_end <- lubridate::ceiling_date(dates, "month") - 1

  df <- data.frame(vals, row.names = as.character(month_end))
  colnames(df) <- column_name
  df
}

#' Reduce sea-level data frame to a single regional mean column
#'
#' @param df A \code{data.frame} where each column is a tide-gauge station
#'   (already standardised) and row names are \code{Date} values.
#' @return A one-column \code{data.frame} named \code{"sealevel"} indexed by
#'   month-end dates.
#' @export
#' @importFrom lubridate ceiling_date
reduce_sealevel_over_region <- function(df) {
  sea_mean <- rowMeans(df, na.rm = TRUE)
  dates     <- as.Date(rownames(df))
  month_end <- lubridate::ceiling_date(dates, "month") - 1
  out       <- data.frame(sealevel = sea_mean, row.names = as.character(month_end))
  out
}

#' Merge a list of data frames on their row-name index
#'
#' @param dataframes A list of \code{data.frame} objects sharing a common row-
#'   name index.
#' @return A single merged \code{data.frame}.
#' @export
merge_dataframes <- function(dataframes) {
  Reduce(function(left, right) {
    merge(left, right, by = "row.names", all = FALSE) |>
      (\(d) { rownames(d) <- d$Row.names; d[, -1] })()
  }, dataframes)
}

#' Load the bundled PSMSL station metadata
#'
#' @return A \code{data.frame} of PSMSL tide-gauge station information.
#' @export
#' @importFrom readr read_csv
load_psmsl_data <- function() {
  path <- system.file("extdata", "psmsl_data.csv", package = "ACI")
  if (nchar(path) == 0) stop("psmsl_data.csv not found in package installation.")
  readr::read_csv(path, show_col_types = FALSE)
}
