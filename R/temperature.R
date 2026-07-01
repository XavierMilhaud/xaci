#' @title Temperature Component of the ACI
#' @description Computes the temperature component of the Actuarial Climate
#'   Index.
#' @name temperature
NULL

#' Compute daily temperature extremum (min or max) for day or night hours
#'
#' @param dataset   List returned by \code{load_component()} for the \code{t2m}
#'   variable (sub-daily, hourly data expected).
#' @param extremum  \code{"min"} or \code{"max"}.
#' @param period    \code{"day"} (hours 6–21) or \code{"night"} (hours 0–5
#'   and 22–23).
#' @return A list with \code{data} [lon × lat × days] and daily \code{time}.
#' @export
temp_extremum <- function(dataset, extremum, period) {
  hours <- as.integer(format(dataset$time, "%H"))
  if (period == "day") {
    keep <- hours %in% 6:21
  } else if (period == "night") {
    keep <- hours %in% c(0:5, 22:23)
  } else {
    stop("'period' must be 'day' or 'night'")
  }

  sub <- dataset
  sub$data <- dataset$data[, , keep, drop = FALSE]
  sub$time <- dataset$time[keep]

  FUN <- if (extremum == "max") max else if (extremum == "min") min else
    stop("'extremum' must be 'min' or 'max'")

  resample_daily(sub, FUN = function(x, na.rm) FUN(x, na.rm = na.rm))
}

#' Compute temperature percentile thresholds for each day of year
#'
#' Uses a rolling window over the reference period followed by a
#' group by day-of-year percentile.
#'
#' @param dataset          Full sub-daily \code{t2m} dataset (list).
#' @param n                Percentile (e.g. 90 or 10).
#' @param reference_period Character vector \code{c("YYYY-MM-DD", "YYYY-MM-DD")}.
#' @param part_of_day      \code{"day"} or \code{"night"}.
#' @return A named numeric vector of length 366 (day-of-year 1–366).
#' @export
calculate_percentiles <- function(dataset, n, reference_period, part_of_day) {
  if (part_of_day == "day") {
    window_size <- 80L
    hours <- as.integer(format(dataset$time, "%H"))
    keep  <- hours %in% 6:21
  } else if (part_of_day == "night") {
    window_size <- 40L
    hours <- as.integer(format(dataset$time, "%H"))
    keep  <- hours %in% c(0:5, 22:23)
  } else {
    stop("'part_of_day' must be 'day' or 'night'")
  }

  ref_start <- as.POSIXct(reference_period[1], tz = "UTC")
  ref_end   <- as.POSIXct(reference_period[2], tz = "UTC")

  time_sub <- dataset$time[keep]
  data_sub <- dataset$data[, , keep, drop = FALSE]

  ref_mask <- time_sub >= ref_start & time_sub <= ref_end
  time_ref <- time_sub[ref_mask]
  data_ref <- data_sub[, , ref_mask, drop = FALSE]

  dims <- dim(data_ref)
  nl   <- dims[1]; nw <- dims[2]; nt <- dims[3]

  # Rolling percentile then group by day-of-year percentile
  # For each spatial cell, apply rolling window of size window_size then
  # aggregate by day to get 366 threshold values.
  day_ref <- as.integer(format(time_ref, "%j"))

  thresholds <- array(NA_real_, c(nl, nw, 366))

  for (i in seq_len(nl)) {
    for (j in seq_len(nw)) {
      series <- data_ref[i, j, ]
      if (all(is.na(series))) next
      # Rolling percentile (centred)
      rolled <- zoo::rollapply(series, width = window_size,
                               FUN = function(x) quantile(x, probs = n / 100,
                                                          na.rm = TRUE),
                               fill = NA, align = "center")
      # Percentile of rolled values per day
      for (d in 1:366) {
        idx <- which(day_ref == d)
        if (length(idx) == 0) next
        thresholds[i, j, d] <- quantile(rolled[idx], probs = n / 100,
                                        na.rm = TRUE)
      }
    }
  }
  thresholds  # [lon x lat x 366]
}

#' Calculate the half-day (day or night) temperature component
#'
#' @param dataset          Full sub-daily \code{t2m} dataset (list).
#' @param reference_period Character vector \code{c("YYYY-MM-DD", "YYYY-MM-DD")}.
#' @param part_of_day      \code{"day"} or \code{"night"}.
#' @param extremum         \code{"min"} or \code{"max"}.
#' @param percentile       Numeric percentile (e.g. 90 or 10).
#' @param above_thresholds Logical. \code{TRUE} counts days above the threshold
#'   (hot extremes); \code{FALSE} counts days below (cold extremes).
#' @return A list with \code{data} [lon × lat × months] and monthly \code{time}.
#' @export
calculate_halfday_component <- function(dataset, reference_period, part_of_day,
                                        extremum, percentile, above_thresholds) {
  # Daily extremum for the chosen part of day
  daily_ext <- temp_extremum(dataset, extremum, part_of_day)

  # Percentile thresholds [lon x lat x 366]
  thresholds_day <- calculate_percentiles(dataset, percentile,
                                          reference_period, part_of_day)

  day <- as.integer(format(as.Date(daily_ext$time), "%j"))
  dims <- dim(daily_ext$data)
  nl <- dims[1]; nw <- dims[2]; nt <- dims[3]

  # Binary: 1 if crossing threshold, 0 otherwise
  crossing <- array(0L, c(nl, nw, nt))
  for (t in seq_len(nt)) {
    thresh_t <- thresholds_day[, , day[t]]
    diff_t   <- daily_ext$data[, , t] - thresh_t
    if (above_thresholds) {
      crossing[, , t] <- ifelse(diff_t > 0, 1L, 0L)
    } else {
      crossing[, , t] <- ifelse(diff_t < 0, 1L, 0L)
    }
  }

  # Monthly frequency (sum / count)
  crossing_dataset <- list(data = crossing, time = daily_ext$time,
                           lon  = dataset$lon, lat = dataset$lat)
  monthly_sum   <- resample_monthly(crossing_dataset, FUN = sum)
  monthly_count <- resample_monthly(crossing_dataset,
                                    FUN = function(x, na.rm) length(x))

  freq_data <- monthly_sum$data / monthly_count$data
  list(data = freq_data, time = monthly_sum$time,
       lon = dataset$lon, lat = dataset$lat)
}

#' Calculate the full temperature component of the ACI
#'
#' Combines day and night half-day components (equal weighting), then
#' standardises relative to the reference period.
#'
#' @param temperature_data_path Path to the hourly \code{t2m} NetCDF file.
#' @param mask_path             Path to the country mask NetCDF file.
#' @param country_abbrev        Three-letter ISO country code.
#' @param reference_period      Character vector \code{c("start", "end")}.
#' @param percentile            Percentile for the threshold. Default \code{90}.
#' @param extremum              \code{"max"} (hot) or \code{"min"} (cold).
#' @param above_thresholds      Logical. Default \code{TRUE}.
#' @param area                  Logical. Default \code{FALSE}.
#' @param admin_level           Integer or \code{NULL}.
#' @param admin_mask            Output of \code{build_admin_mask()}, or
#'   \code{NULL}.
#' @param crs_metric            EPSG code. Default \code{4326}.
#' @param computed_components   Logical. Default \code{FALSE}.
#' @param save      Logical. Default \code{FALSE}.
#' @param save_dir  Character. Default \code{"results/<country_abbrev>"}.
#' @param load_dir  Character. Default \code{"results/<country_abbrev>"}.
#' @return Named numeric vector, standardised list, or \code{data.frame}
#'   per admin unit.
#' @export
temperature_component <- function(temperature_data_path,
                                  mask_path             = NULL,
                                  country_abbrev,
                                  reference_period,
                                  percentile            = 90,
                                  extremum              = "max",
                                  above_thresholds      = TRUE,
                                  area                  = FALSE,
                                  admin_level           = NULL,
                                  admin_mask            = NULL,
                                  crs_metric            = 4326,
                                  computed_components   = FALSE,
                                  save                  = FALSE,
                                  save_dir              = paste0("results/", country_abbrev),
                                  load_dir              = paste0("results/", country_abbrev)) {

  ref_tag <- paste(substr(reference_period[1], 1, 4),
                   substr(reference_period[2], 1, 4), sep = "_")
  label   <- paste0("temperature_t", as.integer(percentile))

  if (computed_components) {
    path <- file.path(load_dir, paste0(label, "_", ref_tag, ".rds"))
    if (!file.exists(path))
      stop("Cached file not found: ", path,
           "\nRun temperature_component() with save = TRUE first.")
    combined <- readRDS(path)
  } else {
    dataset      <- load_component(temperature_data_path, "t2m", mask_path)
    dataset$data <- dataset$data - 273.15   # Kelvin -> Celsius

    day_comp   <- calculate_halfday_component(dataset, reference_period, "day",
                                              extremum, percentile,
                                              above_thresholds)
    night_comp <- calculate_halfday_component(dataset, reference_period, "night",
                                              extremum, percentile,
                                              above_thresholds)
    combined <- list(
      data = 0.5 * (day_comp$data + night_comp$data),
      time = day_comp$time,
      lon  = dataset$lon,
      lat  = dataset$lat
    )

    if (save) {
      dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(combined,
              file.path(save_dir, paste0(label, "_", ref_tag, ".rds")))
    }
  }

  # Résolution du masque admin
  if (is.null(admin_mask) && !is.null(admin_level)) {
    tmp        <- load_component(temperature_data_path, "t2m", mask_path)
    admin_mask <- build_admin_mask(tmp$lon, tmp$lat, country_abbrev,
                                   admin_level, crs_metric)
    rm(tmp)
  }

  if (is.null(admin_mask)) {
    return(standardize_metric(combined, reference_period, area))
  }
  col_prefix   <- sprintf("t%d", as.integer(percentile))
  standardized <- standardize_metric(combined, reference_period, area = FALSE)
  reduce_dataarray_to_dataframe(standardized, column_name = col_prefix,
                                admin_mask = admin_mask)
}
