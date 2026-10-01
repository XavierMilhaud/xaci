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
#' Uses a rolling window (in \strong{days}) over the reference period,
#' followed by a group-by-day-of-year percentile. Matches the CLIMDEX / ACI
#' methodology (see \code{ACI (2018)}, Appendix A of Garrido et al.): the
#' threshold for a given calendar day is the \code{n}-th percentile of the
#' \strong{daily} TX/TN values falling within a window of \code{window_days}
#' days (default 5, i.e. \eqn{\pm}{+/-}2 days) around that calendar day,
#' pooled across all years of the reference period.
#'
#' \strong{Bug fix #1 (see NEWS):} earlier versions of this function computed
#' the rolling window/percentile on \strong{hourly} \code{t2m} values (with
#' \code{window_size} expressed in hours: 80 for day, 40 for night), then
#' compared the resulting threshold to a \strong{daily} extremum in
#' \code{.crossing_frequency()}. Since the daily max (or min) of ~16 (or 8)
#' hourly values almost always exceeds (or falls below) the 90th (or 10th)
#' percentile of the marginal hourly distribution, this made the exceedance
#' frequency during the reference period itself massively higher than the
#' intended ~10% (observed ~70-85% on synthetic data), instead of recovering
#' the reference frequency approximately by construction. \code{calculate_percentiles()}
#' now takes the already-reduced \strong{daily} extremum series (the output
#' of \code{temp_extremum()}) as input, so the threshold and the tested
#' variable are computed from the same daily quantity.
#'
#' \strong{Bug fix #2 (see NEWS):} even after bug fix #1, the threshold was
#' still computed as a \strong{percentile of rolling percentiles}: a
#' \code{window_days}-wide rolling \code{n}-th percentile was first applied
#' along the daily series, and a \strong{second} \code{n}-th percentile was
#' then taken of those already-extreme rolled values, grouped by
#' day-of-year across reference years. This two-stage procedure is
#' systematically biased: taking the \code{n}-th percentile of a set of
#' local \code{n}-th percentiles pushes the final threshold further into the
#' tail than a single, direct percentile would, so it under-counts the true
#' exceedance frequency (observed ~5.6% instead of ~10% on synthetic data,
#' even with bug fix #1 alone applied). The standard CLIMDEX/ACI methodology
#' instead \strong{pools the raw daily values} that fall within
#' \code{window_days} of a given calendar day, across \strong{all} years of
#' the reference period, into a single sample, and takes \strong{one}
#' percentile of that pooled sample. \code{calculate_percentiles()} now
#' follows this pooling approach (no more rolling window / \code{zoo}
#' dependency for this function), which empirically recovers the intended
#' ~10% exceedance frequency on the reference period itself.
#'
#' @param daily_dataset    List with \code{data} [lon x lat x days] and daily
#'   \code{time}, as returned by \code{temp_extremum()} -- i.e. the daily
#'   TX (for T90) or TN (for T10) series, \strong{not} the raw hourly
#'   dataset.
#' @param n                Percentile (e.g. 90 or 10).
#' @param reference_period Character vector \code{c("YYYY-MM-DD", "YYYY-MM-DD")}.
#' @param window_days      Width, in days, of the window used to pool
#'   neighbouring calendar days before taking the percentile (e.g. 5 pools
#'   \eqn{\pm}{+/-}2 days around each calendar day). Default \code{5L},
#'   matching the standard CLIMDEX/ACI convention.
#' @return A numeric array \code{[lon x lat x 366]} (day-of-year 1-366).
#' @export
calculate_percentiles <- function(daily_dataset, n, reference_period,
                                  window_days = 5L) {
  ref_start <- as.POSIXct(reference_period[1], tz = "UTC")
  ref_end   <- as.POSIXct(reference_period[2], tz = "UTC")

  time_all <- daily_dataset$time
  data_all <- daily_dataset$data

  ref_mask <- time_all >= ref_start & time_all <= ref_end
  time_ref <- time_all[ref_mask]
  data_ref <- data_all[, , ref_mask, drop = FALSE]

  dims <- dim(data_ref)
  nl   <- dims[1]; nw <- dims[2]; nt <- dims[3]

  day_ref <- as.integer(format(time_ref, "%j"))
  half    <- window_days %/% 2

  thresholds <- array(NA_real_, c(nl, nw, 366))

  for (i in seq_len(nl)) {
    for (j in seq_len(nw)) {
      series <- data_ref[i, j, ]
      if (all(is.na(series))) next
      for (d in 1:366) {
        occ <- which(day_ref == d)
        if (length(occ) == 0) next
        # Pool the RAW daily values within +/- half calendar days of EVERY
        # occurrence of day-of-year d across the reference period (i.e. the
        # actual consecutive dates in the time series, correctly spanning
        # year boundaries via clamping at the very start/end of the whole
        # series), then take a SINGLE percentile of that pooled sample --
        # the standard CLIMDEX/ACI approach, instead of a percentile of
        # rolling percentiles (see bug fix #2 above).
        window_idx <- unique(unlist(lapply(occ, function(k) {
          lo <- max(1L, k - half); hi <- min(nt, k + half)
          lo:hi
        })))
        thresholds[i, j, d] <- quantile(series[window_idx], probs = n / 100,
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
#' @param window_days      Passed to \code{calculate_percentiles()}: width,
#'   in days, of the rolling window used to compute the percentile
#'   threshold. Default \code{5L}.
#' @return A list with \code{data} [lon × lat × months] and monthly \code{time}.
#' @export
calculate_halfday_component <- function(dataset, reference_period, part_of_day,
                                        extremum, percentile, above_thresholds,
                                        window_days = 5L) {
  # Daily extremum for the chosen part of day -- computed ONCE, then reused
  # both as the tested variable and as the basis for the percentile
  # threshold, so both are guaranteed to be the same quantity.
  daily_ext <- temp_extremum(dataset, extremum, part_of_day)

  # Percentile thresholds [lon x lat x 366], computed from the DAILY
  # extremum series (not from raw hourly values -- see calculate_percentiles()).
  thresholds_day <- calculate_percentiles(daily_ext, percentile,
                                          reference_period,
                                          window_days = window_days)

  .crossing_frequency(daily_ext, thresholds_day, above_thresholds,
                      dataset$lon, dataset$lat)
}

#' Compute monthly threshold-crossing frequency from daily extrema
#'
#' Shared helper behind \code{calculate_halfday_component()} (base-R) and
#' \code{calculate_halfday_component_terra()} (terra). Kept independent of
#' how \code{daily_ext}/\code{thresholds_day} were produced, so both loading
#' paths reuse the exact same, already-tested logic.
#'
#' @param daily_ext      List with \code{data} [lon x lat x days] and daily
#'   \code{time}, as returned by \code{temp_extremum()}/\code{temp_extremum_terra()}
#'   (converted to list form).
#' @param thresholds_day Array \code{[lon x lat x 366]}, as returned by
#'   \code{calculate_percentiles()}/\code{calculate_percentiles_terra()}
#'   (converted to array form).
#' @param above_thresholds Logical. \code{TRUE} counts days above the
#'   threshold (hot extremes); \code{FALSE} counts days below (cold
#'   extremes).
#' @param lon,lat Coordinate vectors, carried through to the output.
#' @return A list with \code{data} [lon x lat x months] and monthly
#'   \code{time}.
#' @keywords internal
.crossing_frequency <- function(daily_ext, thresholds_day, above_thresholds,
                                lon, lat) {
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
                           lon  = lon, lat = lat)
  monthly_sum   <- resample_monthly(crossing_dataset, FUN = sum)
  monthly_count <- resample_monthly(crossing_dataset,
                                    FUN = function(x, na.rm) length(x))

  freq_data <- monthly_sum$data / monthly_count$data
  list(data = freq_data, time = monthly_sum$time, lon = lon, lat = lat)
}

#' Calculate the full temperature component of the ACI
#'
#' Combines day and night half-day components (equal weighting), then
#' standardises relative to the reference period.
#'
#' @param temperature_data_path Path to the hourly \code{t2m} NetCDF file.
#' @param country_abbrev        Three-letter ISO country code.
#' @param reference_period      Character vector \code{c("start", "end")}.
#'   Climatological baseline used for standardisation.
#' @param study_period          Character vector \code{c("start", "end")}.
#'   Full period covered by the study; used to name the cached grid-cell-level
#'   \code{.rds} file (e.g. \code{"temperature_t90_1980_2020.rds"}), so that
#'   distinct runs over different study windows don't collide or get mixed up.
#' @param mask_path             Path to the country mask NetCDF file.
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
#' @param save_dir  Character. Default \code{NULL}, which resolves to a sub-directory of \code{tempdir()}.
#' @param load_dir  Character. Default \code{NULL}, which resolves to a sub-directory of \code{tempdir()}.
#' @param window_days Passed to \code{calculate_percentiles()}: width, in
#'   days, of the rolling window used to compute the percentile threshold.
#'   Default \code{5L}.
#' @return Named numeric vector, standardised list, or \code{data.frame}
#'   per admin unit.
#' @export
temperature_component <- function(temperature_data_path,
                                  country_abbrev,
                                  reference_period,
                                  study_period,
                                  mask_path             = NULL,
                                  percentile            = 90,
                                  extremum              = "max",
                                  above_thresholds      = TRUE,
                                  area                  = FALSE,
                                  admin_level           = NULL,
                                  admin_mask            = NULL,
                                  crs_metric            = 4326,
                                  computed_components   = FALSE,
                                  save                  = FALSE,
                                  save_dir              = NULL,
                                  load_dir              = NULL,
                                  window_days           = 5L) {

  save_dir <- .resolve_cache_dir(save_dir, file.path("xaci_results", country_abbrev))
  load_dir <- .resolve_cache_dir(load_dir, file.path("xaci_results", country_abbrev))

  study_tag <- paste(substr(study_period[1], 1, 4), substr(reference_period[2], 1, 4),
                     substr(study_period[2], 1, 4), sep = "_")
  label     <- paste0("temperature_t", as.integer(percentile))

  if (computed_components) {
    path <- file.path(load_dir, paste0(label, "_", study_tag, ".rds"))
    if (!file.exists(path))
      stop("Cached file not found: ", path,
           "\nRun temperature_component() with save = TRUE first.")
    combined <- readRDS(path)
  } else {
    dataset      <- load_component(temperature_data_path, "t2m", mask_path)
    dataset$data <- dataset$data - 273.15   # Kelvin -> Celsius

    day_comp   <- calculate_halfday_component(dataset, reference_period, "day",
                                              extremum, percentile,
                                              above_thresholds,
                                              window_days = window_days)
    night_comp <- calculate_halfday_component(dataset, reference_period, "night",
                                              extremum, percentile,
                                              above_thresholds,
                                              window_days = window_days)
    combined <- list(
      data = 0.5 * (day_comp$data + night_comp$data),
      time = day_comp$time,
      lon  = dataset$lon,
      lat  = dataset$lat
    )

    if (save) {
      dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(combined,
              file.path(save_dir, paste0(label, "_", study_tag, ".rds")))
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
  out <- reduce_dataarray_to_dataframe(standardized, column_name = col_prefix,
                                       admin_mask = admin_mask)
  effective_admin_level <- if (!is.null(admin_level)) admin_level else admin_mask$admin_level
  .attach_spatial_attrs(out,
                        country_abbrev = country_abbrev,
                        admin_level    = effective_admin_level,
                        crs_metric     = crs_metric)
}
