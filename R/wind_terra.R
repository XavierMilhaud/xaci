#' @title Terra-based Wind Component
#' @description Memory-safe equivalents of \code{wind_power()} and
#'   \code{wind_component()}. Only \code{wind_power_terra()} touches raw
#'   hourly data (via terra); \code{wind_thresholds()}'s internal logic,
#'   threshold-crossing, and monthly aggregation already operate on DAILY
#'   resolution data (small even for 40+ years), so they are reused as-is via
#'   the shared helpers \code{.wind_thresholds_from_wp()},
#'   \code{.days_above_from_wp()}, \code{.monthly_frequency_from_binary()}
#'   defined in \code{wind.R} -- no logic is duplicated.
#' @name wind_terra
NULL

#' Calculate daily wind power from u10 and v10 components (terra version)
#'
#' Terra-based equivalent of \code{wind_power()}: computes daily mean u/v via
#' \code{resample_daily_terra()} (block-by-block, never loading the full
#' hourly cube in RAM), then wind speed/power arithmetic directly on the
#' (now daily, small) SpatRasters before converting back to the package's
#' list format.
#'
#' @param u10_r,v10_r \code{terra::SpatRaster}, hourly resolution, with
#'   \code{terra::time()} set.
#' @inheritParams wind_power
#' @return Same structure as \code{wind_power()}: \code{list(data, time, lon, lat)}.
#' @export
wind_power_terra <- function(u10_r, v10_r, reference_period = NULL) {
  rho <- 1.23  # air density kg/m3

  u_daily_r <- resample_daily_terra(u10_r, fun = "mean")
  v_daily_r <- resample_daily_terra(v10_r, fun = "mean")

  ws_daily_r <- sqrt(u_daily_r^2 + v_daily_r^2)
  wp_r       <- 0.5 * rho * ws_daily_r^3

  wp <- .spatraster_to_list(wp_r)   # deja journalier -> petit, on repasse en liste

  if (!is.null(reference_period)) {
    ref_start <- as.POSIXct(reference_period[1], tz = "UTC")
    ref_end   <- as.POSIXct(reference_period[2], tz = "UTC")
    keep      <- wp$time >= ref_start & wp$time <= ref_end
    wp$data   <- wp$data[, , keep, drop = FALSE]
    wp$time   <- wp$time[keep]
  }
  wp
}

#' Calculate monthly wind exceedance frequency (terra version)
#'
#' Drop-in, memory-safe replacement for
#' \code{calculate_period_wind_exceedance_frequency()}. Only the initial
#' \code{wind_power_terra()} call differs from the base-R version; the
#' threshold, crossing, and monthly-aggregation steps reuse the exact same
#' helpers, since they already operate on small, daily-resolution data.
#'
#' @param u10_r,v10_r \code{terra::SpatRaster}, hourly resolution.
#' @inheritParams calculate_period_wind_exceedance_frequency
#' @return Same as \code{calculate_period_wind_exceedance_frequency()}.
#' @export
calculate_period_wind_exceedance_frequency_terra <- function(u10_r, v10_r,
                                                              reference_period) {
  wp    <- wind_power_terra(u10_r, v10_r)
  thr   <- .wind_thresholds_from_wp(wp, reference_period, wp$lon, wp$lat)
  above <- .days_above_from_wp(wp, thr, wp$lon, wp$lat)
  .monthly_frequency_from_binary(above, wp$lon, wp$lat)
}

#' Calculate the wind component of the ACI (terra version)
#'
#' Drop-in, memory-safe replacement for \code{wind_component()}, intended for
#' full-resolution, grid-cell-level ("area = FALSE") workflows over long
#' historical periods (40+ years hourly).
#'
#' @inheritParams wind_component
#' @return Same as \code{wind_component()}.
#' @export
wind_component_terra <- function(wind_u10_data_path,
                                 wind_v10_data_path,
                                 country_abbrev,
                                 reference_period,
                                 mask_path             = NULL,
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

  if (computed_components) {
    path <- file.path(load_dir, paste0("wind_", ref_tag, ".rds"))
    if (!file.exists(path)) {
      stop("Cached file not found: ", path,
          "\nRun wind_component_terra() with save = TRUE first.")
    }
    freq <- readRDS(path)
  } else {
    u10_r <- load_component_terra(wind_u10_data_path, "u10", mask_path)
    v10_r <- load_component_terra(wind_v10_data_path, "v10", mask_path)
    freq  <- calculate_period_wind_exceedance_frequency_terra(u10_r, v10_r,
                                                              reference_period)
    if (save) {
      dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(freq, file.path(save_dir, paste0("wind_", ref_tag, ".rds")))
    }
  }

  # Resolution du masque admin : une SEULE couche suffit pour lon/lat (voir
  # la meme optimisation dans temperature_component_terra()).
  if (is.null(admin_mask) && !is.null(admin_level)) {
    tmp      <- load_component_terra(wind_u10_data_path, "u10", mask_path)
    tmp_list <- .spatraster_to_list(tmp[[1]])
    admin_mask <- build_admin_mask(tmp_list$lon, tmp_list$lat, country_abbrev,
                                   admin_level, crs_metric)
    rm(tmp, tmp_list)
  }

  if (is.null(admin_mask)) {
    return(standardize_metric(freq, reference_period, area))
  }
  standardized <- standardize_metric(freq, reference_period, area = FALSE)
  reduce_dataarray_to_dataframe(standardized, column_name = "wind",
                                admin_mask = admin_mask)
}
