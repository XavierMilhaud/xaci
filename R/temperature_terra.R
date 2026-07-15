#' @title Terra-based Temperature Component
#' @description Memory-safe equivalents of \code{calculate_halfday_component()}
#'   and \code{temperature_component()}, for full-resolution, grid-cell-level
#'   workflows over long historical periods (see \code{R/component_terra.R}
#'   for the underlying loading/reduction primitives).
#'
#'   Only the hourly-scale steps (\code{temp_extremum_terra()},
#'   \code{calculate_percentiles_terra()}) run through terra. Once reduced to
#'   daily/monthly resolution, the exact same \code{.crossing_frequency()}
#'   helper used by the base-R pipeline takes over -- no logic is duplicated.
#' @name temperature_terra
NULL

#' Calculate the half-day (day or night) temperature component (terra version)
#'
#' @param r A \code{terra::SpatRaster}, hourly resolution, temperature
#'   already in the desired unit (e.g. Celsius), with \code{terra::time()} set.
#' @inheritParams calculate_halfday_component
#' @return Same structure as \code{calculate_halfday_component()}:
#'   \code{list(data, time, lon, lat)}.
#' @export
calculate_halfday_component_terra <- function(r, reference_period, part_of_day,
                                              extremum, percentile,
                                              above_thresholds) {
  daily_ext_r  <- temp_extremum_terra(r, extremum, part_of_day)
  thresholds_r <- calculate_percentiles_terra(r, percentile, reference_period,
                                              part_of_day)

  daily_ext      <- .spatraster_to_list(daily_ext_r)          # [lon x lat x jours]
  thresholds_day <- .spatraster_to_array_only(thresholds_r)   # [lon x lat x 366]

  .crossing_frequency(daily_ext, thresholds_day, above_thresholds,
                      daily_ext$lon, daily_ext$lat)
}

#' Compute the temperature ACI component (terra version)
#'
#' Drop-in, memory-safe replacement for \code{temperature_component()},
#' intended for full-resolution, grid-cell-level ("area = FALSE") workflows
#' over long historical periods (40+ years hourly).
#'
#' @inheritParams temperature_component
#' @return Same as \code{temperature_component()}: a standardized metric list,
#'   or a data frame if \code{admin_mask}/\code{admin_level} is provided.
#' @export
temperature_component_terra <- function(temperature_data_path,
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
                                        save_dir              = paste0("results/", country_abbrev),
                                        load_dir              = paste0("results/", country_abbrev)) {

  study_tag <- paste(substr(study_period[1], 1, 4),
                     substr(study_period[2], 1, 4), sep = "_")
  label     <- paste0("temperature_t", as.integer(percentile))

  if (computed_components) {
    path <- file.path(load_dir, paste0(label, "_", study_tag, ".rds"))
    if (!file.exists(path)) {
      stop("Cached file not found: ", path,
           "\nRun temperature_component_terra() with save = TRUE first.")
    }
    combined <- readRDS(path)
  } else {
    r <- load_component_terra(temperature_data_path, "t2m", mask_path)
    r <- r - 273.15   # Kelvin -> Celsius ; reste paresseux (SpatRaster)

    day_comp   <- calculate_halfday_component_terra(r, reference_period, "day",
                                                    extremum, percentile,
                                                    above_thresholds)
    night_comp <- calculate_halfday_component_terra(r, reference_period, "night",
                                                    extremum, percentile,
                                                    above_thresholds)
    combined <- list(
      data = 0.5 * (day_comp$data + night_comp$data),
      time = day_comp$time,
      lon  = day_comp$lon,
      lat  = day_comp$lat
    )

    if (save) {
      dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(combined,
              file.path(save_dir, paste0(label, "_", study_tag, ".rds")))
    }
  }

  # Resolution du masque admin : une SEULE couche suffit pour recuperer
  # lon/lat (contrairement a la version base-R qui rechargeait tout le cube
  # horaire une seconde fois rien que pour ca).
  if (is.null(admin_mask) && !is.null(admin_level)) {
    tmp      <- load_component_terra(temperature_data_path, "t2m", mask_path)
    tmp_list <- .spatraster_to_list(tmp[[1]])
    admin_mask <- build_admin_mask(tmp_list$lon, tmp_list$lat, country_abbrev,
                                   admin_level, crs_metric)
    rm(tmp, tmp_list)
  }

  if (is.null(admin_mask)) {
    return(standardize_metric(combined, reference_period, area))
  }
  col_prefix   <- sprintf("t%d", as.integer(percentile))
  standardized <- standardize_metric(combined, reference_period, area = FALSE)
  reduce_dataarray_to_dataframe(standardized, column_name = col_prefix,
                                admin_mask = admin_mask)
}
