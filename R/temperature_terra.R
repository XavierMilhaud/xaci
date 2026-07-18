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
#'   \strong{Non masque} : le masquage se fait ici, une fois les donnees
#'   reduites (voir note ci-dessous).
#' @param mask_path Path to the mask NetCDF file, or \code{NULL} (no masking).
#' @param threshold Numeric threshold for the mask. Default \code{0.8}.
#' @param cores Passed to \code{calculate_percentiles_terra()}'s own
#'   \code{cores} (see its performance note) -- the rolling-window percentile
#'   step is by far the most expensive part of this function. Default
#'   \code{1} (sequential, identical to previous behaviour).
#' @inheritParams calculate_halfday_component
#' @return Same structure as \code{calculate_halfday_component()}:
#'   \code{list(data, time, lon, lat)}.
#' @export
calculate_halfday_component_terra <- function(r, reference_period, part_of_day,
                                              extremum, percentile,
                                              above_thresholds,
                                              mask_path = NULL, threshold = 0.8,
                                              cores = 1L) {
  daily_ext_r  <- temp_extremum_terra(r, extremum, part_of_day)
  thresholds_r <- calculate_percentiles_terra(r, percentile, reference_period,
                                              part_of_day, cores = cores)

  # Conversion Kelvin -> Celsius APRES reduction (et non sur r brut, hourly,
  # en amont) : max/min et quantile sont tous deux INVARIANTS PAR
  # TRANSLATION (max(x - c) = max(x) - c ; quantile(x - c, p) = quantile(x, p)
  # - c), donc le resultat final est rigoureusement identique -- mais
  # soustraire une constante sur r brut (~219000 couches pour 25 ans)
  # declenche la MEME limite interne de terra que terra::mask() ("[-] cannot
  # write more than 65535 layers"), alors que daily_ext_r (~12800 couches) et
  # thresholds_r (366 couches) restent tous deux largement en dessous.
  daily_ext_r  <- daily_ext_r  - 273.15
  thresholds_r <- thresholds_r - 273.15

  # Masquage APRES reduction (et non sur r brut, hourly, en amont) : le
  # masque est purement spatial (identique a chaque pas de temps), donc
  # l'ordre ne change pas le resultat -- mais daily_ext_r (~12800 couches
  # pour 35 ans) et thresholds_r (366 couches) restent tous deux largement
  # sous la limite de 65535 couches de terra::mask(), contrairement a r brut
  # (~300000 couches horaires) qui la depasse.
  if (!is.null(mask_path)) {
    daily_ext_r  <- apply_mask_terra(daily_ext_r, mask_path, threshold)
    thresholds_r <- apply_mask_terra(thresholds_r, mask_path, threshold)
  }

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
#' @param cores Passed through to \code{calculate_halfday_component_terra()} /
#'   \code{calculate_percentiles_terra()} (see its performance note on
#'   \code{terra::roll()} having no built-in parallelism). Default \code{1}.
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
                                        cores                 = 1L,
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
    r <- load_netcdf_terra(temperature_data_path, "t2m")
    # Kelvin -> Celsius : deplace dans calculate_halfday_component_terra(),
    # APRES reduction temporelle (voir la note associee la-bas) -- r reste
    # ici en Kelvin, ce qui n'a aucune incidence puisque max/min et quantile
    # sont invariants par translation.

    day_comp   <- calculate_halfday_component_terra(r, reference_period, "day",
                                                    extremum, percentile,
                                                    above_thresholds, mask_path,
                                                    cores = cores)
    night_comp <- calculate_halfday_component_terra(r, reference_period, "night",
                                                    extremum, percentile,
                                                    above_thresholds, mask_path,
                                                    cores = cores)
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
  # horaire une seconde fois rien que pour ca). load_netcdf_terra() est lazy
  # et NON masque : inutile de masquer ici, le masquage ne change ni les
  # dimensions ni les coordonnees.
  if (is.null(admin_mask) && !is.null(admin_level)) {
    tmp      <- load_netcdf_terra(temperature_data_path, "t2m")
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
  out <- reduce_dataarray_to_dataframe(standardized, column_name = col_prefix,
                                       admin_mask = admin_mask)
  effective_admin_level <- if (!is.null(admin_level)) admin_level else admin_mask$admin_level
  .attach_spatial_attrs(out,
                        country_abbrev = country_abbrev,
                        admin_level    = effective_admin_level,
                        crs_metric     = crs_metric)
}
