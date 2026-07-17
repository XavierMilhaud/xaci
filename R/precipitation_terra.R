#' @title Terra-based Precipitation Component
#' @description Memory-safe equivalents of
#'   \code{calculate_maximum_precipitation_over_window()} and
#'   \code{precipitation_component()}. Only the hourly-to-daily reduction
#'   runs through terra; the rolling-sum and monthly-max logic already
#'   operates on small, daily-resolution data, reused as-is via the shared
#'   helper \code{.max_precipitation_from_daily()} (defined in
#'   \code{precipitation.R}) -- no logic is duplicated.
#' @name precipitation_terra
NULL

#' Calculate maximum precipitation over a rolling window (terra version)
#'
#' @param r A \code{terra::SpatRaster}, hourly (or sub-daily) resolution
#'   precipitation, with \code{terra::time()} set. \strong{Non masque} : le
#'   masquage se fait ici, une fois les donnees reduites a la resolution
#'   journaliere (voir note ci-dessous).
#' @param mask_path Path to the mask NetCDF file, or \code{NULL} (no masking).
#' @param threshold Numeric threshold for the mask. Default \code{0.8}.
#' @inheritParams calculate_maximum_precipitation_over_window
#' @return Same structure as \code{calculate_maximum_precipitation_over_window()}.
#' @export
calculate_maximum_precipitation_over_window_terra <- function(r,
                                                              var_name    = "tp",
                                                              window_size = 5L,
                                                              mask_path   = NULL,
                                                              threshold   = 0.8) {
  daily_r <- resample_daily_terra(r, fun = "sum")

  # Masquage APRES reduction horaire -> journaliere (et non avant, sur les
  # donnees brutes) : le masque est purement spatial, identique a chaque pas
  # de temps, donc l'ordre ne change pas le resultat -- mais masquer ~12800
  # couches journalieres (35 ans) plutot que ~300000 couches horaires reste
  # sous la limite de 65535 couches de terra::mask() et evite le traitement
  # par blocs (bien plus lent) de apply_mask_terra().
  if (!is.null(mask_path)) {
    daily_r <- apply_mask_terra(daily_r, mask_path, threshold)
  }

  daily   <- .spatraster_to_list(daily_r)
  .max_precipitation_from_daily(daily, var_name, window_size)
}

#' Calculate the precipitation component of the ACI (terra version)
#'
#' Drop-in, memory-safe replacement for \code{precipitation_component()}.
#'
#' @inheritParams precipitation_component
#' @return Same as \code{precipitation_component()}.
#' @export
precipitation_component_terra <- function(precipitation_data_path,
                                          country_abbrev,
                                          reference_period,
                                          study_period,
                                          mask_path             = NULL,
                                          var_name              = "tp",
                                          window_size           = 5L,
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

  if (computed_components) {
    path <- file.path(load_dir, paste0("precipitation_", study_tag, ".rds"))
    if (!file.exists(path)) {
      stop("Cached file not found: ", path,
           "\nRun precipitation_component_terra() with save = TRUE first.")
    }
    period_max <- readRDS(path)
  } else {
    r <- load_netcdf_terra(precipitation_data_path, var_name)
    period_max <- calculate_maximum_precipitation_over_window_terra(
      r, var_name, window_size, mask_path = mask_path
    )
    if (save) {
      dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(period_max,
              file.path(save_dir, paste0("precipitation_", study_tag, ".rds")))
    }
  }

  # Resolution du masque admin : une SEULE couche suffit pour lon/lat.
  # load_netcdf_terra() est lazy et NON masque : inutile de masquer ici, le
  # masquage ne change ni les dimensions ni les coordonnees.
  if (is.null(admin_mask) && !is.null(admin_level)) {
    tmp      <- load_netcdf_terra(precipitation_data_path, var_name)
    tmp_list <- .spatraster_to_list(tmp[[1]])
    admin_mask <- build_admin_mask(tmp_list$lon, tmp_list$lat, country_abbrev,
                                   admin_level, crs_metric)
    rm(tmp, tmp_list)
  }

  if (is.null(admin_mask)) {
    return(standardize_metric(period_max, reference_period, area))
  }
  standardized <- standardize_metric(period_max, reference_period, area = FALSE)
  out <- reduce_dataarray_to_dataframe(standardized, column_name = "precipitation",
                                       admin_mask = admin_mask)
  effective_admin_level <- if (!is.null(admin_level)) admin_level else admin_mask$admin_level
  .attach_spatial_attrs(out,
                        country_abbrev = country_abbrev,
                        admin_level    = effective_admin_level,
                        crs_metric     = crs_metric)
}
