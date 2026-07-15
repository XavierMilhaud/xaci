#' @title Terra-based Drought Component
#' @description Memory-safe equivalents of \code{max_consecutive_dry_days()}
#'   and \code{drought_component()}. Only the hourly-to-daily reduction runs
#'   through terra; \code{drought_interpolate()} and the consecutive-dry-day
#'   scan already operate on small, annual/daily-resolution data, so they are
#'   reused as-is via the shared helper
#'   \code{.max_consecutive_dry_days_from_daily()} (defined in
#'   \code{drought.R}) -- no logic is duplicated.
#' @name drought_terra
NULL

#' Calculate maximum consecutive dry days (terra version)
#'
#' @param r A \code{terra::SpatRaster}, hourly (or sub-daily) resolution
#'   precipitation, with \code{terra::time()} set.
#' @return Same structure as \code{max_consecutive_dry_days()}.
#' @export
max_consecutive_dry_days_terra <- function(r) {
  daily_r <- resample_daily_terra(r, fun = "sum")
  daily   <- .spatraster_to_list(daily_r)
  .max_consecutive_dry_days_from_daily(daily)
}

#' Calculate the drought component of the ACI (terra version)
#'
#' Drop-in, memory-safe replacement for \code{drought_component()}.
#'
#' @inheritParams drought_component
#' @return Same as \code{drought_component()}.
#' @export
drought_component_terra <- function(precipitation_data_path,
                                    country_abbrev,
                                    reference_period,
                                    study_period,
                                    mask_path             = NULL,
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
    path <- file.path(load_dir, paste0("drought_", study_tag, ".rds"))
    if (!file.exists(path)) {
      stop("Cached file not found: ", path,
           "\nRun drought_component_terra() with save = TRUE first.")
    }
    cdd_monthly <- readRDS(path)
  } else {
    r          <- load_component_terra(precipitation_data_path, "tp", mask_path)
    cdd_annual <- max_consecutive_dry_days_terra(r)
    cdd_monthly <- drought_interpolate(cdd_annual)

    if (save) {
      dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(cdd_monthly,
              file.path(save_dir, paste0("drought_", study_tag, ".rds")))
    }
  }

  # Resolution du masque admin : une SEULE couche suffit pour lon/lat.
  if (is.null(admin_mask) && !is.null(admin_level)) {
    tmp      <- load_component_terra(precipitation_data_path, "tp", mask_path)
    tmp_list <- .spatraster_to_list(tmp[[1]])
    admin_mask <- build_admin_mask(tmp_list$lon, tmp_list$lat, country_abbrev,
                                   admin_level, crs_metric)
    rm(tmp, tmp_list)
  }

  if (is.null(admin_mask)) {
    return(standardize_metric(cdd_monthly, reference_period, area))
  }
  standardized <- standardize_metric(cdd_monthly, reference_period, area = FALSE)
  reduce_dataarray_to_dataframe(standardized, column_name = "drought",
                                admin_mask = admin_mask)
}
