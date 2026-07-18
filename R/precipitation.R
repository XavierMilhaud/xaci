#' @title Precipitation Component of the ACI
#' @description Computes the precipitation component of the Actuarial Climate
#'   Index.
#' @name precipitation
NULL

#' Calculate maximum precipitation over a rolling window
#'
#' Computes the rolling \code{window_size}-day sum of precipitation, then
#' takes the monthly maximum.
#'
#' @param dataset     List returned by \code{load_component()} for \code{tp}.
#'                    May be sub-daily (e.g. hourly ERA5 data); it is always
#'                    resampled to daily totals first.
#' @param var_name    Variable name in \code{dataset}. Default \code{"tp"},
#'                    inherited from ERA5 (variable name in the NetCDF).
#' @param window_size Rolling window in DAYS. Default \code{5}.
#' @return A list with \code{data} [lon x lat x months] and \code{time}.
#' @export
calculate_maximum_precipitation_over_window <- function(dataset,
                                                        var_name    = "tp",
                                                        window_size = 5L) {
  # BUGFIX : window_size est documente comme un nombre de JOURS, mais
  # calculate_rolling_sum() applique zoo::rollsum() directement sur la
  # resolution native des donnees. Avec des donnees horaires (ex. ERA5),
  # window_size = 5 roulait sur 5 HEURES au lieu de 5 jours. On resample
  # donc explicitement en totaux journaliers d'abord (comme le fait deja
  # max_consecutive_dry_days() dans drought.R), afin que window_size soit
  # bien interprete en jours quelle que soit la resolution native du fichier.
  daily <- resample_daily(dataset, FUN = sum)
  .max_precipitation_from_daily(daily, var_name, window_size)
}

#' Compute monthly max rolling precipitation from an already-daily series
#'
#' Shared helper behind \code{calculate_maximum_precipitation_over_window()}
#' (base-R) and its terra equivalent. Operates exclusively on DAILY-resolution
#' precipitation totals (small, even for 40+ years of data).
#'
#' @param daily       List \code{list(data, time, lon, lat)}, daily
#'   resolution (e.g. from \code{resample_daily(dataset, FUN = sum)}).
#' @param var_name    Passed through to \code{calculate_rolling_sum()}.
#' @param window_size Rolling window in days.
#' @return A list with \code{data} [lon x lat x months] and \code{time}.
#' @keywords internal
.max_precipitation_from_daily <- function(daily, var_name, window_size) {
  rolling <- calculate_rolling_sum(daily, var_name, window_size)
  data    <- rolling$data
  time    <- rolling$time
  dims    <- dim(data)
  nl      <- dims[1]; nw <- dims[2]

  # Toujours agréger au mois
  key     <- format(time, "%Y-%m")
  periods <- unique(key)

  out <- array(NA_real_, c(nl, nw, length(periods)))
  for (k in seq_along(periods)) {
    idx        <- which(key == periods[k])
    out[, , k] <- suppressWarnings(
      apply(data[, , idx, drop = FALSE], c(1, 2), max, na.rm = TRUE)
    )
  }
  # Seul max() est utilise ici, donc seul -Inf pouvait apparaitre en
  # pratique -- is.infinite() est garde par coherence avec resample_daily()/
  # resample_monthly() (component.R), au cas ou cette fonction serait un
  # jour generalisee a d'autres FUN (ex. min).
  out[is.infinite(out)] <- NA

  period_dates <- as.POSIXct(paste0(periods, "-01"),
                             format = "%Y-%m-%d", tz = "UTC")

  list(data = out, time = period_dates, lon = daily$lon, lat = daily$lat)
}

#' Calculate the precipitation component of the ACI
#'
#' Computes the standardised anomaly of maximum monthly precipitation over a
#' rolling 5-day window.
#'
#' @param precipitation_data_path Path to the precipitation NetCDF file.
#' @param country_abbrev          Three-letter ISO country code.
#' @param reference_period        Character vector \code{c("start", "end")}.
#' @param study_period            Character vector \code{c("start", "end")}.
#'   Full period covered by the study; used to name the cached grid-cell-level
#'   \code{.rds} file (e.g. \code{"precipitation_1980_2020.rds"}).
#' @param mask_path               Path to the country mask NetCDF file, or
#'   \code{NULL}.
#' @param var_name                Variable name in the NetCDF. Default
#'   \code{"tp"}.
#' @param window_size             Rolling window in days. Default \code{5}.
#' @param area                    Logical. Default \code{FALSE}.
#' @param admin_level             Integer or \code{NULL}.
#' @param admin_mask              Output of \code{build_admin_mask()}, or
#'   \code{NULL}.
#' @param crs_metric              EPSG code. Default \code{4326}.
#' @param computed_components     Logical. Default \code{FALSE}.
#' @param save      Logical. Default \code{FALSE}.
#' @param save_dir  Character. Default \code{"results/<country_abbrev>"}.
#' @param load_dir  Character. Default \code{"results/<country_abbrev>"}.
#' @return Named numeric vector, standardised list, or \code{data.frame}
#'   per admin unit.
#' @export
precipitation_component <- function(precipitation_data_path,
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
    if (!file.exists(path))
      stop("Cached file not found: ", path,
           "\nRun precipitation_component() with save = TRUE first.")
    period_max <- readRDS(path)
  } else {
    dataset    <- load_component(precipitation_data_path, var_name, mask_path)
    period_max <- calculate_maximum_precipitation_over_window(dataset, var_name,
                                                              window_size)
    if (save) {
      dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(period_max,
              file.path(save_dir, paste0("precipitation_", study_tag, ".rds")))
    }
  }

  # Résolution du masque admin
  if (is.null(admin_mask) && !is.null(admin_level)) {
    tmp        <- load_component(precipitation_data_path, var_name, mask_path)
    admin_mask <- build_admin_mask(tmp$lon, tmp$lat, country_abbrev,
                                   admin_level, crs_metric)
    rm(tmp)
  }

  if (is.null(admin_mask)) {
    return(standardize_metric(period_max, reference_period, area))
  }
  standardized <- standardize_metric(period_max, reference_period, area = FALSE)
  out <- reduce_dataarray_to_dataframe(standardized, column_name = "precipitation",
                                       admin_mask = admin_mask)
  # admin_level peut ne pas avoir ete fourni ici si admin_mask a ete
  # construit ailleurs et transmis directement (cf. build_admin_mask()) :
  # on retombe alors sur celui memorise dans admin_mask lui-meme, pour que
  # plot_aci_map() puisse determiner country_abbrev/admin_level sans que
  # l'utilisateur ait besoin de les repreciser.
  effective_admin_level <- if (!is.null(admin_level)) admin_level else admin_mask$admin_level
  .attach_spatial_attrs(out,
                        country_abbrev = country_abbrev,
                        admin_level    = effective_admin_level,
                        crs_metric     = crs_metric)
}
