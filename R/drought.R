#' @title Drought Component of the ACI
#' @description Computes the drought component of the Actuarial Climate Index
#'   via maximum consecutive dry days (CDD).
#' @name drought
NULL

#' Calculate maximum consecutive dry days (CDD) per year
#'
#' A dry day is defined as total daily precipitation < 1 mm.  The function
#' returns the annual maximum number of consecutive dry days for each grid
#' cell.
#'
#' @param dataset List returned by \code{load_component()} for \code{tp}
#'   (total precipitation), with at least daily time resolution.
#' @return A list with \code{data} [lon × lat × years] and annual \code{time}.
#' @export
max_consecutive_dry_days <- function(dataset) {
  # Resample to daily totals first (in case sub-daily data provided)
  daily <- resample_daily(dataset, FUN = sum)
  .max_consecutive_dry_days_from_daily(daily)
}

#' Compute annual max consecutive dry days from an already-daily series
#'
#' Shared helper behind \code{max_consecutive_dry_days()} (base-R) and its
#' terra equivalent. Operates exclusively on DAILY-resolution precipitation
#' totals (small, even for 40+ years of data).
#'
#' @param daily List \code{list(data, time, lon, lat)}, daily resolution
#'   (e.g. from \code{resample_daily(dataset, FUN = sum)}).
#' @return A list with \code{data} [lon x lat x years] and annual \code{time}.
#' @keywords internal
.max_consecutive_dry_days_from_daily <- function(daily) {
  data    <- daily$data
  time    <- as.Date(daily$time)
  dims    <- dim(data)
  nl      <- dims[1]; nw <- dims[2]; nt <- dims[3]

  years      <- unique(as.integer(format(time, "%Y")))
  year_index <- as.integer(format(time, "%Y"))

  out <- array(NA_real_, c(nl, nw, length(years)))

  for (i in seq_len(nl)) {
    for (j in seq_len(nw)) {
      series <- data[i, j, ]
      # Dry day indicator (< 1 mm = 0.001 m)
      dry <- ifelse(series < 0.001, 1L, 0L)
      dry[is.na(series)] <- NA_integer_

      # Cumulative consecutive dry days (reset on wet days)
      cdd_series <- integer(nt)
      for (t in seq_len(nt)) {
        if (is.na(dry[t])) {
          cdd_series[t] <- NA_integer_
        } else if (dry[t] == 0L) {
          cdd_series[t] <- 0L
        } else {
          cdd_series[t] <- (if (t == 1L) 0L else
            ifelse(is.na(cdd_series[t - 1L]), 0L, cdd_series[t - 1L])) + 1L
        }
      }

      for (k in seq_along(years)) {
        idx_yr <- which(year_index == years[k])
        out[i, j, k] <- max(cdd_series[idx_yr], na.rm = TRUE)
      }
    }
  }

  annual_time <- as.POSIXct(paste0(years, "-12-31"), format = "%Y-%m-%d",
                            tz = "UTC")
  list(data = out, time = annual_time, lon = daily$lon, lat = daily$lat)
}

#' Linearly interpolate annual CDD to monthly resolution
#'
#' For each pair of consecutive years \eqn{k} and \eqn{k+1} the monthly CDD
#' for month \eqn{m \in 1..12} of year \eqn{k} is:
#' \deqn{CDD_m = \frac{12-m}{12} \cdot CDD_k + \frac{m}{12} \cdot CDD_{k+1}}
#' The last year repeats its own value for all 12 months.
#'
#' @param cdd_annual List returned by \code{max_consecutive_dry_days()}.
#' @return A list with \code{data} [lon × lat × months] and monthly \code{time}.
#' @export
drought_interpolate <- function(cdd_annual) {
  data  <- cdd_annual$data
  time  <- cdd_annual$time
  years <- as.integer(format(time, "%Y"))
  dims  <- dim(data)
  nl    <- dims[1]; nw <- dims[2]; ny <- dims[3]

  monthly_strings <- character(0)   # ← stocke les dates comme strings
  monthly_list    <- list()

  for (k in seq_len(ny - 1L)) {
    for (m in 1:12) {
      w1     <- (12 - m) / 12
      w2     <- m / 12
      interp <- w1 * data[, , k] + w2 * data[, , k + 1L]
      monthly_list[[length(monthly_list) + 1L]] <- interp
      monthly_strings <- c(monthly_strings,
                           sprintf("%d-%02d-01", years[k], m))
    }
  }

  # Dernière année
  for (m in 1:12) {
    monthly_list[[length(monthly_list) + 1L]] <- data[, , ny]
    monthly_strings <- c(monthly_strings,
                         sprintf("%d-%02d-01", years[ny], m))
  }

  # Conversion unique à la fin → pas de perte de type
  monthly_times <- as.POSIXct(monthly_strings, format = "%Y-%m-%d", tz = "UTC")

  out_data <- array(NA_real_, c(nl, nw, length(monthly_list)))
  for (k in seq_along(monthly_list)) out_data[, , k] <- monthly_list[[k]]

  list(data = out_data, time = monthly_times,
       lon  = cdd_annual$lon, lat = cdd_annual$lat)
}

#' Calculate the drought component of the ACI
#'
#' Computes standardised consecutive dry days.
#'
#' @param precipitation_data_path Path to the precipitation NetCDF file.
#' @param country_abbrev          Three-letter ISO country code (e.g.
#'   \code{"FRA"}). Used to build \code{save_dir}/\code{load_dir} defaults
#'   and to construct the admin mask when \code{admin_level} is not
#'   \code{NULL}.
#' @param reference_period        Character vector \code{c("start", "end")}.
#' @param study_period             Character vector \code{c("start", "end")}.
#'   Full period covered by the study; used to name the cached grid-cell-level
#'   \code{.rds} file (e.g. \code{"drought_1980_2020.rds"}).
#' @param mask_path               Path to the country mask NetCDF file, or
#'   \code{NULL}.
#' @param area                    Logical. If \code{TRUE} return national
#'   spatial mean as a named numeric vector. Ignored when \code{admin_level}
#'   or \code{admin_mask} is not \code{NULL}. Default \code{FALSE}.
#' @param admin_level             Integer or \code{NULL}. If not \code{NULL},
#'   builds the admin mask internally and returns one column per unit.
#'   Ignored when \code{admin_mask} is supplied directly.
#' @param admin_mask              Output of \code{build_admin_mask()}, or
#'   \code{NULL}. When supplied, takes precedence over \code{admin_level}
#'   (no rebuild).
#' @param crs_metric              EPSG code for the metric CRS used when
#'   building the admin mask. Default \code{4326}.
#' @param computed_components     Logical. If \code{TRUE}, reloads a
#'   previously saved \code{.rds} file from \code{load_dir} instead of
#'   recomputing. Default \code{FALSE}.
#' @param save      Logical. Default \code{FALSE}.
#' @param save_dir  Character. Default \code{"results/<country_abbrev>"}.
#' @param load_dir  Character. Default \code{"results/<country_abbrev>"}.
#' @return Named numeric vector (\code{area = TRUE}), standardised list
#'   (\code{area = FALSE}), or \code{data.frame} per admin unit.
#' @export
drought_component <- function(precipitation_data_path,
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
    if (!file.exists(path))
      stop("Cached file not found: ", path,
           "\nRun drought_component() with save = TRUE first.")
    cdd_monthly <- readRDS(path)
  } else {
    dataset     <- load_component(precipitation_data_path, "tp", mask_path)
    cdd_annual  <- max_consecutive_dry_days(dataset)
    cdd_monthly <- drought_interpolate(cdd_annual)

    if (save) {
      dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(cdd_monthly,
              file.path(save_dir, paste0("drought_", study_tag, ".rds")))
    }
  }

  # Résolution du masque admin
  if (is.null(admin_mask) && !is.null(admin_level)) {
    tmp        <- load_component(precipitation_data_path, "tp", mask_path)
    admin_mask <- build_admin_mask(tmp$lon, tmp$lat, country_abbrev,
                                   admin_level, crs_metric)
    rm(tmp)
  }

  if (is.null(admin_mask)) {
    return(standardize_metric(cdd_monthly, reference_period, area))
  }
  standardized <- standardize_metric(cdd_monthly, reference_period, area = FALSE)
  reduce_dataarray_to_dataframe(standardized, column_name = "drought",
                                admin_mask = admin_mask)
}
