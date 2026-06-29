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
#' @param var_name    Variable name in \code{dataset}. Default \code{"tp"}.
#' @param window_size Rolling window in days. Default \code{5}.
#' @return A list with \code{data} [lon x lat x months] and \code{time}.
#' @export
calculate_maximum_precipitation_over_window <- function(dataset,
                                                        var_name    = "tp",
                                                        window_size = 5L) {
  rolling <- calculate_rolling_sum(dataset, var_name, window_size)
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
    out[, , k] <- apply(data[, , idx, drop = FALSE], c(1, 2),
                        max, na.rm = TRUE)
  }
  out[out == -Inf] <- NA

  period_dates <- as.POSIXct(paste0(periods, "-01"),
                             format = "%Y-%m-%d", tz = "UTC")

  list(data = out, time = period_dates, lon = dataset$lon, lat = dataset$lat)
}

#' Calculate the precipitation component of the ACI
#'
#' Computes the standardised anomaly of maximum monthly precipitation over a
#' rolling 5-day window.
#'
#' @param precipitation_data_path Path to the precipitation NetCDF file.
#' @param mask_path               Path to the country mask NetCDF file, or
#'   \code{NULL}.
#' @param reference_period        Character vector \code{c("start", "end")}.
#' @param var_name                Variable name in the NetCDF. Default
#'   \code{"tp"}.
#' @param window_size             Rolling window in days. Default \code{5}.
#' @param area                    Logical. If \code{TRUE} return national
#'   spatial mean as a named numeric vector. Ignored when \code{admin_mask}
#'   is not \code{NULL}. Default \code{FALSE}.
#' @param admin_mask              Output of \code{build_admin_mask()}, or
#'   \code{NULL} (default) for national behaviour.
#' @param save      Logical. If \code{TRUE}, saves the grid-cell-level object
#'   to \code{save_dir} before aggregation. Default \code{FALSE}.
#' @param save_dir  Character. Directory for the cached \code{.rds} file.
#'   Created if it does not exist. Default \code{"results/<country_abbrev>"}.
#' @return If \code{admin_mask} is \code{NULL} and \code{area = TRUE}: a named
#'   numeric vector (standardised monthly values).
#'   If \code{admin_mask} is \code{NULL} and \code{area = FALSE}: a list with
#'   \code{data} [lon x lat x months] and \code{time}.
#'   If \code{admin_mask} is not \code{NULL}: a \code{data.frame} with one
#'   column \code{precipitation_<unit>} per administrative unit, indexed by
#'   month-start dates.
#' @export
precipitation_component <- function(precipitation_data_path,
                                    mask_path        = NULL,
                                    reference_period,
                                    var_name         = "tp",
                                    window_size      = 5L,
                                    area             = FALSE,
                                    admin_mask       = NULL,
                                    save             = FALSE,
                                    save_dir         = paste("results/",
                                      strsplit(precipitation_data_path, "/", fixed = TRUE)[[1]][3],
                                      sep = "")) {

  dataset    <- load_component(precipitation_data_path, var_name, mask_path)
  period_max <- calculate_maximum_precipitation_over_window(dataset, var_name,
                                                            window_size)

  if (save) {
    ref_tag <- paste(substr(reference_period[1], 1, 4),
                     substr(reference_period[2], 1, 4), sep = "_")
    dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
    saveRDS(period_max, file.path(save_dir, paste0("precipitation_", ref_tag, ".rds")))
  }

  # --- Country level ---
  if (is.null(admin_mask)) {
    return(standardize_metric(period_max, reference_period, area))
  }

  # --- Administrative level specified ---
  standardized <- standardize_metric(period_max, reference_period, area = FALSE)
  reduce_dataarray_to_dataframe(standardized, column_name = "precipitation",
                                admin_mask = admin_mask)
}
