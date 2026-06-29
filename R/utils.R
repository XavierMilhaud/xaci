#' @title Utility Functions for ACI Package
#' @description Helper functions to manipulate and merge climate data.
#' @name utils
NULL

# Internal lookup table: ISO-3 code -> English country name as expected by
# rnaturalearth::ne_states(). Only countries relevant to the ACI are listed;
# extend as needed.
.iso3_to_ne_name <- c(
  AUS = "Australia",
  AUT = "Austria",
  BEL = "Belgium",
  CAN = "Canada",
  CHE = "Switzerland",
  CZE = "Czechia",
  DEU = "Germany",
  DNK = "Denmark",
  ESP = "Spain",
  FIN = "Finland",
  FRA = "France",
  GBR = "United Kingdom",
  GRC = "Greece",
  HUN = "Hungary",
  IRL = "Ireland",
  ITA = "Italy",
  LUX = "Luxembourg",
  NLD = "Netherlands",
  NOR = "Norway",
  NZL = "New Zealand",
  POL = "Poland",
  PRT = "Portugal",
  ROU = "Romania",
  SVK = "Slovakia",
  SVN = "Slovenia",
  SWE = "Sweden",
  USA = "United States of America"
)

#' Convert an ISO-3 country code to its English name for rnaturalearth
#'
#' Internal helper used by \code{build_admin_mask()} and
#' \code{assign_sealevel_to_admin()} to translate \code{country_abbrev}
#' (ISO-3, e.g. \code{"FRA"}) into the English name expected by
#' \code{rnaturalearth::ne_states()}.
#'
#' @param iso3 A single ISO-3 character string (case-insensitive).
#' @return The corresponding English country name.
#' @keywords internal
.iso3_to_country_name <- function(iso3) {
  iso3 <- toupper(trimws(iso3))
  name <- .iso3_to_ne_name[iso3]
  if (is.na(name))
    stop(
      "Unknown ISO-3 code: '", iso3, "'. ",
      "Please add it to the .iso3_to_ne_name table in utils.R."
    )
  unname(name)
}

#' Build standard ERA5 file paths from country and years
#'
#' Internal helper that reconstructs the paths produced by
#' \code{download_era5_all()} and \code{download_mask()}.
#'
#' @param country_abbrev ISO-3 country code (e.g. \code{"FRA"}).
#' @param years          Integer vector of years (e.g. \code{2011:2015}).
#' @param base_dir       Root data directory. Default \code{"data/era5"}.
#' @return A named list with elements \code{t2m}, \code{tp}, \code{u10},
#'   \code{v10}, and \code{mask}.
#' @keywords internal
.build_era5_paths <- function(country_abbrev, years, base_dir = "data/era5") {
  iso   <- toupper(country_abbrev)
  period <- paste(years[1], years[length(years)], sep = "_")
  era5_dir <- file.path(base_dir, iso)
  list(
    t2m      = file.path(era5_dir, sprintf("t2m_%s.nc",  period)),
    tp       = file.path(era5_dir, sprintf("tp_%s.nc",   period)),
    u10      = file.path(era5_dir, sprintf("u10_%s.nc",  period)),
    v10      = file.path(era5_dir, sprintf("v10_%s.nc",  period)),
    mask     = file.path(base_dir, iso, sprintf("mask_%s.nc", iso))
  )
}

#' Load a NetCDF variable as a 3D array [lon x lat x time]
#'
#' @param path Path to the NetCDF file.
#' @param var_name Name of the variable to extract.
#' @return A list with fields: \code{data} (3D array), \code{lon}, \code{lat}, \code{time}.
#' @export
#' @importFrom ncdf4 nc_open nc_close ncvar_get ncatt_get
load_netcdf <- function(path, var_name) {
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))

  data <- ncdf4::ncvar_get(nc, var_name)
  lon  <- ncdf4::ncvar_get(nc, "longitude")
  lat  <- ncdf4::ncvar_get(nc, "latitude")

  time_raw  <- ncdf4::ncvar_get(nc, "time")
  time_atts <- ncdf4::ncatt_get(nc, "time")
  time_unit <- time_atts$units

  # Parse time units: "hours since YYYY-MM-DD" or "days since ..."
  origin <- sub(".*(since )(.+)", "\\2", time_unit)
  unit   <- trimws(sub(" since.*", "", time_unit))

  if (grepl("hour", unit)) {
    time <- as.POSIXct(origin, tz = "UTC") + time_raw * 3600
  } else if (grepl("day", unit)) {
    time <- as.POSIXct(origin, tz = "UTC") + time_raw * 86400
  } else {
    stop("Unsupported time unit: ", time_unit)
  }

  list(data = data, lon = lon, lat = lat, time = time, var_name = var_name)
}

#' Apply a country mask to a NetCDF data array
#'
#' Sets grid cells to NA where the mask value is below \code{threshold}.
#'
#' @param dataset List returned by \code{load_netcdf()}.
#' @param mask_path Path to the mask NetCDF file (variable: \code{country}).
#' @param var_name Name of the variable inside \code{dataset}.
#' @param threshold Numeric threshold; cells with mask < threshold become NA. Default 0.8.
#' @return The dataset list with \code{data} masked in-place.
#' @export
apply_mask <- function(dataset, mask_path, var_name, threshold = 0.8) {
  mask_nc <- ncdf4::nc_open(mask_path)
  on.exit(ncdf4::nc_close(mask_nc))

  mask_vals <- ncdf4::ncvar_get(mask_nc, "country")  # [lon x lat]
  country_mask <- mask_vals >= threshold              # logical [lon x lat]

  data <- dataset$data
  # Broadcast mask over time dimension
  nt <- dim(data)[3]
  for (t in seq_len(nt)) {
    slice <- data[, , t]
    slice[!country_mask] <- NA
    data[, , t] <- slice
  }
  dataset$data <- data
  dataset
}

#' Standardise a monthly metric relative to a reference period
#'
#' For each calendar month, computes the mean and standard deviation over the
#' reference period, then returns (x - mean) / sd. Optionally averages over
#' the spatial dimensions first.
#'
#' @param metric A list with fields \code{data} [lon x lat x time] or [time],
#'   \code{time}, and optionally \code{lon}/\code{lat}.
#' @param reference_period Character vector of length 2: start and end dates
#'   (\code{"YYYY-MM-DD"}).
#' @param area Logical. If \code{TRUE} the spatial mean is computed before
#'   standardising and a named numeric vector is returned. Default \code{FALSE}.
#' @return If \code{area = TRUE}: a named numeric vector (names = dates).
#'   Otherwise: a list with the same structure as \code{metric} but standardised
#'   values.
#' @export
standardize_metric <- function(metric, reference_period, area = FALSE) {
  ref_start <- as.POSIXct(reference_period[1], tz = "UTC")
  ref_end   <- as.POSIXct(reference_period[2], tz = "UTC")

  time   <- metric$time
  data   <- metric$data
  is_3d  <- length(dim(data)) == 3

  # Optional spatial averaging
  if (area && is_3d) {
    # Collapse lon x lat -> mean over non-NA cells
    nt   <- dim(data)[3]
    data <- vapply(seq_len(nt), function(t) mean(data[, , t], na.rm = TRUE),
                   numeric(1))
    is_3d <- FALSE
  }

  months_all <- as.integer(format(time, "%m"))
  ref_mask   <- time >= ref_start & time <= ref_end
  ref_months <- months_all[ref_mask]

  # Compute reference monthly stats
  if (is_3d) {
    nl <- dim(data)[1]; nw <- dim(data)[2]
    ref_mean <- array(NA_real_, c(nl, nw, 12))
    ref_sd   <- array(NA_real_, c(nl, nw, 12))
    for (m in 1:12) {
      idx <- which(ref_mask & months_all == m)
      if (length(idx) == 0) next
      slices       <- data[, , idx, drop = FALSE]
      ref_mean[, , m] <- apply(slices, c(1, 2), mean, na.rm = TRUE)
      ref_sd[, , m]   <- apply(slices, c(1, 2), sd,   na.rm = TRUE)
      ref_sd[, , m][is.na(ref_sd[, , m]) | ref_sd[, , m] < .Machine$double.eps] <- 1
    }
    # Standardise
    out <- array(NA_real_, dim(data))
    for (t in seq_along(time)) {
      m         <- months_all[t]
      out[, , t] <- (data[, , t] - ref_mean[, , m]) / ref_sd[, , m]
    }
    metric$data <- out
    return(metric)
  } else {
    ref_mean_v <- tapply(data[ref_mask], ref_months, mean, na.rm = TRUE)
    ref_sd_v   <- tapply(data[ref_mask], ref_months, sd,   na.rm = TRUE)
    ref_sd_v[is.na(ref_sd_v) | ref_sd_v < .Machine$double.eps] <- 1
    out <- (data - ref_mean_v[months_all]) / ref_sd_v[months_all]
    names(out) <- format(time, "%Y-%m-%d")
    return(out)
  }
}

#' Reduce a standardised spatial metric to a data frame
#'
#' If \code{admin_mask} is \code{NULL}, averages over all non-NA cells
#' (national mean). Otherwise computes a weighted mean
#' per administrative unit using surface-fraction weights.
#'
#' @param metric List with fields \code{data} [lon x lat x time] and
#'   \code{time}.
#' @param column_name Character. Column name prefix. Default \code{"value"}.
#' @param admin_mask Output of \code{build_admin_mask()}, or \code{NULL}
#'   (default) for national mean.
#' @return A \code{data.frame} with one column per administrative unit (or one
#'   column for the national mean), indexed by month-start dates.
#' @export
reduce_dataarray_to_dataframe <- function(metric, column_name = "value",
                                          admin_mask = NULL) {
  data  <- metric$data
  time  <- metric$time
  dates <- as.Date(format(as.Date(time), "%Y-%m-01"))
  nt    <- dim(data)[3]

  if (is.null(admin_mask)) {
    # At (national) country level
    vals <- vapply(seq_len(nt),
                   function(t) mean(data[, , t], na.rm = TRUE),
                   numeric(1))
    df <- data.frame(vals, row.names = as.character(dates))
    colnames(df) <- column_name
    return(df)
  }

  # Weighted mean per administrative unit
  units   <- admin_mask$units
  lon     <- admin_mask$lon
  lat     <- admin_mask$lat
  weights <- admin_mask$weights
  nw      <- length(lat)

  out <- matrix(NA_real_, nrow = nt, ncol = length(units),
                dimnames = list(as.character(dates),
                                paste0(column_name, "_", units)))

  for (u_idx in seq_along(units)) {
    u <- units[u_idx]
    for (t in seq_len(nt)) {
      num <- 0; den <- 0
      for (i in seq_along(lon)) {
        for (j in seq_along(lat)) {
          w_vec <- weights[[(i - 1L) * nw + j]]
          w     <- w_vec[u]
          if (is.na(w) || w == 0) next
          v <- data[i, j, t]
          if (is.na(v)) next
          num <- num + w * v
          den <- den + w
        }
      }
      out[t, u_idx] <- if (den > 0) num / den else NA_real_
    }
  }

  as.data.frame(out)
}

#' Reduce sea-level data to a single column or one column per administrative unit
#'
#' @param df               A \code{data.frame} of standardised tide-gauge
#'   values with \code{"YYYY-MM-01"} row names and one column per station.
#' @param admin_assignment Output of \code{assign_sealevel_to_admin()}, or
#'   \code{NULL} (default) for national mean. Must be a list with elements
#'   \code{station_ids} (named list of PSMSL IDs per unit) and \code{factors}
#'   (named numeric vector of coastal fractions per unit).
#' @return A \code{data.frame} named \code{"sealevel"} (national) or
#'   \code{"sealevel_<unit>"} (per admin unit). Units without coastal stations
#'   have \code{NA} values.
#' @export
reduce_sealevel_over_region <- function(df, admin_assignment = NULL) {
  dates <- as.Date(format(as.Date(rownames(df)), "%Y-%m-01"))

  # --- Cas national ---
  if (is.null(admin_assignment)) {
    sea_mean <- rowMeans(df, na.rm = TRUE)
    return(data.frame(sealevel = sea_mean,
                      row.names = as.character(dates)))
  }

  # --- Cas administratif ---
  # Toutes les unités connues viennent de admin_assignment$factors
  all_units   <- names(admin_assignment$factors)
  station_ids <- admin_assignment$station_ids

  out <- matrix(NA_real_, nrow = nrow(df),
                ncol = length(all_units),
                dimnames = list(as.character(dates),
                                paste0("sealevel_", all_units)))

  for (u in all_units) {
    ids <- station_ids[[u]]
    if (is.null(ids) || length(ids) == 0) next

    col_names <- intersect(paste0("Measurement_", ids), colnames(df))
    if (length(col_names) == 0) next

    out[, paste0("sealevel_", u)] <- rowMeans(
      df[, col_names, drop = FALSE], na.rm = TRUE
    )
  }

  as.data.frame(out)
}

#' Merge a list of data frames on their row-name index
#'
#' @param dataframes A list of \code{data.frame} objects sharing a common row-
#'   name index.
#' @return A single merged \code{data.frame}.
#' @export
merge_dataframes <- function(dataframes) {
  Reduce(function(left, right) {
    merge(left, right, by = "row.names", all = FALSE) |>
      (\(d) { rownames(d) <- d$Row.names; d[, -1] })()
  }, dataframes)
}

#' Aggregate a monthly data frame to a given temporal granularity
#'
#' @param df A data.frame with "YYYY-MM-01" Date row names (monthly).
#' @param granularity One of "month", "season", "semester", "year".
#' @return A data.frame with aggregated values and appropriate row names.
#' @export
#' @importFrom dplyr case_when
aggregate_granularity <- function(df, granularity = "month") {
  dates <- as.Date(rownames(df))
  year  <- as.integer(format(dates, "%Y"))
  month <- as.integer(format(dates, "%m"))

  group_key <- switch(granularity,

                      month = format(dates, "%Y-%m"),

                      year = as.character(year),

                      semester = {
                        sem <- ifelse(month <= 6, 1L, 2L)
                        sprintf("%d-S%d", year, sem)
                      },

                      season = {
                        season_label <- dplyr::case_when(
                          month %in% c(12, 1, 2)  ~ "DJF",
                          month %in% c(3, 4, 5)   ~ "MAM",
                          month %in% c(6, 7, 8)   ~ "JJA",
                          month %in% c(9, 10, 11) ~ "SON"
                        )
                        season_year <- ifelse(month == 12, year + 1L, year)
                        sprintf("%d-%s", season_year, season_label)
                      },

                      stop("'granularity' must be one of: month, season, semester, year")
  )

  df$..group.. <- group_key
  agg <- aggregate(. ~ ..group.., data = df, FUN = mean, na.rm = TRUE)
  rownames(agg) <- agg$`..group..`
  agg$`..group..` <- NULL
  agg[order(rownames(agg)), , drop = FALSE]
}


#' Load the bundled PSMSL station metadata
#'
#' @return A \code{data.frame} of PSMSL tide-gauge station information.
#' @export
#' @importFrom readr read_csv
load_psmsl_data <- function() {
  path <- system.file("extdata", "psmsl_data.csv", package = "xaci")
  if (nchar(path) == 0) stop("psmsl_data.csv not found in package installation.")

  df <- readr::read_csv(path, show_col_types = FALSE)
  required_cols <- c("ID", "Country", "Lat.", "Lon.")
  missing <- setdiff(required_cols, colnames(df))
  if (length(missing) > 0)
    stop("psmsl_data.csv is missing required columns: ",
         paste(missing, collapse = ", "))
  names(df)[names(df) == "Lat."] <- "lat"
  names(df)[names(df) == "Lon."] <- "lon"
  df
}


#' Build a spatial weight matrix mapping ERA5 grid cells to administrative units
#'
#' For each ERA5 grid cell, computes the fraction of its surface area that
#' falls within each administrative unit polygon (using \code{sf} geometry
#' intersection). All geometries are projected to \code{crs_metric} before
#' area calculations to ensure results are in metres squared.
#' The result can be computationally expensive for fine grids and large
#' countries — consider caching it with \code{saveRDS()}.
#'
#' @param lon          Numeric vector of ERA5 longitudes.
#' @param lat          Numeric vector of ERA5 latitudes.
#' @param country_abbrev ISO-3 country code (e.g. \code{"FRA"}).
#' @param admin_level  Integer. Administrative level: \code{1} for regions,
#'   \code{2} for departments, etc. Default \code{1}.
#' @param resolution   Numeric. Half-width of ERA5 grid cells in degrees.
#'   Default \code{0.25}.
#' @param crs_metric   Integer. EPSG code of a metric CRS appropriate for
#'   the country, used for accurate area calculations. Default \code{4326}
#'   (WGS84, not recommended for production — prefer a local CRS such as
#'   \code{2154} for France or \code{27700} for the UK).
#' @return A list with elements:
#'   \describe{
#'     \item{\code{weights}}{A named list of length
#'       \code{length(lon) * length(lat)}. Each element is a named numeric
#'       vector of (unit -> fractional area weight) pairs.}
#'     \item{\code{units}}{Character vector of all administrative unit names.}
#'     \item{\code{lon}}{Input longitudes.}
#'     \item{\code{lat}}{Input latitudes.}
#'   }
#' @export
#' @importFrom sf st_as_sfc st_bbox st_sf st_intersection st_area
#'   st_transform st_crs
#' @importFrom rnaturalearth ne_states
build_admin_mask <- function(lon, lat, country_abbrev,
                             admin_level = 1,
                             resolution  = 0.25,
                             crs_metric  = 4326) {
  # Administrative polygons projected into the metric CRS
  admin_sf <- rnaturalearth::ne_states(
    country     = .iso3_to_country_name(country_abbrev),
    returnclass = "sf"
  )
  admin_sf <- admin_sf[, c("name", "geometry")]
  admin_sf <- sf::st_transform(admin_sf, crs_metric)

  # Build ERA5 grid cells as squared polygons, and project
  nl <- length(lon)
  nw <- length(lat)

  cell_polys <- lapply(seq_len(nl), function(i) {
    lapply(seq_len(nw), function(j) {
      cell <- sf::st_as_sfc(
        sf::st_bbox(c(
          xmin = lon[i] - resolution,
          xmax = lon[i] + resolution,
          ymin = lat[j] - resolution,
          ymax = lat[j] + resolution
        ), crs = sf::st_crs(4326)
        ))
      sf::st_transform(cell, crs_metric)
    })
  })

  # For each cell, compute area fraction per administrative unit
  weights <- vector("list", nl * nw)
  names(weights) <- as.character(seq_len(nl * nw))

  for (i in seq_len(nl)) {
    for (j in seq_len(nw)) {
      cell    <- cell_polys[[i]][[j]]
      cell_sf <- sf::st_sf(geometry = cell)
      inter   <- suppressWarnings(sf::st_intersection(cell_sf, admin_sf))

      if (nrow(inter) == 0) {
        weights[[(i - 1L) * nw + j]] <- setNames(numeric(0), character(0))
        next
      }

      areas      <- as.numeric(sf::st_area(inter))
      cell_area  <- as.numeric(sf::st_area(cell))
      fractions  <- areas / cell_area
      unit_names <- inter$name

      weights[[(i - 1L) * nw + j]] <- setNames(fractions, unit_names)
    }
  }

  list(
    weights = weights,
    units   = unique(admin_sf$name),
    lon     = lon,
    lat     = lat
  )
}
