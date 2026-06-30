#' @title Sea Level Component of the ACI
#' @description Processes PSMSL tide-gauge data and computes the sea-level
#'   component of the Actuarial Climate Index.
#' @name sealevel
NULL

# Month-fraction → month number mapping
.MONTH_MAPPING <- c(
  "0417" = "01", "125"  = "02", "2083" = "03", "2917" = "04",
  "375"  = "05", "4583" = "06", "5417" = "07", "625"  = "08",
  "7083" = "09", "7917" = "10", "875"  = "11", "9583" = "12"
)

#' Load sea-level txt files from a directory
#'
#' @param directory Path to the directory containing PSMSL \code{.txt} files.
#' @return A \code{data.frame} with one column per station and a numeric date
#'   as row names.
#' @export
sealevel_load_data <- function(directory) {
  files <- list.files(directory, pattern = "\\.txt$", full.names = TRUE)
  if (length(files) == 0) stop("No .txt files found in: ", directory)

  dfs <- lapply(files, function(f) {
    raw <- utils::read.table(f, sep = ";", header = FALSE,
                             col.names = c("Date", "Measurement", "V3", "V4"),
                             colClasses = c("numeric", "numeric",
                                            "character", "character"),
                             fill = TRUE)
    raw <- raw[, c("Date", "Measurement")]
    station_name <- paste0("Measurement_", tools::file_path_sans_ext(basename(f)))
    colnames(raw)[2] <- station_name
    raw
  })

  combined <- Reduce(function(a, b) merge(a, b, by = "Date", all = TRUE), dfs)
  rownames(combined) <- as.character(combined$Date)
  combined[, -1, drop = FALSE]
}

#' Load PSMSL station coordinates from the global station list CSV
#'
#' Reads the PSMSL station metadata CSV (columns: Station Name, ID, Lat.,
#' Lon., GLOSS ID, Country, Date, Coastline, Station) and returns coordinates
#' for the stations present in the data directory.
#'
#' @param directory Path to the PSMSL data directory. The metadata file
#'   \code{psmsl_data.csv} must be located in the same directory or its
#'   parent. Alternatively, \code{meta_path} can be supplied directly.
#' @param meta_path Optional explicit path to the CSV file. If \code{NULL}
#'   (default), the function looks for \code{psmsl_data.csv} first in
#'   \code{directory}, then one level up.
#' @param station_ids Integer vector of PSMSL station IDs to keep. If
#'   \code{NULL} (default), all stations in the CSV are returned.
#' @return A \code{data.frame} with columns \code{station_id} (character,
#'   e.g. \code{"Measurement_1"}), \code{lon}, \code{lat}.
#' @keywords internal
sealevel_load_metadata <- function(directory,
                                   meta_path = NULL,
                                   station_ids = NULL) {
  # Localiser le fichier de métadonnées
  if (is.null(meta_path)) {
    candidate <- system.file("extdata", "psmsl_data.csv", package = "xaci")
    if (file.exists(candidate)) {
      meta_path <- candidate
    } else {
      stop("Cannot find 'psmsl_data.csv' in:\n  ", candidate,
           "\nSupply the path explicitly via the `meta_path` argument.")
    }
  }

  meta <- utils::read.csv(meta_path, stringsAsFactors = FALSE,
                          strip.white = TRUE)

  # Normaliser les noms de colonnes (retire espaces, points, majuscules)
  colnames(meta) <- tolower(gsub("[. ]+", "_", colnames(meta)))
  # Colonnes attendues après normalisation : station_name, id, lat_, lon_, ...
  # Renommer lat_ / lon_ si nécessaire
  colnames(meta) <- sub("^lat_$", "lat", colnames(meta))
  colnames(meta) <- sub("^lon_$", "lon", colnames(meta))

  required <- c("id", "lat", "lon")
  missing  <- setdiff(required, colnames(meta))
  if (length(missing) > 0L)
    stop("Column(s) missing from metadata CSV: ",
         paste(missing, collapse = ", "),
         "\nFound: ", paste(colnames(meta), collapse = ", "))

  # Filtrer sur les IDs demandés
  if (!is.null(station_ids))
    meta <- meta[meta$id %in% station_ids, , drop = FALSE]

  if (nrow(meta) == 0L)
    stop("No matching stations found in the metadata CSV.")

  # Construire station_id cohérent avec sealevel_load_data()
  meta$station_id <- paste0("Measurement_", meta$id)
  meta[, c("station_id", "lon", "lat")]
}

#' Correct PSMSL float date format to Date objects
#'
#' PSMSL encodes dates as \code{YYYY.fraction} where the fraction
#' identifies the month.
#'
#' @param df \code{data.frame} with numeric row names (PSMSL date format).
#' @return The same \code{data.frame} with \code{Date} row names
#'   (\code{"YYYY-MM-01"}), rows with unrecognised dates removed.
#' @export
sealevel_correct_date_format <- function(df) {
  convert_date <- function(date_str) {
    parts <- strsplit(as.character(date_str), "\\.")[[1]]
    year  <- parts[1]
    frac  <- if (length(parts) > 1) substr(parts[2], 1, 4) else "0417"
    month <- .MONTH_MAPPING[frac]
    if (is.na(month)) return(NA_character_)
    paste0(year, "-", month, "-01")
  }

  date_strings <- vapply(rownames(df), convert_date, character(1))
  valid        <- !is.na(date_strings)
  df           <- df[valid, , drop = FALSE]
  rownames(df) <- date_strings[valid]
  df[order(rownames(df)), , drop = FALSE]
}

#' Replace PSMSL sentinel values (-99999) with NA
#'
#' @param df \code{data.frame} of sea-level measurements.
#' @return Cleaned \code{data.frame}.
#' @export
sealevel_clean_data <- function(df) {
  df[df == -99999] <- NA
  df
}

#' Compute monthly reference statistics for sea-level data
#'
#' @param df               Clean \code{data.frame} (row names = \code{"YYYY-MM-DD"}).
#' @param reference_period Character vector \code{c("start", "end")}.
#' @param stats            \code{"means"} or \code{"std"}.
#' @return A named numeric vector of length 12 (one value per calendar month).
#' @export
sealevel_compute_monthly_stats <- function(df, reference_period, stats) {
  dates    <- as.Date(rownames(df))
  ref_mask <- dates >= as.Date(reference_period[1]) &
    dates <=  as.Date(reference_period[2])
  df_ref   <- df[ref_mask, , drop = FALSE]
  row_mean <- rowMeans(df_ref, na.rm = TRUE)
  months   <- as.integer(format(as.Date(rownames(df_ref)), "%m"))

  if (stats == "means") {
    tapply(row_mean, months, mean, na.rm = TRUE)
  } else if (stats == "std") {
    tapply(row_mean, months, sd, na.rm = TRUE)
  } else {
    stop("'stats' must be 'means' or 'std'")
  }
}

#' Standardise sea-level data over the study period
#'
#' @param df               Clean \code{data.frame}.
#' @param monthly_means    Named numeric vector (months 1–12).
#' @param monthly_std_devs Named numeric vector (months 1–12).
#' @param study_period     Character vector \code{c("start", "end")}.
#' @return A \code{data.frame} of standardised anomalies for the study period,
#'   with rows containing all-NA removed.
#' @export
sealevel_standardize_data <- function(df, monthly_means, monthly_std_devs,
                                      study_period) {
  dates      <- as.Date(rownames(df))
  study_mask <- dates >= as.Date(study_period[1]) &
    dates <=  as.Date(study_period[2])
  df_study   <- df[study_mask, , drop = FALSE]
  months     <- as.integer(format(as.Date(rownames(df_study)), "%m"))

  out <- df_study
  for (r in seq_len(nrow(out))) {
    m        <- months[r]
    out[r, ] <- (df_study[r, ] - monthly_means[m]) / monthly_std_devs[m]
  }
  out[!apply(is.na(out), 1, all), , drop = FALSE]
}

#' Full sea-level processing pipeline
#'
#' @param directory        Path to the directory with PSMSL \code{.txt} files.
#' @param study_period     Character vector \code{c("start", "end")}.
#' @param reference_period Character vector \code{c("start", "end")}.
#' @return A named list with:
#'   \describe{
#'     \item{\code{data}}{Standardised \code{data.frame} of anomalies
#'       \code{[time x stations]}, row names \code{"YYYY-MM-DD"}.}
#'     \item{\code{coords}}{A \code{data.frame} with columns
#'       \code{station_id}, \code{lon}, \code{lat}, one row per station
#'       present in \code{data}.}
#'   }
#' @export
sealevel_process <- function(directory, study_period, reference_period) {

  df <- sealevel_load_data(directory)
  df <- sealevel_correct_date_format(df)
  df <- sealevel_clean_data(df)
  # IDs des stations présentes dans les fichiers .txt
  station_ids <- as.integer(
    sub("^Measurement_", "", colnames(df))
  )
  coords <- sealevel_load_metadata(directory,
                                   meta_path = NULL,
                                   station_ids = station_ids)

  # Garder uniquement les stations présentes dans df
  coords <- coords[coords$station_id %in% colnames(df), , drop = FALSE]
  # Aligner l'ordre sur les colonnes de df
  coords <- coords[match(colnames(df), coords$station_id), , drop = FALSE]

  monthly_means <- sealevel_compute_monthly_stats(df, reference_period, "means")
  monthly_std   <- sealevel_compute_monthly_stats(df, reference_period, "std")
  standardized  <- sealevel_standardize_data(df, monthly_means, monthly_std,
                                             study_period)

  list(data = standardized, coords = coords)
}


#' Download PSMSL tide-gauge data for a country
#'
#' Reads the bundled \code{psmsl_data.csv} to identify stations for the given
#' country abbreviation, downloads the corresponding data files from the PSMSL
#' website, and stores them locally.
#'
#' @param country_abbrev Three-letter ISO country code (e.g. \code{"FRA"}).
#' @param dest_dir Destination directory. Defaults to
#'   \code{"data/psmsl/<country_abbrev>"}.
#' @return Invisibly, the path to the destination directory.
#' @export
request_sealevel_data <- function(country_abbrev,
                                  dest_dir = NULL) {
  if (is.null(dest_dir))
    dest_dir <- file.path("data", "psmsl", toupper(country_abbrev))
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  psmsl  <- load_psmsl_data()
  # Filter by country column (adjust column name to match actual CSV)
  country_col <- grep("country|Country|COUNTRY", colnames(psmsl), value = TRUE)[1]
  id_col      <- grep("^id$|^ID$|station_id|STATION_ID", colnames(psmsl),
                      value = TRUE, ignore.case = TRUE)[1]

  stations <- psmsl[psmsl[[country_col]] == country_abbrev, ]
  if (nrow(stations) == 0)
    warning("No PSMSL stations found for country: ", country_abbrev)

  base_url <- "https://www.psmsl.org/data/obtaining/rlr.monthly.data/"
  for (i in seq_len(nrow(stations))) {
    station_id <- stations[[id_col]][i]
    url  <- paste0(base_url, station_id, ".rlrdata")
    dest <- file.path(dest_dir, paste0(station_id, ".txt"))
    tryCatch(
      utils::download.file(url, dest, quiet = TRUE),
      error = function(e) warning("Could not download station ", station_id,
                                  ": ", conditionMessage(e))
    )
  }
  invisible(dest_dir)
}


#' Assign PSMSL tide-gauge stations to administrative units and compute
#' coastal factors
#'
#' Performs a spatial join between PSMSL station coordinates and administrative
#' unit polygons, and computes for each unit the fraction of its perimeter
#' that is coastline. The coastline layer is cropped to the country bounding
#' box before intersection to speed up computation. All geometries are
#' projected to \code{crs_metric} before length calculations to ensure
#' results are in metres.
#'
#' @param country_abbrev ISO-3 country code (e.g. \code{"FRA"}).
#' @param admin_level    Integer >= 0. Administrative level fetched via
#'   GADM (see \code{.load_admin_sf()} in \code{utils.R}): \code{0} for the
#'   national boundary, \code{1} for regions, \code{2} for departments, etc.
#'   Default \code{1}.
#' @param crs_metric     Integer. EPSG code of a metric CRS appropriate for
#'   the country, used for accurate length calculations. Default \code{4326}
#'   (WGS84, not recommended for production — prefer a local CRS such as
#'   \code{2154} for France or \code{27700} for the UK).
#' @return A list with two elements:
#'   \describe{
#'     \item{\code{station_ids}}{Named list: keys are administrative unit
#'       names, values are integer vectors of PSMSL station IDs within that
#'       unit.}
#'     \item{\code{factors}}{Named numeric vector: keys are administrative
#'       unit names, values are the coastal fraction (coastline length /
#'       total perimeter) in \code{[0, 1]}. Zero for landlocked units.}
#'   }
#' @export
#' @importFrom sf st_as_sf st_join st_intersection st_length st_cast
#'   st_transform st_crs st_bbox st_crop
#' @importFrom rnaturalearth ne_coastline
assign_sealevel_to_admin <- function(country_abbrev, admin_level = 1,
                                     crs_metric = 4326) {

  psmsl <- load_psmsl_data()
  psmsl <- psmsl[psmsl$Country == country_abbrev, ]
  if (nrow(psmsl) == 0)
    stop("No PSMSL stations found for country: ", country_abbrev)

  # Administrative polygons (GADM, genuinely respects admin_level -- see
  # .load_admin_sf() in utils.R)
  admin_sf <- .load_admin_sf(country_abbrev, admin_level)

  # Projection into the metric CRS
  admin_sf <- sf::st_transform(admin_sf, crs_metric)

  # Worldwide coastline, filtrated on the country bounding box, then projected
  coastline <- rnaturalearth::ne_coastline(returnclass = "sf")
  coastline <- sf::st_transform(coastline, crs_metric)
  coastline <- sf::st_crop(coastline, sf::st_bbox(admin_sf))

  # Spatial join of stations -> administrative units (in WGS84)
  stations_sf <- sf::st_as_sf(psmsl, coords = c("lon", "lat"), crs = 4326)
  stations_sf <- sf::st_transform(stations_sf, crs_metric)
  idx <- sf::st_nearest_feature(stations_sf, admin_sf)
  joined <- cbind(
    stations_sf,
    sf::st_drop_geometry(admin_sf[idx, ])
  )
  #  joined      <- sf::st_join(stations_sf, admin_sf)
  station_ids <- split(joined$ID, joined$name)
  station_ids <- station_ids[!sapply(station_ids, is.null)]

  # Coastline factor per administrative unit
  factors <- sapply(admin_sf$name, function(u) {
    unit_geom <- admin_sf[admin_sf$name == u, ]

    # Coastal length intersecting this unit
    coast_clip <- suppressWarnings(sf::st_intersection(coastline, unit_geom))
    coast_len  <- if (nrow(coast_clip) == 0) {
      0
    } else {
      sum(as.numeric(sf::st_length(coast_clip)))
    }

    # Total perimeter of the unit
    perimeter <- sum(as.numeric(
      sf::st_length(sf::st_cast(unit_geom, "MULTILINESTRING"))
    ))

    if (perimeter == 0) 0 else min(coast_len / perimeter, 1)
  })

  list(
    station_ids = station_ids,
    factors     = setNames(factors, admin_sf$name)
  )
}

#' Interpolate tide-gauge sea-level values onto an ERA5 grid using IDW
#'
#' @param raw         List returned by \code{sealevel_process()}, with fields
#'   \code{data} (standardised \code{data.frame} \code{[time x stations]}) and
#'   \code{coords} (\code{data.frame} with \code{station_id}, \code{lon},
#'   \code{lat}).
#' @param lon         Numeric vector of grid longitudes (length nl).
#' @param lat         Numeric vector of grid latitudes  (length nw).
#' @param max_dist_km Numeric. Cells farther than this from every station
#'   receive \code{NA}. Default \code{500}.
#' @param power       Numeric. IDW power parameter. Default \code{2}.
#' @return A list with \code{data} (array \code{[nl x nw x nt]}),
#'   \code{lon}, \code{lat}, \code{time} (POSIXct).
#' @keywords internal
interpolate_sealevel_to_grid <- function(raw, lon, lat,
                                         max_dist_km = 500,
                                         power       = 2) {
  df     <- raw$data
  coords <- raw$coords
  nt     <- nrow(df)
  ns     <- nrow(coords)
  nl     <- length(lon)
  nw     <- length(lat)

  if (ns == 0L)
    stop("No station coordinates available for interpolation.")

  # --- Géométries sf ---
  grid_pts <- sf::st_as_sf(
    expand.grid(lon = lon, lat = lat),
    coords = c("lon", "lat"), crs = 4326
  )
  station_pts <- sf::st_as_sf(
    coords[, c("lon", "lat")],
    coords = c("lon", "lat"), crs = 4326
  )

  # --- Matrice de distances [n_cells x n_stations] en km ---
  dist_km <- units::drop_units(
    sf::st_distance(grid_pts, station_pts)
  ) / 1000

  # --- Poids IDW ---
  dist_safe    <- ifelse(dist_km < 0.001, 0.001, dist_km)
  weights_raw  <- 1 / dist_safe^power
  weights_raw[dist_km > max_dist_km] <- 0

  weight_sum   <- rowSums(weights_raw)
  no_station   <- weight_sum == 0
  weights_norm <- weights_raw / weight_sum
  weights_norm[no_station, ] <- NA_real_

  # --- Valeurs [ns x nt] ---
  # Aligner les colonnes de df sur l'ordre de coords
  val_mat <- t(as.matrix(df[, coords$station_id, drop = FALSE]))

  # --- Interpolation matricielle [n_cells x nt] ---
  interp_mat <- weights_norm %*% val_mat

  # --- Reshape en array [nl x nw x nt] ---
  out <- array(NA_real_, c(nl, nw, nt))
  for (t in seq_len(nt)) {
    out[, , t] <- matrix(interp_mat[, t], nrow = nl, ncol = nw)
  }

  list(
    data = out,
    lon  = lon,
    lat  = lat,
    time = as.POSIXct(rownames(df), format = "%Y-%m-%d", tz = "UTC")
  )
}

#' Calculate the sea-level component of the ACI
#'
#' @param country_abbrev   Three-letter country code (e.g. \code{"FRA"}).
#' @param study_period     Character vector \code{c("start", "end")}.
#' @param reference_period Character vector \code{c("start", "end")}.
#' @param mask_path   Path to the country mask NetCDF. Used to extract the
#'   ERA5 grid when \code{grid_cell = TRUE}.
#' @param grid_cell   Logical. If \code{TRUE}, interpolates the standardised
#'   anomalies onto the ERA5 grid and returns a list with a
#'   \code{[nl x nw x nt]} array — consistent with all other component
#'   functions. If \code{FALSE} (default), returns a \code{data.frame} of
#'   station anomalies (national or per admin unit).
#' @param max_dist_km Numeric. Maximum distance (km) for IDW interpolation.
#'   Only used when \code{grid_cell = TRUE}. Default \code{500}.
#' @param data_dir Character or \code{NULL}. Path to PSMSL \code{.txt}
#'   files. If \code{NULL}, data are downloaded automatically.
#' @param admin_level Integer or \code{NULL}. If not \code{NULL}, returns one
#'   column per admin unit. Ignored when \code{admin_assignment} is supplied.
#' @param admin_assignment Output of \code{assign_sealevel_to_admin()}, or
#'   \code{NULL}. When supplied, takes precedence over \code{admin_level}.
#' @param crs_metric  EPSG code used when building \code{admin_assignment}
#'   internally. Default \code{4326}.
#' @param save     Logical. Default \code{FALSE}.
#' @param save_dir Character. Default \code{"results/<country_abbrev>"}.
#' @param computed_components Logical. Default \code{FALSE}.
#' @param load_dir Character. Default \code{"results/<country_abbrev>"}.
#' @return If \code{grid_cell = TRUE}: list with array
#'   \code{[nl x nw x nt]}, \code{lon}, \code{lat}, \code{time}.
#'   Otherwise: \code{data.frame} of station anomalies (national or per
#'   admin unit).
#' @export
sealevel_component <- function(country_abbrev,
                               study_period,
                               reference_period,
                               mask_path           = NULL,
                               grid_cell           = FALSE,
                               max_dist_km         = 500,
                               data_dir            = paste0("data/psmsl/", country_abbrev),
                               admin_level         = NULL,
                               admin_assignment    = NULL,
                               crs_metric          = 4326,
                               computed_components = FALSE,
                               save                = FALSE,
                               save_dir            = paste0("results/", country_abbrev),
                               load_dir            = paste0("results/", country_abbrev)) {

  ref_tag <- paste(substr(reference_period[1], 1, 4),
                   substr(reference_period[2], 1, 4), sep = "_")

  if (computed_components) {
    path <- file.path(load_dir, paste0("sealevel_", ref_tag, ".rds"))
    if (!file.exists(path))
      stop("Cached file not found: ", path,
           "\nRun sealevel_component() with save = TRUE first.")
    raw <- readRDS(path)
  } else {
    if (!is.null(data_dir) && dir.exists(data_dir)) {
      directory <- data_dir
    } else {
      directory <- request_sealevel_data(country_abbrev, dest_dir = data_dir)
    }
    raw <- sealevel_process(directory, study_period, reference_period)

    if (save) {
      dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(raw, file.path(save_dir, paste0("sealevel_", ref_tag, ".rds")))
    }
  }

  # --- Mode grille ERA5 ---
  # La grille est extraite depuis le masque ERA5, comme dans les autres composantes
  if (grid_cell) {
    if (is.null(mask_path))
      stop("'mask_path' must be provided when grid_cell = TRUE.")
    grid <- load_component("data/era5/FRA/tp_2011_2015.nc", var_name="tp", mask_path)
    return(interpolate_sealevel_to_grid(raw,
                                        lon         = grid$lon,
                                        lat         = grid$lat,
                                        max_dist_km = max_dist_km))
  }

  # --- Résolution de admin_assignment ---
  if (is.null(admin_assignment) && !is.null(admin_level)) {
    admin_assignment <- assign_sealevel_to_admin(country_abbrev,
                                                 admin_level, crs_metric)
  }

  # --- Mode station (national ou administratif) ---
  if (is.null(admin_assignment)) {
    return(reduce_sealevel_over_region(raw$data))
  }
  reduce_sealevel_over_region(raw$data, admin_assignment = admin_assignment)
}
