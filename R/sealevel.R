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
#' for the requested stations.
#'
#' @param meta_path Optional explicit path to the CSV file. If \code{NULL}
#'   (default), the metadata CSV bundled with the package
#'   (\code{inst/extdata/psmsl_data.csv}, located via \code{system.file()})
#'   is used.
#' @param station_ids Integer vector of PSMSL station IDs to keep. If
#'   \code{NULL} (default), all stations in the CSV are returned.
#' @return A \code{data.frame} with columns \code{station_id} (character,
#'   e.g. \code{"Measurement_1"}), \code{lon}, \code{lat}.
#' @keywords internal
sealevel_load_metadata <- function(meta_path = NULL,
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
#' @return A numeric matrix, 12 rows (calendar months \code{"1"}-\code{"12"}
#'   as row names) x one column per station (station names as returned by
#'   \code{colnames(df)}). Each station is standardised against its own
#'   monthly reference statistics, independently of the other stations --
#'   consistent with how the other ACI components (ERA5 grid cells,
#'   administrative units) are each standardised against their own
#'   reference, not a value pooled across the whole spatial domain.
#' @export
sealevel_compute_monthly_stats <- function(df, reference_period, stats) {
  dates    <- as.Date(rownames(df))
  ref_mask <- dates >= as.Date(reference_period[1]) &
    dates <=  as.Date(reference_period[2])
  df_ref   <- df[ref_mask, , drop = FALSE]
  # levels = 1:12 force les 12 mois a etre presents (NA le cas echeant) et
  # dans cet ordre, meme si un mois entier est absent de la periode de
  # reference pour l'ensemble du jeu de donnees -- evite un decalage entre
  # le numero de mois et la position dans le resultat.
  months   <- factor(as.integer(format(as.Date(rownames(df_ref)), "%m")),
                     levels = 1:12)

  compute_col <- function(col) {
    # Nombre d'observations REELLEMENT disponibles dans la periode de
    # reference, par mois -- distingue explicitement "0 observation"
    # (aucune moyenne de reference definissable, station hors de la
    # periode de reference pour ce mois) de "1 observation" (moyenne
    # definissable, mais variance non estimable).
    n_obs <- as.numeric(tapply(col, months, function(x) sum(!is.na(x))))

    if (stats == "means") {
      m <- as.numeric(tapply(col, months, mean, na.rm = TRUE))
      # BUG FIX (voir NEWS) : avec 0 observation, tapply(..., mean, na.rm=TRUE)
      # renvoie NaN (et non NA). NaN se propage ensuite silencieusement dans
      # sealevel_standardize_data() et n'est exclu de reduce_sealevel_over_region()
      # que par accident (is.na(NaN) vaut TRUE en R, mais ce n'est pas un
      # comportement explicite/documente). On force ici NA_real_, et
      # sealevel_process() detecte et signale explicitement ces stations
      # (voir sa documentation) plutot que de laisser faire silencieusement.
      m[n_obs == 0] <- NA_real_
      m
    } else if (stats == "std") {
      # Garde-fou INCHANGE par rapport a la version d'origine : un mois avec
      # un seul echantillon (sd indefini, NA) OU une variance reellement
      # nulle (valeurs identiques par coincidence, sd = 0) -- y compris
      # n_obs == 0 (aucune donnee du tout, sd = NA egalement) -- recoit un
      # ecart-type de secours de 1. Ce choix reste volontairement
      # INCONDITIONNEL (pas de distinction sur n_obs ici, contrairement a
      # "means" ci-dessus) : la detection des stations sans donnee de
      # reference (voir sealevel_process()) se fait entierement via
      # 'means' (qui, lui, est bien NA pour n_obs == 0) -- la valeur de
      # 'std' dans ce cas n'a aucune consequence sur le resultat final,
      # puisque (NA - std_quelconque) reste NA. Inutile donc d'introduire
      # ici une distinction qui casserait par ailleurs le cas legitime
      # "variance nulle avec n_obs >= 2" (ex. deux releves identiques).
      sd_v <- as.numeric(tapply(col, months, sd, na.rm = TRUE))
      sd_v[is.na(sd_v) | sd_v < .Machine$double.eps] <- 1
      sd_v
    } else {
      stop("'stats' must be 'means' or 'std'")
    }
  }

  result <- vapply(df_ref, compute_col, numeric(12))
  dimnames(result) <- list(as.character(1:12), colnames(df_ref))
  result
}

#' Standardise sea-level data over the study period
#'
#' @param df               Clean \code{data.frame}.
#' @param monthly_means    Numeric matrix, 12 rows (months \code{"1"}-\code{"12"})
#'   x one column per station, as returned by
#'   \code{sealevel_compute_monthly_stats(..., stats = "means")}.
#' @param monthly_std_devs Same shape as \code{monthly_means}, for
#'   \code{stats = "std"}.
#' @param study_period     Character vector \code{c("start", "end")}.
#' @return A \code{data.frame} of standardised anomalies for the study period,
#'   with rows containing all-NA removed. Each station's values are
#'   standardised against its own monthly reference (see
#'   \code{sealevel_compute_monthly_stats()}), not a value pooled across
#'   stations.
#' @export
sealevel_standardize_data <- function(df, monthly_means, monthly_std_devs,
                                      study_period) {
  dates      <- as.Date(rownames(df))
  study_mask <- dates >= as.Date(study_period[1]) &
    dates <=  as.Date(study_period[2])
  df_study   <- df[study_mask, , drop = FALSE]
  months     <- as.integer(format(as.Date(rownames(df_study)), "%m"))
  stations   <- colnames(df_study)

  out <- df_study
  for (r in seq_len(nrow(out))) {
    m        <- as.character(months[r])
    out[r, ] <- (df_study[r, stations] - monthly_means[m, stations]) /
      monthly_std_devs[m, stations]
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
#'       \code{[time x stations]}, row names \code{"YYYY-MM-DD"}. Stations
#'       listed in \code{excluded_stations} are entirely absent from this
#'       data.frame (all-\code{NA} columns are dropped), not merely NA.}
#'     \item{\code{coords}}{A \code{data.frame} with columns
#'       \code{station_id}, \code{lon}, \code{lat}, one row per station
#'       present in \code{data} (excluded stations are also dropped here).}
#'     \item{\code{excluded_stations}}{Character vector of station column
#'       names (possibly empty) that have ZERO observations in
#'       \code{reference_period} for at least one calendar month, and are
#'       therefore impossible to standardise against Eq. A.12 (no reference
#'       mean is definable for that station/month) -- see the "Bug fix"
#'       note below. Use this to know which stations (e.g. ones installed
#'       after 1990) silently contribute nothing, so this isn't found out
#'       by surprise downstream.}
#'   }
#' @section Bug fix (see NEWS):
#' Stations with literally no data during \code{reference_period} (e.g. a
#' tide gauge installed in 2018, for a 1961-1990 reference period) used to
#' get a reference mean of \code{NaN} (not \code{NA}), which propagated
#' through \code{sealevel_standardize_data()} to make \strong{the entire
#' standardised series of that station \code{NaN}} -- including during
#' years where the station DOES have perfectly good data. This had no
#' visible effect only because \code{reduce_sealevel_over_region()}'s
#' \code{rowMeans(..., na.rm = TRUE)} happens to also drop \code{NaN}
#' (R treats \code{NaN} as a kind of \code{NA}), so the station was
#' silently and permanently excluded from every national/regional average
#' it could have contributed to, with no warning. For the French stations
#' listed in Garrido et al.'s Table B.2, 12 of the 40 stations (30\%) have
#' zero overlap with 1961-1990 and were affected. This function now (a)
#' produces clean \code{NA} instead of \code{NaN} for this case, and more
#' importantly (b) explicitly detects and reports these stations via
#' \code{warning()} and the \code{excluded_stations} return value, instead
#' of relying on an implicit, easy-to-miss floating-point coincidence.
#' Excluding such stations is not itself a choice this function makes --
#' there is no principled reference baseline to compute for a station that
#' did not exist during the reference period -- but that exclusion is now
#' visible rather than silent.
#' @export
sealevel_process <- function(directory, study_period, reference_period) {

  df <- sealevel_load_data(directory)
  df <- sealevel_correct_date_format(df)
  df <- sealevel_clean_data(df)
  # IDs des stations présentes dans les fichiers .txt
  station_ids <- as.integer(
    sub("^Measurement_", "", colnames(df))
  )
  coords <- sealevel_load_metadata(meta_path = NULL,
                                   station_ids = station_ids)

  # Garder uniquement les stations présentes dans df
  coords <- coords[coords$station_id %in% colnames(df), , drop = FALSE]
  # Aligner l'ordre sur les colonnes de df
  coords <- coords[match(colnames(df), coords$station_id), , drop = FALSE]

  monthly_means <- sealevel_compute_monthly_stats(df, reference_period, "means")
  monthly_std   <- sealevel_compute_monthly_stats(df, reference_period, "std")

  # BUG FIX (voir NEWS et la documentation ci-dessus) : detection EXPLICITE
  # des stations sans AUCUNE donnee de reference pour au moins un mois
  # calendaire -- impossible a standardiser pour ce(s) mois (Eq. A.12 exige
  # une moyenne et un ecart-type de reference). On distingue :
  #   - "jamais utilisables" (tous les 12 mois sont NA) : la station n'a
  #     litteralement aucun recouvrement avec reference_period -- avertissement
  #     explicite, la station sera totalement absente de la sortie.
  #   - "partiellement utilisables" (certains mois seulement) : cas plus rare
  #     (ex. des trous saisonniers dans les releves), avertissement plus
  #     discret car la station contribue quand meme sur ses mois valides.
  na_per_month   <- is.na(monthly_means)
  n_na_months    <- colSums(na_per_month)
  never_usable   <- colnames(monthly_means)[n_na_months == 12L]
  partly_usable  <- colnames(monthly_means)[n_na_months > 0L & n_na_months < 12L]

  if (length(never_usable) > 0) {
    warning(
      sprintf(
        paste0(
          "sealevel_process() : %d station(s) sans AUCUNE donnee sur la ",
          "periode de reference (%s - %s), donc sans moyenne/ecart-type de ",
          "reference definissable pour aucun mois -- elles seront ABSENTES ",
          "de la sortie (data, coords), y compris pour leurs propres annees ",
          "de bonnes donnees hors reference : %s"
        ),
        length(never_usable), reference_period[1], reference_period[2],
        paste(never_usable, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (length(partly_usable) > 0) {
    warning(
      sprintf(
        paste0(
          "sealevel_process() : %d station(s) avec une couverture ",
          "PARTIELLE de la periode de reference (%s - %s) -- certains mois ",
          "calendaires n'ont aucune donnee de reference et seront NA pour ",
          "ces mois uniquement : %s"
        ),
        length(partly_usable), reference_period[1], reference_period[2],
        paste(partly_usable, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  standardized  <- sealevel_standardize_data(df, monthly_means, monthly_std,
                                             study_period)

  # Les stations "jamais utilisables" ont desormais une colonne entierement
  # NA dans `standardized` (plus de NaN -- voir sealevel_compute_monthly_stats()) ;
  # on les retire explicitement de data ET de coords, plutot que de les
  # laisser trainer comme colonnes NA silencieuses.
  if (length(never_usable) > 0) {
    keep_cols   <- setdiff(colnames(standardized), never_usable)
    standardized <- standardized[, keep_cols, drop = FALSE]
    coords       <- coords[coords$station_id %in% keep_cols, , drop = FALSE]
  }

  list(data = standardized, coords = coords, excluded_stations = never_usable)
}


#' Download PSMSL tide-gauge data for a country
#'
#' Reads the bundled \code{psmsl_data.csv} to identify stations for the given
#' country abbreviation, downloads the corresponding data files from the PSMSL
#' website, and stores them locally.
#'
#' @param country_abbrev Three-letter ISO country code (e.g. \code{"FRA"}).
#' @param dest_dir Destination directory. If \code{NULL} (default),
#'   resolves to a sub-directory of \code{tempdir()}.
#' @return Invisibly, the path to the destination directory.
#' @export
request_sealevel_data <- function(country_abbrev,
                                  dest_dir = NULL) {
  dest_dir <- .resolve_cache_dir(dest_dir,
                                 file.path("xaci_psmsl", toupper(country_abbrev)))
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

  # --- Interpolation matricielle [n_cells x nt], instant par instant ---
  # NB: en R, `0 * NA` vaut `NA`, pas `0` (meme piege que dans .compute_aci_grid,
  # cf. aci.R). Une seule multiplication matricielle globale `weights_norm %*%
  # val_mat` ferait donc que le NA d'UNE SEULE station a l'instant t contamine
  # TOUTES les cellules de la grille a cet instant t, y compris celles dont le
  # poids sur cette station est nul (car trop eloignee). Pour chaque instant t,
  # on neutralise donc les stations NA (poids mis a 0) puis on renormalise les
  # poids restants sur les stations effectivement disponibles a cet instant.
  n_cells    <- nrow(weights_norm)
  interp_mat <- matrix(NA_real_, nrow = n_cells, ncol = nt)
  for (t in seq_len(nt)) {
    v_t    <- val_mat[, t]
    na_st  <- is.na(v_t)
    w_t    <- weights_norm
    w_t[, na_st] <- 0
    v_t[na_st]   <- 0

    row_w  <- rowSums(w_t, na.rm = TRUE)
    valid  <- !is.na(row_w) & row_w > 0
    w_t[valid, ] <- w_t[valid, , drop = FALSE] / row_w[valid]

    interp_mat[valid, t] <- w_t[valid, , drop = FALSE] %*% v_t
  }

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
#' @param study_period     Character vector \code{c("start", "end")}. Required
#'   to filter and/or download PSMSL tide-gauge data (unlike ERA5-based
#'   components which read the full NetCDF file and filter in downstream steps).
#' @param reference_period Character vector \code{c("start", "end")}.
#' @param mask_path   Path to the country mask NetCDF. Used to extract the
#'   ERA5 grid when \code{area = FALSE}.
#' @param area        Logical. Mirrors the \code{area} argument of all other
#'   component functions: if \code{FALSE} (default), interpolates the
#'   standardised anomalies onto the ERA5 grid and returns a list with a
#'   \code{[lon x lat x t]} array — consistent with the grid-cell output of
#'   \code{temperature_component()}, \code{precipitation_component()}, etc.
#'   If \code{TRUE}, returns a \code{data.frame} of aggregated anomalies
#'   (national or per admin unit, depending on \code{admin_level}).
#' @param max_dist_km Numeric. Maximum distance (km) for IDW interpolation.
#'   Only used when \code{area = FALSE}. Default \code{500}.
#' @param sealevel_dir Character or \code{NULL}. Path to the directory
#'   containing PSMSL \code{.txt} files. Named \code{sealevel_dir} for
#'   consistency with the \code{sealevel_dir} argument of
#'   \code{calculate_aci()}. If \code{NULL} or the directory does not exist,
#'   data are downloaded automatically.
#' @param admin_level Integer or \code{NULL}. If not \code{NULL}, returns one
#'   column per admin unit. Ignored when \code{admin_assignment} is supplied.
#' @param admin_assignment Output of \code{assign_sealevel_to_admin()}, or
#'   \code{NULL}. When supplied, takes precedence over \code{admin_level}.
#' @param crs_metric  EPSG code used when building \code{admin_assignment}
#'   internally. Default \code{4326}.
#' @param computed_components Logical. If \code{TRUE}, reloads a previously
#'   saved \code{.rds} file from \code{load_dir}. Default \code{FALSE}.
#' @param save     Logical. If \code{TRUE}, saves the processed result to
#'   \code{save_dir}. Default \code{FALSE}.
#' @param save_dir Character. Directory for saving results.
#'   Default \code{NULL}, which resolves to a sub-directory of \code{tempdir()}.
#' @param load_dir Character. Directory from which to reload a cached result
#'   when \code{computed_components = TRUE}.
#'   Default \code{NULL}, which resolves to a sub-directory of \code{tempdir()}.
#' @return If \code{area = FALSE}: a list with a \code{[lon x lat x t]} array,
#'   \code{lon}, \code{lat}, \code{time} — same structure as other grid-cell
#'   components. If \code{area = TRUE}: a \code{data.frame} of station
#'   anomalies (national or per admin unit).
#' @export
sealevel_component <- function(country_abbrev,
                               study_period,
                               reference_period,
                               mask_path           = NULL,
                               area                = TRUE,
                               max_dist_km         = 500,
                               sealevel_dir        = NULL,
                               admin_level         = NULL,
                               admin_assignment    = NULL,
                               crs_metric          = 4326,
                               computed_components = FALSE,
                               save                = FALSE,
                               save_dir            = NULL,
                               load_dir            = NULL) {

  save_dir <- .resolve_cache_dir(save_dir, file.path("xaci_results", country_abbrev))
  load_dir <- .resolve_cache_dir(load_dir, file.path("xaci_results", country_abbrev))

  study_tag <- paste(substr(study_period[1], 1, 4), substr(reference_period[2], 1, 4),
                     substr(study_period[2], 1, 4), sep = "_")

  if (computed_components) {
    path <- file.path(load_dir, paste0("sealevel_", study_tag, ".rds"))
    if (!file.exists(path))
      stop("Cached file not found: ", path,
           "\nRun sealevel_component() with save = TRUE first.")
    raw <- readRDS(path)
  } else {
    if (!is.null(sealevel_dir) && dir.exists(sealevel_dir)) {
      directory <- sealevel_dir
    } else {
      directory <- request_sealevel_data(country_abbrev, dest_dir = sealevel_dir)
    }
    raw <- sealevel_process(directory, study_period, reference_period)

    if (save) {
      dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(raw, file.path(save_dir, paste0("sealevel_", study_tag, ".rds")))
    }
  }

  # --- Mode grille ERA5 (area = FALSE, comme les autres composantes) ---
  if (!area) {
    if (is.null(mask_path))
      stop("'mask_path' must be provided when area = FALSE.")
    # NB: on ne peut pas passer par load_component()/load_netcdf() ici : ces
    # fonctions supposent un cube climatique 3D (lon x lat x time) et lisent
    # inconditionnellement une variable "time", qui n'existe pas dans le
    # fichier de masque (2D, lon x lat uniquement -- cf. apply_mask(), qui
    # le lit deja comme tel). On recupere lon/lat directement.
    mask_nc <- ncdf4::nc_open(mask_path)
    grid_lon <- ncdf4::ncvar_get(mask_nc, "longitude")
    grid_lat <- ncdf4::ncvar_get(mask_nc, "latitude")
    ncdf4::nc_close(mask_nc)
    return(interpolate_sealevel_to_grid(raw,
                                        lon         = grid_lon,
                                        lat         = grid_lat,
                                        max_dist_km = max_dist_km))
  }

  # --- Résolution de admin_assignment ---
  if (is.null(admin_assignment) && !is.null(admin_level)) {
    admin_assignment <- assign_sealevel_to_admin(country_abbrev,
                                                 admin_level, crs_metric)
  }

  # --- Mode agrégé (national ou administratif) ---
  if (is.null(admin_assignment)) {
    return(reduce_sealevel_over_region(raw$data))
  }
  reduce_sealevel_over_region(raw$data, admin_assignment = admin_assignment)
}
