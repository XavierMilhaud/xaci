#' @title Sea Level Component of the ACI
#' @description Processes PSMSL tide-gauge data and computes the sea-level
#'   component of the Actuarial Climate Index.
#' @name sealevel
NULL

# Month-fraction → month number mapping (from the Python source)
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
              dates <  as.Date(reference_period[2])
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
                dates <  as.Date(study_period[2])
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
#' @return A standardised \code{data.frame} of sea-level anomalies.
#' @export
sealevel_process <- function(directory, study_period, reference_period) {
  df <- sealevel_load_data(directory)
  df <- sealevel_correct_date_format(df)
  df <- sealevel_clean_data(df)
  monthly_means <- sealevel_compute_monthly_stats(df, reference_period, "means")
  monthly_std   <- sealevel_compute_monthly_stats(df, reference_period, "std")
  sealevel_standardize_data(df, monthly_means, monthly_std, study_period)
}

#' Calculate the sea-level component of the ACI
#'
#' Downloads PSMSL tide-gauge data for the given country, runs the full
#' processing pipeline, and returns the regional mean standardised anomaly.
#' Mirrors \code{SeaLevelComponent} constructor + \code{process()} call inside
#' \code{ActuarialClimateIndex.calculate_aci}.
#'
#' @param country_abbrev   Three-letter country code (e.g. \code{"FRA"}).
#' @param study_period     Character vector \code{c("start", "end")}.
#' @param reference_period Character vector \code{c("start", "end")}.
#' @return A one-column \code{data.frame} named \code{"sealevel"} with
#'   month-end \code{Date} row names.
#' @export
sealevel_component <- function(country_abbrev, study_period, reference_period) {
  # Download data (calls request_sealevel_data equivalent)
  directory <- request_sealevel_data(country_abbrev)
  raw       <- sealevel_process(directory, study_period, reference_period)
  reduce_sealevel_over_region(raw)
}

#' Download PSMSL tide-gauge data for a country
#'
#' Reads the bundled \code{psmsl_data.csv} to identify stations for the given
#' country abbreviation, downloads the corresponding data files from the PSMSL
#' website, and stores them locally.
#'
#' @param country_abbrev Three-letter ISO country code (e.g. \code{"FRA"}).
#' @param dest_dir       Destination directory. Defaults to
#'   \code{"data/sealevel_data_<country_abbrev>"}.
#' @return Invisibly, the path to the destination directory.
#' @export
request_sealevel_data <- function(country_abbrev,
                                   dest_dir = NULL) {
  if (is.null(dest_dir))
    dest_dir <- file.path("data", paste0("sealevel_data_", country_abbrev))
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
