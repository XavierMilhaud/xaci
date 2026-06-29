#' @title Download ERA5 and Country Mask Data
#' @description Functions to download ERA5 climate variables and country mask
#'   from the Copernicus Climate Data Store (CDS) via the \pkg{ecmwfr} package
#'   (v2.0+, new CDS API — Personal Access Token only, no UID required).
#' @name download
NULL

# ERA5 variable names on the CDS API
.ERA5_VARIABLES <- c(
  t2m  = "2m_temperature",
  tp   = "total_precipitation",
  u10  = "10m_u_component_of_wind",
  v10  = "10m_v_component_of_wind"
)

#' Set your CDS Personal Access Token
#'
#' Wrapper around \code{\link[ecmwfr]{wf_set_key}} for the new CDS API
#' (post-October 2024). The token is stored securely in the system keyring and
#' never needs to appear in your scripts again.
#'
#' @details
#' To obtain your token:
#' \enumerate{
#'   \item Create a free account at \url{https://cds.climate.copernicus.eu}.
#'   \item Go to \emph{your profile} → \emph{Personal Access Token}.
#'   \item Copy the token (a UUID string) and pass it to this function once.
#' }
#'
#' @param token Character. Your CDS Personal Access Token (UUID format).
#'
#' @return Invisibly \code{NULL}. The token is saved in the system keyring.
#' @export
#' @examples
#' \dontrun{
#' cds_set_key("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
#' }
cds_set_key <- function(token) {
  if (!requireNamespace("ecmwfr", quietly = TRUE))
    stop("Package 'ecmwfr' is required. Install it with: install.packages('ecmwfr')")
  ecmwfr::wf_set_key(key = token)
  message("CDS token saved. You only need to run cds_set_key() once per machine.")
  invisible(NULL)
}

#' Download an ERA5 variable from the Copernicus CDS
#'
#' Downloads hourly ERA5 single-level data for one variable and one or several
#' years, for a given geographical bounding box.  Files are downloaded year by
#' year and then optionally merged into a single NetCDF using the \pkg{ncdf4}
#' package.
#'
#' The CDS Personal Access Token must be stored beforehand with
#' \code{\link{cds_set_key}}.
#'
#' @param variable Character. Short name of the ERA5 variable: one of
#'   \code{"t2m"}, \code{"tp"}, \code{"u10"}, \code{"v10"}.
#' @param years Integer vector. Years to download (e.g. \code{1961:1990}).
#' @param area Numeric vector of length 4: \code{c(north, west, south, east)}
#'   in decimal degrees. Default is metropolitan France:
#'   \code{c(51.5, -5.5, 41.0, 10.0)}.
#' @param country_abbrev Character. Three-letter ISO 3166-1 alpha-3 country
#'   code (e.g. \code{"FRA"}). Used to build the default output path
#'   \code{data/era5/<country_abbrev>/}.
#'   Ignored when \code{dest_dir} is supplied explicitly. One of
#'   \code{country_abbrev} or \code{dest_dir} must be provided.
#' @param dest_dir Character. Directory where files will be saved. If
#'   \code{NULL} (default), built automatically from \code{country_abbrev}
#'   and \code{years} as \code{data/era5/<country_abbrev>/}.
#'   Created if it does not exist.
#' @param merge Logical. If \code{TRUE} (default) and more than one year is
#'   requested, the individual yearly files are merged into a single file named
#'   \code{<variable>_<first_year>_<last_year>.nc} using
#'   \code{\link[ncdf4]{nc_open}} / \code{\link[ncdf4]{ncvar_get}} and a
#'   time-concatenation loop (no external tool required).
#' @param overwrite Logical. If \code{FALSE} (default) skip years whose output
#'   file already exists.
#'
#' @return Invisibly, the path to the final NetCDF file (merged if
#'   \code{merge = TRUE}, otherwise the directory containing yearly files).
#'
#' @export
#' @examples
#' \dontrun{
#' # Store your token once
#' cds_set_key("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
#'
#' # Automatic path: data/era5/FRA/tp_1961_1990.nc
#' download_era5(
#'   variable       = "tp",
#'   years          = 1961:1990,
#'   area           = c(51.5, -5.5, 41.0, 10.0),
#'   country_abbrev = "FRA"
#' )
#'
#' # Or explicit path:
#' download_era5(
#'   variable = "tp",
#'   years    = 1961:1990,
#'   area     = c(51.5, -5.5, 41.0, 10.0),
#'   dest_dir = "my/custom/path"
#' )
#' }
download_era5 <- function(variable       = c("t2m", "tp", "u10", "v10"),
                          years,
                          area           = c(51.5, -5.5, 41.0, 10.0),
                          country_abbrev = NULL,
                          dest_dir       = NULL,
                          merge          = TRUE,
                          overwrite      = FALSE) {

  if (!requireNamespace("ecmwfr", quietly = TRUE))
    stop("Package 'ecmwfr' is required. Install it with: install.packages('ecmwfr')")

  variable <- match.arg(variable)
  cds_var  <- .ERA5_VARIABLES[variable]

  if (is.null(dest_dir)) {
    if (is.null(country_abbrev))
      stop("Provide either 'country_abbrev' (to build the path automatically) ",
           "or 'dest_dir' (to set it explicitly).")
    dest_dir <- file.path("data", "era5", toupper(country_abbrev))
  }

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  yearly_files <- character(length(years))

  for (i in seq_along(years)) {
    yr       <- years[i]
    out_file <- file.path(dest_dir, sprintf("%s_%d.nc", variable, yr))
    yearly_files[i] <- out_file

    if (file.exists(out_file) && !overwrite) {
      message(sprintf("  Skipping %d (file exists): %s", yr, out_file))
      next
    }

    message(sprintf("  Downloading %s for year %d ...", variable, yr))

    request <- list(
      dataset_short_name = "reanalysis-era5-single-levels",
      product_type       = "reanalysis",
      variable           = cds_var,
      year               = as.character(yr),
      month              = sprintf("%02d", 1:12),
      day                = sprintf("%02d", 1:31),
      time               = sprintf("%02d:00", 0:23),
      area               = area,          # N, W, S, E
      data_format        = "netcdf_legacy",
      target             = basename(out_file)
    )

    tryCatch(
      suppressWarnings(
        ecmwfr::wf_request(
          request  = request,
          path     = normalizePath(dest_dir, mustWork = FALSE),
          transfer = TRUE,
          verbose  = TRUE
        )
      ),
      error = function(e) {
        warning(sprintf("Failed to download %s for year %d: %s",
                        variable, yr, conditionMessage(e)))
      }
    )
  }

  # Merge yearly files into a single NetCDF
  if (merge && length(years) > 1) {
    merged_path <- file.path(
      dest_dir,
      sprintf("%s_%d_%d.nc", variable, min(years), max(years))
    )
    message("  Merging yearly files into: ", merged_path)
    .merge_netcdf_files(yearly_files[file.exists(yearly_files)],
                        merged_path, variable)
    return(invisible(merged_path))
  }

  invisible(dest_dir)
}

#' Download all ERA5 variables required by the xaci package
#'
#' Convenience wrapper that calls \code{\link{download_era5}} four times (one
#' per variable: \code{t2m}, \code{tp}, \code{u10}, \code{v10}).
#'
#' @param years     Integer vector of years to download.
#' @param area      Bounding box \code{c(north, west, south, east)}.
#'   Default: metropolitan France.
#' @param country_abbrev Character. ISO-3 country code (e.g. \code{"FRA"}).
#'   Used to build the default output path. One of \code{country_abbrev} or
#'   \code{dest_dir} must be provided.
#' @param dest_dir Character. Output directory. If \code{NULL} (default),
#'   built automatically as \code{data/era5/<country_abbrev>/}.
#' @param merge     Logical. Merge yearly files. Default \code{TRUE}.
#' @param overwrite Logical. Overwrite existing files. Default \code{FALSE}.
#'
#' @return Invisibly, a named character vector of output paths (one per
#'   variable).
#' @export
#' @examples
#' \dontrun{
#' cds_set_key("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
#'
#' # Produces: data/era5/FRA/t2m_1961_1990.nc, tp_..., u10_..., v10_...
#' download_era5_all(
#'   years          = 1961:1990,
#'   area           = c(51.5, -5.5, 41.0, 10.0),
#'   country_abbrev = "FRA"
#' )
#' }
download_era5_all <- function(years,
                              area           = c(51.5, -5.5, 41.0, 10.0),
                              country_abbrev = NULL,
                              dest_dir       = NULL,
                              merge          = TRUE,
                              overwrite      = FALSE) {
  vars  <- c("t2m", "tp", "u10", "v10")
  paths <- setNames(character(length(vars)), vars)

  for (v in vars) {
    message(sprintf("\n=== Variable: %s ===", v))
    paths[v] <- download_era5(
      variable       = v,
      years          = years,
      area           = area,
      country_abbrev = country_abbrev,
      dest_dir       = dest_dir,
      merge          = merge,
      overwrite      = overwrite
    )
  }
  invisible(paths)
}

#' Download the country mask from the Copernicus CDS
#'
#' Downloads the ERA5 land-sea mask for the given bounding box, then rescales
#' it to \code{[0, 1]} and saves it as \code{mask_<country_abbrev>.nc} in
#' \code{dest_dir}. The mask variable is renamed to \code{country} so it is
#' directly compatible with \code{\link{apply_mask}}.
#'
#' @param country_abbrev Three-letter ISO 3166-1 alpha-3 country code
#'   (e.g. \code{"FRA"}).
#' @param area Numeric vector \code{c(north, west, south, east)}.
#'   Default: metropolitan France.
#' @param dest_dir Character. Directory for the output file.
#'   Default: \code{"data/era5/<country_abbrev>/"}.
#' @param overwrite Logical. Overwrite if the file already exists.
#'   Default \code{FALSE}.
#'
#' @return Invisibly, the path to the mask NetCDF file.
#' @export
#' @examples
#' \dontrun{
#' cds_set_key("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
#'
#' # Produces: data/era5/FRA/mask_FRA.nc
#' download_mask(
#'   country_abbrev = "FRA",
#'   area      = c(51.5, -5.5, 41.0, 10.0)
#' )
#' }
download_mask <- function(country_abbrev = "FRA",
                          area           = c(51.5, -5.5, 41.0, 10.0),
                          dest_dir       = NULL,
                          overwrite      = FALSE) {

  if (is.null(dest_dir))
    dest_dir <- file.path("data", "era5", toupper(country_abbrev))

  if (!requireNamespace("ecmwfr", quietly = TRUE))
    stop("Package 'ecmwfr' is required. Install it with: install.packages('ecmwfr')")

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  raw_file  <- file.path(dest_dir, sprintf("lsm_raw_%s.nc", country_abbrev))
  mask_file <- file.path(dest_dir, sprintf("mask_%s.nc",    country_abbrev))

  if (file.exists(mask_file) && !overwrite) {
    message("Mask file already exists: ", mask_file)
    return(invisible(mask_file))
  }

  message("Downloading land-sea mask for: ", country_abbrev)

  # Download one arbitrary hour of the land-sea mask variable
  request <- list(
    dataset_short_name = "reanalysis-era5-single-levels",
    product_type       = "reanalysis",
    variable           = "land_sea_mask",
    year               = "1990",
    month              = "01",
    day                = "01",
    time               = "00:00",
    area               = area,
    data_format        = "netcdf_legacy",
    target             = basename(raw_file)
  )

  tryCatch(
    suppressWarnings(
      ecmwfr::wf_request(
        request  = request,
        path     = normalizePath(dest_dir, mustWork = FALSE),
        transfer = TRUE,
        verbose  = TRUE
      )
    ),
    error = function(e) stop("Mask download failed: ", conditionMessage(e))
  )

  # Rename variable to 'country' and keep only the spatial slice
  .rename_mask_variable(raw_file, mask_file)

  message("Mask saved to: ", mask_file)
  invisible(mask_file)
}

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Merge multiple yearly NetCDF files along the time dimension
#'
#' @param files Character vector of NetCDF paths.
#' @param output Output NetCDF file.
#' @param variable Variable name.
#' @keywords internal

.merge_netcdf_files <- function(files, output, variable) {

  if (length(files) == 0) {
    warning("No files to merge.")
    return(invisible(NULL))
  }

  if (length(files) == 1) {
    file.copy(files, output, overwrite = TRUE)
    return(invisible(output))
  }

  ## ---------- Read metadata ----------
  nc0 <- ncdf4::nc_open(files[1])

  lon <- ncdf4::ncvar_get(nc0, "longitude")
  lat <- ncdf4::ncvar_get(nc0, "latitude")
  time_units <- ncdf4::ncatt_get(nc0, "time", "units")$value
  time_cal   <- ncdf4::ncatt_get(nc0, "time", "calendar")$value

  ## Total number of timesteps
  nt_total <- 0
  for (f in files) {
    nc <- ncdf4::nc_open(f)
    nt_total <- nt_total + nc$dim$time$len
    ncdf4::nc_close(nc)
  }

  ## ---------- Create output file ----------
  dim_lon <- ncdf4::ncdim_def(
    "longitude",
    "degrees_east",
    vals = lon
  )

  dim_lat <- ncdf4::ncdim_def(
    "latitude",
    "degrees_north",
    vals = lat
  )

  ## Dummy values, overwritten afterwards
  dim_time <- ncdf4::ncdim_def(
    "time",
    units = time_units,
    vals = seq_len(nt_total),
    unlim = TRUE
  )

  ## Retrieve fill value if available
  fillvalue <- ncdf4::ncatt_get(
    nc0,
    variable,
    "_FillValue"
  )$value

  if (is.null(fillvalue))
    fillvalue <- NA_real_

  var_def <- ncdf4::ncvar_def(
    variable,
    units = "",
    dim = list(dim_lon, dim_lat, dim_time),
    missval = fillvalue,
    prec = "float",
    compression = 4
  )

  nc_out <- ncdf4::nc_create(output, var_def)

  ncdf4::ncatt_put(
    nc_out,
    "time",
    "calendar",
    time_cal
  )

  ## ---------- Write data incrementally ----------
  time_index <- 1
  all_time <- numeric(nt_total)

  for (f in files) {
    message("  Adding ", basename(f))

    nc <- ncdf4::nc_open(f)
    t <- ncdf4::ncvar_get(nc, "time")
    d <- ncdf4::ncvar_get(nc, variable)
    nt <- length(t)

    ncdf4::ncvar_put(
      nc_out,
      variable,
      d,
      start = c(1, 1, time_index),
      count = c(-1, -1, nt)
    )

    all_time[time_index:(time_index + nt - 1)] <- t
    time_index <- time_index + nt
    ncdf4::nc_close(nc)

    rm(d)
    gc(FALSE)
  }

  ## Write time axis
  ncdf4::ncvar_put(
    nc_out,
    "time",
    all_time
  )

  ncdf4::nc_close(nc_out)
  invisible(output)
}

#' Concatenate two 3-D arrays along the third dimension (time)
#' @keywords internal
abind_time <- function(a, b) {
  da <- dim(a); db <- dim(b)
  out <- array(NA_real_, c(da[1], da[2], da[3] + db[3]))
  out[, , seq_len(da[3])]            <- a
  out[, , da[3] + seq_len(db[3])]   <- b
  out
}

#' Rename the ERA5 land-sea mask variable to 'country' and drop the time dim
#' @keywords internal
.rename_mask_variable <- function(raw_file, mask_file) {
  nc  <- ncdf4::nc_open(raw_file)
  lon <- ncdf4::ncvar_get(nc, "longitude")
  lat <- ncdf4::ncvar_get(nc, "latitude")

  # lsm may be 3-D [lon x lat x time=1]; take first slice
  lsm_raw <- ncdf4::ncvar_get(nc, "lsm")
  ncdf4::nc_close(nc)

  lsm <- if (length(dim(lsm_raw)) == 3) lsm_raw[, , 1] else lsm_raw

  dim_lon <- ncdf4::ncdim_def("longitude", "degrees_east",  lon)
  dim_lat <- ncdf4::ncdim_def("latitude",  "degrees_north", lat)
  var_def <- ncdf4::ncvar_def("country", "1", list(dim_lon, dim_lat),
                              missval = NA, prec = "float",
                              longname = "Country land fraction (0-1)")
  nc_out  <- ncdf4::nc_create(mask_file, var_def)
  ncdf4::ncvar_put(nc_out, "country", lsm)
  ncdf4::nc_close(nc_out)

  file.remove(raw_file)
  invisible(mask_file)
}
