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
#' @param dest_dir Character. Directory where files will be saved. Created if
#'   it does not exist. Default: \code{"data/era5"}.
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
#' # Download total precipitation for France, 1961–1990
#' download_era5(
#'   variable = "tp",
#'   years    = 1961:1990,
#'   area     = c(51.5, -5.5, 41.0, 10.0),
#'   dest_dir = "data/era5"
#' )
#' }
download_era5 <- function(variable  = c("t2m", "tp", "u10", "v10"),
                          years,
                          area      = c(51.5, -5.5, 41.0, 10.0),
                          dest_dir  = "data/era5",
                          merge     = TRUE,
                          overwrite = FALSE) {

  if (!requireNamespace("ecmwfr", quietly = TRUE))
    stop("Package 'ecmwfr' is required. Install it with: install.packages('ecmwfr')")

  variable <- match.arg(variable)
  cds_var  <- .ERA5_VARIABLES[variable]

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
#' @param dest_dir  Directory for output files. Default: \code{"data/era5"}.
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
#' download_era5_all(
#'   years    = 1961:1990,
#'   area     = c(51.5, -5.5, 41.0, 10.0),
#'   dest_dir = "data/era5"
#' )
#' }
download_era5_all <- function(years,
                              area      = c(51.5, -5.5, 41.0, 10.0),
                              dest_dir  = "data/era5",
                              merge     = TRUE,
                              overwrite = FALSE) {
  vars  <- c("t2m", "tp", "u10", "v10")
  paths <- setNames(character(length(vars)), vars)

  for (v in vars) {
    message(sprintf("\n=== Variable: %s ===", v))
    paths[v] <- download_era5(
      variable  = v,
      years     = years,
      area      = area,
      dest_dir  = dest_dir,
      merge     = merge,
      overwrite = overwrite
    )
  }
  invisible(paths)
}

#' Download the country mask from the Copernicus CDS
#'
#' Downloads the ERA5 land-sea mask for the given bounding box, then rescales
#' it to \code{[0, 1]} and saves it as \code{mask_<area_name>.nc} in
#' \code{dest_dir}. The mask variable is renamed to \code{country} so it is
#' directly compatible with \code{\link{apply_mask}}.
#'
#' @param area_name Character. Short label used in the output file name
#'   (e.g. \code{"france"}).
#' @param area Numeric vector \code{c(north, west, south, east)}.
#'   Default: metropolitan France.
#' @param dest_dir Character. Directory for the output file.
#'   Default: \code{"data/era5"}.
#' @param overwrite Logical. Overwrite if the file already exists.
#'   Default \code{FALSE}.
#'
#' @return Invisibly, the path to the mask NetCDF file.
#' @export
#' @examples
#' \dontrun{
#' cds_set_key("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
#'
#' download_mask(
#'   area_name = "france",
#'   area      = c(51.5, -5.5, 41.0, 10.0),
#'   dest_dir  = "data/era5"
#' )
#' }
download_mask <- function(area_name = "france",
                          area      = c(51.5, -5.5, 41.0, 10.0),
                          dest_dir  = "data/era5",
                          overwrite = FALSE) {

  if (!requireNamespace("ecmwfr", quietly = TRUE))
    stop("Package 'ecmwfr' is required. Install it with: install.packages('ecmwfr')")

  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  raw_file  <- file.path(dest_dir, sprintf("lsm_raw_%s.nc", area_name))
  mask_file <- file.path(dest_dir, sprintf("mask_%s.nc",    area_name))

  if (file.exists(mask_file) && !overwrite) {
    message("Mask file already exists: ", mask_file)
    return(invisible(mask_file))
  }

  message("Downloading land-sea mask for: ", area_name)

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
#' @param files      Character vector of NetCDF paths to merge.
#' @param output     Output path for the merged file.
#' @param variable   Short variable name (\code{"t2m"}, \code{"tp"}, etc.).
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

  # Read all files and concatenate along time
  nc1   <- ncdf4::nc_open(files[1])
  lon   <- ncdf4::ncvar_get(nc1, "longitude")
  lat   <- ncdf4::ncvar_get(nc1, "latitude")
  t_raw <- ncdf4::ncvar_get(nc1, "time")
  d_raw <- ncdf4::ncvar_get(nc1, variable)
  t_att <- ncdf4::ncatt_get(nc1, "time")
  ncdf4::nc_close(nc1)

  all_time <- t_raw
  all_data <- d_raw

  for (f in files[-1]) {
    nc_i   <- ncdf4::nc_open(f)
    t_i    <- ncdf4::ncvar_get(nc_i, "time")
    d_i    <- ncdf4::ncvar_get(nc_i, variable)
    ncdf4::nc_close(nc_i)
    all_time <- c(all_time, t_i)
    all_data <- abind_time(all_data, d_i)
  }

  # Write merged file
  dim_lon  <- ncdf4::ncdim_def("longitude", "degrees_east",  lon)
  dim_lat  <- ncdf4::ncdim_def("latitude",  "degrees_north", lat)
  dim_time <- ncdf4::ncdim_def("time", t_att$units, all_time, unlim = TRUE)

  var_def  <- ncdf4::ncvar_def(variable, "", list(dim_lon, dim_lat, dim_time),
                               missval = NA, prec = "float")
  nc_out   <- ncdf4::nc_create(output, var_def)
  ncdf4::ncvar_put(nc_out, variable, all_data)
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
