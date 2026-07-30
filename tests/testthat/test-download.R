library(testthat)

# ---------------------------------------------------------------------------
# download.R
#
# download_era5()/download_era5_all()/download_mask()/cds_set_key() all call
# out to ecmwfr::wf_request()/wf_set_key() (real Copernicus CDS network
# calls). These are mocked here with testthat::local_mocked_bindings() so
# the full control flow (path building, overwrite-skip logic, merging,
# mask renaming) runs for real against small synthetic NetCDF files, with
# zero network access. The pure helpers (.merge_netcdf_files, abind_time,
# .rename_mask_variable) are tested directly, without any mocking.
# ---------------------------------------------------------------------------

# --- abind_time ---------------------------------------------------------------

test_that("abind_time concatène deux tableaux le long de la 3e dimension (temps)", {
  a <- array(1:8,  dim = c(2, 2, 2))   # valeurs 1..8
  b <- array(9:12, dim = c(2, 2, 1))   # valeurs 9..12

  out <- abind_time(a, b)

  expect_equal(dim(out), c(2, 2, 3))
  expect_equal(out[, , 1:2], a)
  expect_equal(out[, , 3], b[, , 1])
})

# --- .merge_netcdf_files --------------------------------------------------------

.build_yearly_nc <- function(path, var, lon, lat, time_vec, origin, vals) {
  build_synthetic_nc(path, var, "unit", lon, lat, time_vec, origin, vals)
}

test_that(".merge_netcdf_files concatène correctement plusieurs fichiers annuels", {
  lon <- c(0, 1); lat <- c(0, 1)
  origin <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")

  time1 <- seq(as.POSIXct("2020-01-01 00:00", tz = "UTC"),
               as.POSIXct("2020-01-01 23:00", tz = "UTC"), by = "hour")  # 24 pas
  time2 <- seq(as.POSIXct("2021-01-01 00:00", tz = "UTC"),
               as.POSIXct("2021-01-01 11:00", tz = "UTC"), by = "hour")  # 12 pas

  f1 <- tempfile(fileext = ".nc"); f2 <- tempfile(fileext = ".nc")
  vals1 <- array(seq_len(2 * 2 * length(time1)), dim = c(2, 2, length(time1)))
  vals2 <- array(seq_len(2 * 2 * length(time2)) + 1000, dim = c(2, 2, length(time2)))
  .build_yearly_nc(f1, "t2m", lon, lat, time1, origin, vals1)
  .build_yearly_nc(f2, "t2m", lon, lat, time2, origin, vals2)

  out_path <- tempfile(fileext = ".nc")
  .merge_netcdf_files(c(f1, f2), out_path, "t2m")

  merged <- load_netcdf(out_path, "t2m")
  expect_equal(dim(merged$data), c(2, 2, length(time1) + length(time2)))
  expect_equal(as.numeric(merged$time[seq_along(time1)]), as.numeric(time1))
  expect_equal(as.numeric(merged$time[length(time1) + seq_along(time2)]), as.numeric(time2))
  expect_equal(merged$data[, , seq_along(time1)], vals1)
  expect_equal(merged$data[, , length(time1) + seq_along(time2)], vals2)
})

test_that(".merge_netcdf_files copie simplement le fichier quand il n'y en a qu'un seul", {
  lon <- c(0, 1); lat <- c(0, 1)
  origin <- as.POSIXct("1900-01-01", tz = "UTC")
  time1  <- seq(as.POSIXct("2020-01-01 00:00", tz = "UTC"),
               as.POSIXct("2020-01-01 05:00", tz = "UTC"), by = "hour")
  vals   <- array(1:24, dim = c(2, 2, 6))

  f1 <- tempfile(fileext = ".nc")
  .build_yearly_nc(f1, "tp", lon, lat, time1, origin, vals)

  out_path <- tempfile(fileext = ".nc")
  result <- .merge_netcdf_files(f1, out_path, "tp")

  expect_equal(as.character(result), out_path)
  expect_true(file.exists(out_path))
  merged <- load_netcdf(out_path, "tp")
  expect_equal(merged$data, vals)
})

test_that(".merge_netcdf_files avertit et ne fait rien quand la liste de fichiers est vide", {
  expect_warning(
    result <- .merge_netcdf_files(character(0), tempfile(fileext = ".nc"), "tp"),
    "No files to merge"
  )
  expect_null(result)
})

# --- .rename_mask_variable -------------------------------------------------------

.build_lsm_netcdf <- function(path, lon, lat, lsm_vals, with_time_dim) {
  dim_lon <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  dim_lat <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  if (with_time_dim) {
    dim_time <- ncdf4::ncdim_def("time", "hours since 1900-01-01", 0, unlim = TRUE)
    var_lsm <- ncdf4::ncvar_def("lsm", "1", list(dim_lon, dim_lat, dim_time),
                                missval = NA, prec = "float")
    nc <- ncdf4::nc_create(path, list(var_lsm))
    ncdf4::ncvar_put(nc, var_lsm, array(lsm_vals, dim = c(length(lon), length(lat), 1)))
  } else {
    var_lsm <- ncdf4::ncvar_def("lsm", "1", list(dim_lon, dim_lat),
                                missval = NA, prec = "float")
    nc <- ncdf4::nc_create(path, list(var_lsm))
    ncdf4::ncvar_put(nc, var_lsm, lsm_vals)
  }
  ncdf4::nc_close(nc)
  invisible(path)
}

test_that(".rename_mask_variable renomme 'lsm' en 'country' et ne garde que le 1er pas de temps (entrée 3D)", {
  lon <- c(0, 1); lat <- c(0, 1)
  lsm_vals <- matrix(c(0, 0.4, 0.9, 1), nrow = 2)

  raw_path  <- tempfile(fileext = ".nc")
  mask_path <- tempfile(fileext = ".nc")
  .build_lsm_netcdf(raw_path, lon, lat, lsm_vals, with_time_dim = TRUE)

  .rename_mask_variable(raw_path, mask_path)

  expect_false(file.exists(raw_path))  # le fichier brut est supprimé
  expect_true(file.exists(mask_path))

  nc <- ncdf4::nc_open(mask_path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  expect_true("country" %in% names(nc$var))
  expect_equal(ncdf4::ncvar_get(nc, "country"), lsm_vals, tolerance = 1e-6)
})

test_that(".rename_mask_variable fonctionne aussi avec une entrée déjà 2D (sans dimension temps)", {
  lon <- c(0, 1); lat <- c(0, 1)
  lsm_vals <- matrix(c(1, 1, 0, 0.5), nrow = 2)

  raw_path  <- tempfile(fileext = ".nc")
  mask_path <- tempfile(fileext = ".nc")
  .build_lsm_netcdf(raw_path, lon, lat, lsm_vals, with_time_dim = FALSE)

  .rename_mask_variable(raw_path, mask_path)

  nc <- ncdf4::nc_open(mask_path)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  expect_equal(ncdf4::ncvar_get(nc, "country"), lsm_vals, tolerance = 1e-6)
})

# --- download_era5 (ecmwfr mocké) -------------------------------------------------

# Simule une réponse CDS : écrit un petit fichier NetCDF annuel horaire (2
# jours seulement, pas l'année complète -- suffisant pour valider le
# comportement, et beaucoup plus rapide). Le nom de variable dans le fichier
# est déduit de request$target (ex. "tp_2020.nc" -> "tp"), comme le fait la
# vraie API CDS (format "netcdf_legacy" avec le nom court ERA5).
# Simule une réponse CDS : écrit un petit fichier NetCDF annuel horaire (2
# jours seulement, pas l'année complète -- suffisant pour valider le
# comportement, et beaucoup plus rapide). Le nom de variable dans le fichier
# est déduit de request$target (ex. "tp_2020.nc" -> "tp"), comme le fait la
# vraie API CDS (format "netcdf_legacy" avec le nom court ERA5).
.mock_wf_request <- function(call_log = NULL) {
  function(request, path, transfer = TRUE, verbose = TRUE) {
    if (!is.null(call_log)) assign("n", get("n", envir = call_log) + 1, envir = call_log)
    var_short <- sub("_.*$", "", request$target)
    yr        <- as.integer(request$year)
    origin    <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
    time_vec  <- seq(as.POSIXct(sprintf("%d-01-01 00:00", yr), tz = "UTC"),
                     as.POSIXct(sprintf("%d-01-02 23:00", yr), tz = "UTC"), by = "hour")
    vals <- array(seq_len(2 * 2 * length(time_vec)), dim = c(2, 2, length(time_vec)))
    build_synthetic_nc(file.path(path, request$target), var_short, "unit",
                       c(0, 1), c(0, 1), time_vec, origin, vals)
    invisible(NULL)
  }
}

test_that("download_era5 échoue si ni country_abbrev ni dest_dir ne sont fournis", {
  expect_error(
    download_era5(variable = "tp", years = 2020, country_abbrev = NULL, dest_dir = NULL),
    "Provide either"
  )
})

test_that("download_era5 fonctionne avec dest_dir fourni seul (sans country_abbrev)", {
  # Regression : dir.create() était auparavant hardcodé sur country_abbrev
  # et plantait ("invalid 'path' argument") quand dest_dir était fourni seul,
  # alors que la doc dit explicitement que c'est un usage valide.
  local_mocked_bindings(wf_request = .mock_wf_request(), .package = "ecmwfr")
  dest <- tempfile()
  result <- download_era5(variable = "tp", years = 2020, country_abbrev = NULL, dest_dir = dest)

  expect_equal(as.character(result), dest)
  expect_true(file.exists(file.path(dest, "source", "tp_2020.nc")))
})

test_that("download_era5 saute une année déjà téléchargée quand overwrite = FALSE", {
  dest <- tempfile(); dir.create(file.path(dest, "source"), recursive = TRUE)
  existing <- file.path(dest, "source", "tp_2020.nc")
  writeLines("sentinel", existing)  # fichier factice, ne doit pas être touché

  call_log <- new.env(); assign("n", 0, envir = call_log)
  local_mocked_bindings(wf_request = .mock_wf_request(call_log), .package = "ecmwfr")

  download_era5(variable = "tp", years = 2020, dest_dir = dest,
                merge = FALSE, overwrite = FALSE)

  expect_equal(get("n", envir = call_log), 0)  # wf_request jamais appelé
  expect_equal(readLines(existing), "sentinel")  # fichier intact
})

test_that("download_era5 télécharge et fusionne plusieurs années en un seul fichier", {
  local_mocked_bindings(wf_request = .mock_wf_request(), .package = "ecmwfr")

  dest <- tempfile()
  result <- download_era5(variable = "tp", years = 2020:2021, dest_dir = dest,
                          merge = TRUE, overwrite = TRUE)

  expected_path <- file.path(dest, "tp_2020_2021.nc")
  expect_equal(as.character(result), expected_path)
  expect_true(file.exists(expected_path))

  merged <- load_netcdf(expected_path, "tp")
  expect_equal(dim(merged$data)[3], 48 * 2)  # 2 jours horaires x 2 années
})

test_that("download_era5 sans fusion (une seule année) renvoie le dossier de destination", {
  local_mocked_bindings(wf_request = .mock_wf_request(), .package = "ecmwfr")

  dest <- tempfile()
  result <- download_era5(variable = "t2m", years = 2020, dest_dir = dest, merge = TRUE)

  expect_equal(as.character(result), dest)  # 1 seule année -> pas de fusion malgré merge=TRUE
  expect_true(file.exists(file.path(dest, "source", "t2m_2020.nc")))
})

test_that("download_era5_all télécharge les 4 variables ERA5", {
  call_log <- new.env(); assign("n", 0, envir = call_log)
  local_mocked_bindings(wf_request = .mock_wf_request(call_log), .package = "ecmwfr")

  dest <- tempfile()
  paths <- download_era5_all(years = 2020, dest_dir = dest, merge = TRUE)

  expect_equal(get("n", envir = call_log), 4)
  expect_setequal(names(paths), c("t2m", "tp", "u10", "v10"))
  expect_true(all(paths == dest))
  for (v in c("t2m", "tp", "u10", "v10")) {
    expect_true(file.exists(file.path(dest, "source", sprintf("%s_2020.nc", v))))
  }
})

# --- download_mask (ecmwfr mocké) -------------------------------------------------

test_that("download_mask ne re-télécharge pas si le masque existe déjà et overwrite = FALSE", {
  dest <- tempfile(); dir.create(dest, recursive = TRUE)
  existing_mask <- file.path(dest, "mask_FRA.nc")
  writeLines("sentinel", existing_mask)

  call_log <- new.env(); assign("n", 0, envir = call_log)
  local_mocked_bindings(wf_request = .mock_wf_request(call_log), .package = "ecmwfr")

  result <- download_mask(country_abbrev = "FRA", dest_dir = dest, overwrite = FALSE)

  expect_equal(get("n", envir = call_log), 0)
  expect_equal(as.character(result), existing_mask)
  expect_equal(readLines(existing_mask), "sentinel")
})

test_that("download_mask télécharge, renomme en 'country' et supprime le fichier brut", {
  dest <- tempfile()

  mock_wf_request <- function(request, path, transfer = TRUE, verbose = TRUE) {
    lon <- c(0, 1); lat <- c(0, 1)
    lsm_vals <- matrix(c(0, 1, 0.5, 1), nrow = 2)
    .build_lsm_netcdf(file.path(path, request$target), lon, lat, lsm_vals,
                      with_time_dim = TRUE)
  }
  local_mocked_bindings(wf_request = mock_wf_request, .package = "ecmwfr")

  result <- download_mask(country_abbrev = "FRA", dest_dir = dest, overwrite = TRUE)

  expect_equal(as.character(result), file.path(dest, "mask_FRA.nc"))
  expect_true(file.exists(file.path(dest, "mask_FRA.nc")))
  expect_false(file.exists(file.path(dest, "lsm_raw_FRA.nc")))

  nc <- ncdf4::nc_open(result)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  expect_true("country" %in% names(nc$var))
})

# --- cds_set_key (ecmwfr mocké) ----------------------------------------------------

test_that("cds_set_key transmet le token à ecmwfr::wf_set_key()", {
  received <- NULL
  local_mocked_bindings(
    wf_set_key = function(key) { received <<- key; invisible(NULL) },
    .package = "ecmwfr"
  )

  expect_message(cds_set_key("my-token-1234"), "CDS token saved")
  expect_equal(received, "my-token-1234")
})
