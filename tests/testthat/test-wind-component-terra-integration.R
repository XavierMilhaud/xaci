library(testthat)

# ---------------------------------------------------------------------------
# Test d'integration bout-en-bout : wind_component() (base-R) vs
# wind_component_terra(), sur de vrais petits fichiers NetCDF synthetiques
# (u10 et v10).
# ---------------------------------------------------------------------------

.build_synthetic_wind_netcdf <- function(path, var_name, lon, lat, time_vec,
                                         origin, base_value, seed) {
  time_hours <- as.numeric(difftime(time_vec, origin, units = "hours"))

  set.seed(seed)
  nlo <- length(lon); nla <- length(lat); nt <- length(time_vec)
  vals <- array(NA_real_, dim = c(nlo, nla, nt))
  for (i in seq_len(nlo)) {
    for (j in seq_len(nla)) {
      vals[i, j, ] <- base_value + (i + j) * 0.5 +
        sin(seq_len(nt) / 400) + rnorm(nt, sd = 0.5)
    }
  }

  dim_lon  <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  dim_lat  <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  dim_time <- ncdf4::ncdim_def("time", paste0("hours since ", format(origin, "%Y-%m-%d %H:%M:%S")),
                               time_hours, unlim = TRUE)
  var_def  <- ncdf4::ncvar_def(var_name, "m s-1", list(dim_lon, dim_lat, dim_time),
                               missval = NA, prec = "double")

  nc <- ncdf4::nc_create(path, list(var_def))
  ncdf4::ncvar_put(nc, var_def, vals)
  ncdf4::nc_close(nc)
  invisible(path)
}

test_that("wind_component_terra concorde avec wind_component (admin_mask = NULL)", {
  skip_if_not_installed("terra")

  tmp_u10 <- tempfile(fileext = ".nc")
  tmp_v10 <- tempfile(fileext = ".nc")
  on.exit(unlink(c(tmp_u10, tmp_v10)), add = TRUE)

  lon <- c(0, 1)
  lat <- c(0, 1)
  origin <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
  # 2 ans de donnees : indispensable pour que .wind_thresholds_from_wp()
  # (mean + 1.28*sd par jour-de-l'annee) soit defini -- sd() d'un
  # echantillon unique par jour-de-l'annee (reference_period = study_period
  # sur une seule annee) est structurellement NA en R.
  time_vec <- seq(as.POSIXct("2000-01-01 00:00", tz = "UTC"),
                  as.POSIXct("2001-12-31 23:00", tz = "UTC"), by = "hour")

  .build_synthetic_wind_netcdf(tmp_u10, "u10", lon, lat, time_vec, origin,
                               base_value = 3, seed = 11)
  .build_synthetic_wind_netcdf(tmp_v10, "v10", lon, lat, time_vec, origin,
                               base_value = 4, seed = 22)

  reference_period <- c("2000-01-01", "2001-12-31")

  res_base  <- wind_component(tmp_u10, tmp_v10, "XX", reference_period,
                              study_period = reference_period, area = FALSE)
  res_terra <- wind_component_terra(tmp_u10, tmp_v10, "XX", reference_period,
                                    study_period = reference_period, area = FALSE)

  expect_equal(dim(res_terra$data), dim(res_base$data))
  non_na <- !is.na(res_base$data) & !is.na(res_terra$data)
  expect_true(any(non_na))
  expect_equal(res_terra$data[non_na], res_base$data[non_na], tolerance = 1e-6)
  expect_equal(as.character(as.Date(res_terra$time)),
               as.character(as.Date(res_base$time)))
})

test_that("wind_component_terra concorde avec wind_component (mask_path fourni)", {
  skip_if_not_installed("terra")

  tmp_u10  <- tempfile(fileext = ".nc")
  tmp_v10  <- tempfile(fileext = ".nc")
  tmp_mask <- tempfile(fileext = ".nc")
  on.exit(unlink(c(tmp_u10, tmp_v10, tmp_mask)), add = TRUE)

  lon <- c(0, 1)
  lat <- c(0, 1)
  origin <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
  time_vec <- seq(as.POSIXct("2000-01-01 00:00", tz = "UTC"),
                  as.POSIXct("2001-12-31 23:00", tz = "UTC"), by = "hour")

  .build_synthetic_wind_netcdf(tmp_u10, "u10", lon, lat, time_vec, origin,
                               base_value = 3, seed = 11)
  .build_synthetic_wind_netcdf(tmp_v10, "v10", lon, lat, time_vec, origin,
                               base_value = 4, seed = 22)

  # Meme convention de masque "country" (seuil 0.8) que component_terra.R :
  # ici on garde 3 cellules sur 4.
  dim_lon <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  dim_lat <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  var_country <- ncdf4::ncvar_def("country", "1", list(dim_lon, dim_lat),
                                  missval = NA, prec = "double")
  keep_matrix <- matrix(c(TRUE, TRUE, TRUE, FALSE), nrow = 2)
  nc <- ncdf4::nc_create(tmp_mask, list(var_country))
  ncdf4::ncvar_put(nc, var_country, matrix(as.numeric(keep_matrix), nrow = 2))
  ncdf4::nc_close(nc)

  reference_period <- c("2000-01-01", "2001-12-31")

  res_base  <- wind_component(tmp_u10, tmp_v10, "XX", reference_period,
                              study_period = reference_period,
                              mask_path = tmp_mask, area = FALSE)
  res_terra <- wind_component_terra(tmp_u10, tmp_v10, "XX", reference_period,
                                    study_period = reference_period,
                                    mask_path = tmp_mask, area = FALSE)

  expect_equal(dim(res_terra$data), dim(res_base$data))
  # wind_power_terra() masque wp_r (deja journalier) en un seul appel -- voir
  # wind_terra.R. On verifie que base-R et terra masquent exactement la
  # meme cellule, sans supposer laquelle a priori.
  masked_base  <- which(apply(is.na(res_base$data),  c(1, 2), all), arr.ind = TRUE)
  masked_terra <- which(apply(is.na(res_terra$data), c(1, 2), all), arr.ind = TRUE)
  expect_equal(nrow(masked_base), 1L)
  expect_equal(unname(masked_base), unname(masked_terra))

  non_na <- !is.na(res_base$data) & !is.na(res_terra$data)
  expect_true(any(non_na))
  expect_equal(res_terra$data[non_na], res_base$data[non_na], tolerance = 1e-6)
})
