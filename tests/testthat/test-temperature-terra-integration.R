library(testthat)

# ---------------------------------------------------------------------------
# Test d'integration bout-en-bout : temperature_component() (base-R) vs
# temperature_component_terra(), sur un vrai petit fichier NetCDF synthetique
# (pas seulement des objets en memoire comme les tests precedents).
#
# Deux scenarios :
#   1. admin_mask = NULL (moyenne nationale) -- area = FALSE
#   2. admin_mask fourni directement (2 regions synthetiques), pour tester
#      reduce_dataarray_to_dataframe() sans dependre d'un telechargement
#      GADM via build_admin_mask().
# ---------------------------------------------------------------------------

.build_synthetic_t2m_netcdf <- function(path, lon, lat, time_vec, origin) {
  time_hours <- as.numeric(difftime(time_vec, origin, units = "hours"))

  set.seed(7)
  nlo <- length(lon); nla <- length(lat); nt <- length(time_vec)
  saison <- 288 + 10 * sin(2 * pi * seq_len(nt) / (24 * 365))  # ~15C en Kelvin
  vals <- array(NA_real_, dim = c(nlo, nla, nt))
  for (i in seq_len(nlo)) {
    for (j in seq_len(nla)) {
      # Un petit decalage par cellule pour s'assurer que les regions/cellules
      # ne sont pas toutes strictement identiques
      vals[i, j, ] <- saison + (i + j) + rnorm(nt, sd = 1.5)
    }
  }

  dim_lon  <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  dim_lat  <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  dim_time <- ncdf4::ncdim_def("time", paste0("hours since ", format(origin, "%Y-%m-%d %H:%M:%S")),
                               time_hours, unlim = TRUE)
  var_t2m  <- ncdf4::ncvar_def("t2m", "K", list(dim_lon, dim_lat, dim_time),
                               missval = NA, prec = "double")

  nc <- ncdf4::nc_create(path, list(var_t2m))
  ncdf4::ncvar_put(nc, var_t2m, vals)
  ncdf4::nc_close(nc)
  invisible(path)
}

.build_synthetic_admin_mask <- function(lon, lat) {
  # 2x2 grille, 2 regions synthetiques ("A" = ligne de latitude 1,
  # "B" = ligne de latitude 2), pondération 1/0 pour eviter tout calcul de
  # fraction de surface -- on isole ici juste la mecanique de
  # reduce_dataarray_to_dataframe(), pas build_admin_mask() (qui telecharge
  # des shapefiles).
  nw <- length(lat)
  weights <- vector("list", length(lon) * nw)
  for (i in seq_along(lon)) {
    for (j in seq_along(lat)) {
      idx <- (i - 1L) * nw + j
      weights[[idx]] <- if (j == 1) c(A = 1, B = 0) else c(A = 0, B = 1)
    }
  }
  list(units = c("A", "B"), lon = lon, lat = lat, weights = weights)
}

test_that("temperature_component_terra concorde avec temperature_component (admin_mask = NULL)", {
  skip_if_not_installed("terra")

  tmp_nc <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp_nc), add = TRUE)

  lon  <- c(0, 1)
  lat  <- c(0, 1)
  origin <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
  time_vec <- seq(as.POSIXct("2001-01-01 00:00", tz = "UTC"),
                  as.POSIXct("2001-12-31 23:00", tz = "UTC"), by = "hour")

  .build_synthetic_t2m_netcdf(tmp_nc, lon, lat, time_vec, origin)

  reference_period <- c("2001-01-01", "2001-12-31")

  res_base <- temperature_component(tmp_nc, "XX", reference_period,
                                    study_period = reference_period,
                                    percentile = 90, extremum = "max",
                                    above_thresholds = TRUE, area = FALSE)
  res_terra <- temperature_component_terra(tmp_nc, "XX", reference_period,
                                           study_period = reference_period,
                                           percentile = 90, extremum = "max",
                                           above_thresholds = TRUE, area = FALSE)

  expect_equal(dim(res_terra$data), dim(res_base$data))
  non_na <- !is.na(res_base$data) & !is.na(res_terra$data)
  expect_true(any(non_na))
  expect_equal(res_terra$data[non_na], res_base$data[non_na], tolerance = 1e-6)
  expect_equal(as.character(as.Date(res_terra$time)),
               as.character(as.Date(res_base$time)))
})

test_that("temperature_component_terra concorde avec temperature_component (admin_mask fourni)", {
  skip_if_not_installed("terra")

  tmp_nc <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp_nc), add = TRUE)

  lon  <- c(0, 1)
  lat  <- c(0, 1)
  origin <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
  time_vec <- seq(as.POSIXct("2001-01-01 00:00", tz = "UTC"),
                  as.POSIXct("2001-12-31 23:00", tz = "UTC"), by = "hour")

  .build_synthetic_t2m_netcdf(tmp_nc, lon, lat, time_vec, origin)
  admin_mask <- .build_synthetic_admin_mask(lon, lat)

  reference_period <- c("2001-01-01", "2001-12-31")

  res_base  <- temperature_component(tmp_nc, "XX", reference_period,
                                     study_period = reference_period,
                                     percentile = 90, extremum = "max",
                                     admin_mask = admin_mask)
  res_terra <- temperature_component_terra(tmp_nc, "XX", reference_period,
                                           study_period = reference_period,
                                           percentile = 90, extremum = "max",
                                           admin_mask = admin_mask)

  expect_s3_class(res_base, "data.frame")
  expect_s3_class(res_terra, "data.frame")
  expect_equal(dim(res_terra), dim(res_base))
  expect_equal(colnames(res_terra), colnames(res_base))
  expect_equal(rownames(res_terra), rownames(res_base))

  for (col in colnames(res_base)) {
    non_na <- !is.na(res_base[[col]]) & !is.na(res_terra[[col]])
    expect_true(any(non_na), info = col)
    expect_equal(res_terra[[col]][non_na], res_base[[col]][non_na],
                 tolerance = 1e-6, info = col)
  }
})

test_that("temperature_component_terra concorde avec temperature_component (mask_path fourni)", {
  skip_if_not_installed("terra")

  tmp_nc   <- tempfile(fileext = ".nc")
  tmp_mask <- tempfile(fileext = ".nc")
  on.exit(unlink(c(tmp_nc, tmp_mask)), add = TRUE)

  lon  <- c(0, 1)
  lat  <- c(0, 1)
  origin <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
  time_vec <- seq(as.POSIXct("2001-01-01 00:00", tz = "UTC"),
                  as.POSIXct("2001-12-31 23:00", tz = "UTC"), by = "hour")

  .build_synthetic_t2m_netcdf(tmp_nc, lon, lat, time_vec, origin)

  # Meme convention de masque "country" (variable, seuil 0.8) que
  # component_terra.R : ici on garde 3 cellules sur 4.
  dim_lon <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  dim_lat <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  var_country <- ncdf4::ncvar_def("country", "1", list(dim_lon, dim_lat),
                                  missval = NA, prec = "double")
  keep_matrix <- matrix(c(TRUE, TRUE, TRUE, FALSE), nrow = 2)
  nc <- ncdf4::nc_create(tmp_mask, list(var_country))
  ncdf4::ncvar_put(nc, var_country, matrix(as.numeric(keep_matrix), nrow = 2))
  ncdf4::nc_close(nc)

  reference_period <- c("2001-01-01", "2001-12-31")

  res_base <- temperature_component(tmp_nc, "XX", reference_period,
                                    study_period = reference_period,
                                    percentile = 90, extremum = "max",
                                    above_thresholds = TRUE,
                                    mask_path = tmp_mask, area = FALSE)
  res_terra <- temperature_component_terra(tmp_nc, "XX", reference_period,
                                           study_period = reference_period,
                                           percentile = 90, extremum = "max",
                                           above_thresholds = TRUE,
                                           mask_path = tmp_mask, area = FALSE)

  expect_equal(dim(res_terra$data), dim(res_base$data))
  # day_comp ET night_comp sont chacun masques separement (voir
  # temperature_terra.R). On verifie que base-R et terra masquent
  # exactement la meme cellule, sans supposer laquelle a priori (voir la
  # meme remarque dans le test drought/precipitation).
  masked_base  <- which(apply(is.na(res_base$data),  c(1, 2), all), arr.ind = TRUE)
  masked_terra <- which(apply(is.na(res_terra$data), c(1, 2), all), arr.ind = TRUE)
  expect_equal(nrow(masked_base), 1L)
  expect_equal(unname(masked_base), unname(masked_terra))

  non_na <- !is.na(res_base$data) & !is.na(res_terra$data)
  expect_true(any(non_na))
  expect_equal(res_terra$data[non_na], res_base$data[non_na], tolerance = 1e-6)
})
