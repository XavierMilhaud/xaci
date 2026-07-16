library(testthat)

# ---------------------------------------------------------------------------
# Tests d'integration bout-en-bout :
#   drought_component() vs drought_component_terra()
#   precipitation_component() vs precipitation_component_terra()
# sur un vrai fichier NetCDF synthetique "tp" (2 ans d'horaire, 2x2 cellules).
#
# 2 ans (pas 1) delibérement, pour que drought_interpolate() ait reellement
# 2 annees a interpoler entre elles (avec 1 seule annee, la boucle
# d'interpolation est vide et le test ne validerait rien de cette partie du
# code).
# ---------------------------------------------------------------------------

.build_synthetic_tp_netcdf <- function(path, lon, lat, time_vec, origin, seed) {
  time_hours <- as.numeric(difftime(time_vec, origin, units = "hours"))

  set.seed(seed)
  nlo <- length(lon); nla <- length(lat)
  n_days <- length(time_vec) / 24
  vals <- array(NA_real_, dim = c(nlo, nla, length(time_vec)))

  for (i in seq_len(nlo)) {
    for (j in seq_len(nla)) {
      # Blocs alternes secs/humides, decales par cellule pour eviter des
      # sequences identiques partout (utile pour bien tester le CDD par
      # cellule independamment).
      block_lengths <- rep(c(8, 4, 15, 6, 10), length.out = 60)
      wet_dry <- rep(rep(c(0, 1), length.out = length(block_lengths)), block_lengths)
      wet_dry <- rep(wet_dry, length.out = n_days)
      shift <- (i + j) %% 5
      if (shift > 0) {
        wet_dry <- c(wet_dry[(shift + 1):length(wet_dry)], wet_dry[seq_len(shift)])
      }
      daily_vals  <- ifelse(wet_dry == 1, 0.002 + 0.0005 * (i + j), 0)
      hourly_vals <- rep(daily_vals, each = 24) / 24
      vals[i, j, ] <- hourly_vals
    }
  }

  dim_lon  <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  dim_lat  <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  dim_time <- ncdf4::ncdim_def("time", paste0("hours since ", format(origin, "%Y-%m-%d %H:%M:%S")),
                               time_hours, unlim = TRUE)
  var_tp   <- ncdf4::ncvar_def("tp", "m", list(dim_lon, dim_lat, dim_time),
                               missval = NA, prec = "double")

  nc <- ncdf4::nc_create(path, list(var_tp))
  ncdf4::ncvar_put(nc, var_tp, vals)
  ncdf4::nc_close(nc)
  invisible(path)
}

.build_synthetic_admin_mask_2x2 <- function(lon, lat) {
  # Meme convention que dans test-temperature-component-terra-integration.R,
  # redefinie ici pour que ce fichier soit autonome (pas de dependance a
  # l'ordre de chargement des fichiers de test).
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

.build_synthetic_country_mask_netcdf <- function(path, lon, lat, keep_matrix) {
  # keep_matrix : matrice [length(lon) x length(lat)], TRUE = cellule gardee
  # (a l'interieur du pays), FALSE = cellule masquee (mise a NA). Meme
  # convention que apply_mask_terra() : variable "country", valeurs
  # continues comparees a un seuil (0.8 par defaut) -- on utilise ici
  # simplement 1/0 pour ne pas ambigüiser le seuil.
  dim_lon <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  dim_lat <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  var_country <- ncdf4::ncvar_def("country", "1", list(dim_lon, dim_lat),
                                  missval = NA, prec = "double")
  nc <- ncdf4::nc_create(path, list(var_country))
  ncdf4::ncvar_put(nc, var_country, matrix(as.numeric(keep_matrix), nrow = length(lon)))
  ncdf4::nc_close(nc)
  invisible(path)
}

test_that("drought_component_terra concorde avec drought_component (admin_mask = NULL)", {
  skip_if_not_installed("terra")

  tmp_nc <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp_nc), add = TRUE)

  lon <- c(0, 1)
  lat <- c(0, 1)
  origin <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
  time_vec <- seq(as.POSIXct("2001-01-01 00:00", tz = "UTC"),
                  as.POSIXct("2002-12-31 23:00", tz = "UTC"), by = "hour")

  .build_synthetic_tp_netcdf(tmp_nc, lon, lat, time_vec, origin, seed = 31)

  reference_period <- c("2001-01-01", "2002-12-31")

  res_base  <- drought_component(tmp_nc, "XX", reference_period,
                                 study_period = reference_period, area = FALSE)
  res_terra <- drought_component_terra(tmp_nc, "XX", reference_period,
                                       study_period = reference_period, area = FALSE)

  expect_equal(dim(res_terra$data), dim(res_base$data))
  non_na <- !is.na(res_base$data) & !is.na(res_terra$data)
  expect_true(any(non_na))
  expect_equal(res_terra$data[non_na], res_base$data[non_na], tolerance = 1e-6)
  expect_equal(as.character(as.Date(res_terra$time)),
               as.character(as.Date(res_base$time)))
})

test_that("drought_component_terra concorde avec drought_component (admin_mask fourni)", {
  skip_if_not_installed("terra")

  tmp_nc <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp_nc), add = TRUE)

  lon <- c(0, 1)
  lat <- c(0, 1)
  origin <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
  time_vec <- seq(as.POSIXct("2001-01-01 00:00", tz = "UTC"),
                  as.POSIXct("2002-12-31 23:00", tz = "UTC"), by = "hour")

  .build_synthetic_tp_netcdf(tmp_nc, lon, lat, time_vec, origin, seed = 32)
  admin_mask <- .build_synthetic_admin_mask_2x2(lon, lat)

  reference_period <- c("2001-01-01", "2002-12-31")

  res_base  <- drought_component(tmp_nc, "XX", reference_period,
                                 study_period = reference_period,
                                 admin_mask = admin_mask)
  res_terra <- drought_component_terra(tmp_nc, "XX", reference_period,
                                       study_period = reference_period,
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

test_that("drought_component_terra concorde avec drought_component (mask_path fourni)", {
  skip_if_not_installed("terra")

  # Ce test couvre precisement le refactor issue du bug des 65535 couches :
  # drought_component_terra() masque desormais APRES reduction journaliere
  # (component_terra.R / drought_terra.R), et non plus sur les donnees
  # horaires brutes. On verifie ici que le resultat final reste identique a
  # la version base-R, ce qui valide a la fois la correction du masquage et
  # la propriete d'invariance d'ordre masquage/agregation qui la justifie.
  tmp_nc   <- tempfile(fileext = ".nc")
  tmp_mask <- tempfile(fileext = ".nc")
  on.exit(unlink(c(tmp_nc, tmp_mask)), add = TRUE)

  lon <- c(0, 1)
  lat <- c(0, 1)
  origin <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
  time_vec <- seq(as.POSIXct("2001-01-01 00:00", tz = "UTC"),
                  as.POSIXct("2002-12-31 23:00", tz = "UTC"), by = "hour")

  .build_synthetic_tp_netcdf(tmp_nc, lon, lat, time_vec, origin, seed = 51)

  # On masque une cellule sur quatre (garde 3, exclut la cellule lon[2]/lat[2])
  keep_matrix <- matrix(c(TRUE, TRUE, TRUE, FALSE), nrow = 2)
  .build_synthetic_country_mask_netcdf(tmp_mask, lon, lat, keep_matrix)

  reference_period <- c("2001-01-01", "2002-12-31")

  res_base  <- drought_component(tmp_nc, "XX", reference_period,
                                 study_period = reference_period,
                                 mask_path = tmp_mask, area = FALSE)
  res_terra <- drought_component_terra(tmp_nc, "XX", reference_period,
                                       study_period = reference_period,
                                       mask_path = tmp_mask, area = FALSE)

  expect_equal(dim(res_terra$data), dim(res_base$data))

  # Plutot que de supposer une cellule precise (l'orientation exacte
  # lon/lat dans le NetCDF depend de load_netcdf()/ncvar_get(), qu'on ne
  # veut pas re-deviner ici), on verifie que les DEUX versions masquent
  # exactement une seule et meme cellule -- c'est la propriete qui compte
  # reellement (coherence base-R / terra), independamment de toute
  # convention d'indexation.
  masked_base  <- which(apply(is.na(res_base$data),  c(1, 2), all), arr.ind = TRUE)
  masked_terra <- which(apply(is.na(res_terra$data), c(1, 2), all), arr.ind = TRUE)
  expect_equal(nrow(masked_base), 1L)
  expect_equal(unname(masked_base), unname(masked_terra))

  non_na <- !is.na(res_base$data) & !is.na(res_terra$data)
  expect_true(any(non_na))
  expect_equal(res_terra$data[non_na], res_base$data[non_na], tolerance = 1e-6)
})

test_that("precipitation_component_terra concorde avec precipitation_component (admin_mask = NULL)", {
  skip_if_not_installed("terra")

  tmp_nc <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp_nc), add = TRUE)

  lon <- c(0, 1)
  lat <- c(0, 1)
  origin <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
  time_vec <- seq(as.POSIXct("2001-01-01 00:00", tz = "UTC"),
                  as.POSIXct("2002-12-31 23:00", tz = "UTC"), by = "hour")

  .build_synthetic_tp_netcdf(tmp_nc, lon, lat, time_vec, origin, seed = 41)

  reference_period <- c("2001-01-01", "2002-12-31")

  res_base  <- precipitation_component(tmp_nc, "XX", reference_period,
                                       study_period = reference_period,
                                       window_size = 5L, area = FALSE)
  res_terra <- precipitation_component_terra(tmp_nc, "XX", reference_period,
                                             study_period = reference_period,
                                             window_size = 5L, area = FALSE)

  expect_equal(dim(res_terra$data), dim(res_base$data))
  non_na <- !is.na(res_base$data) & !is.na(res_terra$data)
  expect_true(any(non_na))
  expect_equal(res_terra$data[non_na], res_base$data[non_na], tolerance = 1e-6)
  expect_equal(as.character(as.Date(res_terra$time)),
               as.character(as.Date(res_base$time)))
})

test_that("precipitation_component_terra concorde avec precipitation_component (admin_mask fourni)", {
  skip_if_not_installed("terra")

  tmp_nc <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp_nc), add = TRUE)

  lon <- c(0, 1)
  lat <- c(0, 1)
  origin <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
  time_vec <- seq(as.POSIXct("2001-01-01 00:00", tz = "UTC"),
                  as.POSIXct("2002-12-31 23:00", tz = "UTC"), by = "hour")

  .build_synthetic_tp_netcdf(tmp_nc, lon, lat, time_vec, origin, seed = 42)
  admin_mask <- .build_synthetic_admin_mask_2x2(lon, lat)

  reference_period <- c("2001-01-01", "2002-12-31")

  res_base  <- precipitation_component(tmp_nc, "XX", reference_period,
                                       study_period = reference_period,
                                       window_size = 5L, admin_mask = admin_mask)
  res_terra <- precipitation_component_terra(tmp_nc, "XX", reference_period,
                                             study_period = reference_period,
                                             window_size = 5L, admin_mask = admin_mask)

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

test_that("precipitation_component_terra concorde avec precipitation_component (mask_path fourni)", {
  skip_if_not_installed("terra")

  tmp_nc   <- tempfile(fileext = ".nc")
  tmp_mask <- tempfile(fileext = ".nc")
  on.exit(unlink(c(tmp_nc, tmp_mask)), add = TRUE)

  lon <- c(0, 1)
  lat <- c(0, 1)
  origin <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
  time_vec <- seq(as.POSIXct("2001-01-01 00:00", tz = "UTC"),
                  as.POSIXct("2002-12-31 23:00", tz = "UTC"), by = "hour")

  .build_synthetic_tp_netcdf(tmp_nc, lon, lat, time_vec, origin, seed = 52)

  keep_matrix <- matrix(c(TRUE, TRUE, TRUE, FALSE), nrow = 2)
  .build_synthetic_country_mask_netcdf(tmp_mask, lon, lat, keep_matrix)

  reference_period <- c("2001-01-01", "2002-12-31")

  res_base  <- precipitation_component(tmp_nc, "XX", reference_period,
                                       study_period = reference_period,
                                       window_size = 5L,
                                       mask_path = tmp_mask, area = FALSE)
  res_terra <- precipitation_component_terra(tmp_nc, "XX", reference_period,
                                             study_period = reference_period,
                                             window_size = 5L,
                                             mask_path = tmp_mask, area = FALSE)

  expect_equal(dim(res_terra$data), dim(res_base$data))

  masked_base  <- which(apply(is.na(res_base$data),  c(1, 2), all), arr.ind = TRUE)
  masked_terra <- which(apply(is.na(res_terra$data), c(1, 2), all), arr.ind = TRUE)
  expect_equal(nrow(masked_base), 1L)
  expect_equal(unname(masked_base), unname(masked_terra))

  non_na <- !is.na(res_base$data) & !is.na(res_terra$data)
  expect_true(any(non_na))
  expect_equal(res_terra$data[non_na], res_base$data[non_na], tolerance = 1e-6)
})
