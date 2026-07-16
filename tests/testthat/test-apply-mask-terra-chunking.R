library(testthat)

# ---------------------------------------------------------------------------
# Tests cibles sur le bug original :
#   > Error: [mask] cannot write more than 65535 layers
#
# 1. apply_mask_terra() doit basculer automatiquement sur un traitement par
#    blocs quand nlyr(r) > 65535, et produire un resultat identique a un
#    masquage direct (verifie couche par couche, y compris a cheval sur les
#    frontieres de blocs).
# 2. Masquer AVANT ou APRES une reduction temporelle (resample_daily_terra())
#    doit donner un resultat strictement identique, puisque le masque est
#    purement spatial (invariant dans le temps). C'est cette propriete qui
#    justifie le refactor de drought_terra.R / precipitation_terra.R /
#    temperature_terra.R / wind_terra.R (masquer apres reduction plutot
#    qu'avant, pour rester sous la limite de 65535 couches).
# ---------------------------------------------------------------------------

.build_country_mask_netcdf_for_rast <- function(path, template_r, keep_matrix_lonlat) {
  # template_r : SpatRaster de reference (meme grille que les donnees a
  # masquer). keep_matrix_lonlat : matrice [ncol x nrow], TRUE = cellule
  # gardee, dans la convention [lon croissant, lat croissant].
  lon <- sort(unique(terra::xFromCol(template_r, seq_len(terra::ncol(template_r)))))
  lat <- sort(unique(terra::yFromRow(template_r, seq_len(terra::nrow(template_r)))))

  dim_lon <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  dim_lat <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  var_country <- ncdf4::ncvar_def("country", "1", list(dim_lon, dim_lat),
                                  missval = NA, prec = "double")
  nc <- ncdf4::nc_create(path, list(var_country))
  ncdf4::ncvar_put(nc, var_country, matrix(as.numeric(keep_matrix_lonlat), nrow = length(lon)))
  ncdf4::nc_close(nc)
  invisible(path)
}

test_that("apply_mask_terra bascule sur le chunking au-dela de 65535 couches et reste correct", {
  skip_if_not_installed("terra")
  skip_on_cran()   # test plus lourd : produit ~70000 couches sur disque

  n_lyr <- 70000L   # > 65535 : reproduit exactement le cas qui declenchait
                     # "[mask] cannot write more than 65535 layers"
  r <- terra::rast(nrows = 2, ncols = 2, nlyrs = n_lyr,
                   vals = rep(seq_len(n_lyr), each = 4))
  terra::time(r) <- as.POSIXct("2000-01-01", tz = "UTC") +
    (seq_len(n_lyr) - 1) * 3600

  tmp_mask <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp_mask), add = TRUE)

  # Garde les 2 cellules du haut, masque les 2 cellules du bas.
  keep_matrix <- matrix(c(TRUE, FALSE, TRUE, FALSE), nrow = 2)
  .build_country_mask_netcdf_for_rast(tmp_mask, r, keep_matrix)

  masked <- apply_mask_terra(r, tmp_mask, threshold = 0.8, chunk_size = 20000)

  expect_equal(terra::nlyr(masked), n_lyr)
  expect_equal(as.character(as.Date(terra::time(masked))),
              as.character(as.Date(terra::time(r))))

  # Verifie le masquage sur des couches au debut, au milieu, a la fin, et a
  # cheval sur les frontieres de blocs (chunk_size = 20000) -- c'est la ou
  # un bug de reassemblage des blocs se remarquerait le plus facilement.
  check_layers <- c(1L, 2L, 19999L, 20000L, 20001L, 39999L, 40000L, 40001L,
                    59999L, 60000L, 60001L, n_lyr)
  arr <- terra::as.array(masked[[check_layers]])   # [nrow x ncol x k]

  for (k in seq_along(check_layers)) {
    layer_val <- check_layers[k]
    layer_arr <- arr[, , k]
    n_kept <- sum(!is.na(layer_arr))
    expect_equal(n_kept, 2L, info = paste("couche", layer_val))
    expect_true(all(layer_arr[!is.na(layer_arr)] == layer_val),
               info = paste("couche", layer_val))
  }
})

test_that("masquer avant ou apres resample_daily_terra donne un resultat identique", {
  skip_if_not_installed("terra")

  # Cette propriete (invariance du masque a l'ordre masquage/agregation,
  # puisque le masque ne varie pas dans le temps) est ce qui justifie le
  # refactor de drought_terra.R, precipitation_terra.R, temperature_terra.R
  # et wind_terra.R : masquer APRES reduction temporelle plutot qu'AVANT.
  time <- seq(as.POSIXct("2000-01-01 00:00", tz = "UTC"), by = "hour",
             length.out = 24 * 3)   # 3 jours, 2x2 cellules
  n_t <- length(time)

  r <- terra::rast(nrows = 2, ncols = 2, nlyrs = n_t,
                   vals = as.vector(sapply(seq_len(n_t), function(t) {
                     c(10, 20, 30, 40) + (t %% 5) + rnorm(4, sd = 0.1)
                   })))
  terra::time(r) <- time

  tmp_mask <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp_mask), add = TRUE)
  keep_matrix <- matrix(c(TRUE, TRUE, TRUE, FALSE), nrow = 2)
  .build_country_mask_netcdf_for_rast(tmp_mask, r, keep_matrix)

  # Ordre 1 (ancien comportement, celui qui declenchait le bug a grande
  # echelle) : masquer AVANT reduction.
  r_masked_first <- apply_mask_terra(r, tmp_mask)
  daily_after    <- resample_daily_terra(r_masked_first, fun = "mean")

  # Ordre 2 (nouveau comportement) : reduire PUIS masquer.
  daily_raw    <- resample_daily_terra(r, fun = "mean")
  daily_masked <- apply_mask_terra(daily_raw, tmp_mask)

  expect_equal(terra::as.array(daily_after), terra::as.array(daily_masked),
              tolerance = 1e-10)
  expect_equal(as.character(as.Date(terra::time(daily_after))),
              as.character(as.Date(terra::time(daily_masked))))
})
