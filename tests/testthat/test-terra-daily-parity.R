library(testthat)

# ---------------------------------------------------------------------------
# Parite resample_daily() / resample_daily_terra() et
#        temp_extremum() / temp_extremum_terra()
# ---------------------------------------------------------------------------
#
# Convention pour construire un SpatRaster equivalent a une liste
# list(data, time, lon, lat) avec nw = 1 (une seule ligne de latitude) : les
# valeurs doivent etre fournies a terra::rast() dans l'ordre [cellule1..N]
# par pas de temps, ce que aperm(data, c(2,1,3)) + as.vector() donne
# directement (voir le raisonnement dans le commentaire de la fonction).
# ---------------------------------------------------------------------------

.list_to_rast_1row <- function(ds) {
  nl <- dim(ds$data)[1]
  nt <- dim(ds$data)[3]
  vals <- as.vector(aperm(ds$data, c(2, 1, 3)))
  r <- terra::rast(nrows = 1, ncols = nl, nlyrs = nt, vals = vals)
  terra::time(r) <- ds$time
  r
}

test_that("resample_daily_terra concorde avec resample_daily (mean/sum, NA partiel)", {
  skip_if_not_installed("terra")

  # 3 jours x 24h, 2 cellules. Cellule 1 a un NA ponctuel (jour 2, heure 10).
  time <- seq(as.POSIXct("2000-01-01 00:00", tz = "UTC"), by = "hour",
              length.out = 72)
  data <- array(NA_real_, dim = c(2, 1, 72))
  data[1, 1, ] <- rep(0:23, 3)
  data[2, 1, ] <- rep(100:123, 3)
  data[1, 1, 24 + 10 + 1] <- NA   # jour 2 (index 25-48), heure locale 10

  ds <- list(data = data, time = time, lon = c(0, 1), lat = 0)
  r  <- .list_to_rast_1row(ds)

  for (fun_name in c("mean", "sum")) {
    FUN_base <- get(fun_name)
    res_base  <- resample_daily(ds, FUN = FUN_base)
    res_terra <- resample_daily_terra(r, fun = fun_name)
    res_terra_arr <- terra::as.array(res_terra)

    # Reorienter [nrow x ncol x nlyr] -> [nl x nw x nt] pour comparer
    # directement (nrow=1 ici donc pas d'ambiguite de flip lat)
    res_terra_arr <- aperm(res_terra_arr, c(2, 1, 3))

    expect_equal(dim(res_terra_arr), dim(res_base$data),
                 info = paste("fun =", fun_name))
    expect_equal(res_terra_arr, res_base$data, tolerance = 1e-8,
                 info = paste("fun =", fun_name))
    expect_equal(as.character(as.Date(terra::time(res_terra))),
                 as.character(as.Date(res_base$time)))
  }
})

test_that("resample_daily_terra convertit -Inf en NA comme resample_daily (jour entierement NA, FUN = max)", {
  skip_if_not_installed("terra")

  # 3 jours x 24h, 2 cellules. Cellule 1 : jour 2 ENTIEREMENT NA -> avec
  # FUN = max, max(NA..., na.rm = TRUE) = -Inf, qui doit etre ramene a NA.
  time <- seq(as.POSIXct("2000-01-01 00:00", tz = "UTC"), by = "hour",
              length.out = 72)
  data <- array(NA_real_, dim = c(2, 1, 72))
  data[1, 1, ] <- rep(0:23, 3)
  data[1, 1, 25:48] <- NA_real_     # jour 2 entierement NA pour la cellule 1
  data[2, 1, ] <- rep(100:123, 3)  # cellule 2 : jamais de NA

  ds <- list(data = data, time = time, lon = c(0, 1), lat = 0)
  r  <- .list_to_rast_1row(ds)

  res_base  <- resample_daily(ds, FUN = max)
  res_terra <- resample_daily_terra(r, fun = "max")
  res_terra_arr <- aperm(terra::as.array(res_terra), c(2, 1, 3))

  # Jour 2, cellule 1 : NA des deux cotes (pas -Inf qui trainerait)
  expect_true(is.na(res_base$data[1, 1, 2]))
  expect_true(is.na(res_terra_arr[1, 1, 2]))
  expect_false(is.infinite(res_terra_arr[1, 1, 2]))

  expect_equal(res_terra_arr, res_base$data, tolerance = 1e-8)
})

test_that("resample_daily_terra convertit +Inf en NA comme resample_daily (jour entierement NA, FUN = min)", {
  skip_if_not_installed("terra")

  # Meme cas de figure que le test precedent, mais avec FUN = min : c'est le
  # cas qui revelait l'asymetrie -Inf/+Inf corrigee dans component.R et
  # component_terra.R (min(NA..., na.rm = TRUE) = +Inf, pas -Inf).
  time <- seq(as.POSIXct("2000-01-01 00:00", tz = "UTC"), by = "hour",
              length.out = 72)
  data <- array(NA_real_, dim = c(2, 1, 72))
  data[1, 1, ] <- rep(0:23, 3)
  data[1, 1, 25:48] <- NA_real_
  data[2, 1, ] <- rep(100:123, 3)

  ds <- list(data = data, time = time, lon = c(0, 1), lat = 0)
  r  <- .list_to_rast_1row(ds)

  res_base  <- resample_daily(ds, FUN = min)
  res_terra <- resample_daily_terra(r, fun = "min")
  res_terra_arr <- aperm(terra::as.array(res_terra), c(2, 1, 3))

  expect_true(is.na(res_base$data[1, 1, 2]))
  expect_true(is.na(res_terra_arr[1, 1, 2]))
  expect_false(is.infinite(res_terra_arr[1, 1, 2]))

  expect_equal(res_terra_arr, res_base$data, tolerance = 1e-8)
})

test_that("temp_extremum_terra concorde avec temp_extremum (day/night x min/max)", {
  skip_if_not_installed("terra")

  # 3 jours x 24h, 2 cellules, avec le meme NA ponctuel que le premier test
  # (jour 2, heure 10, cellule 1) pour verifier que le filtrage jour/nuit
  # + na.rm restent coherents entre les deux implementations.
  time <- seq(as.POSIXct("2000-01-01 00:00", tz = "UTC"), by = "hour",
              length.out = 72)
  data <- array(NA_real_, dim = c(2, 1, 72))
  data[1, 1, ] <- rep(0:23, 3)
  data[2, 1, ] <- rep(100:123, 3)
  data[1, 1, 24 + 10 + 1] <- NA

  ds <- list(data = data, time = time, lon = c(0, 1), lat = 0)
  r  <- .list_to_rast_1row(ds)

  for (period in c("day", "night")) {
    for (extremum in c("min", "max")) {
      res_base  <- temp_extremum(ds, extremum, period)
      res_terra <- temp_extremum_terra(r, extremum, period)
      res_terra_arr <- aperm(terra::as.array(res_terra), c(2, 1, 3))

      label <- paste("period =", period, "/ extremum =", extremum)
      expect_equal(dim(res_terra_arr), dim(res_base$data), info = label)
      expect_equal(res_terra_arr, res_base$data, tolerance = 1e-8, info = label)
      expect_equal(as.character(as.Date(terra::time(res_terra))),
                   as.character(as.Date(res_base$time)), info = label)
    }
  }
})

test_that("temp_extremum_terra rejette les arguments invalides comme temp_extremum", {
  skip_if_not_installed("terra")

  time <- seq(as.POSIXct("2000-01-01 00:00", tz = "UTC"), by = "hour",
              length.out = 24)
  r <- terra::rast(nrows = 1, ncols = 1, nlyrs = 24, vals = 0:23)
  terra::time(r) <- time

  expect_error(temp_extremum_terra(r, "max", "afternoon"))
  expect_error(temp_extremum_terra(r, "median", "day"))
})
