library(testthat)

# ---------------------------------------------------------------------------
# precipitation.R
# ---------------------------------------------------------------------------

test_that("calculate_maximum_precipitation_over_window calcule le bon maximum mensuel", {
  # 10 jours de janvier 2000, valeurs croissantes 1..10
  time <- as.POSIXct(paste0("2000-01-", sprintf("%02d", 1:10)), tz = "UTC")
  data <- array(1:10, dim = c(1, 1, 10))
  ds   <- list(data = data, time = time, lon = 0, lat = 0)

  res <- calculate_maximum_precipitation_over_window(ds, var_name = "tp",
                                                     window_size = 3L)

  # Somme glissante (k=3, align="right") : 6,9,12,15,18,21,24,27 (2 premiers NA)
  # Maximum mensuel = 27
  expect_equal(dim(res$data), c(1, 1, 1))
  expect_equal(as.numeric(res$data[1, 1, 1]), 27)
  expect_equal(format(res$time, "%Y-%m-%d"), "2000-01-01")
})

test_that("calculate_maximum_precipitation_over_window distingue bien deux mois", {
  time <- as.POSIXct(c("2000-01-01", "2000-01-02", "2000-01-03",
                       "2000-02-01", "2000-02-02", "2000-02-03"), tz = "UTC")
  # Janvier : 5, 10, 2   | Février : 1, 1, 100
  data <- array(c(5, 10, 2, 1, 1, 100), dim = c(1, 1, 6))
  ds   <- list(data = data, time = time, lon = 0, lat = 0)

  res <- calculate_maximum_precipitation_over_window(ds, window_size = 2L)

  # Janvier, k=2, align="right" : NA, 15, 12        -> max = 15
  # Février, k=2, align="right" : NA,  2, 101       -> max = 101
  expect_equal(dim(res$data)[3], 2)
  expect_equal(as.numeric(res$data[1, 1, 1]), 15)
  expect_equal(as.numeric(res$data[1, 1, 2]), 101)
})

test_that("calculate_maximum_precipitation_over_window renvoie NA quand toute la fenêtre est manquante", {
  # Une seule journée dans le mois avec une fenêtre de 5 jours : la somme
  # glissante est entièrement NA -> max(..., na.rm = TRUE) vaut -Inf, qui doit
  # être ramené à NA en sortie.
  time <- as.POSIXct("2000-03-01", tz = "UTC")
  data <- array(10, dim = c(1, 1, 1))
  ds   <- list(data = data, time = time, lon = 0, lat = 0)

  res <- calculate_maximum_precipitation_over_window(ds, window_size = 5L)

  expect_true(is.na(res$data[1, 1, 1]))
})

test_that("calculate_maximum_precipitation_over_window conserve lon/lat multi-cellules", {
  time <- as.POSIXct(paste0("2000-01-0", 1:5), tz = "UTC")
  # 2 cellules spatiales (nl=2, nw=1), valeurs différentes par cellule.
  # Attention : array() remplit en column-major (le 1er indice varie le plus
  # vite), donc c(rep(1,5), rep(100,5)) ne donnerait PAS "cellule1 = que des 1,
  # cellule2 = que des 100". On assigne explicitement par indice pour éviter
  # ce piège.
  data <- array(NA_real_, dim = c(2, 1, 5))
  data[1, 1, ] <- 1     # cellule 1 : constante à 1
  data[2, 1, ] <- 100   # cellule 2 : constante à 100
  ds   <- list(data = data, time = time, lon = c(0, 1), lat = 0)

  res <- calculate_maximum_precipitation_over_window(ds, window_size = 2L)

  expect_equal(dim(res$data)[1], 2)
  # Cellule 1 : sommes glissantes de 1 -> max = 2 ; cellule 2 : sommes de 100 -> max = 200
  expect_equal(as.numeric(res$data[1, 1, 1]), 2)
  expect_equal(as.numeric(res$data[2, 1, 1]), 200)
})
