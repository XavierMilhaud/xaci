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

test_that("calculate_maximum_precipitation_over_window interprete window_size en JOURS meme avec des donnees horaires (regression du bug corrige)", {
  # 10 jours d'horaire, tp = 1/24 mm chaque heure -> total journalier = 1 mm/jour.
  # AVANT LE CORRECTIF : window_size=5 roulait directement sur les donnees
  # horaires brutes -> une fenetre de 5 HEURES (~5/24 = 0.208 mm), pas 5
  # jours. APRES CORRECTIF : resample_daily() est applique d'abord, donc
  # window_size=5 roule bien sur 5 jours -> 5 mm.
  time <- seq(as.POSIXct("2000-01-01 00:00", tz = "UTC"), by = "hour",
              length.out = 24 * 10)
  data <- array(1 / 24, dim = c(1, 1, length(time)))
  ds   <- list(data = data, time = time, lon = 0, lat = 0)

  res <- calculate_maximum_precipitation_over_window(ds, window_size = 5L)

  expect_equal(as.numeric(res$data[1, 1, 1]), 5, tolerance = 1e-8)
})
