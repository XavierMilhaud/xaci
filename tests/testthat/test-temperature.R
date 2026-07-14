library(testthat)

# ---------------------------------------------------------------------------
# temperature.R
# ---------------------------------------------------------------------------

test_that("temp_extremum sélectionne les bonnes heures et calcule le bon extremum", {
  # 24 pas horaires sur un seul jour, valeurs = heure (0..23)
  time <- as.POSIXct("2000-01-01 00:00", tz = "UTC") + (0:23) * 3600
  data <- array(0:23, dim = c(1, 1, 24))
  ds   <- list(data = data, time = time, lon = 0, lat = 0)

  # Jour = heures 6-21 -> valeurs 6..21
  expect_equal(as.numeric(temp_extremum(ds, "max", "day")$data[1, 1, 1]), 21)
  expect_equal(as.numeric(temp_extremum(ds, "min", "day")$data[1, 1, 1]), 6)

  # Nuit = heures 0-5 et 22-23 -> valeurs 0,1,2,3,4,5,22,23
  expect_equal(as.numeric(temp_extremum(ds, "max", "night")$data[1, 1, 1]), 23)
  expect_equal(as.numeric(temp_extremum(ds, "min", "night")$data[1, 1, 1]), 0)
})

test_that("temp_extremum rejette les arguments invalides", {
  time <- as.POSIXct("2000-01-01 00:00", tz = "UTC") + (0:23) * 3600
  ds   <- list(data = array(0:23, dim = c(1, 1, 24)), time = time, lon = 0, lat = 0)

  expect_error(temp_extremum(ds, "max", "afternoon"))
  expect_error(temp_extremum(ds, "median", "day"))
})

test_that("temp_extremum conserve la dimension temporelle jour-par-jour", {
  # 2 jours de 24h chacun
  time <- as.POSIXct("2000-01-01 00:00", tz = "UTC") + (0:47) * 3600
  data <- array(rep(0:23, 2), dim = c(1, 1, 48))
  ds   <- list(data = data, time = time, lon = 0, lat = 0)

  res <- temp_extremum(ds, "max", "day")
  expect_equal(dim(res$data)[3], 2)
  expect_equal(as.numeric(res$data[1, 1, ]), c(21, 21))
})

# ---------------------------------------------------------------------------
# calculate_percentiles / calculate_halfday_component
# Ces fonctions combinent une fenêtre glissante (largeur fixe de 80h/40h) et un
# regroupement par jour-de-l'année : impossible à vérifier "à la main" sur un
# petit jeu de données. On teste donc leur STRUCTURE (dimensions, type, bornes)
# sur une série synthétique d'un an, plutôt que des valeurs exactes.
# ---------------------------------------------------------------------------

test_that("calculate_percentiles renvoie un tableau [lon x lat x 366] cohérent", {
  set.seed(1)
  time <- seq(as.POSIXct("2000-01-01 00:00", tz = "UTC"),
             by = "hour", length.out = 24 * 365)
  vals <- 15 + 10 * sin(2 * pi * seq_along(time) / (24 * 365)) +
    rnorm(length(time), sd = 1)
  ds  <- list(data = array(vals, dim = c(1, 1, length(time))),
             time = time, lon = 0, lat = 0)

  res <- calculate_percentiles(ds, n = 90,
                               reference_period = c("2000-01-01", "2000-12-31"),
                               part_of_day = "day")

  expect_equal(dim(res), c(1, 1, 366))
  expect_true(is.numeric(res))
  expect_true(any(!is.na(res)))
})

test_that("calculate_percentiles rejette un part_of_day invalide", {
  time <- as.POSIXct("2000-01-01 00:00", tz = "UTC") + (0:23) * 3600
  ds   <- list(data = array(0:23, dim = c(1, 1, 24)), time = time, lon = 0, lat = 0)
  expect_error(calculate_percentiles(ds, 90, c("2000-01-01", "2000-01-01"), "midi"))
})

test_that("calculate_halfday_component renvoie une fréquence mensuelle bornée entre 0 et 1", {
  set.seed(1)
  time <- seq(as.POSIXct("2000-01-01 00:00", tz = "UTC"),
             by = "hour", length.out = 24 * 365)
  vals <- 15 + 10 * sin(2 * pi * seq_along(time) / (24 * 365)) +
    rnorm(length(time), sd = 1)
  ds  <- list(data = array(vals, dim = c(1, 1, length(time))),
             time = time, lon = 0, lat = 0)

  res <- calculate_halfday_component(ds,
                                     reference_period = c("2000-01-01", "2000-12-31"),
                                     part_of_day = "day", extremum = "max",
                                     percentile = 90, above_thresholds = TRUE)

  expect_equal(dim(res$data)[1:2], c(1, 1))
  expect_equal(dim(res$data)[3], 12)   # agrégation mensuelle -> 12 mois
  non_na <- res$data[!is.na(res$data)]
  expect_true(all(non_na >= 0 & non_na <= 1))
})
