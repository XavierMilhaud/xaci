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
#
# BUG FIX (voir NEWS) : calculate_percentiles() prenait auparavant le jeu de
# donnees HORAIRE brut et calculait le seuil sur une fenetre glissante de
# window_size PAS HORAIRES (80 pour le jour, 40 pour la nuit), seuil ensuite
# compare a un extremum QUOTIDIEN dans .crossing_frequency() -- deux
# grandeurs differentes. Elle prend maintenant en entree la serie
# QUOTIDIENNE deja reduite (sortie de temp_extremum()), avec une fenetre
# glissante exprimee en JOURS (window_days, defaut 5), pour etre comparee a
# la meme grandeur que celle testee.
# ---------------------------------------------------------------------------

test_that("calculate_percentiles renvoie un tableau [lon x lat x 366] cohérent", {
  set.seed(1)
  time <- seq(as.POSIXct("2000-01-01 00:00", tz = "UTC"),
              by = "hour", length.out = 24 * 365)
  vals <- 15 + 10 * sin(2 * pi * seq_along(time) / (24 * 365)) +
    rnorm(length(time), sd = 1)
  ds  <- list(data = array(vals, dim = c(1, 1, length(time))),
              time = time, lon = 0, lat = 0)

  daily <- temp_extremum(ds, "max", "day")
  res <- calculate_percentiles(daily, n = 90,
                               reference_period = c("2000-01-01", "2000-12-31"))

  expect_equal(dim(res), c(1, 1, 366))
  expect_true(is.numeric(res))
  expect_true(any(!is.na(res)))
})

test_that("calculate_halfday_component rejette un part_of_day invalide", {
  # La validation de part_of_day se fait dans temp_extremum(), appelee par
  # calculate_halfday_component() -- calculate_percentiles() elle-meme n'a
  # plus de notion de part_of_day (elle recoit deja une serie quotidienne).
  time <- as.POSIXct("2000-01-01 00:00", tz = "UTC") + (0:23) * 3600
  ds   <- list(data = array(0:23, dim = c(1, 1, 24)), time = time, lon = 0, lat = 0)
  expect_error(calculate_halfday_component(ds, c("2000-01-01", "2000-01-01"),
                                           "midi", "max", 90, TRUE))
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

# ---------------------------------------------------------------------------
# Test de non-regression CENTRAL pour les deux correctifs ci-dessus : par
# construction, un seuil de n-ieme percentile calcule sur la periode de
# reference doit etre depasse environ (100-n)% du temps PENDANT cette meme
# periode de reference.
#   - Avant le correctif #1 : ~73% pour T90 et ~85% pour T10 (seuil construit
#     sur des valeurs HORAIRES, compare a un extremum QUOTIDIEN).
#   - Avec le correctif #1 seul (avant le #2) : ~5.6%/5.7% (percentile de
#     percentiles glissants, au lieu d'un percentile unique sur les valeurs
#     brutes poolees -- voir bug fix #2 dans calculate_percentiles()).
#   - Avec les deux correctifs : ~9.8%/9.9% sur des donnees synthetiques de
#     10 ans (mesure empirique).
# Les bornes ci-dessous sont resserrees pour detecter une regression de l'un
# ou l'autre bug, tout en laissant de la marge pour le bruit d'echantillonnage.
# ---------------------------------------------------------------------------

test_that("calculate_halfday_component recouvre ~10% d'exceedance sur la periode de reference elle-meme", {
  set.seed(123)
  time <- seq(as.POSIXct("1990-01-01 00:00", tz = "UTC"),
              as.POSIXct("1999-12-31 23:00", tz = "UTC"), by = "hour")
  doy  <- as.numeric(format(time, "%j"))
  hour <- as.numeric(format(time, "%H"))
  temp <- 12 + 10 * sin(2 * pi * (doy - 80) / 365.25) +
    6 * sin(2 * pi * (hour - 9) / 24) + rnorm(length(time), sd = 1.5)
  ds  <- list(data = array(temp, dim = c(1, 1, length(time))),
              time = time, lon = 0, lat = 0)
  ref <- c("1990-01-01", "1999-12-31")

  res90 <- calculate_halfday_component(ds, ref, "day", "max", 90,
                                       above_thresholds = TRUE)
  res10 <- calculate_halfday_component(ds, ref, "day", "min", 10,
                                       above_thresholds = FALSE)

  f90 <- mean(as.numeric(res90$data), na.rm = TRUE)
  f10 <- mean(as.numeric(res10$data), na.rm = TRUE)

  expect_gt(f90, 0.07); expect_lt(f90, 0.14)
  expect_gt(f10, 0.07); expect_lt(f10, 0.14)
})
