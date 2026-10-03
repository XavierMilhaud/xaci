library(testthat)

# ---------------------------------------------------------------------------
# wind.R
# ---------------------------------------------------------------------------

test_that("wind_power calcule la puissance éolienne journalière attendue", {
  # Deux pas horaires le même jour : u=3, v=4 (constant) -> vitesse = 5 m/s
  time <- as.POSIXct(c("2000-01-01 00:00", "2000-01-01 12:00"), tz = "UTC")
  u10  <- list(data = array(c(3, 3), dim = c(1, 1, 2)), time = time, lon = 0, lat = 0)
  v10  <- list(data = array(c(4, 4), dim = c(1, 1, 2)), time = time, lon = 0, lat = 0)

  res <- wind_power(u10, v10)

  # wp = 0.5 * 1.23 * 5^3 = 76.875
  expect_equal(dim(res$data), c(1, 1, 1))
  expect_equal(as.numeric(res$data[1, 1, 1]), 76.875)
})

test_that("wind_power filtre correctement sur reference_period", {
  # 4 jours déjà quotidiens, u=1, v=0 -> ws=1, wp=0.5*1.23*1=0.615 partout
  time <- as.POSIXct(paste0("2000-01-0", 1:4), tz = "UTC")
  u10  <- list(data = array(1, dim = c(1, 1, 4)), time = time, lon = 0, lat = 0)
  v10  <- list(data = array(0, dim = c(1, 1, 4)), time = time, lon = 0, lat = 0)

  res <- wind_power(u10, v10, reference_period = c("2000-01-02", "2000-01-03"))

  expect_equal(dim(res$data)[3], 2)
  expect_equal(as.numeric(res$data[1, 1, ]), c(0.615, 0.615))
})

test_that("wind_thresholds, days_above_wind_thresholds et calculate_period_wind_exceedance_frequency s'enchaînent correctement", {
  # 3 dates au même jour-de-l'année (1er janvier), 3 années différentes.
  # 2000 et 2001 (référence) ont la même valeur -> écart-type nul -> seuil = valeur elle-même.
  # 2002 (hors référence) a une valeur bien supérieure -> doit dépasser le seuil.
  time <- as.POSIXct(c("2000-01-01", "2001-01-01", "2002-01-01"), tz = "UTC")
  u10  <- list(data = array(c(2, 2, 10), dim = c(1, 1, 3)), time = time, lon = 0, lat = 0)
  v10  <- list(data = array(0, dim = c(1, 1, 3)), time = time, lon = 0, lat = 0)
  ref  <- c("2000-01-01", "2001-12-31")

  thr <- wind_thresholds(u10, v10, ref)
  # wp(2000) = wp(2001) = 0.5*1.23*2^3 = 4.92 ; écart-type nul -> seuil = 4.92 pour les 3 pas
  # (même jour-de-l'année 1 pour les 3 dates)
  expect_equal(as.numeric(thr$data[1, 1, ]), rep(4.92, 3))

  above <- days_above_wind_thresholds(u10, v10, ref)
  # 2000, 2001 : wp == seuil (pas strictement supérieur) -> 0
  # 2002 : wp = 0.5*1.23*10^3 = 615 >> seuil 4.92 -> 1
  expect_equal(as.numeric(above$data[1, 1, ]), c(0, 0, 1))

  freq <- calculate_period_wind_exceedance_frequency(u10, v10, ref)
  # Chaque date tombe dans un mois différent (2000-01, 2001-01, 2002-01),
  # donc la fréquence mensuelle == la valeur binaire elle-même.
  expect_equal(dim(freq$data)[3], 3)
  expect_equal(as.numeric(freq$data[1, 1, ]), c(0, 0, 1))
})

# ---------------------------------------------------------------------------
# Test de non-regression pour le bug de moyenne vectorielle (voir NEWS) :
# avant le correctif, u et v etaient moyennes separement sur la journee puis
# la magnitude du vecteur moyen etait prise -- ce qui sous-estime la vitesse
# moyenne reelle des que la direction varie dans la journee (les composantes
# opposees s'annulent partiellement avant que la magnitude ne soit prise).
# Ici, le vent souffle a une vitesse CONSTANTE de 8 m/s, mais sa direction
# fait un tour complet (360 deg) sur 4 pas de temps du meme jour : la vitesse
# instantanee est TOUJOURS 8 m/s, donc une vitesse moyenne journaliere
# correctement calculee doit rester exactement 8 m/s (et non ~0, qui serait
# le resultat d'une moyenne vectorielle sur un tour complet).
# ---------------------------------------------------------------------------
test_that("wind_power ne sous-estime pas la vitesse via une moyenne vectorielle quand la direction tourne", {
  speed <- 8
  time  <- as.POSIXct(c("2000-01-01 00:00", "2000-01-01 06:00",
                        "2000-01-01 12:00", "2000-01-01 18:00"), tz = "UTC")
  # Direction : Est, Nord, Ouest, Sud (tour complet de 360 deg sur la journee)
  u10 <- list(data = array(c(speed, 0, -speed, 0), dim = c(1, 1, 4)),
              time = time, lon = 0, lat = 0)
  v10 <- list(data = array(c(0, speed, 0, -speed), dim = c(1, 1, 4)),
              time = time, lon = 0, lat = 0)

  res <- wind_power(u10, v10)

  expected_wp <- 0.5 * 1.23 * speed^3   # 0.5 * 1.23 * 8^3 = 314.88
  expect_equal(dim(res$data), c(1, 1, 1))
  expect_equal(as.numeric(res$data[1, 1, 1]), expected_wp, tolerance = 1e-8)
  # NB : l'ancienne moyenne vectorielle aurait donne ici une magnitude de
  # vecteur moyen ~= 0 (les 4 composantes s'annulent exactement sur un tour
  # complet), donc une puissance eolienne ~= 0 au lieu de 314.88.
})

test_that("wind_power gère un vecteur nul sans erreur", {
  time <- as.POSIXct("2000-01-01 00:00", tz = "UTC")
  u10  <- list(data = array(0, dim = c(1, 1, 1)), time = time, lon = 0, lat = 0)
  v10  <- list(data = array(0, dim = c(1, 1, 1)), time = time, lon = 0, lat = 0)

  res <- wind_power(u10, v10)
  expect_equal(as.numeric(res$data[1, 1, 1]), 0)
})
