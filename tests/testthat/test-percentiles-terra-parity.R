library(testthat)

# ---------------------------------------------------------------------------
# Test de non-regression : calculate_percentiles() (base R) et
# calculate_percentiles_terra() (terra) doivent renvoyer des resultats
# identiques (aux arrondis flottants pres).
#
# Depuis le correctif du "double quantile" (voir NEWS / R/component.R /
# R/component_terra.R), les deux fonctions prennent en entree une serie deja
# QUOTIDIENNE (pas horaire) et n'ont plus de notion de part_of_day : elles
# regroupent directement les valeurs BRUTES d'une fenetre de +/- (window_days
# %/% 2) jours autour de chaque occurrence d'un jour-de-l'annee, sur toutes
# les annees de la periode de reference, et prennent un seul quantile de cet
# echantillon poole. Comme il n'y a plus de fenetre glissante
# (zoo::rollapply / terra::roll), il n'y a plus non plus d'ambiguite
# d'alignement centre/decalage a corriger entre les deux moteurs : ce test
# verifie que les deux implementations produisent bien le meme resultat.
# ---------------------------------------------------------------------------

test_that("calculate_percentiles et calculate_percentiles_terra concordent", {
  skip_if_not_installed("terra")

  set.seed(42)
  # 5 ans de donnees QUOTIDIENNES (deja reduites -- ce n'est plus le role de
  # ces fonctions de partir de donnees horaires) suffisent pour un test
  # rapide tout en couvrant plusieurs occurrences par jour-de-l'annee.
  time_vec <- seq(as.POSIXct("2001-01-01", tz = "UTC"),
                  as.POSIXct("2005-12-31", tz = "UTC"), by = "day")
  n_t    <- length(time_vec)
  saison <- 15 + 10 * sin(2 * pi * seq_along(time_vec) / 365.25)
  vals   <- saison + rnorm(n_t, sd = 2)

  daily_ds <- list(data = array(vals, dim = c(1, 1, n_t)), time = time_vec,
                   lon = 0, lat = 0)
  r_daily  <- terra::rast(nrows = 1, ncols = 1, nlyrs = n_t, vals = vals)
  terra::time(r_daily) <- time_vec

  reference_period <- c("2001-01-01", "2005-12-31")

  res_base  <- calculate_percentiles(daily_ds, n = 90,
                                     reference_period = reference_period)
  res_terra <- calculate_percentiles_terra(r_daily, n = 90,
                                           reference_period = reference_period)
  res_terra_arr <- terra::as.array(res_terra)

  vec_base  <- as.numeric(res_base)
  vec_terra <- as.numeric(res_terra_arr)

  # Memes positions de NA (jours sans donnees, ex. le 366e sur une annee non
  # bissextile)
  expect_equal(is.na(vec_base), is.na(vec_terra))

  commun <- !is.na(vec_base) & !is.na(vec_terra)
  expect_true(any(commun))   # sanity check : le test ne doit pas etre vide
  expect_equal(vec_base[commun], vec_terra[commun], tolerance = 1e-6)
})
