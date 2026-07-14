library(testthat)

# ---------------------------------------------------------------------------
# Test de non-regression : calculate_percentiles() (base R / zoo) et
# calculate_percentiles_terra() (terra) doivent renvoyer des resultats
# identiques (aux arrondis flottants pres).
#
# Ce test fige deux correctifs valides empiriquement dans
# calculate_percentiles_terra() (voir R/component_terra.R) :
#   1. Decalage de +1 pas entre zoo::rollapply(align="center") et
#      terra::roll(type="around") pour une largeur de fenetre PAIRE.
#   2. terra::roll calcule une valeur sur une fenetre incomplete en bord de
#      serie, la ou zoo::rollapply(fill = NA) renvoie strictement NA.
#
# Si une future version de terra change son comportement de bord ou
# d'alignement, ce test doit le detecter.
# ---------------------------------------------------------------------------

test_that("calculate_percentiles et calculate_percentiles_terra concordent", {
  skip_if_not_installed("terra")

  set.seed(42)
  # 2 ans suffisent pour un test rapide tout en couvrant plusieurs occurrences
  # par jour-de-l'annee (necessaire pour que le quantile inter-annees ait un
  # sens).
  time_vec <- seq(as.POSIXct("2001-01-01 00:00", tz = "UTC"),
                 as.POSIXct("2002-12-31 23:00", tz = "UTC"), by = "hour")
  n_t    <- length(time_vec)
  saison <- 15 + 10 * sin(2 * pi * seq_along(time_vec) / (24 * 365))
  vals   <- saison + rnorm(n_t, sd = 2)

  ds <- list(data = array(vals, dim = c(1, 1, n_t)), time = time_vec,
            lon = 0, lat = 0)
  r  <- terra::rast(nrows = 1, ncols = 1, nlyrs = n_t, vals = vals)
  terra::time(r) <- time_vec

  reference_period <- c("2001-01-01", "2002-12-31")

  res_base  <- calculate_percentiles(ds, n = 90,
                                     reference_period = reference_period,
                                     part_of_day = "day")
  res_terra <- calculate_percentiles_terra(r, n = 90,
                                           reference_period = reference_period,
                                           part_of_day = "day")
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
