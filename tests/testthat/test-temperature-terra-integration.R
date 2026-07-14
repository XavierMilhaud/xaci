library(testthat)

# ---------------------------------------------------------------------------
# Parite calculate_halfday_component() vs calculate_halfday_component_terra()
#
# Les deux briques hourly-scale (temp_extremum/calculate_percentiles et leurs
# equivalents terra) sont deja validees individuellement
# (test-terra-daily-parity.R, test-percentiles-terra-parity.R). Ce test-ci
# valide la "colle" : conversion SpatRaster -> liste/array
# (.spatraster_to_list / .spatraster_to_array_only) et le partage du helper
# .crossing_frequency() entre les deux chemins.
# ---------------------------------------------------------------------------

test_that("calculate_halfday_component_terra concorde avec calculate_halfday_component", {
  skip_if_not_installed("terra")

  set.seed(123)
  # 2 ans d'horaire, grille 1x1 -- suffisant pour valider l'integration sans
  # rendre le test trop lent.
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

  for (part_of_day in c("day", "night")) {
    for (extremum in c("min", "max")) {
      res_base <- calculate_halfday_component(ds, reference_period, part_of_day,
                                               extremum, 90, TRUE)
      res_terra <- calculate_halfday_component_terra(r, reference_period,
                                                      part_of_day, extremum,
                                                      90, TRUE)

      label <- paste("part_of_day =", part_of_day, "/ extremum =", extremum)
      expect_equal(dim(res_terra$data), dim(res_base$data), info = label)

      non_na <- !is.na(res_base$data) & !is.na(res_terra$data)
      expect_true(any(non_na), info = label)
      expect_equal(res_terra$data[non_na], res_base$data[non_na],
                  tolerance = 1e-6, info = label)
      expect_equal(as.character(as.Date(res_terra$time)),
                  as.character(as.Date(res_base$time)), info = label)
    }
  }
})
