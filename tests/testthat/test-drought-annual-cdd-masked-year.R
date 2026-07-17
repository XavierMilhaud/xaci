library(testthat)

# ---------------------------------------------------------------------------
# Test de non-regression :
#   max(cdd_series[idx_yr], na.rm = TRUE) sur une cellule masquee (toutes ses
#   valeurs NA) pour une ANNEE CALENDAIRE ENTIERE renvoyait -Inf (pas NA),
#   avec le warning "no non-missing arguments to max; returning -Inf" -- une
#   vraie fuite de donnees (is.na(-Inf) est FALSE), distincte des bugs
#   sum()/length() deja corriges ailleurs (component.R, wind.R). Reperee sur
#   un run reel de calculate_aci() sur 25 ans de donnees France entiere.
#
# .max_consecutive_dry_days_from_daily() est le helper PARTAGE entre
# max_consecutive_dry_days() (base-R) et max_consecutive_dry_days_terra()
# (terra) -- le corriger une fois beneficie aux deux moteurs.
# ---------------------------------------------------------------------------

test_that(".max_consecutive_dry_days_from_daily ne laisse plus fuiter -Inf sur une annee entierement masquee", {
  # 2 annees completes, une cellule (2,2) entierement NA sur les 2 annees
  # (equivalent a une cellule masquee sur toute la duree de l'etude).
  daily_time <- seq(as.Date("2001-01-01"), as.Date("2002-12-31"), by = "day")
  nt <- length(daily_time)
  set.seed(1)
  data <- array(runif(2 * 2 * nt, 0, 5), dim = c(2, 2, nt))
  data[2, 2, ] <- NA_real_

  daily <- list(data = data, time = as.POSIXct(daily_time, tz = "UTC"),
               lon = c(0, 1), lat = c(0, 1))

  # Aucun warning ne doit etre emis (avant le correctif : "no non-missing
  # arguments to max; returning -Inf", un par annee et par cellule masquee).
  expect_warning(
    out <- xaci:::.max_consecutive_dry_days_from_daily(daily),
    regexp = NA
  )

  expect_equal(dim(out$data), c(2, 2, 2))   # 2 cellules lon x lat, 2 annees
  # La cellule masquee doit etre NA -- explicitement PAS -Inf (is.na(-Inf)
  # est FALSE en R, ce qui est precisement ce qui laissait fuiter le bug).
  expect_true(all(is.na(out$data[2, 2, ])))
  expect_false(any(is.infinite(out$data)))

  # Les cellules non masquees doivent rester des CDD annuels valides
  # (bornes 0..366, jamais NA ni -Inf).
  non_masked <- out$data[-2, , , drop = FALSE]
  expect_true(all(!is.na(non_masked)))
  expect_true(all(non_masked >= 0 & non_masked <= 366))
})

test_that("max_consecutive_dry_days_terra() beneficie du meme correctif (helper partage)", {
  skip_if_not_installed("terra")

  daily_time <- seq(as.Date("2001-01-01"), as.Date("2002-12-31"), by = "day")
  nt <- length(daily_time)
  set.seed(2)
  # Meme structure de donnees que le test base-R ci-dessus, pour comparaison
  # directe : cellule (2,2) entierement NA sur les 2 annees.
  vals_by_cell <- matrix(runif(4 * nt, 0, 5), nrow = 4)
  vals_by_cell[4, ] <- NA_real_   # cellule (2,2) en indexation raster row-major

  r <- terra::rast(nrows = 2, ncols = 2, nlyrs = nt,
                   vals = as.vector(vals_by_cell))
  terra::time(r) <- as.POSIXct(daily_time, tz = "UTC")

  out_terra <- withCallingHandlers(
    xaci:::.max_consecutive_dry_days_from_daily(xaci:::.spatraster_to_list(r)),
    warning = function(w) fail(paste("Warning inattendu :", conditionMessage(w)))
  )

  expect_false(any(is.infinite(out_terra$data)))
  expect_true(any(is.na(out_terra$data)))   # la cellule masquee doit etre NA
})
