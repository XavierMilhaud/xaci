## ---------------------------------------------------------------------------
## resample_daily_terra(transform = ...) : calcul de la vitesse du vent PAR BLOC
##
## Regression : wind_component_terra() echouait avec
##   "[^] cannot write more than 65535 layers"
## sur de longues series horaires, car sqrt(u10^2 + v10^2) etait calcule d'un
## seul bloc sur toute la serie (terra ecrit alors le resultat dans un
## GeoTIFF temporaire, limite a 65535 couches). Le calcul est maintenant fait
## bloc par bloc, avant la reduction journaliere.
## ---------------------------------------------------------------------------

.make_uv_hourly <- function(n_hours, seed = 1) {
  set.seed(seed)
  times <- seq(as.POSIXct("2000-01-01 00:00", tz = "UTC"),
               by = "hour", length.out = n_hours)
  ua <- array(stats::rnorm(4 * n_hours), dim = c(2, 2, n_hours))
  va <- array(stats::rnorm(4 * n_hours), dim = c(2, 2, n_hours))
  u <- terra::rast(ua, extent = terra::ext(0, 2, 0, 2), crs = "EPSG:4326")
  v <- terra::rast(va, extent = terra::ext(0, 2, 0, 2), crs = "EPSG:4326")
  terra::time(u) <- times
  terra::time(v) <- times
  list(u = u, v = v, ua = ua, va = va, times = times)
}

.expected_daily_mean_ws <- function(ua, va, times) {
  day <- factor(format(times, "%Y-%m-%d"), levels = unique(format(times, "%Y-%m-%d")))
  ws  <- sqrt(ua^2 + va^2)
  exp_dxy <- apply(ws, c(1, 2), function(x) as.numeric(tapply(x, day, mean)))
  aperm(exp_dxy, c(2, 3, 1))   # -> [row x col x jours]
}

test_that("resample_daily_terra(transform=) : decoupe par bloc == calcul en RAM", {
  skip_if_not_installed("terra")
  skip_on_cran()

  d <- .make_uv_hourly(24L * 365L * 2L)             # 2 ans horaires

  expected <- .expected_daily_mean_ws(d$ua, d$va, d$times)

  out <- suppressMessages(resample_daily_terra(
    d$u, fun = "mean",
    transform = function(u, keep) sqrt(u^2 + d$v[[keep]]^2),
    max_layers_per_chunk = 2000L                     # force ~9 blocs
  ))

  testthat::expect_equal(terra::nlyr(out), 365L * 2L)
  testthat::expect_equal(terra::as.array(out), expected, tolerance = 1e-10)
})

test_that("resample_daily_terra(transform=) : OK au-dela de 65535 couches, meme ecriture disque forcee", {
  skip_if_not_installed("terra")
  skip_on_cran()

  d <- .make_uv_hourly(24L * 2920L)                  # 8 ans = 70080 couches > 65535
  testthat::expect_gt(terra::nlyr(d$u), 65535L)

  expected <- .expected_daily_mean_ws(d$ua, d$va, d$times)

  # todisk = TRUE force terra a ecrire chaque resultat intermediaire dans un
  # GeoTIFF temporaire (le cas qui declenchait l'erreur des 65535 couches).
  terra::terraOptions(todisk = TRUE)
  out <- tryCatch(
    suppressMessages(resample_daily_terra(
      d$u, fun = "mean",
      transform = function(u, keep) sqrt(u^2 + d$v[[keep]]^2)
    )),
    finally = terra::terraOptions(todisk = FALSE)
  )

  testthat::expect_equal(terra::nlyr(out), 2920L)
  testthat::expect_equal(terra::as.array(out), expected, tolerance = 1e-10)
})
