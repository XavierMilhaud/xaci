## ---------------------------------------------------------------------------
## Test de non-regression : resample_daily_terra() -- decoupage temporel
##
## Objectif : verifier que le decoupage en blocs (target_chunk_gb) ne change
## JAMAIS le resultat numerique par rapport au chemin non-decoupe (baseline
## garde en RAM), quel que soit :
##   - la fonction d'agregation (sum / max / min / mean)
##   - le nombre de blocs (target_chunk_gb tres petit -> beaucoup de blocs)
##   - la presence de journees entierement NA
##
## Ne necessite AUCUNE donnee ERA5 : le raster est synthetique (petit,
## reproductible via set.seed()).
##
## Ces tests sont lourds (~35 000 couches horaires, des centaines de fichiers
## temporaires) : ils sont ignores sur CRAN.
##
## NB : le nombre de blocs est plafonne dans resample_daily_terra()
## (max_chunks = 500) pour ne pas depasser la limite de fichiers ouverts du
## systeme (1024 sous Linux). Les seuils utilises ici restent donc sous ce
## plafond, sauf dans le test dedie au plafond (voir plus bas).
## ---------------------------------------------------------------------------

## 1. Construction d'un raster horaire synthetique -----------------------
.make_synthetic_hourly_raster <- function() {
  set.seed(42)
  nlon <- 5L
  nlat <- 4L
  years <- 2000:2003                       # 4 ans horaires -> ~35 000 couches
  times <- seq(as.POSIXct(paste0(years[1], "-01-01 00:00"), tz = "UTC"),
               as.POSIXct(paste0(years[length(years)], "-12-31 23:00"), tz = "UTC"),
               by = "hour")
  nlyr <- length(times)

  vals <- array(stats::runif(nlon * nlat * nlyr, min = 0, max = 10),
                dim = c(nlat, nlon, nlyr))

  ## Injecte des NA epars (pour tester na.rm=TRUE de facon realiste)
  na_idx <- sample(length(vals), size = floor(0.01 * length(vals)))
  vals[na_idx] <- NA

  ## Injecte UNE journee ENTIEREMENT NA (teste le cas -Inf/+Inf -> NA de
  ## is.infinite(), qui doit rester identique que l'on decoupe ou non)
  full_na_day   <- format(times[1000], "%Y-%m-%d")
  full_na_hours <- which(format(times, "%Y-%m-%d") == full_na_day)
  vals[, , full_na_hours] <- NA

  r <- terra::rast(vals, extent = terra::ext(0, nlon, 0, nlat), crs = "EPSG:4326")
  terra::time(r) <- times
  r
}

## 2. Comparaison baseline (non-decoupe) vs decoupe force ------------------
.expect_chunked_equals_baseline <- function(r, fn, thr) {
  baseline <- resample_daily_terra(r, fun = fn, target_chunk_gb = 1)  # 1 seul bloc ici
  base_arr  <- terra::as.array(baseline)
  base_time <- terra::time(baseline)

  chunked  <- suppressMessages(resample_daily_terra(r, fun = fn, target_chunk_gb = thr))
  chk_arr  <- terra::as.array(chunked)
  chk_time <- terra::time(chunked)

  testthat::expect_identical(dim(chk_arr), dim(base_arr))
  testthat::expect_identical(chk_time, base_time)
  testthat::expect_equal(chk_arr, base_arr, tolerance = 0)          # egalite stricte attendue
  testthat::expect_identical(which(is.na(chk_arr)), which(is.na(base_arr)))
}

test_that("resample_daily_terra : le decoupage temporel ne change pas le resultat", {
  skip_if_not_installed("terra")
  skip_on_cran()   # test lourd : ~35 000 couches, jusqu'a ~260 blocs sur disque

  r <- .make_synthetic_hourly_raster()

  funs <- c("sum", "max", "min", "mean")
  # 1e-3 -> ~6 blocs ; 1e-4 -> ~53 blocs ; 2e-5 -> ~260 blocs (reste sous le
  # plafond de 500 blocs de resample_daily_terra()).
  chunk_thresholds <- c(1e-3, 1e-4, 2e-5)

  for (fn in funs) {
    for (thr in chunk_thresholds) {
      .expect_chunked_equals_baseline(r, fn, thr)
    }
  }
})

test_that("resample_daily_terra : le nombre de blocs est plafonne (pas d'erreur 'cannot open N files')", {
  skip_if_not_installed("terra")
  skip_on_cran()   # test lourd : ~500 fichiers temporaires

  r <- .make_synthetic_hourly_raster()

  # 1e-8 Go demanderait ~1461 blocs (un par jour), soit plus que la limite de
  # fichiers ouverts d'un Linux standard (1024) : sans plafond, terra echoue
  # avec "cannot open 1461 files". Avec max_chunks = 500, le resultat doit
  # rester identique au chemin non-decoupe.
  .expect_chunked_equals_baseline(r, "sum", 1e-8)
})
