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
## ---------------------------------------------------------------------------

library(xaci)
library(terra)

set.seed(42)

## 1. Construction d'un raster horaire synthetique -----------------------
nlon <- 5L
nlat <- 4L
years <- 2000:2003                       # 4 ans horaires -> ~35 000 couches
times <- seq(as.POSIXct(paste0(years[1], "-01-01 00:00"), tz = "UTC"),
             as.POSIXct(paste0(years[length(years)], "-12-31 23:00"), tz = "UTC"),
             by = "hour")
nlyr <- length(times)

vals <- array(runif(nlon * nlat * nlyr, min = 0, max = 10), dim = c(nlat, nlon, nlyr))

## Injecte des NA epars (pour tester na.rm=TRUE de facon realiste)
na_idx <- sample(length(vals), size = floor(0.01 * length(vals)))
vals[na_idx] <- NA

## Injecte UNE journee ENTIEREMENT NA (teste le cas -Inf/+Inf -> NA de
## is.infinite(), qui doit rester identique que l'on decoupe ou non)
full_na_day <- format(times[1000], "%Y-%m-%d")
full_na_hours <- which(format(times, "%Y-%m-%d") == full_na_day)
vals[, , full_na_hours] <- NA

r <- terra::rast(vals, extent = terra::ext(0, nlon, 0, nlat), crs = "EPSG:4326")
terra::time(r) <- times

cat(sprintf("Raster synthetique : %d couches horaires, %dx%d cellules (~%.4f Go)\n",
            terra::nlyr(r), nlon, nlat,
            as.numeric(terra::ncell(r)) * terra::nlyr(r) * 8 / 1024^3))

## 2. Comparaison baseline (non-decoupe) vs decoupe force ------------------
funs             <- c("sum", "max", "min", "mean")
chunk_thresholds <- c(1, 0.01, 1e-4, 1e-8)   # 1e-8 force le decoupage maximal

all_ok <- TRUE

for (fn in funs) {
  cat(sprintf("\n=== fun = \"%s\" ===\n", fn))

  baseline <- resample_daily_terra(r, fun = fn, target_chunk_gb = 1)   # reste en 1 bloc ici
  base_arr <- terra::as.array(baseline)
  base_time <- terra::time(baseline)

  for (thr in chunk_thresholds[-1]) {   # on saute 1 (= baseline elle-meme)
    chunked  <- resample_daily_terra(r, fun = fn, target_chunk_gb = thr)
    chk_arr  <- terra::as.array(chunked)
    chk_time <- terra::time(chunked)

    dims_ok  <- identical(dim(base_arr), dim(chk_arr))
    time_ok  <- identical(base_time, chk_time)
    vals_ok  <- isTRUE(all.equal(base_arr, chk_arr, tolerance = 0))  # egalite stricte attendue
    na_ok    <- identical(which(is.na(base_arr)), which(is.na(chk_arr)))

    status <- if (dims_ok && time_ok && vals_ok && na_ok) "OK" else "ECHEC"
    if (status == "ECHEC") all_ok <- FALSE

    cat(sprintf(
      "  target_chunk_gb = %-10g -> %d bloc(s) attendus | dims=%s time=%s valeurs=%s NA=%s -> %s\n",
      thr, max(1L, ceiling((as.numeric(terra::ncell(r)) * terra::nlyr(r) * 8 / 1024^3) / thr)),
      dims_ok, time_ok, vals_ok, na_ok, status
    ))

    if (!vals_ok) {
      diff <- abs(base_arr - chk_arr)
      cat(sprintf("    -> ecart max observe : %g\n", max(diff, na.rm = TRUE)))
    }
  }
}

cat("\n----------------------------------------\n")
cat(if (all_ok) "TOUS LES TESTS SONT PASSES : resultats identiques quel que soit target_chunk_gb.\n"
    else "AU MOINS UN TEST A ECHOUE : voir le detail ci-dessus.\n")
cat("----------------------------------------\n")

cat("\nVersion terra :", as.character(utils::packageVersion("terra")), "\n")
cat("Version xaci  :", as.character(utils::packageVersion("xaci")), "\n")
