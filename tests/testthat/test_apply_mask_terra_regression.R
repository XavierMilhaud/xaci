## ---------------------------------------------------------------------------
## Test de non-regression : apply_mask_terra() -- precision & correction du
## masquage, independamment du decoupage par blocs (chunk_size)
##
## Deux proprietes verifiees :
##   1. CORRECTION : le resultat masque est identique a un "oracle" calcule
##      en base R pur (sans passer par terra::mask()), pour lever tout doute
##      sur le comportement de l'argument maskvalue/maskvalues de terra.
##   2. PRECISION  : le resultat ne depend PAS de chunk_size (valeurs
##      CONTINUES, non entieres, pour que toute troncature float32 soit
##      visible -- le test integre au package utilise des entiers, qui ne
##      revelent pas ce probleme).
##
## Ne necessite AUCUNE donnee ERA5.
## ---------------------------------------------------------------------------

library(xaci)
library(terra)

set.seed(123)

## 1. Construction d'un raster horaire synthetique CONTINU ------------------
nlon <- 3L
nlat <- 3L
n_lyr <- 70005L                     # > 65535 : force la branche par blocs

times <- as.POSIXct("2000-01-01 00:00", tz = "UTC") + (seq_len(n_lyr) - 1) * 3600

vals <- array(runif(nlon * nlat * n_lyr, min = -50, max = 50), dim = c(nlat, nlon, n_lyr))

r <- terra::rast(vals, extent = terra::ext(0, nlon, 0, nlat), crs = "EPSG:4326")
terra::time(r) <- times

## Masque spatial simple (constant dans le temps) : 1 cellule sur 9 gardee
keep_matrix <- matrix(FALSE, nrow = nlat, ncol = nlon)
keep_matrix[2, 2] <- TRUE            # cellule centrale gardee, les 8 autres masquees

tmp_mask <- tempfile(fileext = ".nc")
on.exit(unlink(tmp_mask), add = TRUE)

lon <- sort(unique(terra::xFromCol(r, seq_len(terra::ncol(r)))))
lat <- sort(unique(terra::yFromRow(r, seq_len(terra::nrow(r)))))
dim_lon <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
dim_lat <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
var_country <- ncdf4::ncvar_def("country", "1", list(dim_lon, dim_lat),
                                missval = NA, prec = "double")
nc <- ncdf4::nc_create(tmp_mask, list(var_country))
## keep_matrix est [nrow x ncol] = [lat x lon] ; le fichier attend [lon x lat]
ncdf4::ncvar_put(nc, var_country, t(keep_matrix))
ncdf4::nc_close(nc)

## 2. Oracle base R (AUCUN terra::mask() implique) ---------------------------
## Convention terra::as.array() : [nrow x ncol x nlyr], lignes = latitude
## DEcroissante -- keep_matrix est deja dans cette convention (construite
## directement sur les dimensions du raster).
expected <- vals
for (k in seq_len(n_lyr)) expected[, , k][!keep_matrix] <- NA

## 3. Comparaison pour plusieurs chunk_size ----------------------------------
chunk_sizes <- c(1000L, 20000L, 69999L)   # du tres decoupe au quasi-1-bloc
all_ok <- TRUE

for (cs in chunk_sizes) {
  masked <- apply_mask_terra(r, tmp_mask, threshold = 0.8, chunk_size = cs)
  arr    <- terra::as.array(masked)

  dims_ok <- identical(dim(arr), dim(expected))
  na_ok   <- identical(which(is.na(arr)), which(is.na(expected)))
  vals_ok <- isTRUE(all.equal(arr, expected, tolerance = 0))

  status <- if (dims_ok && na_ok && vals_ok) "OK" else "ECHEC"
  if (status != "OK") all_ok <- FALSE

  cat(sprintf("chunk_size = %-6d -> dims=%s NA=%s valeurs=%s -> %s\n",
              cs, dims_ok, na_ok, vals_ok, status))

  if (!vals_ok) {
    diff <- abs(arr - expected)
    cat(sprintf("  -> ecart max observe : %g\n", max(diff, na.rm = TRUE)))
  }
}

cat("\n----------------------------------------\n")
cat(if (all_ok) "TOUS LES TESTS SONT PASSES : masquage correct et invariant a chunk_size.\n"
    else "AU MOINS UN TEST A ECHOUE : voir le detail ci-dessus.\n")
cat("----------------------------------------\n")
