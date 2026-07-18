#' @title Terra-based Loading and Hourly-to-Daily Reduction
#' @description Memory-safe alternatives to \code{load_netcdf()}/\code{apply_mask()}
#'   and to the hourly-resolution steps of the temperature, precipitation and wind
#'   pipelines (\code{temp_extremum()}, \code{calculate_percentiles()}, the daily
#'   resampling inside \code{wind_power()}).
#'
#'   \strong{Why this file exists:} for a whole country at hourly resolution
#'   over 40+ years, \code{ncdf4::ncvar_get()} materialises the full
#'   \code{[lon x lat x time]} array in RAM in one shot (tens of GB). \code{terra}
#'   reads NetCDF lazily (via GDAL) and processes computations block-by-block,
#'   writing results to disk instead of accumulating them in memory.
#'
#'   Only the hourly-scale steps are ported here. Once data has been reduced
#'   to DAILY resolution (via \code{resample_daily_terra()}, \code{temp_extremum_terra()}, ...),
#'   the resulting object is small enough (~40 years x 365 days) to convert back
#'   to a plain array with \code{.spatraster_to_list()} and hand off, unchanged, to the
#'   rest of the existing (already tested) pipeline -- \code{resample_monthly()},
#'   \code{standardize_metric()}, \code{.compute_aci_grid()}, etc.
#' @name component_terra
NULL

#' Lazily load a NetCDF variable as a terra SpatRaster
#'
#' Equivalent of \code{load_netcdf()}, but never materialises pixel values in
#' RAM: only metadata (dimensions, CRS, time) is read up front.
#'
#' @param path     Path to the NetCDF file.
#' @param var_name Name of the variable to extract.
#' @return A \code{terra::SpatRaster} with one layer per time step and
#'   \code{terra::time()} attached.
#' @export
#' @importFrom terra rast
load_netcdf_terra <- function(path, var_name) {
  r <- terra::rast(path, subds = var_name)
  names(r) <- rep(var_name, terra::nlyr(r))

  # terra reconnait generalement le temps CF (cas standard ERA5). En cas
  # d'echec, on retombe sur le MEME parsing manuel que load_netcdf(), mais
  # applique seulement au petit vecteur de temps (quelques dizaines de
  # milliers de valeurs), jamais aux donnees elles-memes.
  if (anyNA(terra::time(r))) {
    nc <- ncdf4::nc_open(path)
    on.exit(ncdf4::nc_close(nc))
    time_raw  <- ncdf4::ncvar_get(nc, "time")
    time_atts <- ncdf4::ncatt_get(nc, "time")
    origin <- sub(".*(since )(.+)", "\\2", time_atts$units)
    unit   <- trimws(sub(" since.*", "", time_atts$units))

    tvec <- if (grepl("hour", unit)) {
      as.POSIXct(origin, tz = "UTC") + time_raw * 3600
    } else if (grepl("day", unit)) {
      as.POSIXct(origin, tz = "UTC") + time_raw * 86400
    } else {
      stop("Unsupported time unit: ", time_atts$units)
    }
    terra::time(r) <- tvec
  }
  r
}

#' Apply a country mask to a SpatRaster
#'
#' Equivalent of \code{apply_mask()}: sets cells to \code{NA} where the mask
#' value is below \code{threshold}. Processed block-by-block by terra.
#'
#' \strong{Note sur la limite de 65535 couches :} le format interne utilise
#' par \code{terra::mask()} pour ecrire son resultat sur disque (fichier
#' temporaire) ne supporte pas plus de 65535 couches en sortie. Au-dela (cas
#' frequent en donnees horaires sur plusieurs decennies : ~300k couches pour
#' 35 ans), on ne peut pas masquer l'objet en un seul appel. Cette fonction
#' bascule donc automatiquement sur un traitement par blocs temporels
#' (`chunk_size` couches a la fois), ecrits en GeoTIFF (BigTIFF) puis
#' recombines en une seule source multi-fichiers, sans jamais materialiser
#' l'ensemble en RAM. Le masque etant purement spatial (identique a chaque
#' pas de temps), le decoupage en blocs ne change pas le resultat : un pixel
#' exclu par le masque l'est de la meme facon dans chaque bloc.
#'
#' @param r          A \code{terra::SpatRaster} (e.g. from \code{load_netcdf_terra()}).
#' @param mask_path  Path to the mask NetCDF file (variable: \code{country}).
#' @param threshold  Numeric threshold. Default \code{0.8}.
#' @param chunk_size Nombre max de couches traitees par bloc quand
#'   \code{nlyr(r)} depasse 65535. Default \code{20000} (marge confortable
#'   sous la limite, ajustable selon la RAM/disque disponibles).
#' @return The masked \code{terra::SpatRaster}.
#' @export
#' @importFrom terra rast compareGeom resample mask nlyr time writeRaster
apply_mask_terra <- function(r, mask_path, threshold = 0.8, chunk_size = 20000) {
  mask_r <- terra::rast(mask_path, subds = "country")
  if (!isTRUE(terra::compareGeom(r, mask_r, stopOnError = FALSE))) {
    mask_r <- terra::resample(mask_r, r[[1]], method = "near")
  }
  keep <- mask_r >= threshold

  n <- terra::nlyr(r)
  if (n <= 65535L) {
    return(terra::mask(r, keep, maskvalue = FALSE))
  }

  warning(
    "[apply_mask_terra] ", n, " couches (> 65535) : masquage par blocs de ",
    chunk_size, " couches (voir ?apply_mask_terra). Envisagez de masquer ",
    "apres reduction temporelle (resample_daily_terra()/tapp()) pour ",
    "eviter ce contournement, plus lent.",
    call. = FALSE
  )

  time_r  <- terra::time(r)
  starts  <- seq(1L, n, by = chunk_size)
  tmp_dir <- tempfile("mask_chunks_")
  dir.create(tmp_dir)
  tmp_files <- character(length(starts))

  for (i in seq_along(starts)) {
    idx <- starts[i]:min(starts[i] + chunk_size - 1L, n)
    tmp_files[i] <- file.path(tmp_dir, sprintf("chunk_%04d.tif", i))
    terra::mask(
      r[[idx]], keep, maskvalue = FALSE,
      filename = tmp_files[i], overwrite = TRUE,
      filetype = "GTiff", gdal = c("BIGTIFF=YES")
    )
  }

  out <- terra::rast(tmp_files)
  terra::time(out) <- time_r
  out
}

#' Load a NetCDF variable and optionally apply a country mask (terra version)
#'
#' Drop-in, memory-safe replacement for \code{load_component()} intended for
#' full-resolution, grid-cell-level ("area = FALSE") workflows over long
#' historical periods.
#'
#' @param data_path Path to the NetCDF file.
#' @param var_name  Name of the variable to extract.
#' @param mask_path  Path to the mask NetCDF file, or \code{NULL} (default).
#' @param threshold  Numeric threshold for the mask. Default \code{0.8}.
#' @param chunk_size Passe a \code{apply_mask_terra()} (voir sa doc) pour le
#'   cas ou \code{nlyr(r)} depasse la limite de 65535 couches.
#' @return A \code{terra::SpatRaster}.
#' @export
load_component_terra <- function(data_path, var_name, mask_path = NULL,
                                 threshold = 0.8, chunk_size = 20000) {
  r <- load_netcdf_terra(data_path, var_name)
  if (!is.null(mask_path)) {
    r <- apply_mask_terra(r, mask_path, threshold, chunk_size = chunk_size)
  }
  r
}

#' Resample a SpatRaster to daily resolution (terra version)
#'
#' Equivalent of \code{resample_daily()}. Groups layers by calendar day and
#' applies \code{fun}, writing the result to disk chunk-by-chunk via
#' \code{terra::tapp()} rather than holding the full input in RAM.
#'
#' @param r        A \code{terra::SpatRaster} with \code{terra::time()} set
#'   (sub-daily time steps expected).
#' @param fun      Aggregation function name understood by \code{terra::tapp()}
#'   (e.g. \code{"mean"}, \code{"sum"}, \code{"max"}, \code{"min"}).
#' @param filename Optional path to write the result directly to disk (highly
#'   recommended for large jobs). Default \code{""} (terra decides, using a
#'   temp file if the result doesn't fit in memory).
#' @return A \code{terra::SpatRaster} with one layer per day.
#' @export
#' @importFrom terra time tapp
resample_daily_terra <- function(r, fun = "mean", filename = "") {
  day_key    <- format(terra::time(r), "%Y-%m-%d")
  day_levels <- unique(day_key)              # ordre chronologique (r est trie par temps)
  day_idx    <- factor(day_key, levels = day_levels)

  # IMPORTANT : contrairement a apply()/zoo, terra::tapp() n'applique PAS
  # na.rm = TRUE implicitement pour les noms de fonctions integrees
  # ("mean", "sum", "min", "max", ...) -- un seul NA dans le groupe fait
  # basculer tout le resultat a NA/NaN. On force donc explicitement
  # na.rm = TRUE, que `fun` soit fourni en chaine ou en fonction, pour
  # rester coherent avec resample_daily() (base R).
  base_fun <- if (is.character(fun)) get(fun, mode = "function") else fun
  # suppressWarnings() : sur un groupe (jour) entierement NA, base_fun (max/
  # min) emet "no non-missing arguments" avant meme la correction -Inf/+Inf
  # -> NA ci-dessous -- purement du bruit puisque ce cas est deja gere.
  fun_narm <- function(x, ...) suppressWarnings(base_fun(x, na.rm = TRUE))

  out <- terra::tapp(r, index = day_idx, fun = fun_narm, filename = "")

  # Miroir de resample_daily() (component.R), maintenant corrigee :
  # is.infinite() attrape -Inf (FUN = max sur journee entierement NA) ET
  # +Inf (FUN = min sur journee entierement NA).
  out <- terra::ifel(is.infinite(out), NA, out)

  terra::time(out) <- as.POSIXct(day_levels, tz = "UTC")
  if (nzchar(filename)) terra::writeRaster(out, filename, overwrite = TRUE)
  out
}

#' Compute daily temperature extremum (min or max) for day or night hours
#' (terra version)
#'
#' Equivalent of \code{temp_extremum()}. Filters layers by hour-of-day (a
#' cheap, lazy operation on a SpatRaster -- no data is read), then reduces to
#' daily resolution via \code{resample_daily_terra()}.
#'
#' @param r        A \code{terra::SpatRaster}, hourly resolution, with
#'   \code{terra::time()} set.
#' @param extremum \code{"min"} or \code{"max"}.
#' @param period   \code{"day"} (hours 6-21) or \code{"night"} (hours 0-5 and
#'   22-23).
#' @param filename Optional output path (see \code{resample_daily_terra()}).
#' @return A \code{terra::SpatRaster} with one layer per day.
#' @export
temp_extremum_terra <- function(r, extremum, period, filename = "") {
  hours <- as.integer(format(terra::time(r), "%H"))
  keep <- if (period == "day") {
    hours %in% 6:21
  } else if (period == "night") {
    hours %in% c(0:5, 22:23)
  } else {
    stop("'period' must be 'day' or 'night'")
  }

  fun <- if (extremum == "max") "max"
  else if (extremum == "min") "min"
  else stop("'extremum' must be 'min' or 'max'")

  resample_daily_terra(r[[keep]], fun = fun, filename = filename)
}

#' Compute temperature percentile thresholds for each day of year (terra version)
#'
#' Equivalent of \code{calculate_percentiles()}. \strong{More experimental than
#' the other functions in this file}: it relies on \code{terra::roll()} for
#' the rolling-window quantile, whose exact argument names have changed across
#' terra versions -- check \code{?terra::roll} against your installed version
#' (\code{packageVersion("terra")}) and validate on a small subset before
#' running on the full 40-year series.
#'
#' \strong{Performance note:} the rolling-window quantile
#' (\code{terra::roll()}) is by far the most expensive step of the whole
#' \code{*_terra} pipeline -- it invokes a custom R callback
#' (\code{stats::quantile()}) at every timestep of \code{reference_period}
#' (filtered to day/night hours), for every cell. Unlike \code{terra::tapp()},
#' \code{terra::roll()} has no built-in \code{cores} argument in the terra
#' version this package was tested against (1.7.65) -- there is no GPU path
#' either (neither terra/GDAL nor base R ship a GPU rolling-quantile
#' primitive). Set \code{cores > 1} to work around this: the raster is split
#' into \code{cores} row-wise spatial tiles (independent computation, no
#' cross-tile dependency, so this cannot change the result), each processed
#' in a separate worker process via \code{parallel::makeCluster()}, and the
#' 366-layer outputs are merged back together.
#'
#' @inheritParams calculate_percentiles
#' @param r A \code{terra::SpatRaster}, hourly resolution, with
#'   \code{terra::time()} set (replaces the \code{dataset} argument of the
#'   base-R version).
#' @param filename Optional output path for the final thresholds.
#' @param cores Positive integer. If \code{cores > 1}, split the raster into
#'   that many row-wise spatial tiles and process them in parallel worker
#'   processes (see performance note above). Default \code{1} (sequential,
#'   identical to previous behaviour).
#' @return A \code{terra::SpatRaster} with 366 layers (day-of-year 1-366).
#' @export
#' @importFrom terra time roll tapp
calculate_percentiles_terra <- function(r, n, reference_period, part_of_day,
                                        filename = "", cores = 1L) {
  window_size <- if (part_of_day == "day") {
    80L
  } else if (part_of_day == "night") {
    40L
  } else {
    stop("'part_of_day' must be 'day' or 'night'")
  }

  hours <- as.integer(format(terra::time(r), "%H"))
  keep  <- if (part_of_day == "day") hours %in% 6:21 else hours %in% c(0:5, 22:23)
  r_sub <- r[[keep]]

  ref_start <- as.POSIXct(reference_period[1], tz = "UTC")
  ref_end   <- as.POSIXct(reference_period[2], tz = "UTC")
  ref_mask  <- terra::time(r_sub) >= ref_start & terra::time(r_sub) <= ref_end
  r_ref     <- r_sub[[ref_mask]]

  out_full <- if (cores <= 1L) {
    .calculate_percentiles_terra_core(r_ref, n, window_size, cores_tapp = 1L)
  } else {
    .calculate_percentiles_terra_tiled(r_ref, n, window_size, cores)
  }

  if (nzchar(filename)) terra::writeRaster(out_full, filename, overwrite = TRUE)
  out_full
}

#' Core sequential percentile computation (no tiling/parallelism)
#'
#' Extracted from \code{calculate_percentiles_terra()} so that the parallel,
#' tiled code path (\code{.calculate_percentiles_terra_tiled()}) can call the
#' EXACT same logic per-tile, guaranteeing byte-for-byte identical results to
#' the sequential path -- parallelism only changes HOW the computation is
#' split across processes, never the computation itself.
#'
#' @param r_ref SpatRaster, already filtered to day/night hours and to
#'   \code{reference_period}, with \code{terra::time()} set.
#' @param n Percentile (0-100).
#' @param window_size Rolling window size (80 for day, 40 for night).
#' @param cores_tapp Passed to \code{terra::tapp()}'s own \code{cores}
#'   argument for the day-of-year grouping step. Kept separate from the
#'   tiling-level \code{cores} of \code{calculate_percentiles_terra()}: when
#'   called from within a tiling worker, this should stay \code{1} to avoid
#'   nesting parallel clusters inside parallel workers.
#' @return A \code{terra::SpatRaster} with 366 layers (day-of-year 1-366).
#' @noRd
.calculate_percentiles_terra_core <- function(r_ref, n, window_size, cores_tapp = 1L) {
  qfun <- function(x, ...) stats::quantile(x, probs = n / 100, na.rm = TRUE)

  # Quantile glissant (fenetre centree, window_size pas de temps), traite par
  # blocs par terra -- jamais charge en entier en RAM.
  #
  # CORRECTIF VALIDE EMPIRIQUEMENT (voir tests/compare_window_alignment.R) :
  # pour une largeur PAIRE (80/40), zoo::rollapply(align = "center") centre
  # la fenetre sur i + 0.5, alors que terra::roll(type = "around") la centre
  # sur i - 0.5 -- un decalage constant de 1 pas. On corrige en decalant la
  # serie roulee d'une couche (rolled_shifted[i] <- rolled[i + 1]) AVANT
  # l'agregation par jour-de-l'annee, pour retrouver exactement la meme
  # convention que la version base-R/zoo. La derniere couche devient NA
  # (il n'existe pas de i+1 pour la derniere position).
  rolled <- terra::roll(r_ref, n = window_size, fun = qfun,
                        type = "around", circular = FALSE)

  nt_ref <- terra::nlyr(rolled)
  pad <- rolled[[nt_ref]]
  terra::values(pad) <- NA
  rolled <- c(rolled[[2:nt_ref]], pad)   # decalage de +1 pas (voir note ci-dessus)

  # CORRECTIF 2 (voir tests/compare_percentiles.R -- residu localise aux
  # bords de la periode de reference) : zoo::rollapply(fill = NA) exige une
  # fenetre COMPLETE de window_size valeurs et renvoie NA sinon ; terra::roll
  # semble calculer une valeur avec une fenetre partielle sur ces memes
  # positions de bord. On force donc explicitement NA sur les positions ou
  # zoo n'aurait pas assez de recul/avance, pour imposer la meme regle
  # stricte quel que soit le comportement interne de terra::roll.
  o_left  <- (window_size - 1) %/% 2
  o_right <- window_size - 1 - o_left
  invalid <- c(seq_len(o_left), seq(nt_ref - o_right + 1, nt_ref))
  invalid <- invalid[invalid >= 1 & invalid <= nt_ref]

  if (length(invalid) > 0) {
    na_layer <- rolled[[1]]
    terra::values(na_layer) <- NA
    for (k in invalid) rolled[[k]] <- na_layer   # remplace la couche k en place
  }

  day_ref <- as.integer(format(terra::time(r_ref), "%j"))
  day_idx <- factor(day_ref, levels = 1:366)

  out <- terra::tapp(rolled, index = day_idx, fun = qfun, filename = "",
                     cores = cores_tapp)

  # CORRECTIF 3 : quand un jour-de-l'annee (typiquement le 366e, sur des annees non bissextiles)
  # n'apparait jamais dans la periode de reference, terra::tapp() ne produit
  # une couche QUE pour les groupes reellement presents -- contrairement a la
  # version base-R qui alloue toujours un tableau [.., 366] avec NA pour les
  # jours absents. On reconstruit donc explicitement une sortie a 366
  # couches, en placant chaque couche calculee a la bonne position (jour de
  # l'annee) et en completant le reste avec des couches NA.
  present_days <- sort(unique(day_ref))
  if (terra::nlyr(out) != length(present_days)) {
    stop("Nombre de couches renvoyees par terra::tapp() incoherent avec le ",
         "nombre de jours-de-l'annee presents dans la periode de reference ",
         "-- le comportement de terra::tapp() sur les niveaux de facteur ",
         "vides a peut-etre change pour votre version de terra ",
         "(", as.character(utils::packageVersion("terra")), ").")
  }

  na_layer <- out[[1]]
  terra::values(na_layer) <- NA
  layers <- rep(list(na_layer), 366)
  for (k in seq_along(present_days)) layers[[present_days[k]]] <- out[[k]]

  terra::rast(layers)
}

#' Tiled/parallel percentile computation
#'
#' Splits \code{r_ref} into \code{cores} contiguous, non-overlapping row-wise
#' spatial tiles, processes each tile in a separate worker process (via
#' \code{.calculate_percentiles_terra_core()} -- the exact same sequential
#' logic, so results are identical to the non-tiled path), and merges the
#' resulting 366-layer tiles back into a single full-extent SpatRaster.
#'
#' The rolling-window quantile (\code{terra::roll()}) is purely a per-cell,
#' independent-in-space computation (no cross-cell dependency), so splitting
#' the grid spatially and recombining afterwards cannot change the result --
#' only how the work is distributed across processes.
#'
#' SpatRaster objects hold external C++ pointers that cannot be sent as-is to
#' another R process; \code{terra::wrap()}/\code{terra::unwrap()} are used to
#' (de)serialize tiles across the cluster, as recommended by the terra
#' documentation for parallel use.
#'
#' @inheritParams .calculate_percentiles_terra_core
#' @param cores Number of tiles/worker processes.
#' @return A \code{terra::SpatRaster} with 366 layers (day-of-year 1-366).
#' @noRd
.calculate_percentiles_terra_tiled <- function(r_ref, n, window_size, cores) {
  nr      <- terra::nrow(r_ref)
  n_tiles <- max(1L, min(as.integer(cores), nr))

  row_breaks <- floor(seq(0, nr, length.out = n_tiles + 1L))
  yres <- terra::yres(r_ref)

  tiles <- vector("list", n_tiles)
  for (t in seq_len(n_tiles)) {
    r1 <- row_breaks[t] + 1L
    r2 <- row_breaks[t + 1L]
    # yFromRow() decroit avec le numero de ligne (convention terra, lignes
    # numerotees du haut/nord vers le bas/sud) : on reconstruit une etendue
    # [y_min, y_max] correcte quel que soit le sens.
    y_r1 <- terra::yFromRow(r_ref, r1)
    y_r2 <- terra::yFromRow(r_ref, r2)
    e <- terra::ext(terra::xmin(r_ref), terra::xmax(r_ref),
                    min(y_r1, y_r2) - yres / 2, max(y_r1, y_r2) + yres / 2)
    tiles[[t]] <- terra::crop(r_ref, e)
  }

  wrapped_tiles <- lapply(tiles, terra::wrap)

  # PSOCK inconditionnellement (pas seulement sous Windows) : GDAL/terra n'est
  # PAS fork-safe -- son etat interne C++ (connexions GDAL, etc.) peut se
  # corrompre apres un fork(), y compris sous Linux/macOS. Verifie
  # empiriquement dans ce package : un cluster FORK plante silencieusement
  # ("Killed") sur des objets terra la ou PSOCK fonctionne de facon fiable.
  cl <- parallel::makeCluster(n_tiles, type = "PSOCK")
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, { library(terra) })

  # Un cluster PSOCK est un processus R totalement independant, qui ne peut
  # PAS retrouver le namespace du package via library(xaci) tant que le
  # package n'est pas formellement installe (ex. sous devtools::load_all()/
  # devtools::test(), qui NE cree PAS un package "installe" au sens ou un
  # nouveau processus pourrait le charger). On detache donc explicitement
  # .calculate_percentiles_terra_core() de l'espace de noms du package en
  # rattachant sa closure a globalenv() avant de l'envoyer au worker comme
  # une VALEUR autonome (argument de parLapply, pas une reference "xaci:::").
  # Ceci est sans danger : la fonction n'appelle que des fonctions
  # explicitement qualifiees (terra::, stats::), jamais un autre helper
  # interne du package.
  core_fun <- .calculate_percentiles_terra_core
  environment(core_fun) <- globalenv()

  results_wrapped <- parallel::parLapply(
    cl, wrapped_tiles,
    function(wt, n, window_size, core_fun) {
      tile_r <- terra::unwrap(wt)
      # cores_tapp = 1 : chaque tuile tourne DEJA dans son propre processus
      # (parallelisme au niveau des tuiles) -- inutile, et risque, d'ouvrir
      # un SECOND cluster imbrique pour le tapp() interne de cette meme tuile.
      out <- core_fun(tile_r, n, window_size, cores_tapp = 1L)
      terra::wrap(out)
    },
    n = n, window_size = window_size, core_fun = core_fun
  )

  tile_results <- lapply(results_wrapped, terra::unwrap)

  if (length(tile_results) == 1L) return(tile_results[[1]])
  do.call(terra::merge, tile_results)
}

#' Reorient a terra array to the package's [lon x lat x layer] convention
#'
#' \code{terra::as.array()} returns \code{[nrow x ncol x nlyr]} with rows
#' (latitude) in decreasing order. The rest of the package expects
#' \code{[lon x lat x layer]} with latitude increasing, matching
#' \code{load_netcdf()}. Shared by \code{.spatraster_to_list()} and by
#' \code{calculate_percentiles_terra()}'s day-of-year output (which has no
#' real time dimension, so \code{.spatraster_to_list()} doesn't apply).
#'
#' @param r A \code{terra::SpatRaster}.
#' @return A numeric array \code{[ncol x nrow x nlyr]} = \code{[lon x lat x layer]}.
#' @keywords internal
#' @importFrom terra as.array
.spatraster_to_array_only <- function(r) {
  arr <- terra::as.array(r)            # [nrow x ncol x nlyr], lat decroissante
  arr <- aperm(arr, c(2, 1, 3))         # -> [ncol x nrow x nlyr] = [lon x lat x layer]
  arr[, rev(seq_len(dim(arr)[2])), , drop = FALSE]   # lat en ordre croissant
}

#' Convert a (small) SpatRaster back to the package's plain-list format
#'
#' Once data has been reduced to daily (or coarser) resolution, it is small
#' enough to hand off to the rest of the existing base-R pipeline
#' (\code{resample_monthly()}, \code{standardize_metric()},
#' \code{.compute_aci_grid()}, etc.) unchanged. This is the bridge between the
#' terra-based loading/reduction steps and that pipeline.
#'
#' @param r A \code{terra::SpatRaster}, reasonably small (daily resolution or
#'   coarser -- NOT intended for hourly data).
#' @return A list with elements \code{data} ([lon x lat x time] array),
#'   \code{lon}, \code{lat}, \code{time} -- the same structure produced by
#'   \code{load_netcdf()}.
#' @export
#' @importFrom terra xFromCol yFromRow ncol nrow time
.spatraster_to_list <- function(r) {
  list(
    data = .spatraster_to_array_only(r),
    lon  = terra::xFromCol(r, seq_len(terra::ncol(r))),
    lat  = rev(terra::yFromRow(r, seq_len(terra::nrow(r)))),
    time = terra::time(r)
  )
}
