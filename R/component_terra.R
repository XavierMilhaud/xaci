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
#' \strong{Precision numerique :} \code{terra} ecrit sur disque en simple
#' precision (\code{datatype = "FLT4S"}) PAR DEFAUT (voir
#' \code{terra::terraOptions()}), des que le resultat ne tient pas en RAM --
#' ce qui inclut la branche par blocs ci-dessous (TOUJOURS ecrite sur
#' disque). Sans le \code{datatype = "FLT8S"} explicite ci-dessous, un
#' masquage qui declenche une ecriture disque perdrait silencieusement de la
#' precision par rapport a un masquage qui reste en RAM -- incoherence
#' potentielle non liee au chunking lui-meme, mais au meme risque general
#' (voir la note similaire dans \code{resample_daily_terra()}).
#'
#' @param r          A \code{terra::SpatRaster} (e.g. from \code{load_netcdf_terra()}).
#' @param mask_path  Path to the mask NetCDF file (variable: \code{country}).
#' @param threshold  Numeric threshold. Default \code{0.8}.
#' @param chunk_size Nombre max de couches traitees par bloc quand
#'   \code{nlyr(r)} depasse 65535. Default \code{20000} (marge confortable
#'   sous la limite, ajustable selon la RAM/disque disponibles). Une valeur
#'   superieure a 65535 est silencieusement plafonnee a 65535, puisque
#'   chaque bloc est lui-meme ecrit en GeoTIFF et soumis a cette meme limite.
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
    return(terra::mask(r, keep, maskvalue = FALSE, datatype = "FLT8S"))
  }

  # Chaque bloc est lui-meme ecrit en GeoTIFF et est donc soumis a la meme
  # limite de 65535 couches. Un chunk_size superieur a cette limite ferait
  # echouer terra::mask() sur le bloc (voir test_apply_mask_terra_regression.R,
  # chunk_size = 69999L) : on le plafonne silencieusement ici.
  if (chunk_size > 65535L) {
    chunk_size <- 65535L
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
      filetype = "GTiff", gdal = c("BIGTIFF=YES"), datatype = "FLT8S"
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
#' applies \code{fun} via \code{terra::tapp()}.
#'
#' \strong{Memory note :} pour agreger par jour, \code{terra::tapp()} doit
#' lire, PAR BLOC SPATIAL, la serie temporelle COMPLETE de \code{r}
#' (\code{readValues()} recupere \code{nrows x ncol x nlyr(r)} valeurs d'un
#' coup, puis \code{matrix()} en fait une copie contigue). Sur une grille
#' pourtant petite (quelques milliers de cellules), \code{nlyr(r)} horaire
#' sur plusieurs decennies (des centaines de milliers de couches) suffit a
#' lui seul a depasser la limite memoire d'un processus R (constate
#' empiriquement : plafond de 16 Go de R sous macOS, "vector memory limit ...
#' reached", pour ~65 ans horaires sur la France entiere) -- MEME quand le
#' resultat final (agrege par jour) est minuscule. C'est exactement la meme
#' classe de probleme que celle documentee pour \code{terra::roll()} dans
#' \code{.calculate_percentiles_terra_tiled()} : le pic memoire depend de la
#' taille de L'ENTREE lue par bloc, pas de la sortie. On applique donc ici le
#' meme principe, en decoupant TEMPORELLEMENT (et non spatialement, puisque
#' c'est le nombre de couches qui explose ici, pas le nombre de cellules) :
#' \code{r} est scinde en blocs de jours CONSECUTIFS (donc de couches
#' horaires contigues -- l'ordre chronologique de \code{r} est requis),
#' chaque bloc est agrege independamment et ecrit sur disque, puis les blocs
#' journaliers (petits) sont recombines. Le resultat est identique, bloc par
#' bloc ou en un seul appel : chaque jour est agrege a partir de ses propres
#' couches horaires, jamais a cheval sur deux blocs.
#'
#' \strong{Precision numerique :} les blocs intermediaires sont ecrits sur
#' disque en \code{datatype = "FLT8S"} (double precision, 64 bits) --
#' explicitement force pour eviter que \code{terra} ne bascule par defaut
#' sur du float 32 bits, ce qui romprait l'invariance du resultat par
#' rapport a \code{target_chunk_gb} (chemin non-decoupe = calcul garde en
#' RAM, en double precision R). Le resultat est ainsi, a l'arithmetique
#' flottante pres (memes operations, meme ordre par groupe), rigoureusement
#' identique quel que soit le nombre de blocs.
#'
#' @param r        A \code{terra::SpatRaster} with \code{terra::time()} set
#'   (sub-daily time steps expected), sorted chronologically.
#' @param fun      Aggregation function name understood by \code{terra::tapp()}
#'   (e.g. \code{"mean"}, \code{"sum"}, \code{"max"}, \code{"min"}).
#' @param filename Optional path to write the final result directly to disk
#'   (highly recommended for large jobs). Default \code{""} (terra decides).
#' @param target_chunk_gb Target raw INPUT size (GB) per temporal chunk, used
#'   to decide how many chunks are needed for memory safety. Conservative
#'   default \code{1} -- lower it further if you still see
#'   \code{mem.maxVSize()}/OOM crashes on your machine; raise it (fewer,
#'   larger chunks, faster overall) only if you have RAM headroom to spare.
#' @return A \code{terra::SpatRaster} with one layer per day.
#' @export
#' @importFrom terra time tapp nlyr ncell rast writeRaster
resample_daily_terra <- function(r, fun = "mean", filename = "", target_chunk_gb = 1) {
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

  total_gb    <- (as.numeric(terra::ncell(r)) * terra::nlyr(r) * 8) / 1024^3
  n_days      <- length(day_levels)
  n_chunks    <- min(max(1L, ceiling(total_gb / target_chunk_gb)), n_days)

  if (n_chunks <= 1L) {
    out <- terra::tapp(r, index = day_idx, fun = fun_narm, filename = "")
  } else {
    message(sprintf(
      "[resample_daily_terra] %d couches (~%.2f Go), decoupe en %d bloc(s) temporel(s) de jours consecutifs pour plafonner le pic memoire.",
      terra::nlyr(r), total_gb, n_chunks
    ))

    day_breaks <- floor(seq(0, n_days, length.out = n_chunks + 1L))
    tmp_dir    <- tempfile("resample_daily_chunks_")
    dir.create(tmp_dir)
    tmp_files  <- character(n_chunks)

    for (i in seq_len(n_chunks)) {
      d_idx   <- (day_breaks[i] + 1L):day_breaks[i + 1L]
      d_sel   <- day_levels[d_idx]
      keep    <- day_key %in% d_sel                     # couches horaires du bloc (contigues)
      sub_idx <- factor(day_key[keep], levels = d_sel)

      tmp_files[i] <- file.path(tmp_dir, sprintf("chunk_%04d.tif", i))
      terra::tapp(r[[keep]], index = sub_idx, fun = fun_narm,
                  filename = tmp_files[i], overwrite = TRUE,
                  wopt = list(filetype = "GTiff", gdal = c("BIGTIFF=YES"),
                              datatype = "FLT8S"))
      # IMPORTANT : filetype/gdal/datatype DOIVENT passer par
      # wopt=list(...) -- passes en arguments nommes directs a
      # tapp(), ils sont silencieusement absorbes par "..." et
      # transmis (sans effet) a fun_narm() au lieu d'atteindre
      # writeRaster(). Bug constate empiriquement : sans wopt,
      # terra ecrivait en float32 par defaut malgre datatype =
      # "FLT8S" affiche en argument direct (voir le test de
      # non-regression : ecarts de l'ordre de l'epsilon float32,
      # ~4.8e-7, entre le chemin decoupe et le chemin en RAM).
    }

    out <- terra::rast(tmp_files)
  }

  # Miroir de resample_daily() (component.R), maintenant corrigee :
  # is.infinite() attrape -Inf (FUN = max sur journee entierement NA) ET
  # +Inf (FUN = min sur journee entierement NA).
  out <- terra::ifel(is.infinite(out), NA, out)

  terra::time(out) <- as.POSIXct(day_levels, tz = "UTC")
  if (nzchar(filename)) {
    terra::writeRaster(out, filename, overwrite = TRUE, datatype = "FLT8S")
  }
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
#' @param target_chunk_gb Passed to \code{resample_daily_terra()} (see its
#'   memory note) for memory-safe temporal chunking of large hourly series.
#' @return A \code{terra::SpatRaster} with one layer per day.
#' @export
temp_extremum_terra <- function(r, extremum, period, filename = "", target_chunk_gb = 1) {
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

  resample_daily_terra(r[[keep]], fun = fun, filename = filename,
                       target_chunk_gb = target_chunk_gb)
}

#' Compute temperature percentile thresholds for each day of year (terra version)
#'
#' Equivalent of \code{calculate_percentiles()}.
#'
#' \strong{Bug fix #1 (see NEWS):} this function used to take the raw
#' \strong{hourly} raster, filter it to day/night hours, and compute the
#' rolling-window percentile directly on those hourly values (with
#' \code{window_size} expressed in hours: 80 for day, 40 for night) --
#' while \code{calculate_halfday_component_terra()} compared the resulting
#' threshold to a \strong{daily} max/min. Because the daily extremum of
#' ~16 (or 8) hourly values almost always crosses the 90th (or 10th)
#' percentile of the hourly distribution, this made the exceedance
#' frequency during the reference period itself massively higher than the
#' intended ~10%. \code{calculate_percentiles_terra()} now takes the
#' already-reduced \strong{daily} extremum raster (the output of
#' \code{temp_extremum_terra()}) as input, with \code{window_days}
#' expressed in \strong{days} (default 5), so the threshold and the tested
#' variable are computed from the same daily quantity.
#'
#' \strong{Bug fix #2 (see NEWS):} even after bug fix #1, the threshold was
#' still computed as a rolling-window \code{n}-th percentile
#' (\code{terra::roll()}) followed by a \strong{second} \code{n}-th
#' percentile of those already-extreme rolled values, grouped by
#' day-of-year (\code{terra::tapp()}). This two-stage procedure
#' systematically under-counts the true exceedance frequency (observed
#' ~5.6% instead of ~10% on synthetic data, even with bug fix #1 alone
#' applied) -- see \code{calculate_percentiles()} (base-R engine) for the
#' full explanation and numeric illustration, which applies identically
#' here. \code{calculate_percentiles_terra()} now instead pools the RAW
#' daily values within \code{window_days} of a given calendar day, across
#' ALL years of the reference period, and takes a SINGLE
#' \code{terra::app()} quantile of that pooled sample per cell, per
#' calendar day -- the standard CLIMDEX/ACI methodology. This also removes
#' the \code{terra::roll()}/\code{terra::tapp()} dependency for this
#' function entirely (simpler, and no longer sensitive to
#' \code{terra::roll()}'s argument names changing across terra versions).
#'
#' \strong{Performance AND memory note:} looping over 366 calendar days and
#' calling \code{terra::app()} on a (typically ~100-200 layer) subset of the
#' reference period for each is comparable in total workload to the single
#' whole-series \code{terra::roll()} call used before bug fix #2, but
#' touches far fewer layers per call. A single such call over a whole
#' country's grid could still, in principle, use substantial memory for a
#' large grid -- as a precaution the raster is still ALWAYS split into small
#' spatial tiles sized to a conservative, fixed memory target (see
#' \code{target_tile_gb} in \code{.calculate_percentiles_terra_tiled()}),
#' regardless of \code{cores} -- \code{cores} only controls how many of
#' those already-memory-safe tiles run concurrently (default \code{1}: one
#' at a time).
#'
#' @inheritParams calculate_percentiles
#' @param r_daily A \code{terra::SpatRaster}, \strong{daily} resolution
#'   (typically the output of \code{temp_extremum_terra()}), with
#'   \code{terra::time()} set. \strong{Not} the raw hourly raster.
#' @param filename Optional output path for the final thresholds.
#' @param cores How many spatial tiles to process IN PARALLEL (a ceiling,
#'   further capped by \code{.safe_cores_terra()} based on RAM available and
#'   a single tile's size). Default \code{1} (sequential -- tiles are still
#'   used for memory safety even at \code{cores = 1}, just processed one
#'   after another instead of concurrently; see performance note above).
#'   \strong{Note:} the NUMBER of tiles is decided independently of
#'   \code{cores}, purely to keep a single \code{terra::roll()} call's memory
#'   footprint bounded (see \code{.calculate_percentiles_terra_tiled()}) --
#'   \code{cores} only controls how many of those (already memory-safe)
#'   tiles run at once.
#' @return A \code{terra::SpatRaster} with 366 layers (day-of-year 1-366).
#' @export
#' @importFrom terra time roll tapp
calculate_percentiles_terra <- function(r_daily, n, reference_period,
                                        window_days = 5L,
                                        filename = "", cores = 1L) {
  ref_start <- as.POSIXct(reference_period[1], tz = "UTC")
  ref_end   <- as.POSIXct(reference_period[2], tz = "UTC")
  ref_mask  <- terra::time(r_daily) >= ref_start & terra::time(r_daily) <= ref_end
  r_ref     <- r_daily[[ref_mask]]

  # Toujours passer par .calculate_percentiles_terra_tiled() : le decoupage
  # en tuiles protege la memoire INDEPENDAMMENT de cores (voir sa doc) --
  # cores = 1 ne doit PAS court-circuiter ce decoupage, sous peine de
  # retomber sur un seul appel terra::roll() geant sur la grille entiere
  # (constate plantant empiriquement, meme sans aucune parallelisation).
  out_full <- .calculate_percentiles_terra_tiled(r_ref, n, window_days, cores)

  if (nzchar(filename)) {
    terra::writeRaster(out_full, filename, overwrite = TRUE, datatype = "FLT8S")
  }
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
#' @param r_ref SpatRaster, DAILY resolution (already reduced from hourly
#'   data via \code{temp_extremum_terra()}), already filtered to
#'   \code{reference_period}, with \code{terra::time()} set.
#' @param n Percentile (0-100).
#' @param window_size Width, in DAYS, of the window used to pool
#'   neighbouring calendar days before taking the percentile (default 5,
#'   i.e. \eqn{\pm}{+/-}2 days, matching the CLIMDEX/ACI convention).
#' @param cores_tapp Number of processes handed to \code{terra::app()} for
#'   each of the 366 per-day-of-year calls. Kept separate from the
#'   tiling-level \code{cores} of \code{calculate_percentiles_terra()}: when
#'   called from within a tiling worker, this should stay \code{1} to avoid
#'   nesting parallel clusters inside parallel workers.
#' @return A \code{terra::SpatRaster} with 366 layers (day-of-year 1-366).
#' @noRd
.calculate_percentiles_terra_core <- function(r_ref, n, window_size, cores_tapp = 1L) {
  qfun <- function(x, ...) stats::quantile(x, probs = n / 100, na.rm = TRUE)

  day_ref <- as.integer(format(terra::time(r_ref), "%j"))
  nt_ref  <- terra::nlyr(r_ref)
  half    <- window_size %/% 2

  na_layer <- r_ref[[1]]
  terra::values(na_layer) <- NA

  # BUG FIX (voir la note dans calculate_percentiles_terra()) : au lieu d'un
  # quantile glissant PUIS d'un quantile des valeurs deja-glissees
  # (terra::roll() + terra::tapp(), une methode a deux etages qui biaise le
  # seuil final vers des valeurs trop extremes), on regroupe directement les
  # valeurs BRUTES tombant dans une fenetre de +/- half jours autour de
  # CHAQUE occurrence d'un jour-de-l'annee donne, sur TOUTES les annees de la
  # periode de reference, et on prend UN SEUL quantile de cet echantillon
  # poole, par cellule -- la methodologie CLIMDEX/ACI standard. On boucle
  # explicitement sur les 366 jours-de-l'annee (plutot que de s'appuyer sur
  # terra::tapp() par facteur), ce qui produit naturellement une sortie a
  # 366 couches y compris pour les jours absents de la periode de reference
  # (ex. le 366e sur des annees non bissextiles), sans post-traitement.
  layers <- vector("list", 366)
  for (d in 1:366) {
    occ <- which(day_ref == d)
    if (length(occ) == 0) {
      layers[[d]] <- na_layer
      next
    }
    window_idx <- unique(unlist(lapply(occ, function(k) {
      lo <- max(1L, k - half); hi <- min(nt_ref, k + half)
      lo:hi
    })))
    layers[[d]] <- terra::app(r_ref[[window_idx]], fun = qfun, cores = cores_tapp)
  }

  terra::rast(layers)
}

#' Detect available system memory, in GB (best effort, cross-platform)
#'
#' Reads OS-specific sources (\code{/proc/meminfo} on Linux, \code{vm_stat}
#' on macOS, \code{wmic} on Windows). Returns \code{NA_real_} if detection
#' fails for any reason (unsupported OS, restricted/sandboxed environment,
#' parsing failure, etc.) -- callers must handle that case explicitly rather
#' than assume a numeric result.
#'
#' @return A single numeric (GB), or \code{NA_real_} if undetectable.
#' @noRd
.detect_available_memory_gb <- function() {
  os <- Sys.info()[["sysname"]]
  tryCatch({
    if (identical(os, "Linux")) {
      meminfo <- readLines("/proc/meminfo")
      line <- grep("^MemAvailable:", meminfo, value = TRUE)
      if (length(line) == 0) line <- grep("^MemFree:", meminfo, value = TRUE)
      kb <- as.numeric(regmatches(line, regexpr("[0-9]+", line)))
      if (length(kb) != 1 || !is.finite(kb)) return(NA_real_)
      kb / 1024^2
    } else if (identical(os, "Darwin")) {
      page_size <- suppressWarnings(as.numeric(system("sysctl -n hw.pagesize", intern = TRUE)))
      vm <- system("vm_stat", intern = TRUE)
      get_pages <- function(pattern) {
        line <- grep(pattern, vm, value = TRUE)
        if (length(line) == 0) return(NA_real_)
        suppressWarnings(as.numeric(gsub("[^0-9]", "", line)))
      }
      free_pages     <- get_pages("Pages free:")
      inactive_pages <- get_pages("Pages inactive:")
      if (!is.finite(page_size) || !is.finite(free_pages) || !is.finite(inactive_pages)) {
        return(NA_real_)
      }
      (free_pages + inactive_pages) * page_size / 1024^3
    } else if (identical(os, "Windows")) {
      out <- system("wmic OS get FreePhysicalMemory /value", intern = TRUE)
      line <- grep("FreePhysicalMemory", out, value = TRUE)
      kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", line)))
      if (length(kb) != 1 || !is.finite(kb)) return(NA_real_)
      kb / 1024^2
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_,
  warning = function(w) NA_real_)
}

#' Cap the requested number of tiles/workers to a memory-safe value
#'
#' \strong{Approximate, best-effort safety net} -- not a guarantee. Estimates
#' peak memory from the ACTUAL size of \code{r_ref} (precise: cells x layers
#' x 8 bytes), multiplied by a rough per-worker overhead factor (input +
#' rolled series + the wrapped copy held in the master process while workers
#' run, plus a fixed R/GDAL startup cost per extra process). Compared against
#' a conservative FRACTION of detected available RAM (never assumes all of
#' it is free for this one computation -- other applications, the current R
#' session's other objects, and the OS itself all need headroom too).
#'
#' This can only ever REDUCE \code{cores_requested}, never increase it, and
#' falls back to the user's request unmodified (with a warning) if available
#' memory can't be detected on this system.
#'
#' @param r_ref SpatRaster, already filtered to day/night hours and
#'   \code{reference_period} (i.e. exactly what \code{.calculate_percentiles_terra_tiled()}
#'   is about to tile).
#' @param cores_requested Integer, what the user asked for.
#' @return Integer, \code{<= cores_requested}.
#' @noRd
.safe_cores_terra <- function(r_ref, cores_requested) {
  if (cores_requested <= 1L) return(1L)

  avail_gb <- .detect_available_memory_gb()
  if (is.na(avail_gb)) {
    warning(
      "[calculate_percentiles_terra] Impossible de detecter la RAM disponible ",
      "sur ce systeme -- 'cores' n'est pas ajuste automatiquement (utilise tel ",
      "quel : ", cores_requested, "). Si vous rencontrez des plantages memoire ",
      "(RStudio qui se ferme, \"error reading from connection\"), reduisez ",
      "'cores' manuellement.",
      call. = FALSE
    )
    return(as.integer(cores_requested))
  }

  gb_per_copy <- (as.numeric(terra::ncell(r_ref)) * terra::nlyr(r_ref) * 8) / 1024^3

  # Constantes approximatives (voir la documentation de la fonction) :
  # - budget_frac : ne jamais compter sur PLUS de la moitie de la RAM
  #   "disponible" pour ce seul calcul (marge pour RStudio, l'OS, le reste
  #   de la session R en cours, etc.)
  # - per_worker_multiplier : donnees d'entree + serie roulee (meme taille)
  #   + la copie wrappee retenue cote maitre pendant l'envoi aux workers
  # - process_overhead_gb : cout fixe (R + GDAL) par processus worker
  budget_frac          <- 0.5
  per_worker_multiplier <- 3
  process_overhead_gb  <- 0.4

  budget_gb  <- avail_gb * budget_frac
  safe_cores <- floor((budget_gb) / (gb_per_copy * per_worker_multiplier / cores_requested + process_overhead_gb))
  # gb_per_copy est la taille de la grille COMPLETE (avant decoupage) ; le
  # cout "donnees" par worker diminue avec le nombre de tuiles (chacune ne
  # porte qu'une fraction spatiale), d'ou la division par cores_requested
  # ci-dessus -- seul le cout fixe process_overhead_gb reste constant par
  # worker, quel que soit le nombre de tuiles.

  safe_cores <- max(1L, min(as.integer(cores_requested), as.integer(safe_cores)))

  if (safe_cores < cores_requested) {
    message(sprintf(
      paste0("[calculate_percentiles_terra] cores demande = %d, reduit a %d ",
             "d'apres la RAM disponible estimee (%.1f Go) et la taille des ",
             "donnees a traiter (%.2f Go/copie). Forcez 'cores' explicitement ",
             "pour outrepasser cette estimation (approximative)."),
      cores_requested, safe_cores, avail_gb, gb_per_copy
    ))
  }

  safe_cores
}


#'
#' \strong{Le decoupage en tuiles n'est PAS uniquement un mecanisme de
#' parallelisation.} Il sert d'abord a plafonner la memoire d'UN SEUL appel
#' \code{terra::roll()} -- constate empiriquement insuffisant a lui seul avec
#' \code{cores = 1} (aucun decoupage) sur une grille France entiere avec 13
#' ans de reference filtres sur les heures de jour : \code{terra::roll()}
#' peut faire planter R/RStudio meme SANS aucune parallelisation, la ou
#' \code{cores > 1} donnait l'illusion que le probleme etait le nombre de
#' processus. Le nombre de tuiles est donc calcule a partir d'une cible de
#' taille memoire FIXE et conservatrice par tuile (\code{target_tile_gb}),
#' independamment de \code{cores} -- \code{cores} ne controle QUE combien de
#' ces tuiles, deja necessaires pour la memoire, sont traitees EN PARALLELE
#' (\code{cores = 1} reste securise : il traite les memes petites tuiles,
#' juste les unes apres les autres au lieu de simultanement).
#'
#' Splits \code{r_ref} into contiguous, non-overlapping row-wise spatial
#' tiles, processes each tile (via \code{.calculate_percentiles_terra_core()}
#' -- the exact same sequential logic, so results are identical to the
#' non-tiled path, whether run sequentially or in parallel), and merges the
#' resulting 366-layer tiles back into a single full-extent SpatRaster.
#'
#' The rolling-window quantile (\code{terra::roll()}) is purely a per-cell,
#' independent-in-space computation (no cross-cell dependency), so splitting
#' the grid spatially and recombining afterwards cannot change the result --
#' only how (and in how many pieces) the work gets done.
#'
#' SpatRaster objects hold external C++ pointers that cannot be sent as-is to
#' another R process; \code{terra::wrap()}/\code{terra::unwrap()} are used to
#' (de)serialize tiles across the cluster, as recommended by the terra
#' documentation for parallel use -- only needed when actually parallelizing
#' (\code{n_workers > 1}); the sequential path avoids that overhead entirely.
#'
#' @inheritParams .calculate_percentiles_terra_core
#' @param cores Requested number of parallel workers (ceiling, further capped
#'   by \code{.safe_cores_terra()} using a single tile's size, not the whole
#'   grid's).
#' @param target_tile_gb Target raw data size (GB) per tile, used to decide
#'   how many tiles are needed for memory safety, REGARDLESS of \code{cores}.
#'   Conservative default \code{0.15} -- deliberately small: a single ~1.5GB
#'   tile (i.e. no tiling at all) was observed to crash R on a 16GB machine
#'   for the "day" part of \code{temperature_component_terra()} (percentile
#'   90/above_thresholds), back when \code{r_ref} still held \strong{hourly}
#'   data (\code{window_size = 80}) -- \code{terra::roll()}'s actual peak
#'   memory was well above what raw data size alone would suggest. Since
#'   \code{r_ref} is now DAILY data (see the bug-fix note in
#'   \code{calculate_percentiles_terra()}), the input is roughly 16-24x
#'   smaller and this scenario is far less likely to recur, but the
#'   conservative default is kept as-is since tiling remains harmless (only
#'   changes how the work is split, never the result). Lower this further if
#'   you still see crashes; raise it (fewer, larger tiles) only if you have
#'   headroom to spare and want fewer, faster per-tile calls.
#' @return A \code{terra::SpatRaster} with 366 layers (day-of-year 1-366).
#' @noRd
.calculate_percentiles_terra_tiled <- function(r_ref, n, window_size, cores,
                                               target_tile_gb = 0.15) {
  nr <- terra::nrow(r_ref)

  total_gb    <- (as.numeric(terra::ncell(r_ref)) * terra::nlyr(r_ref) * 8) / 1024^3
  n_tiles_mem <- max(1L, ceiling(total_gb / target_tile_gb))
  # Au moins autant de tuiles que de coeurs demandes (sinon certains workers
  # n'auraient rien a faire), mais jamais plus de lignes que la grille n'en a.
  n_tiles <- min(max(n_tiles_mem, as.integer(cores)), nr)
  n_tiles <- max(1L, n_tiles)

  if (n_tiles == 1L) {
    return(.calculate_percentiles_terra_core(r_ref, n, window_size, cores_tapp = 1L))
  }

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

  message(sprintf(
    "[calculate_percentiles_terra] %d tuile(s) spatiale(s) (~%.2f Go/tuile), pour plafonner le pic memoire.",
    n_tiles, total_gb / n_tiles
  ))

  # Nombre de WORKERS paralleles : plafonne par cores, par le nombre de
  # tuiles (inutile d'ouvrir plus de workers que de taches), ET par la RAM
  # disponible estimee a partir de la taille d'UNE SEULE tuile (et non plus
  # de la grille entiere comme avant) -- cores=1 saute directement au
  # traitement sequentiel ci-dessous, sans jamais tenter d'estimation RAM
  # inutile pour un seul worker.
  n_workers <- if (as.integer(cores) <= 1L) {
    1L
  } else {
    .safe_cores_terra(tiles[[1]], min(as.integer(cores), n_tiles))
  }

  if (n_workers <= 1L) {
    # Sequentiel PAR TUILE : le pic memoire ne depend plus que de la taille
    # d'UNE tuile (target_tile_gb), jamais de la grille entiere -- c'est ce
    # qui manquait avec l'ancien cores<=1 (qui traitait tout en un seul
    # appel terra::roll(), sans aucun decoupage).
    tile_results <- lapply(tiles, function(t) {
      .calculate_percentiles_terra_core(t, n, window_size, cores_tapp = 1L)
    })
  } else {
    wrapped_tiles <- lapply(tiles, terra::wrap)

    # PSOCK inconditionnellement (pas seulement sous Windows) : GDAL/terra
    # n'est PAS fork-safe -- son etat interne C++ (connexions GDAL, etc.)
    # peut se corrompre apres un fork(), y compris sous Linux/macOS. Verifie
    # empiriquement dans ce package : un cluster FORK plante silencieusement
    # ("Killed") sur des objets terra la ou PSOCK fonctionne de facon fiable.
    cl <- parallel::makeCluster(n_workers, type = "PSOCK")
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, { library(terra) })

    # Un cluster PSOCK est un processus R totalement independant, qui ne
    # peut PAS retrouver le namespace du package via library(xaci) tant que
    # le package n'est pas formellement installe (ex. sous
    # devtools::load_all()/devtools::test()). On detache donc explicitement
    # .calculate_percentiles_terra_core() de l'espace de noms du package en
    # rattachant sa closure a globalenv() avant de l'envoyer au worker comme
    # une VALEUR autonome. Sans danger : la fonction n'appelle que des
    # fonctions explicitement qualifiees (terra::, stats::).
    core_fun <- .calculate_percentiles_terra_core
    environment(core_fun) <- baseenv()

    # parLapply gere deja la file d'attente si n_tiles > n_workers (les
    # tuiles excedentaires sont distribuees aux workers au fur et a mesure
    # qu'ils se liberent) -- pas besoin de gerer ca a la main.
    results_wrapped <- parallel::parLapply(
      cl, wrapped_tiles,
      function(wt, n, window_size, core_fun) {
        tile_r <- terra::unwrap(wt)
        out <- core_fun(tile_r, n, window_size, cores_tapp = 1L)
        terra::wrap(out)
      },
      n = n, window_size = window_size, core_fun = core_fun
    )
    tile_results <- lapply(results_wrapped, terra::unwrap)
  }

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
