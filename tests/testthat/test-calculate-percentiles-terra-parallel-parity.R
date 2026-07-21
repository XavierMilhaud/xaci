library(testthat)

# ---------------------------------------------------------------------------
# calculate_percentiles_terra(..., cores = N) decoupe la grille en tuiles
# spatiales traitees par des processus separes (parallel::makeCluster,
# PSOCK). Le calcul est un pur calcul par cellule (aucune dependance
# spatiale croisee), donc le decoupage ne doit JAMAIS changer le resultat --
# seulement la facon dont le travail est reparti. Ce test verifie cette
# invariance : cores = 1 (sequentiel) et cores > 1 (tuiles) doivent produire
# un resultat rigoureusement identique.
# ---------------------------------------------------------------------------

test_that("calculate_percentiles_terra(cores > 1) donne un resultat identique a cores = 1", {
  skip_if_not_installed("terra")
  skip_if_not_installed("parallel")
  skip_on_cran()   # lance un cluster PSOCK, plus lourd qu'un test unitaire classique

  nlo <- 2; nla <- 4
  time_vec <- seq(as.POSIXct("2000-01-01 00:00", tz = "UTC"),
                  as.POSIXct("2001-06-30 23:00", tz = "UTC"), by = "hour")
  nt <- length(time_vec)
  ncell <- nlo * nla

  set.seed(7)
  cell_offset <- rnorm(ncell, sd = 3)
  vals_matrix <- matrix(NA_real_, nrow = ncell, ncol = nt)
  for (c in seq_len(ncell)) {
    vals_matrix[c, ] <- 280 + cell_offset[c] + 5 * sin(seq_len(nt) / (24 * 30)) +
      rnorm(nt, sd = 2)
  }

  r <- terra::rast(nrows = nla, ncols = nlo, nlyrs = nt,
                   xmin = 0, xmax = nlo, ymin = 0, ymax = nla)
  terra::values(r) <- vals_matrix
  terra::time(r) <- time_vec

  reference_period <- c("2000-01-01", "2001-06-30")

  out_seq   <- calculate_percentiles_terra(r, n = 90, reference_period = reference_period,
                                           part_of_day = "day")
  out_tiled <- calculate_percentiles_terra(r, n = 90, reference_period = reference_period,
                                           part_of_day = "day", cores = 2)

  arr_seq   <- terra::as.array(out_seq)
  arr_tiled <- terra::as.array(out_tiled)

  expect_equal(dim(arr_seq), dim(arr_tiled))
  expect_identical(is.na(arr_seq), is.na(arr_tiled))
  expect_equal(arr_seq, arr_tiled, tolerance = 1e-10)
})

test_that("calculate_percentiles_terra(cores > 1) fonctionne avec plus de tuiles que de lignes", {
  skip_if_not_installed("terra")
  skip_if_not_installed("parallel")
  skip_on_cran()

  # Grille avec MOINS de lignes que de coeurs demandes : .calculate_percentiles_terra_tiled()
  # doit plafonner silencieusement le nombre de tuiles au nombre de lignes
  # disponibles plutot que de produire des tuiles vides/erreurs.
  nlo <- 2; nla <- 2
  time_vec <- seq(as.POSIXct("2000-01-01 00:00", tz = "UTC"),
                  as.POSIXct("2000-12-31 23:00", tz = "UTC"), by = "hour")
  nt <- length(time_vec)
  ncell <- nlo * nla

  set.seed(9)
  vals_matrix <- matrix(rnorm(ncell * nt, mean = 280, sd = 3), nrow = ncell, ncol = nt)

  r <- terra::rast(nrows = nla, ncols = nlo, nlyrs = nt,
                   xmin = 0, xmax = nlo, ymin = 0, ymax = nla)
  terra::values(r) <- vals_matrix
  terra::time(r) <- time_vec

  reference_period <- c("2000-01-01", "2000-12-31")

  expect_no_error(
    out_tiled <- calculate_percentiles_terra(r, n = 90, reference_period = reference_period,
                                             part_of_day = "night", cores = 8)
  )
  expect_equal(terra::nlyr(out_tiled), 366L)
})

test_that(".calculate_percentiles_terra_tiled : cores=1 avec decoupage force donne le meme resultat qu'1 seule tuile", {
  skip_if_not_installed("terra")
  skip_on_cran()

  # Regression cible : cores<=1 court-circuitait AUPARAVANT tout decoupage
  # en tuiles (appel direct a .calculate_percentiles_terra_core sur la
  # grille entiere), alors que le decoupage sert D'ABORD a plafonner la
  # memoire d'UN SEUL appel terra::roll() -- constate plantant sur une
  # grille France entiere / 13 ans de reference / heures de jour, MEME SANS
  # aucune parallelisation. Ce test force un decoupage en plusieurs tuiles
  # (target_tile_gb tres petit) tout en restant sequentiel (cores = 1), et
  # verifie que le resultat est identique a un traitement en 1 seule tuile.
  nlo <- 4; nla <- 6
  time_vec <- seq(as.POSIXct("2000-01-01 00:00", tz = "UTC"),
                  as.POSIXct("2001-06-30 23:00", tz = "UTC"), by = "hour")
  nt <- length(time_vec)
  ncell <- nlo * nla

  set.seed(42)
  cell_offset <- rnorm(ncell, sd = 3)
  vals_matrix <- matrix(NA_real_, nrow = ncell, ncol = nt)
  for (c in seq_len(ncell)) {
    vals_matrix[c, ] <- 280 + cell_offset[c] + 5 * sin(seq_len(nt) / (24 * 30)) +
      rnorm(nt, sd = 2)
  }

  r <- terra::rast(nrows = nla, ncols = nlo, nlyrs = nt,
                   xmin = 0, xmax = nlo, ymin = 0, ymax = nla)
  terra::values(r) <- vals_matrix
  terra::time(r) <- time_vec

  reference_period <- c("2000-01-01", "2001-06-30")
  hours <- as.integer(format(terra::time(r), "%H"))
  keep  <- hours %in% 6:21
  r_sub <- r[[keep]]
  ref_mask <- terra::time(r_sub) >= as.POSIXct(reference_period[1], tz = "UTC") &
    terra::time(r_sub) <= as.POSIXct(reference_period[2], tz = "UTC")
  r_ref <- r_sub[[ref_mask]]
  total_gb <- as.numeric(terra::ncell(r_ref)) * terra::nlyr(r_ref) * 8 / 1024^3

  out_1tile <- xaci:::.calculate_percentiles_terra_tiled(r_ref, 90, 80L, cores = 1L,
                                                         target_tile_gb = 10)
  out_3tiles_seq <- xaci:::.calculate_percentiles_terra_tiled(r_ref, 90, 80L, cores = 1L,
                                                              target_tile_gb = total_gb / 3)

  expect_equal(terra::as.array(out_1tile), terra::as.array(out_3tiles_seq),
               tolerance = 1e-10)
})
