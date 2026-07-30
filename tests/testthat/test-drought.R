library(testthat)

# ---------------------------------------------------------------------------
# drought.R
#
# NB : .max_consecutive_dry_days_from_daily() a deja un test de
# non-regression dedie (fuite -Inf sur annee masquee) dans
# test-drought-annual-cdd-masked-year.R, et drought_component_terra() est
# deja compare a drought_component() (parite base-R / terra) dans
# test-drought-precipitation-component-terra-integration.R. Ce fichier
# complete avec : la correction du calcul du CDD lui-meme (valeurs connues
# a la main), drought_interpolate(), et le pipeline drought_component()
# (branche area = TRUE, cache save/computed_components, erreurs).
# ---------------------------------------------------------------------------

# --- .max_consecutive_dry_days_from_daily (correction de base) ---------------

test_that(".max_consecutive_dry_days_from_daily calcule le bon CDD annuel sur une sequence connue", {
  # Sequence sur 10 jours (annee 2001) : D D D W D D D D W D
  # (D = sec, valeur 0 ; W = humide, valeur 5 mm = 0.005 m > seuil 1 mm)
  # Series de jours consecutifs secs : 3, puis 4 (jours 5-8) -> max = 4
  vals <- c(0, 0, 0, 0.005, 0, 0, 0, 0, 0.005, 0)
  time <- seq(as.Date("2001-01-01"), by = "day", length.out = 10)
  daily <- list(data = array(vals, dim = c(1, 1, 10)),
                time = as.POSIXct(time, tz = "UTC"), lon = 0, lat = 0)

  out <- .max_consecutive_dry_days_from_daily(daily)

  expect_equal(dim(out$data), c(1, 1, 1))  # une seule annee
  expect_equal(out$data[1, 1, 1], 4)
  expect_equal(as.Date(out$time), as.Date("2001-12-31"))
})

test_that(".max_consecutive_dry_days_from_daily distingue plusieurs cellules independamment", {
  # Cellule (1,1) : tout sec (10 jours) -> CDD = 10
  # Cellule (1,2) : tout humide -> CDD = 0
  # Cellule (2,1) : alternance stricte D/W -> CDD = 1
  time <- seq(as.Date("2001-01-01"), by = "day", length.out = 10)
  data <- array(NA_real_, dim = c(2, 2, 10))
  data[1, 1, ] <- 0
  data[1, 2, ] <- 0.005
  data[2, 1, ] <- rep(c(0, 0.005), 5)
  data[2, 2, ] <- 0  # sert de "remplissage", non verifie ici

  daily <- list(data = data, time = as.POSIXct(time, tz = "UTC"), lon = c(0, 1), lat = c(0, 1))
  out <- .max_consecutive_dry_days_from_daily(daily)

  expect_equal(out$data[1, 1, 1], 10)
  expect_equal(out$data[1, 2, 1], 0)
  expect_equal(out$data[2, 1, 1], 1)
})

test_that(".max_consecutive_dry_days_from_daily : le calcul du CDD n'est PAS reinitialise a la frontiere d'annee (comportement actuel, a confirmer)", {
  # NB IMPORTANT (comportement decouvert en testant, a confirmer avec
  # l'auteur) : cdd_series est calculee comme une serie CUMULATIVE UNIQUE sur
  # tout l'historique (elle n'est jamais remise a zero au 1er janvier), et
  # c'est seulement APRES coup qu'on prend le max(cdd_series[annee]) pour
  # chaque annee. Consequence concrete : une secheresse commencee en
  # decembre d'une annee et qui se poursuit en janvier de la suivante fait
  # apparaitre, dans le CDD annuel de la 2e annee, un nombre de jours qui
  # inclut des jours secs de la 1ere annee (le compteur n'a jamais ete
  # remis a 0 au changement d'annee).
  # 5 derniers jours de 2001 secs + 3 premiers jours de 2002 secs (meme
  # sequence continue, jamais de jour humide) :
  time <- seq(as.Date("2001-12-27"), by = "day", length.out = 8)
  data <- array(0, dim = c(1, 1, 8))  # tout sec, aucune interruption

  daily <- list(data = data, time = as.POSIXct(time, tz = "UTC"), lon = 0, lat = 0)
  out <- .max_consecutive_dry_days_from_daily(daily)

  expect_equal(dim(out$data), c(1, 1, 2))
  expect_equal(out$data[1, 1, 1], 5)  # 2001 : 5 jours secs (27..31 dec)
  # 2002 : le compteur continue depuis 2001 (5) plutot que repartir a 0,
  # donc les 3 jours de janvier affichent 6, 7, 8 -- pas 1, 2, 3.
  expect_equal(out$data[1, 1, 2], 8)
})

# --- max_consecutive_dry_days (wrapper resample_daily + helper) --------------

test_that("max_consecutive_dry_days agrege d'abord en donnees journalieres avant de calculer le CDD", {
  # Meme sequence D D D W D D D D W D que le 1er test, mais fournie a
  # resolution HORAIRE (24 pas identiques par jour, valeur = total / 24) :
  # verifie que le wrapper resample_daily(FUN = sum) reconstruit bien les
  # memes totaux journaliers avant de calculer le CDD (attendu : 4).
  daily_vals <- c(0, 0, 0, 0.005, 0, 0, 0, 0, 0.005, 0)
  hourly_vals <- rep(daily_vals, each = 24) / 24
  time <- seq(as.POSIXct("2001-01-01 00:00", tz = "UTC"),
              as.POSIXct("2001-01-10 23:00", tz = "UTC"), by = "hour")

  dataset <- list(data = array(hourly_vals, dim = c(1, 1, length(time))),
                  time = time, lon = 0, lat = 0)

  out <- max_consecutive_dry_days(dataset)

  expect_equal(dim(out$data), c(1, 1, 1))
  expect_equal(out$data[1, 1, 1], 4)
})

# --- drought_interpolate ------------------------------------------------------

test_that("drought_interpolate interpole lineairement entre annees successives", {
  # 1 cellule, 2 annees : CDD(2001) = 10, CDD(2002) = 22
  cdd_annual <- list(
    data = array(c(10, 22), dim = c(1, 1, 2)),
    time = as.POSIXct(c("2001-12-31", "2002-12-31"), tz = "UTC"),
    lon = 0, lat = 0
  )

  out <- drought_interpolate(cdd_annual)

  expect_equal(dim(out$data), c(1, 1, 24))  # 12 mois x 2 annees
  expect_equal(as.Date(out$time)[1],  as.Date("2001-01-01"))
  expect_equal(as.Date(out$time)[12], as.Date("2001-12-01"))
  expect_equal(as.Date(out$time)[13], as.Date("2002-01-01"))
  expect_equal(as.Date(out$time)[24], as.Date("2002-12-01"))

  # CDD_m = (12-m)/12 * 10 + m/12 * 22, pour l'annee 2001
  expect_equal(out$data[1, 1, 1],  11/12 * 10 + 1/12  * 22)  # janvier
  expect_equal(out$data[1, 1, 6],  6/12  * 10 + 6/12  * 22)  # juin : moyenne simple
  expect_equal(out$data[1, 1, 12], 0/12  * 10 + 12/12 * 22)  # decembre : rejoint l'annee 2002

  # Derniere annee (2002) : valeur repetee sur les 12 mois
  expect_true(all(out$data[1, 1, 13:24] == 22))
})

test_that("drought_interpolate avec une seule annee repete simplement sa valeur sur 12 mois", {
  cdd_annual <- list(
    data = array(15, dim = c(1, 1, 1)),
    time = as.POSIXct("2005-12-31", tz = "UTC"),
    lon = 0, lat = 0
  )

  out <- drought_interpolate(cdd_annual)

  expect_equal(dim(out$data), c(1, 1, 12))
  expect_true(all(out$data == 15))
  expect_equal(as.Date(out$time)[1],  as.Date("2005-01-01"))
  expect_equal(as.Date(out$time)[12], as.Date("2005-12-01"))
})

# --- drought_component (pipeline complet) -------------------------------------

.build_drought_tp_netcdf <- function(path, lon, lat, time_vec, origin) {
  # Pattern secheresse/pluie identique sur toutes les cellules : blocs de
  # 10 jours secs puis 5 jours humides, repetes sur toute la periode.
  n_days <- length(time_vec) / 24
  block   <- rep(c(rep(0, 10), rep(1, 5)), length.out = n_days)
  daily_vals  <- ifelse(block == 1, 0.003, 0)
  hourly_vals <- rep(daily_vals, each = 24) / 24

  vals <- array(rep(hourly_vals, each = length(lon) * length(lat)),
                dim = c(length(lon), length(lat), length(time_vec)))

  build_synthetic_nc(path, "tp", "m", lon, lat, time_vec, origin, vals)
}

test_that("drought_component(area = TRUE) est cohérent avec l'enchaînement manuel des sous-étapes", {
  lon <- c(0, 1); lat <- c(0, 1)
  origin   <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
  time_vec <- seq(as.POSIXct("2001-01-01 00:00", tz = "UTC"),
                   as.POSIXct("2002-12-31 23:00", tz = "UTC"), by = "hour")

  tp_path <- tempfile(fileext = ".nc")
  .build_drought_tp_netcdf(tp_path, lon, lat, time_vec, origin)

  reference_period <- c("2001-01-01", "2002-12-31")

  res <- drought_component(tp_path, "XX", reference_period,
                            study_period = reference_period, area = TRUE)

  # Reproduction manuelle du pipeline interne
  ds          <- load_component(tp_path, "tp", NULL)
  cdd_annual  <- max_consecutive_dry_days(ds)
  cdd_monthly <- drought_interpolate(cdd_annual)
  expected    <- standardize_metric(cdd_monthly, reference_period, area = TRUE)

  expect_equal(res, expected)
  expect_true(is.numeric(res))
  expect_false(is.null(names(res)))
})

test_that("drought_component : save = TRUE puis computed_components = TRUE redonnent le même résultat (cache)", {
  lon <- c(0, 1); lat <- c(0, 1)
  origin   <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
  time_vec <- seq(as.POSIXct("2001-01-01 00:00", tz = "UTC"),
                   as.POSIXct("2002-12-31 23:00", tz = "UTC"), by = "hour")

  tp_path <- tempfile(fileext = ".nc")
  .build_drought_tp_netcdf(tp_path, lon, lat, time_vec, origin)

  cache_dir <- tempfile()
  dir.create(cache_dir)
  reference_period <- c("2001-01-01", "2002-12-31")
  study_period      <- reference_period  # tag de cache "drought_2001_2002_2002.rds"

  res_fresh <- drought_component(tp_path, "XX", reference_period, study_period,
                                  area = FALSE, save = TRUE, save_dir = cache_dir)

  expect_true(file.exists(file.path(cache_dir, "drought_2001_2002_2002.rds")))

  res_cached <- drought_component(tp_path, "XX", reference_period, study_period,
                                   area = FALSE, computed_components = TRUE,
                                   load_dir = cache_dir)

  expect_equal(res_cached, res_fresh)
})

test_that("drought_component échoue explicitement si computed_components = TRUE sans cache disponible", {
  expect_error(
    drought_component("unused.nc", "XX",
                       reference_period = c("2001-01-01", "2002-12-31"),
                       study_period     = c("2001-01-01", "2002-12-31"),
                       computed_components = TRUE,
                       load_dir = tempfile()),
    "Cached file not found"
  )
})
