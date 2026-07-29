library(testthat)

# ---------------------------------------------------------------------------
# sealevel.R
# ---------------------------------------------------------------------------

# --- sealevel_clean_data ----------------------------------------------------

test_that("sealevel_clean_data remplace le sentinel -99999 par NA sans toucher aux autres valeurs", {
  df <- data.frame(
    Measurement_1 = c(123.4, -99999, 0, -99998.9),
    Measurement_2 = c(-99999, 55.5, -99999, 12)
  )
  cleaned <- sealevel_clean_data(df)

  expect_true(is.na(cleaned$Measurement_1[2]))
  expect_true(is.na(cleaned$Measurement_2[1]))
  expect_true(is.na(cleaned$Measurement_2[3]))
  # Valeur proche du sentinel mais différente : NON remplacée
  expect_equal(cleaned$Measurement_1[4], -99998.9)
  expect_equal(cleaned$Measurement_1[1], 123.4)
  expect_equal(cleaned$Measurement_2[2], 55.5)
  expect_equal(cleaned$Measurement_2[4], 12)
})

test_that("sealevel_clean_data laisse un data.frame sans sentinel inchangé", {
  df <- data.frame(Measurement_1 = c(1, 2, 3))
  expect_equal(sealevel_clean_data(df), df)
})

# --- sealevel_correct_date_format -------------------------------------------

test_that("sealevel_correct_date_format convertit les fractions PSMSL connues en dates", {
  df <- data.frame(Measurement_1 = c(10, 20, 30))
  rownames(df) <- c("2020.0417", "2020.9583", "2020.125")

  out <- sealevel_correct_date_format(df)

  # Triées par ordre chronologique croissant
  expect_equal(rownames(out), c("2020-01-01", "2020-02-01", "2020-12-01"))
  # Les valeurs suivent leur ligne d'origine (pas mélangées lors du tri)
  expect_equal(out["2020-01-01", 1], 10)
  expect_equal(out["2020-12-01", 1], 20)
  expect_equal(out["2020-02-01", 1], 30)
})

test_that("sealevel_correct_date_format retire les lignes dont la fraction est inconnue", {
  df <- data.frame(Measurement_1 = c(10, 20))
  rownames(df) <- c("2020.0417", "2020.9999")  # 9999 n'existe pas dans .MONTH_MAPPING

  out <- sealevel_correct_date_format(df)

  expect_equal(nrow(out), 1)
  expect_equal(rownames(out), "2020-01-01")
})

test_that("sealevel_correct_date_format traite une année sans fraction comme janvier (défaut 0417)", {
  df <- data.frame(Measurement_1 = c(42))
  rownames(df) <- c("2020")  # pas de partie décimale du tout

  out <- sealevel_correct_date_format(df)

  expect_equal(rownames(out), "2020-01-01")
})

# --- sealevel_compute_monthly_stats -----------------------------------------

# Jeu de données à une seule station, valeurs choisies pour vérification manuelle :
#   janvier : 2 observations (100, 102) -> mean = 101, sd = sqrt(2)
#   février : 2 observations identiques (200, 200) -> mean = 200, sd = 0 -> garde-fou -> 1
#   mars    : 1 seule observation (50) -> sd = NA -> garde-fou -> 1
build_stats_fixture <- function() {
  df <- data.frame(Measurement_1 = c(100, 102, 200, 200, 50))
  rownames(df) <- c("2010-01-01", "2011-01-01", "2010-02-01",
                     "2011-02-01", "2010-03-01")
  df
}

test_that(".compute_monthly_stats calcule les bonnes moyennes mensuelles sur la période de référence", {
  df <- build_stats_fixture()
  means <- sealevel_compute_monthly_stats(df, c("2010-01-01", "2011-12-31"), "means")

  expect_equal(unname(means["1"]), 101)
  expect_equal(unname(means["2"]), 200)
  expect_equal(unname(means["3"]), 50)
})

test_that(".compute_monthly_stats applique le garde-fou (sd = 0 ou NA) -> 1", {
  df <- build_stats_fixture()
  stds <- sealevel_compute_monthly_stats(df, c("2010-01-01", "2011-12-31"), "std")

  expect_equal(unname(stds["1"]), sqrt(2))
  expect_equal(unname(stds["2"]), 1)  # sd réel = 0 -> forcé à 1
  expect_equal(unname(stds["3"]), 1)  # sd = NA (1 seule obs) -> forcé à 1
  expect_false(anyNA(stds))
})

test_that(".compute_monthly_stats rejette un argument 'stats' invalide", {
  df <- build_stats_fixture()
  expect_error(
    sealevel_compute_monthly_stats(df, c("2010-01-01", "2011-12-31"), "variance")
  )
})

test_that(".compute_monthly_stats ignore les observations hors période de référence", {
  df <- build_stats_fixture()
  # Période de référence ne couvrant que 2010 : février et janvier n'ont
  # alors chacun qu'une observation -> sd forcé à 1 pour tous les mois.
  means <- sealevel_compute_monthly_stats(df, c("2010-01-01", "2010-12-31"), "means")
  expect_equal(unname(means["1"]), 100)
  expect_equal(unname(means["2"]), 200)
})

# --- sealevel_standardize_data -----------------------------------------------

test_that("sealevel_standardize_data standardise correctement et étend hors période de référence", {
  df <- build_stats_fixture()
  # Ajout d'une observation janvier 2012, hors référence mais dans l'étude
  df <- rbind(df, data.frame(Measurement_1 = 105))
  rownames(df)[nrow(df)] <- "2012-01-15"

  means <- sealevel_compute_monthly_stats(df, c("2010-01-01", "2011-12-31"), "means")
  stds  <- sealevel_compute_monthly_stats(df, c("2010-01-01", "2011-12-31"), "std")

  out <- sealevel_standardize_data(df, means, stds,
                                    study_period = c("2010-01-01", "2012-12-31"))

  expect_equal(out["2010-01-01", 1], (100 - 101) / sqrt(2))
  expect_equal(out["2011-01-01", 1], (102 - 101) / sqrt(2))
  expect_equal(out["2010-02-01", 1], 0)   # (200-200)/1
  expect_equal(out["2012-01-15", 1], (105 - 101) / sqrt(2))
  expect_equal(nrow(out), 6)  # toutes les lignes conservées, aucune n'est all-NA
})

test_that("sealevel_standardize_data retire les lignes entièrement NA", {
  df <- build_stats_fixture()
  df <- rbind(df, data.frame(Measurement_1 = NA_real_))
  rownames(df)[nrow(df)] <- "2012-02-15"

  means <- sealevel_compute_monthly_stats(df, c("2010-01-01", "2011-12-31"), "means")
  stds  <- sealevel_compute_monthly_stats(df, c("2010-01-01", "2011-12-31"), "std")

  out <- sealevel_standardize_data(df, means, stds,
                                    study_period = c("2010-01-01", "2012-12-31"))

  expect_false("2012-02-15" %in% rownames(out))
  expect_equal(nrow(out), 5)  # la ligne NA (2012-02-15) a été retirée
})

test_that("sealevel_standardize_data filtre bien sur study_period (exclut ce qui est hors bornes)", {
  df <- build_stats_fixture()
  means <- sealevel_compute_monthly_stats(df, c("2010-01-01", "2011-12-31"), "means")
  stds  <- sealevel_compute_monthly_stats(df, c("2010-01-01", "2011-12-31"), "std")

  out <- sealevel_standardize_data(df, means, stds,
                                    study_period = c("2010-01-01", "2010-12-31"))

  expect_equal(sort(rownames(out)), c("2010-01-01", "2010-02-01", "2010-03-01"))
})

# --- sealevel_load_data ------------------------------------------------------

test_that("sealevel_load_data fusionne plusieurs fichiers stations par Date, avec NA pour les dates manquantes", {
  dir <- tempfile()
  dir.create(dir)
  writeLines(c("2010.0417;100;;", "2010.125;200;;"), file.path(dir, "1.txt"))
  writeLines(c("2010.0417;300;;", "2010.2083;400;;"), file.path(dir, "3.txt"))

  df <- sealevel_load_data(dir)

  expect_equal(sort(colnames(df)), c("Measurement_1", "Measurement_3"))
  expect_equal(nrow(df), 3)  # 3 dates distinctes au total (0417, 125, 2083)

  # Ligne commune aux deux stations
  common_row <- df[rownames(df) == "2010.0417", ]
  expect_equal(common_row$Measurement_1, 100)
  expect_equal(common_row$Measurement_3, 300)

  # Ligne où seule la station 1 a une valeur
  row_1_only <- df[rownames(df) == "2010.125", ]
  expect_equal(row_1_only$Measurement_1, 200)
  expect_true(is.na(row_1_only$Measurement_3))
})

test_that("sealevel_load_data échoue explicitement si aucun fichier .txt n'est trouvé", {
  dir <- tempfile()
  dir.create(dir)
  expect_error(sealevel_load_data(dir), "No .txt files found")
})

# --- sealevel_load_metadata ---------------------------------------------------

test_that("sealevel_load_metadata retrouve les coordonnées des stations bundlées par ID", {
  dir <- tempfile()
  dir.create(dir)
  meta <- sealevel_load_metadata(dir, station_ids = c(1, 3))

  expect_setequal(meta$station_id, c("Measurement_1", "Measurement_3"))
  brest <- meta[meta$station_id == "Measurement_1", ]
  expect_equal(brest$lat, 48.383)
  expect_equal(brest$lon, -4.495)
  sheerness <- meta[meta$station_id == "Measurement_3", ]
  expect_equal(sheerness$lat, 51.446)
  expect_equal(sheerness$lon, 0.743)
})

test_that("sealevel_load_metadata échoue si aucun station_id ne correspond", {
  dir <- tempfile()
  dir.create(dir)
  expect_error(
    sealevel_load_metadata(dir, station_ids = c(999999999)),
    "No matching stations"
  )
})

test_that("sealevel_load_metadata échoue si des colonnes requises manquent dans un meta_path personnalisé", {
  dir <- tempfile()
  dir.create(dir)
  bad_csv <- file.path(dir, "bad_meta.csv")
  writeLines(c("Station Name,ID,Country", "FOO,1,FRA"), bad_csv)

  expect_error(
    sealevel_load_metadata(dir, meta_path = bad_csv),
    "Column\\(s\\) missing"
  )
})

# --- sealevel_process (pipeline complet) ------------------------------------

test_that("sealevel_process enchaîne chargement, nettoyage et standardisation correctement", {
  # NB IMPORTANT (découvert en écrivant ce test, à confirmer avec l'auteur) :
  # sealevel_compute_monthly_stats() calcule row_mean = rowMeans(df_ref) AVANT
  # le tapply par mois, c.-à-d. une moyenne/écart-type UNIQUE, poolé sur
  # l'ensemble des stations du réseau pour chaque mois -- et non une
  # standardisation propre à chaque station. sealevel_standardize_data()
  # applique ensuite ces mêmes monthly_means/monthly_std_devs à CHAQUE
  # colonne (station) : deux stations avec des niveaux moyens différents
  # (ex. Bretagne vs Manche) sont donc standardisées contre une référence
  # commune, pas contre leur propre historique. Si l'intention était une
  # standardisation par station, c'est un bug à corriger dans
  # sealevel_compute_monthly_stats()/sealevel_standardize_data(); en l'état,
  # ce test documente le comportement actuel.
  dir <- tempfile()
  dir.create(dir)
  # Station 1 = BREST (FRA), Station 3 = SHEERNESS (GBR) dans psmsl_data.csv bundlé
  writeLines(c("2010.0417;100;;", "2011.0417;102;;", "2012.0417;110;;"),
             file.path(dir, "1.txt"))
  writeLines(c("2010.0417;50;;", "2011.0417;54;;", "2012.0417;60;;"),
             file.path(dir, "3.txt"))

  result <- sealevel_process(
    directory         = dir,
    study_period      = c("2010-01-01", "2012-12-31"),
    reference_period  = c("2010-01-01", "2011-12-31")
  )

  expect_named(result, c("data", "coords"))
  expect_setequal(colnames(result$data), c("Measurement_1", "Measurement_3"))
  # coords aligné avec l'ordre des colonnes de data
  expect_equal(result$coords$station_id, colnames(result$data))

  brest_col     <- result$coords$station_id == "Measurement_1"
  sheerness_col <- result$coords$station_id == "Measurement_3"
  expect_equal(result$coords$lon[brest_col], -4.495)
  expect_equal(result$coords$lat[sheerness_col], 51.446)

  # Moyenne/écart-type janvier poolés sur les 2 stations et 2 années de
  # référence : row_mean(2010) = (100+50)/2 = 75, row_mean(2011) = (102+54)/2 = 78
  # -> mean = 76.5, sd = sqrt(((75-76.5)^2+(78-76.5)^2)/1) = sqrt(4.5)
  pooled_mean <- 76.5
  pooled_sd   <- sqrt(4.5)

  m1 <- "Measurement_1"
  expect_equal(result$data["2010-01-01", m1], (100 - pooled_mean) / pooled_sd)
  expect_equal(result$data["2011-01-01", m1], (102 - pooled_mean) / pooled_sd)
  expect_equal(result$data["2012-01-01", m1], (110 - pooled_mean) / pooled_sd)

  m3 <- "Measurement_3"
  expect_equal(result$data["2010-01-01", m3], (50 - pooled_mean) / pooled_sd)
  expect_equal(result$data["2011-01-01", m3], (54 - pooled_mean) / pooled_sd)
  expect_equal(result$data["2012-01-01", m3], (60 - pooled_mean) / pooled_sd)
})
