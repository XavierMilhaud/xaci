library(testthat)

# ---------------------------------------------------------------------------
# utils.R : standardize_metric / reduce_dataarray_to_dataframe / aggregate_granularity
# ---------------------------------------------------------------------------

test_that("standardize_metric renvoie 0 partout quand la valeur d'un mois est identique chaque année (3D)", {
  # 2 ans de données mensuelles (24 pas), mêmes valeurs répétées chaque année
  # -> écart-type de référence nul pour chaque mois -> ramené à 1 -> résultat
  # standardisé = (x - x) / 1 = 0 partout. Choix délibéré pour un test 100%
  # déterministe sans avoir à recalculer moyenne/écart-type à la main.
  time <- seq(as.POSIXct("2000-01-01", tz = "UTC"), by = "month", length.out = 24)
  vals <- rep(1:12, 2)
  ds   <- list(data = array(vals, dim = c(1, 1, 24)), time = time, lon = 0, lat = 0)

  res <- standardize_metric(ds, reference_period = c("2000-01-01", "2001-12-31"),
                            area = FALSE)

  expect_equal(dim(res$data), c(1, 1, 24))
  expect_equal(as.numeric(res$data), rep(0, 24), tolerance = 1e-8)
})

test_that("standardize_metric avec area = TRUE effondre la dimension spatiale avant standardisation", {
  # 2 cellules (nl=2, nw=1), 2 pas de temps correspondant au même mois
  # (janvier) sur 2 années -> même moyenne spatiale (15) aux deux pas ->
  # écart-type nul -> résultat standardisé = 0.
  data <- array(NA_real_, dim = c(2, 1, 2))
  data[1, 1, ] <- 10
  data[2, 1, ] <- 20
  time <- as.POSIXct(c("2000-01-15", "2001-01-15"), tz = "UTC")
  ds   <- list(data = data, time = time, lon = c(0, 1), lat = 0)

  res <- standardize_metric(ds, reference_period = c("2000-01-01", "2001-12-31"),
                            area = TRUE)

  # area = TRUE -> retourne un vecteur numérique nommé, plus une liste
  expect_true(is.numeric(res))
  expect_length(res, 2)
  expect_equal(as.numeric(res), c(0, 0), tolerance = 1e-8)
})

test_that("reduce_dataarray_to_dataframe calcule la moyenne nationale et nomme les dates au 1er du mois", {
  metric <- list(
    data = array(c(10, 20, 30), dim = c(1, 1, 3)),
    time = as.POSIXct(c("2000-01-15", "2000-02-20", "2000-03-05"), tz = "UTC")
  )

  res <- reduce_dataarray_to_dataframe(metric, column_name = "test")

  expect_equal(rownames(res), c("2000-01-01", "2000-02-01", "2000-03-01"))
  expect_equal(colnames(res), "test")
  expect_equal(res$test, c(10, 20, 30))
})

test_that("reduce_dataarray_to_dataframe ignore les NA lors de la moyenne inter-cellules", {
  data <- array(NA_real_, dim = c(2, 1, 1))
  data[1, 1, 1] <- 10
  data[2, 1, 1] <- NA
  metric <- list(data = data, time = as.POSIXct("2000-01-15", tz = "UTC"))

  res <- reduce_dataarray_to_dataframe(metric)   # column_name par défaut = "value"

  expect_equal(colnames(res), "value")
  expect_equal(res$value[1], 10)
})

test_that("aggregate_granularity agrège correctement par mois/année/semestre", {
  df <- data.frame(value = 1:12,
                   row.names = paste0("2000-", sprintf("%02d", 1:12), "-01"))

  res_month <- aggregate_granularity(df, "month")
  expect_equal(res_month["2000-01", "value"], 1)
  expect_equal(res_month["2000-12", "value"], 12)

  res_year <- aggregate_granularity(df, "year")
  expect_equal(res_year["2000", "value"], 6.5)

  res_sem <- aggregate_granularity(df, "semester")
  expect_equal(res_sem["2000-S1", "value"], mean(1:6))
  expect_equal(res_sem["2000-S2", "value"], mean(7:12))
})

test_that("aggregate_granularity applique la convention météorologique pour les saisons", {
  # Décembre est rattaché à l'hiver de l'année SUIVANTE (2001-DJF), tandis que
  # janvier/février restent rattachés à l'hiver de l'année en cours (2000-DJF).
  df <- data.frame(value = 1:12,
                   row.names = paste0("2000-", sprintf("%02d", 1:12), "-01"))

  res <- aggregate_granularity(df, "season")

  expect_equal(res["2000-DJF", "value"], mean(c(1, 2)))    # jan, fév
  expect_equal(res["2000-MAM", "value"], mean(c(3, 4, 5))) # mar, avr, mai
  expect_equal(res["2000-JJA", "value"], mean(c(6, 7, 8))) # jun, jul, aoû
  expect_equal(res["2000-SON", "value"], mean(c(9, 10, 11))) # sep, oct, nov
  expect_equal(res["2001-DJF", "value"], 12)               # déc -> hiver 2001
})

test_that("aggregate_granularity rejette une granularité invalide", {
  df <- data.frame(value = 1:12,
                   row.names = paste0("2000-", sprintf("%02d", 1:12), "-01"))
  expect_error(aggregate_granularity(df, "decade"))
})
