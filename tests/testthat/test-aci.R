library(testthat)

# ---------------------------------------------------------------------------
# utils.R
# ---------------------------------------------------------------------------

test_that("merge_dataframes fusionne correctement deux data.frames", {
  df1 <- data.frame(a = 1:3, row.names = c("2000-01-31", "2000-02-29", "2000-03-31"))
  df2 <- data.frame(b = 4:6, row.names = c("2000-01-31", "2000-02-29", "2000-03-31"))
  res <- merge_dataframes(list(df1, df2))
  expect_equal(ncol(res), 2)
  expect_equal(nrow(res), 3)
  expect_true("a" %in% colnames(res))
  expect_true("b" %in% colnames(res))
})

test_that("reduce_sealevel_over_region calcule la moyenne des stations", {
  df <- data.frame(
    st1 = c(1.0, 2.0, 3.0),
    st2 = c(3.0, 4.0, 5.0),
    row.names = c("2000-01-15", "2000-02-15", "2000-03-15")
  )
  res <- reduce_sealevel_over_region(df)
  expect_equal(ncol(res), 1)
  expect_equal(colnames(res), "sealevel")
  expect_equal(as.numeric(res[1, 1]), 2.0)
})

test_that("standardize_metric standardise correctement un vecteur 1D", {
  # Série mensuelle simple sur 3 ans (36 mois)
  set.seed(42)
  times <- seq(as.POSIXct("1990-01-01", tz = "UTC"),
               by = "month", length.out = 36)
  vals  <- rnorm(36, mean = 10, sd = 2)
  metric <- list(data = vals, time = times)

  ref <- c("1990-01-01", "1991-12-31")
  res <- standardize_metric(metric, ref, area = FALSE)
  expect_length(res, 36)
  expect_true(is.numeric(res))
  # La moyenne sur la période de référence doit être proche de 0
  ref_idx <- times >= as.POSIXct("1990-01-01", tz="UTC") &
    times <= as.POSIXct("1991-12-31", tz="UTC")
  expect_lt(abs(mean(res[ref_idx], na.rm = TRUE)), 0.5)
})

# ---------------------------------------------------------------------------
# component.R
# ---------------------------------------------------------------------------

test_that("resample_daily agrège correctement par jour", {
  # 4 pas de temps sur 2 jours (2 par jour)
  time <- as.POSIXct(c("2000-01-01 00:00", "2000-01-01 12:00",
                       "2000-01-02 00:00", "2000-01-02 12:00"),
                     tz = "UTC")
  data <- array(c(1,3,2,4), dim = c(1,1,4))
  ds   <- list(data = data, time = time, lon = 0, lat = 0)
  res  <- resample_daily(ds, FUN = mean)

  expect_equal(dim(res$data)[3], 2)
  expect_equal(as.numeric(res$data[1, 1, 1]), 2)   # mean(1,3)
  expect_equal(as.numeric(res$data[1, 1, 2]), 3)   # mean(2,4)
})

test_that("resample_monthly agrège correctement par mois", {
  time <- as.POSIXct(c("2000-01-15", "2000-01-20",
                       "2000-02-10", "2000-02-25"), tz = "UTC")
  vals <- c(10, 20, 30, 40)
  ds   <- list(data = vals, time = time)
  res  <- resample_monthly(ds, FUN = mean)
  expect_equal(length(res$data), 2)
  expect_equal(res$data[1], 15)   # mean(10,20)
  expect_equal(res$data[2], 35)   # mean(30,40)
})

test_that("calculate_rolling_sum produit les bonnes sommes glissantes", {
  time <- as.POSIXct(paste0("2000-01-0", 1:6), tz = "UTC")
  data <- array(rep(1, 6), dim = c(1, 1, 6))
  ds   <- list(data = data, time = time, lon = 0, lat = 0)
  res  <- calculate_rolling_sum(ds, "tp", window_size = 3)
  # Les 2 premières valeurs doivent être NA (fenêtre incomplète)
  expect_true(is.na(res$data[1, 1, 1]))
  expect_true(is.na(res$data[1, 1, 2]))
  expect_equal(as.numeric(res$data[1, 1, 3]), 3)
  expect_equal(as.numeric(res$data[1, 1, 6]), 3)
})

# ---------------------------------------------------------------------------
# drought.R
# ---------------------------------------------------------------------------

test_that("max_consecutive_dry_days retourne le bon maximum annuel", {
  # 10 jours : alternance mouillé/sec puis 4 jours secs consécutifs
  time <- as.POSIXct(paste0("2000-01-", sprintf("%02d", 1:10)), tz = "UTC")
  # tp en m : seuil = 0.001 m
  prec <- c(0.002, 0, 0, 0, 0.005, 0.002, 0, 0, 0, 0)  # 4 CDD max
  data <- array(prec, dim = c(1, 1, 10))
  ds   <- list(data = data, time = time, lon = 0, lat = 0)
  res  <- max_consecutive_dry_days(ds)
  expect_equal(as.numeric(res$data[1, 1, 1]), 4)
})

test_that("drought_interpolate produit 12 * ny valeurs mensuelles", {
  # 3 années annuelles
  time <- as.POSIXct(c("2000-12-31", "2001-12-31", "2002-12-31"), tz = "UTC")
  data <- array(c(10, 20, 30), dim = c(1, 1, 3))
  cdd  <- list(data = data, time = time, lon = 0, lat = 0)
  res  <- drought_interpolate(cdd)
  # (ny-1)*12 + 12 = ny*12
  expect_equal(dim(res$data)[3], 3 * 12)
})

# ---------------------------------------------------------------------------
# sealevel.R
# ---------------------------------------------------------------------------

test_that("sealevel_correct_date_format convertit les dates PSMSL", {
  df <- data.frame(val = c(100, 200),
                   row.names = c("2000.0417", "2000.125"))
  res <- sealevel_correct_date_format(df)
  expect_equal(rownames(res)[1], "2000-01-01")
  expect_equal(rownames(res)[2], "2000-02-01")
})

test_that("sealevel_clean_data remplace -99999 par NA", {
  df <- data.frame(a = c(100, -99999, 200))
  res <- sealevel_clean_data(df)
  expect_true(is.na(res$a[2]))
  expect_equal(res$a[1], 100)
})

test_that("sealevel_compute_monthly_stats calcule correctement les moyennes", {
  dates <- seq(as.Date("1990-01-01"), as.Date("1991-12-01"), by = "month")
  vals  <- rep(c(10, 20, 10, 20, 10, 20, 10, 20, 10, 20, 10, 20), 2)
  df    <- data.frame(s1 = vals, row.names = as.character(dates))
  ref   <- c("1990-01-01", "1992-01-01")
  means <- sealevel_compute_monthly_stats(df, ref, "means")
  expect_length(means, 12)
  expect_equal(as.numeric(means[1]), 10)  # janvier
})
