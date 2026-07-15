library(testthat)

# ---------------------------------------------------------------------------
# Parite wind_power() / wind_power_terra() et
#        calculate_period_wind_exceedance_frequency() / ..._terra()
# ---------------------------------------------------------------------------

.wind_list_to_rast_1row <- function(ds) {
  nl <- dim(ds$data)[1]
  nt <- dim(ds$data)[3]
  vals <- as.vector(aperm(ds$data, c(2, 1, 3)))
  r <- terra::rast(nrows = 1, ncols = nl, nlyrs = nt, vals = vals)
  terra::time(r) <- ds$time
  r
}

test_that("wind_power_terra concorde avec wind_power (avec et sans reference_period)", {
  skip_if_not_installed("terra")

  set.seed(99)
  time <- seq(as.POSIXct("2000-01-01 00:00", tz = "UTC"), by = "hour",
             length.out = 24 * 10)   # 10 jours
  n_t <- length(time)
  u_vals <- 3 + sin(seq_len(n_t) / 5) + rnorm(n_t, sd = 0.5)
  v_vals <- 4 + cos(seq_len(n_t) / 7) + rnorm(n_t, sd = 0.5)

  u10 <- list(data = array(u_vals, dim = c(1, 1, n_t)), time = time, lon = 0, lat = 0)
  v10 <- list(data = array(v_vals, dim = c(1, 1, n_t)), time = time, lon = 0, lat = 0)
  u10_r <- .wind_list_to_rast_1row(u10)
  v10_r <- .wind_list_to_rast_1row(v10)

  res_base  <- wind_power(u10, v10)
  res_terra <- wind_power_terra(u10_r, v10_r)
  expect_equal(dim(res_terra$data), dim(res_base$data))
  expect_equal(as.numeric(res_terra$data), as.numeric(res_base$data), tolerance = 1e-8)

  # Avec filtre reference_period
  ref <- c("2000-01-03", "2000-01-06")
  res_base_ref  <- wind_power(u10, v10, reference_period = ref)
  res_terra_ref <- wind_power_terra(u10_r, v10_r, reference_period = ref)
  expect_equal(dim(res_terra_ref$data), dim(res_base_ref$data))
  expect_equal(as.numeric(res_terra_ref$data), as.numeric(res_base_ref$data),
              tolerance = 1e-8)
})

test_that("calculate_period_wind_exceedance_frequency_terra concorde avec la version base-R", {
  skip_if_not_installed("terra")

  set.seed(99)
  time <- seq(as.POSIXct("2001-01-01 00:00", tz = "UTC"), by = "hour",
             length.out = 24 * 365 * 2)   # 2 ans
  n_t <- length(time)
  u_vals <- 3 + sin(seq_len(n_t) / 400) + rnorm(n_t, sd = 0.5)
  v_vals <- 4 + cos(seq_len(n_t) / 500) + rnorm(n_t, sd = 0.5)

  u10 <- list(data = array(u_vals, dim = c(1, 1, n_t)), time = time, lon = 0, lat = 0)
  v10 <- list(data = array(v_vals, dim = c(1, 1, n_t)), time = time, lon = 0, lat = 0)
  u10_r <- .wind_list_to_rast_1row(u10)
  v10_r <- .wind_list_to_rast_1row(v10)

  ref <- c("2001-01-01", "2002-12-31")

  res_base  <- calculate_period_wind_exceedance_frequency(u10, v10, ref)
  res_terra <- calculate_period_wind_exceedance_frequency_terra(u10_r, v10_r, ref)

  expect_equal(dim(res_terra$data), dim(res_base$data))
  non_na <- !is.na(res_base$data) & !is.na(res_terra$data)
  expect_true(any(non_na))
  expect_equal(res_terra$data[non_na], res_base$data[non_na], tolerance = 1e-6)
  expect_equal(as.character(as.Date(res_terra$time)),
              as.character(as.Date(res_base$time)))
})
