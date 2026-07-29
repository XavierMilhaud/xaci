library(testthat)

# ---------------------------------------------------------------------------
# utils.R
# ---------------------------------------------------------------------------

# --- .iso3_to_country_name ---------------------------------------------------

test_that(".iso3_to_country_name convertit un code ISO-3 connu en nom anglais", {
  expect_equal(.iso3_to_country_name("FRA"), "France")
  expect_equal(.iso3_to_country_name("GBR"), "United Kingdom")
})

test_that(".iso3_to_country_name est insensible à la casse et aux espaces", {
  expect_equal(.iso3_to_country_name(" fra "), "France")
  expect_equal(.iso3_to_country_name("DeU"), "Germany")
})

test_that(".iso3_to_country_name échoue sur un code inconnu", {
  expect_error(.iso3_to_country_name("ZZZ"), "Unknown ISO-3 code")
})

# --- .build_era5_paths -------------------------------------------------------

test_that(".build_era5_paths construit les 5 chemins attendus pour une plage d'années", {
  paths <- .build_era5_paths("FRA", 2011:2015)

  expect_equal(paths$t2m,  "data/era5/FRA/t2m_2011_2015.nc")
  expect_equal(paths$tp,   "data/era5/FRA/tp_2011_2015.nc")
  expect_equal(paths$u10,  "data/era5/FRA/u10_2011_2015.nc")
  expect_equal(paths$v10,  "data/era5/FRA/v10_2011_2015.nc")
  expect_equal(paths$mask, "data/era5/FRA/mask_FRA.nc")
})

test_that(".build_era5_paths met le code pays en majuscules et gère une année unique", {
  paths <- .build_era5_paths("fra", 2020)
  expect_equal(paths$t2m,  "data/era5/FRA/t2m_2020_2020.nc")
  expect_equal(paths$mask, "data/era5/FRA/mask_FRA.nc")
})

test_that(".build_era5_paths respecte un base_dir personnalisé", {
  paths <- .build_era5_paths("DEU", 2018:2019, base_dir = "custom/root")
  expect_equal(paths$t2m,  "custom/root/DEU/t2m_2018_2019.nc")
  expect_equal(paths$mask, "custom/root/DEU/mask_DEU.nc")
})

# --- load_netcdf --------------------------------------------------------------

test_that("load_netcdf lit correctement un cube [lon x lat x time] avec un temps en 'hours since'", {
  lon <- c(-1, 0, 1)
  lat <- c(43, 44)
  origin   <- as.POSIXct("2000-01-01 00:00:00", tz = "UTC")
  time_vec <- origin + (0:5) * 3600  # 6 pas horaires
  vals <- array(seq_len(length(lon) * length(lat) * length(time_vec)),
                dim = c(length(lon), length(lat), length(time_vec)))

  nc_path <- tempfile(fileext = ".nc")
  build_synthetic_nc(nc_path, "t2m", "K", lon, lat, time_vec, origin, vals)

  ds <- load_netcdf(nc_path, "t2m")

  expect_equal(dim(ds$data), c(3, 2, 6))
  expect_equal(as.numeric(ds$data), as.numeric(vals))
  expect_equal(as.numeric(ds$lon), lon)
  expect_equal(as.numeric(ds$lat), lat)
  expect_equal(as.numeric(ds$time), as.numeric(time_vec))
  expect_equal(ds$var_name, "t2m")
})

test_that("load_netcdf gère un temps en 'days since'", {
  lon <- c(0, 1)
  lat <- c(10, 20)
  origin   <- as.POSIXct("2000-01-01 00:00:00", tz = "UTC")
  time_vec <- origin + (0:2) * 86400  # 3 jours
  vals <- array(1:12, dim = c(2, 2, 3))

  dim_lon  <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  dim_lat  <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  dim_time <- ncdf4::ncdim_def(
    "time", paste0("days since ", format(origin, "%Y-%m-%d %H:%M:%S")),
    as.numeric(difftime(time_vec, origin, units = "days")), unlim = TRUE
  )
  ncvar <- ncdf4::ncvar_def("tp", "m", list(dim_lon, dim_lat, dim_time),
                            missval = NA, prec = "double")
  nc_path <- tempfile(fileext = ".nc")
  nc <- ncdf4::nc_create(nc_path, list(ncvar))
  ncdf4::ncvar_put(nc, ncvar, vals)
  ncdf4::nc_close(nc)

  ds <- load_netcdf(nc_path, "tp")
  expect_equal(dim(ds$data), c(2, 2, 3))
  expect_equal(as.numeric(ds$time), as.numeric(time_vec))
  expect_equal(as.numeric(ds$data), as.numeric(vals))
})

test_that("load_netcdf échoue sur une unité de temps non supportée", {
  lon <- c(0); lat <- c(0)
  dim_lon  <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  dim_lat  <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  dim_time <- ncdf4::ncdim_def("time", "months since 2000-01-01 00:00:00",
                                0:2, unlim = TRUE)
  ncvar <- ncdf4::ncvar_def("t2m", "K", list(dim_lon, dim_lat, dim_time),
                            missval = NA, prec = "double")
  nc_path <- tempfile(fileext = ".nc")
  nc <- ncdf4::nc_create(nc_path, list(ncvar))
  ncdf4::ncvar_put(nc, ncvar, array(1:3, dim = c(1, 1, 3)))
  ncdf4::nc_close(nc)

  expect_error(load_netcdf(nc_path, "t2m"), "Unsupported time unit")
})

# --- apply_mask -----------------------------------------------------------

test_that("apply_mask passe en NA les cellules sous le seuil, sur tous les pas de temps", {
  lon <- c(-1, 0, 1)
  lat <- c(43, 44)
  origin   <- as.POSIXct("2000-01-01 00:00:00", tz = "UTC")
  time_vec <- origin + (0:2) * 3600
  vals <- array(seq_len(3 * 2 * 3), dim = c(3, 2, 3))

  nc_path <- tempfile(fileext = ".nc")
  build_synthetic_nc(nc_path, "t2m", "K", lon, lat, time_vec, origin, vals)
  ds <- load_netcdf(nc_path, "t2m")

  # Masque [lon x lat] : cellule (2,1) et (3,2) sous le seuil 0.8
  mask_vals <- matrix(c(1, 0.5, 1,   1, 1, 0.7), nrow = 3, ncol = 2)
  mask_path <- tempfile(fileext = ".nc")
  build_synthetic_mask(mask_path, lon, lat, mask_vals = mask_vals)

  out <- apply_mask(ds, mask_path, "t2m", threshold = 0.8)

  expect_true(all(is.na(out$data[2, 1, ])))
  expect_true(all(is.na(out$data[3, 2, ])))
  # Les autres cellules restent inchangées sur tous les pas de temps
  expect_equal(out$data[1, 1, ], ds$data[1, 1, ])
  expect_equal(out$data[3, 1, ], ds$data[3, 1, ])
  expect_equal(out$data[1, 2, ], ds$data[1, 2, ])
  expect_equal(out$data[2, 2, ], ds$data[2, 2, ])
})

test_that("apply_mask garde une cellule pile au seuil (>=)", {
  # lon/lat de longueur >= 2 : ncdf4::ncvar_get() "collapse"-erait (drop) une
  # dimension de longueur 1, ce qui casserait la structure [lon x lat x time]
  # attendue par apply_mask() -- non pertinent ici, on teste juste le seuil.
  lon <- c(0, 1); lat <- c(0, 1)
  origin   <- as.POSIXct("2000-01-01", tz = "UTC")
  time_vec <- origin + (0:1) * 3600
  vals <- array(seq_len(2 * 2 * 2), dim = c(2, 2, 2))

  nc_path <- tempfile(fileext = ".nc")
  build_synthetic_nc(nc_path, "t2m", "K", lon, lat, time_vec, origin, vals)
  ds <- load_netcdf(nc_path, "t2m")

  # Toutes les cellules exactement au seuil -> aucune ne doit être masquée
  mask_path <- tempfile(fileext = ".nc")
  build_synthetic_mask(mask_path, lon, lat, mask_vals = matrix(0.8, 2, 2))

  out <- apply_mask(ds, mask_path, "t2m", threshold = 0.8)
  expect_false(anyNA(out$data))
  expect_equal(out$data, ds$data)
})

# --- merge_dataframes ---------------------------------------------------------

test_that("merge_dataframes fait une jointure interne sur les row.names", {
  df1 <- data.frame(X = c(1, 2, 3), row.names = c("a", "b", "c"))
  df2 <- data.frame(Y = c(10, 20, 30), row.names = c("b", "c", "d"))

  out <- merge_dataframes(list(df1, df2))

  expect_setequal(rownames(out), c("b", "c"))  # intersection seulement (all = FALSE)
  expect_equal(out["b", "X"], 2)
  expect_equal(out["b", "Y"], 10)
  expect_equal(out["c", "X"], 3)
  expect_equal(out["c", "Y"], 20)
})

test_that("merge_dataframes s'enchaîne correctement sur plus de deux data.frames", {
  df1 <- data.frame(X = c(1, 2), row.names = c("a", "b"))
  df2 <- data.frame(Y = c(10, 20), row.names = c("a", "b"))
  df3 <- data.frame(Z = c(100, 200), row.names = c("a", "b"))

  out <- merge_dataframes(list(df1, df2, df3))

  expect_equal(sort(rownames(out)), c("a", "b"))
  expect_equal(out["a", c("X", "Y", "Z")], data.frame(X = 1, Y = 10, Z = 100, row.names = "a"))
})

# --- reduce_dataarray_to_dataframe (branche admin_mask) ------------------------

test_that("reduce_dataarray_to_dataframe calcule une moyenne pondérée correcte par unité administrative", {
  # Grille 2x2 : lon = c(0,1), lat = c(10,20)
  # Poids par cellule (index = (i-1)*nw + j, nw = 2) :
  #   cell(1,1) -> idx 1 : UnitA = 1,   UnitB = 0
  #   cell(1,2) -> idx 2 : UnitA = 0,   UnitB = 1
  #   cell(2,1) -> idx 3 : UnitA = 0.5, UnitB = 0.5
  #   cell(2,2) -> idx 4 : UnitA = 0,   UnitB = 0   (cellule sans contribution)
  weights <- vector("list", 4)
  weights[[1]] <- c(UnitA = 1,   UnitB = 0)
  weights[[2]] <- c(UnitA = 0,   UnitB = 1)
  weights[[3]] <- c(UnitA = 0.5, UnitB = 0.5)
  weights[[4]] <- c(UnitA = 0,   UnitB = 0)

  admin_mask <- list(units = c("UnitA", "UnitB"), lon = c(0, 1), lat = c(10, 20),
                      weights = weights)

  # Valeurs de cellule constantes dans le temps : (1,1)=10, (1,2)=20, (2,1)=30, (2,2)=40
  data <- array(NA_real_, dim = c(2, 2, 2))
  data[1, 1, ] <- 10; data[1, 2, ] <- 20
  data[2, 1, ] <- 30; data[2, 2, ] <- 40
  metric <- list(data = data,
                 time = as.POSIXct(c("2020-01-15", "2020-02-15"), tz = "UTC"))

  out <- reduce_dataarray_to_dataframe(metric, column_name = "value", admin_mask = admin_mask)

  expect_equal(colnames(out), c("value_UnitA", "value_UnitB"))
  expect_equal(rownames(out), c("2020-01-01", "2020-02-01"))
  # UnitA : (1*10 + 0.5*30) / (1 + 0.5)
  expect_equal(out[1, "value_UnitA"], (1 * 10 + 0.5 * 30) / 1.5)
  # UnitB : (1*20 + 0.5*30) / (1 + 0.5)
  expect_equal(out[1, "value_UnitB"], (1 * 20 + 0.5 * 30) / 1.5)
  # Constant dans le temps -> même valeur au 2e pas de temps
  expect_equal(unname(unlist(out[1, ])), unname(unlist(out[2, ])))
})

# --- reduce_sealevel_over_region (branche administrative) ----------------------

test_that("reduce_sealevel_over_region moyenne les stations par unité et renvoie NA pour une unité sans station", {
  df <- data.frame(
    Measurement_1 = c(10, 12),
    Measurement_2 = c(20, 22),
    Measurement_3 = c(30, 32),
    row.names = c("2020-01-01", "2020-02-01")
  )
  admin_assignment <- list(
    station_ids = list(UnitA = c(1, 2), UnitB = c(3)),
    factors     = c(UnitA = 0.4, UnitB = 0.9, UnitC = 0)  # UnitC : aucune station
  )

  out <- reduce_sealevel_over_region(df, admin_assignment = admin_assignment)

  expect_equal(colnames(out), c("sealevel_UnitA", "sealevel_UnitB", "sealevel_UnitC"))
  expect_equal(out["2020-01-01", "sealevel_UnitA"], mean(c(10, 20)))
  expect_equal(out["2020-02-01", "sealevel_UnitA"], mean(c(12, 22)))
  expect_equal(out["2020-01-01", "sealevel_UnitB"], 30)
  expect_true(all(is.na(out[, "sealevel_UnitC"])))
})

test_that("reduce_sealevel_over_region calcule la moyenne nationale quand admin_assignment = NULL", {
  df <- data.frame(
    Measurement_1 = c(10, NA),
    Measurement_2 = c(20, 22),
    row.names = c("2020-01-01", "2020-02-01")
  )
  out <- reduce_sealevel_over_region(df, admin_assignment = NULL)

  expect_equal(colnames(out), "sealevel")
  expect_equal(out["2020-01-01", "sealevel"], mean(c(10, 20)))
  expect_equal(out["2020-02-01", "sealevel"], 22)  # NA ignorée (na.rm = TRUE)
})

# --- load_psmsl_data -----------------------------------------------------------

test_that("load_psmsl_data charge le CSV bundlé et renomme lat/lon", {
  df <- load_psmsl_data()

  expect_true(all(c("ID", "Country", "lat", "lon") %in% colnames(df)))
  brest <- df[df$ID == 1, ]
  expect_equal(nrow(brest), 1)
  expect_equal(brest$Country, "FRA")
  expect_equal(as.numeric(brest$lat), 48.383)
  expect_equal(as.numeric(brest$lon), -4.495)
})

# --- .attach_spatial_attrs / .get_spatial_attrs --------------------------------

test_that(".attach_spatial_attrs / .get_spatial_attrs font un aller-retour correct", {
  x <- array(1:4, dim = c(2, 2))
  x <- .attach_spatial_attrs(x, lon = c(1, 2), lat = c(3, 4),
                             country_abbrev = "FRA", admin_level = 1L)

  attrs <- .get_spatial_attrs(x)
  expect_equal(attrs$lon, c(1, 2))
  expect_equal(attrs$lat, c(3, 4))
  expect_equal(attrs$country_abbrev, "FRA")
  expect_equal(attrs$admin_level, 1L)
  expect_null(attrs$time)
  expect_null(attrs$crs_metric)
})

test_that(".attach_spatial_attrs n'écrit pas d'attribut pour les arguments NULL", {
  x <- array(1:4, dim = c(2, 2))
  x <- .attach_spatial_attrs(x, lon = c(1, 2))  # tout le reste est NULL

  expect_null(attr(x, "lat"))
  expect_null(attr(x, "time"))
  expect_null(attr(x, "country_abbrev"))
  expect_equal(attr(x, "lon"), c(1, 2))
})
