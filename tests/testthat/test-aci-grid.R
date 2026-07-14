library(testthat)

# ---------------------------------------------------------------------------
# aci.R : .compute_aci_grid
# ---------------------------------------------------------------------------
#
# ACI = (T_high - T_low + P + D + alpha * SL + W) / (5 + alpha)
# alpha (par cellule) = 1 si la cellule a un signal sealevel non-NA au 1er pas
# de temps, 0 sinon (matrice figée dans le temps, déterminée uniquement au
# premier pas de temps).
# ---------------------------------------------------------------------------

test_that(".compute_aci_grid applique la bonne formule avec sea level (alpha variable par cellule)", {
  # 2 cellules spatiales (nl=2, nw=1), 2 pas de temps.
  # Cellule 1 : signal sealevel présent (non-NA) -> alpha = 1, dénominateur = 6
  # Cellule 2 : pas de signal sealevel (NA)      -> alpha = 0, dénominateur = 5
  t_high <- array(NA_real_, dim = c(2, 1, 2)); t_high[1, 1, ] <- 10; t_high[2, 1, ] <- 20
  t_low  <- array(2,  dim = c(2, 1, 2))   # constante -> valeur identique partout, pas d'ambiguïté d'ordre
  prec   <- array(0,  dim = c(2, 1, 2))
  drought<- array(0,  dim = c(2, 1, 2))
  wind   <- array(0,  dim = c(2, 1, 2))
  sl     <- array(NA_real_, dim = c(2, 1, 2)); sl[1, 1, ] <- 5   # cellule 2 reste NA

  time <- as.POSIXct(c("2000-01-01", "2000-02-01"), tz = "UTC")
  comp_t_high  <- list(data = t_high,  lon = c(0, 1), lat = 0, time = time)
  comp_t_low   <- list(data = t_low)
  comp_prec    <- list(data = prec)
  comp_drought <- list(data = drought)
  comp_wind    <- list(data = wind)
  comp_sl      <- list(data = sl)

  res <- .compute_aci_grid(comp_t_high, comp_t_low, comp_prec, comp_drought,
                           comp_wind, comp_sl, col_high = "t90", col_low = "t10")

  # Cellule 1 : (10 - 2 + 0 + 0 + 1*5 + 0) / (5 + 1) = 13 / 6
  expect_equal(as.numeric(res$ACI[1, 1, ]), rep(13 / 6, 2), tolerance = 1e-8)
  # Cellule 2 : (20 - 2 + 0 + 0 + 0*0 + 0) / (5 + 0) = 18 / 5
  expect_equal(as.numeric(res$ACI[2, 1, ]), rep(18 / 5, 2), tolerance = 1e-8)

  # Les composantes individuelles doivent être renvoyées telles quelles, avec
  # les noms de colonnes fournis
  expect_equal(res$t90, t_high)
  expect_equal(res$t10, t_low)
  expect_equal(res$precipitation, prec)
  expect_equal(res$drought, drought)
  expect_equal(res$wind, wind)
  # sealevel renvoyé = données BRUTES (le NA de la cellule 2 n'est PAS remplacé
  # par 0 dans la sortie ; le remplacement par 0 n'est qu'un calcul interne
  # utilisé pour le numérateur)
  expect_equal(res$sealevel, sl)

  expect_equal(res$lon, c(0, 1))
  expect_equal(res$lat, 0)
  expect_equal(res$time, time)
})

test_that(".compute_aci_grid sans sea level utilise alpha = 0 partout (dénominateur = 5)", {
  t_high <- array(NA_real_, dim = c(2, 1, 1)); t_high[1, 1, ] <- 10; t_high[2, 1, ] <- 20
  t_low  <- array(2, dim = c(2, 1, 1))
  prec   <- array(0, dim = c(2, 1, 1))
  drought<- array(0, dim = c(2, 1, 1))
  wind   <- array(0, dim = c(2, 1, 1))

  comp_t_high  <- list(data = t_high, lon = c(0, 1), lat = 0,
                       time = as.POSIXct("2000-01-01", tz = "UTC"))
  comp_t_low   <- list(data = t_low)
  comp_prec    <- list(data = prec)
  comp_drought <- list(data = drought)
  comp_wind    <- list(data = wind)

  res <- .compute_aci_grid(comp_t_high, comp_t_low, comp_prec, comp_drought,
                           comp_wind, comp_sl = NULL)

  # Cellule 1 : (10 - 2) / 5 = 1.6 ; Cellule 2 : (20 - 2) / 5 = 3.6
  expect_equal(as.numeric(res$ACI[1, 1, ]), 1.6, tolerance = 1e-8)
  expect_equal(as.numeric(res$ACI[2, 1, ]), 3.6, tolerance = 1e-8)
  expect_true(all(res$sealevel == 0))
})

test_that(".compute_aci_grid respecte les noms de colonnes col_high/col_low personnalisés", {
  t_high <- array(15, dim = c(1, 1, 1))
  t_low  <- array(5,  dim = c(1, 1, 1))
  zero   <- array(0,  dim = c(1, 1, 1))
  comp_t_high  <- list(data = t_high, lon = 0, lat = 0, time = as.POSIXct("2000-01-01", tz = "UTC"))
  comp_t_low   <- list(data = t_low)
  comp_prec    <- list(data = zero)
  comp_drought <- list(data = zero)
  comp_wind    <- list(data = zero)

  res <- .compute_aci_grid(comp_t_high, comp_t_low, comp_prec, comp_drought,
                           comp_wind, comp_sl = NULL,
                           col_high = "t95", col_low = "t5")

  expect_true("t95" %in% names(res))
  expect_true("t5"  %in% names(res))
  expect_false("t90" %in% names(res))
  expect_equal(as.numeric(res$ACI[1, 1, 1]), (15 - 5) / 5)
})

test_that(".compute_aci_grid neutralise correctement les NA de sealevel pour les cellules non-côtières", {
  # Vérifie explicitement la protection documentée dans le code contre la
  # contamination NA : une cellule avec alpha = 0 mais dont le sealevel brut
  # est NA à un pas de temps ultérieur ne doit PAS produire un ACI = NA.
  t_high <- array(10, dim = c(1, 1, 2))
  t_low  <- array(2,  dim = c(1, 1, 2))
  zero   <- array(0,  dim = c(1, 1, 2))
  sl     <- array(NA_real_, dim = c(1, 1, 2))   # NA au 1er pas -> alpha = 0, et NA partout

  comp_t_high  <- list(data = t_high, lon = 0, lat = 0,
                       time = as.POSIXct(c("2000-01-01", "2000-02-01"), tz = "UTC"))
  comp_t_low   <- list(data = t_low)
  comp_prec    <- list(data = zero)
  comp_drought <- list(data = zero)
  comp_wind    <- list(data = zero)
  comp_sl      <- list(data = sl)

  res <- .compute_aci_grid(comp_t_high, comp_t_low, comp_prec, comp_drought,
                           comp_wind, comp_sl)

  expect_false(anyNA(res$ACI))
  expect_equal(as.numeric(res$ACI[1, 1, ]), rep((10 - 2) / 5, 2))
})
