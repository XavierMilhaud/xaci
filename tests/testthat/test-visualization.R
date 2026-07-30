library(testthat)

# ---------------------------------------------------------------------------
# visualization.R
#
# Les fonctions de tracé retournent des objets ggplot : on vérifie non
# seulement qu'elles s'exécutent sans erreur, mais que les données
# effectivement rendues (via ggplot2::layer_data()/ggplot_build()) sont
# correctes. Le mode "administratif" (plot_aci_map/.admin_df_to_sf) dépend
# de .load_admin_sf() -> geodata::gadm(), qui télécharge de vraies
# frontières GADM (réseau + package geodata installé). On mocke donc
# .load_admin_sf() directement via testthat::local_mocked_bindings(.package
# = "xaci") pour renvoyer 2 polygones synthétiques ("UnitA"/"UnitB"), quel
# que soit ce qui est réellement installé sur la machine -- aucun accès
# réseau, et le comportement est identique partout. patchwork/gganimate/
# gifski ne sont pas installés dans le sandbox où ces tests ont été écrits :
# les tests correspondants sont encadrés par skip_if_not_installed() et
# s'exécuteront chez toi si ces paquets Suggests sont présents.
# ---------------------------------------------------------------------------

# --- Fixture commune : petit data.frame ACI (format mensuel) ------------------

.build_aci_df <- function() {
  data.frame(
    t90           = c(1.0,  2.0,  3.0,  4.0),
    t10           = c(-1.0, -2.0, -1.0,  0.0),
    precipitation = c(0.5, -0.5,  0.2, -0.2),
    drought       = c(0.1,  0.1,  0.1,  0.1),
    wind          = c(-0.3, 0.3, -0.1,  0.1),
    sealevel      = c(0.2,  0.2,  0.3,  0.3),
    ACI           = c(0.1,  0.05, 0.15, 0.2),
    row.names     = c("2010-01", "2010-02", "2010-03", "2010-04")
  )
}

# --- .check_aci_df -------------------------------------------------------------

test_that(".check_aci_df accepte un data.frame valide et rejette les colonnes manquantes", {
  df <- .build_aci_df()
  expect_true(.check_aci_df(df))

  expect_error(.check_aci_df(df[, setdiff(colnames(df), "wind")]), "missing column")
  expect_error(.check_aci_df(df[, setdiff(colnames(df), "t10")]), "temperature columns")
})

# --- .parse_dates ---------------------------------------------------------------

test_that(".parse_dates reconnaît les 4 formats de granularité", {
  monthly <- data.frame(x = c(1, 1), row.names = c("2010-01", "2010-02"))
  expect_equal(.parse_dates(monthly), as.Date(c("2010-01-01", "2010-02-01")))

  annual <- data.frame(x = c(1, 1), row.names = c("2010", "2011"))
  expect_equal(.parse_dates(annual), as.Date(c("2010-01-01", "2011-01-01")))

  semester <- data.frame(x = c(1, 1), row.names = c("2010-S1", "2010-S2"))
  expect_equal(.parse_dates(semester), as.Date(c("2010-01-01", "2010-07-01")))

  seasonal <- data.frame(x = c(1, 1, 1, 1),
                         row.names = c("2010-DJF", "2010-MAM", "2010-JJA", "2010-SON"))
  expect_equal(.parse_dates(seasonal),
               as.Date(c("2010-01-01", "2010-03-01", "2010-06-01", "2010-09-01")))
})

test_that(".parse_dates échoue sur un format de date non reconnu", {
  bad <- data.frame(x = 1, row.names = "not-a-date")
  expect_error(.parse_dates(bad), "Unrecognised row name format")
})

# --- .component_colours ---------------------------------------------------------

test_that(".component_colours nomme les couleurs selon col_high/col_low personnalisés", {
  cols <- .component_colours("t95", "t05")
  expect_setequal(names(cols),
                  c("t95", "t05", "precipitation", "drought", "wind", "sealevel", "ACI"))
  expect_equal(unname(cols["t95"]), "#D62728")
  expect_equal(unname(cols["t05"]), "#1F77B4")
})

# --- .infer_t_cols ---------------------------------------------------------------

test_that(".infer_t_cols identifie correctement la colonne froide (basse) et chaude (haute)", {
  df <- data.frame(t90 = 1, t10 = 1, precipitation = 1)
  res <- .infer_t_cols(df)
  expect_equal(res$low, "t10")
  expect_equal(res$high, "t90")

  # Ordre des colonnes inversé dans le data.frame : le tri doit rester correct
  df2 <- data.frame(t10 = 1, t90 = 1)
  res2 <- .infer_t_cols(df2)
  expect_equal(res2$low, "t10")
  expect_equal(res2$high, "t90")
})

test_that(".infer_t_cols échoue avec moins de 2 colonnes de température", {
  df <- data.frame(t90 = 1, precipitation = 1)
  expect_error(.infer_t_cols(df), "temperature columns")
})

# --- .bbox_with_margin -----------------------------------------------------------

test_that(".bbox_with_margin calcule une marge proportionnelle correcte", {
  bbox <- .bbox_with_margin(lon = c(0, 10), lat = c(40, 50), margin_frac = 0.1)
  # diff(lon) = 10 -> pad = 1 ; diff(lat) = 10 -> pad = 1
  expect_equal(bbox$xlim, c(-1, 11))
  expect_equal(bbox$ylim, c(39, 51))
})

test_that(".bbox_with_margin utilise la marge par défaut (5%)", {
  bbox <- .bbox_with_margin(lon = c(-2, 2), lat = c(40, 46))
  expect_equal(bbox$xlim, c(-2 - 0.2, 2 + 0.2))
  expect_equal(bbox$ylim, c(40 - 0.3, 46 + 0.3))
})

# --- .diverging_scale --------------------------------------------------------------

test_that(".diverging_scale porte le bon nom de légende et inverse la direction", {
  sc <- .diverging_scale("My label", "RdBu", TRUE, 9)
  expect_s3_class(sc, "ScaleContinuous")
  expect_equal(sc$name, "My label")

  df <- data.frame(x = 1:3, y = 1, value = c(1, 2, 3))
  build_fill <- function(reverse) {
    p <- ggplot2::ggplot(df, ggplot2::aes(x, y, fill = value)) +
      ggplot2::geom_tile() +
      .diverging_scale("lab", "RdBu", reverse, 9)
    ggplot2::ggplot_build(p)$data[[1]]$fill
  }
  fill_rev  <- build_fill(TRUE)
  fill_norm <- build_fill(FALSE)
  expect_false(identical(fill_rev, fill_norm))
  expect_equal(fill_rev, rev(fill_norm))  # min/max exactement inversés
})

# --- .slice_to_df ------------------------------------------------------------------

.build_grid_array <- function() {
  arr <- array(NA_real_, dim = c(2, 2, 3))
  arr[1, 1, ] <- c(1, 2, 3)
  arr[1, 2, ] <- c(4, 5, 6)
  arr[2, 1, ] <- c(7, 8, 9)
  arr[2, 2, ] <- c(NA, NA, NA)  # cellule toujours manquante
  .attach_spatial_attrs(arr, lon = c(-1, 1), lat = c(41, 45),
                        time = c("2010-01", "2010-02", "2010-03"),
                        country_abbrev = "ZZ")
}

test_that(".slice_to_df extrait la bonne tranche temporelle et retire les NA", {
  arr <- .build_grid_array()
  res <- .slice_to_df(arr, variable = NULL, time_index = 1)

  expect_equal(nrow(res$df), 3)  # cellule (2,2) toujours NA -> exclue
  expect_setequal(res$df$value, c(1, 4, 7))
  expect_equal(res$time_label, "2010-01")
})

test_that(".slice_to_df calcule la moyenne temporelle avec time_index = 'mean'", {
  arr <- .build_grid_array()
  res <- .slice_to_df(arr, variable = NULL, time_index = "mean")

  expect_equal(nrow(res$df), 3)
  expect_setequal(res$df$value, c(mean(1:3), mean(4:6), mean(7:9)))
  expect_equal(res$time_label, "temporal mean")
})

test_that(".slice_to_df fonctionne aussi via une liste parente + `variable`", {
  arr <- .build_grid_array()
  parent <- list(ACI = unclass(arr))  # perd les attributs -> on les repasse via la liste
  parent$lon  <- attr(arr, "lon")
  parent$lat  <- attr(arr, "lat")
  parent$time <- attr(arr, "time")

  res <- .slice_to_df(parent, variable = "ACI", time_index = 2)
  expect_setequal(res$df$value, c(2, 5, 8))
  expect_equal(res$time_label, "2010-02")
})

test_that(".slice_to_df échoue si time_index est hors bornes", {
  arr <- .build_grid_array()
  expect_error(.slice_to_df(arr, variable = NULL, time_index = 10), "must be in")
})

test_that(".slice_to_df échoue si toutes les valeurs sont NA", {
  arr <- array(NA_real_, dim = c(2, 2, 1))
  arr <- .attach_spatial_attrs(arr, lon = c(0, 1), lat = c(0, 1))
  expect_error(.slice_to_df(arr, variable = NULL, time_index = 1), "No non-NA value")
})

# --- plot_aci_timeseries ----------------------------------------------------------

test_that("plot_aci_timeseries rend la bonne série ACI et le bon nombre de couches", {
  df <- .build_aci_df()

  p_full <- plot_aci_timeseries(df, smooth = TRUE, fill_area = TRUE)
  expect_s3_class(p_full, "ggplot")
  # hline + ribbon + line + smooth = 4 couches
  expect_equal(length(p_full$layers), 4)

  p_min <- plot_aci_timeseries(df, smooth = FALSE, fill_area = FALSE)
  # hline + line = 2 couches
  expect_equal(length(p_min$layers), 2)

  # La couche "line" (2e couche de p_min) porte bien les valeurs ACI
  line_data <- ggplot2::layer_data(p_min, 2)
  expect_equal(line_data$y, df$ACI)
})

# --- plot_aci_components -----------------------------------------------------------

test_that("plot_aci_components réarrange correctement les composantes en format long", {
  df <- .build_aci_df()
  p <- plot_aci_components(df, type = "line", components = c("t90", "wind"))

  ld <- ggplot2::layer_data(p, 1)  # geom_line
  expect_equal(length(unique(ld$group)), 2)
  expect_setequal(ld$y, c(df$t90, df$wind))
})

test_that("plot_aci_components échoue sur un nom de composante inconnu", {
  df <- .build_aci_df()
  expect_error(plot_aci_components(df, components = c("t90", "not_a_component")),
               "Unknown component")
})

# --- plot_aci_distribution ---------------------------------------------------------

test_that("plot_aci_distribution inclut l'ACI quand include_aci = TRUE", {
  df <- .build_aci_df()
  p <- plot_aci_distribution(df, components = c("t90", "wind"), include_aci = TRUE,
                             type = "boxplot")
  ld <- ggplot2::layer_data(p, 1)
  expect_equal(length(unique(ld$x)), 3)  # t90, wind, ACI
})

test_that("plot_aci_distribution échoue sur un nom de composante inconnu", {
  df <- .build_aci_df()
  expect_error(plot_aci_distribution(df, components = "bogus"), "Unknown component")
})

# --- plot_aci_map : mode raster (grid-cell) -----------------------------------------

test_that("plot_aci_map (raster, sans bordures) rend les bonnes valeurs et la bonne bbox", {
  arr <- .build_grid_array()
  p <- plot_aci_map(arr, time_index = "mean", borders = FALSE)

  expect_s3_class(p, "ggplot")
  expect_equal(nrow(p$data), 3)  # 3 cellules non-NA
  expect_setequal(p$data$value, c(mean(1:3), mean(4:6), mean(7:9)))

  bbox <- .bbox_with_margin(c(-1, 1), c(41, 45))
  built <- ggplot2::ggplot_build(p)
  expect_equal(built$layout$panel_params[[1]]$x_range, bbox$xlim)
  expect_equal(built$layout$panel_params[[1]]$y_range, bbox$ylim)
})

.mock_admin_sf <- function(country_abbrev = NULL, admin_level = NULL, cache_dir = NULL) {
  # Synthétique : 2 polygones "UnitA"/"UnitB" côte à côte, jamais de vraies
  # données GADM. Mocké via local_mocked_bindings(.package = "xaci") pour
  # rester indépendant de ce qui est réellement installé/téléchargeable sur
  # la machine (le vrai package geodata, s'il est installé, irait chercher
  # les vraies régions administratives du pays demandé).
  p1 <- sf::st_polygon(list(rbind(c(-2, 40), c(0, 40), c(0, 46), c(-2, 46), c(-2, 40))))
  p2 <- sf::st_polygon(list(rbind(c(0, 40), c(2, 40), c(2, 46), c(0, 46), c(0, 40))))
  sf::st_sf(name = c("UnitA", "UnitB"), geometry = sf::st_sfc(p1, p2, crs = 4326))
}

test_that("plot_aci_map (raster, avec bordures) superpose le fond de carte du pays (.load_admin_sf mocké)", {
  local_mocked_bindings(.load_admin_sf = .mock_admin_sf, .package = "xaci")
  arr <- .build_grid_array()  # country_abbrev = "ZZ" attaché
  p <- plot_aci_map(arr, time_index = 1, borders = TRUE)

  expect_equal(length(p$layers), 2)  # geom_raster + geom_sf (fond de carte)
  built <- ggplot2::ggplot_build(p)
  expect_equal(nrow(built$data[[2]]), 2)  # 2 polygones synthétiques (UnitA/UnitB)
})

test_that("plot_aci_map avertit et ignore crs_metric en mode raster", {
  arr <- .build_grid_array()
  expect_warning(plot_aci_map(arr, time_index = 1, borders = FALSE, crs_metric = 2154),
                 "ignored in raster mode")
})

# --- plot_aci_map : mode administratif (choropleth, .load_admin_sf mocké) ----------

.build_admin_aci_df <- function() {
  df <- data.frame(
    ACI_UnitA = c(0.1, 0.2, 0.3),
    ACI_UnitB = c(-0.1, -0.2, -0.3),
    row.names = c("2010-01", "2010-02", "2010-03")
  )
  attr(df, "country_abbrev") <- "FRA"
  attr(df, "admin_level")    <- 1
  df
}

test_that(".admin_df_to_sf joint les valeurs ACI aux polygones administratifs (via attributs)", {
  local_mocked_bindings(.load_admin_sf = .mock_admin_sf, .package = "xaci")
  df <- .build_admin_aci_df()
  res <- .admin_df_to_sf(df, variable = "ACI", time_index = 1,
                         country_abbrev = NULL, admin_level = NULL, crs_metric = NULL)

  expect_setequal(res$sf$name, c("UnitA", "UnitB"))
  expect_equal(res$sf$value[res$sf$name == "UnitA"], 0.1)
  expect_equal(res$sf$value[res$sf$name == "UnitB"], -0.1)
  expect_equal(res$time_label, "2010-01")
})

test_that(".admin_df_to_sf calcule la moyenne temporelle avec time_index = 'mean'", {
  local_mocked_bindings(.load_admin_sf = .mock_admin_sf, .package = "xaci")
  df <- .build_admin_aci_df()
  res <- .admin_df_to_sf(df, "ACI", "mean", "FRA", 1, NULL)
  expect_equal(res$sf$value[res$sf$name == "UnitA"], mean(c(0.1, 0.2, 0.3)))
})

test_that(".admin_df_to_sf échoue si country_abbrev/admin_level ne peuvent être déterminés", {
  df <- data.frame(ACI_UnitA = 1, row.names = "2010-01")  # sans attributs
  expect_error(
    .admin_df_to_sf(df, "ACI", 1, NULL, NULL, NULL),
    "could not be determined"
  )
})

test_that("plot_aci_map (choropleth) fonctionne de bout en bout sur un data.frame admin", {
  local_mocked_bindings(.load_admin_sf = .mock_admin_sf, .package = "xaci")
  df <- .build_admin_aci_df()
  p <- plot_aci_map(df, variable = "ACI", time_index = "mean")

  expect_s3_class(p, "ggplot")
  built <- ggplot2::ggplot_build(p)
  expect_equal(nrow(built$data[[1]]), 2)
})

# --- plot_aci_map_mean --------------------------------------------------------------

test_that("plot_aci_map_mean filtre correctement sur `period` avant de moyenner", {
  arr <- .build_grid_array()  # time = 2010-01, 2010-02, 2010-03
  p_period <- plot_aci_map_mean(arr, period = c("2010-02", "2010-03"), borders = FALSE)

  # Moyenne attendue sur les 2 derniers pas de temps seulement :
  # cell(1,1) = mean(2,3) = 2.5 ; cell(1,2) = mean(5,6) = 5.5 ; cell(2,1) = mean(8,9) = 8.5
  expect_setequal(p_period$data$value, c(2.5, 5.5, 8.5))
  expect_match(p_period$labels$title, "2010-02 - 2010-03")
})

test_that("plot_aci_map_mean sans `period` moyenne sur toute la période disponible", {
  arr <- .build_grid_array()
  p_full <- plot_aci_map_mean(arr, borders = FALSE)
  expect_setequal(p_full$data$value, c(mean(1:3), mean(4:6), mean(7:9)))
  expect_match(p_full$labels$title, "full period")
})

# --- plot_aci_dashboard (nécessite patchwork) ---------------------------------------

test_that("plot_aci_dashboard assemble les 4 graphiques (patchwork)", {
  skip_if_not_installed("patchwork")
  df <- .build_aci_df()
  p <- plot_aci_dashboard(df)
  expect_s3_class(p, "patchwork")
})

test_that("plot_aci_dashboard échoue proprement si patchwork est absent", {
  skip_if(requireNamespace("patchwork", quietly = TRUE),
          "patchwork est installé : ce test ne s'applique pas ici")
  df <- .build_aci_df()
  expect_error(plot_aci_dashboard(df), "patchwork.*required")
})

# --- animate_aci_map (nécessite gganimate + gifski) ---------------------------------

test_that("animate_aci_map anime le mode grid-cell (gganimate)", {
  skip_if_not_installed("gganimate")
  skip_if_not_installed("gifski")
  data <- list(ACI = array(1:12, dim = c(2, 2, 3)),
               lon = c(-1, 1), lat = c(41, 45),
               time = c("2010-01", "2010-02", "2010-03"))
  anim <- animate_aci_map(data, variable = "ACI", borders = FALSE)
  expect_true(inherits(anim, "gganim") || inherits(anim, "gif_image"))
})
