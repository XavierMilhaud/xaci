library(testthat)

# ---------------------------------------------------------------------------
# Dispatch engine ("base" vs "terra") dans calculate_aci()
#
# On teste .resolve_component_functions() directement (comparaison
# d'identite de fonction, expect_identical) plutot que d'appeler
# calculate_aci() de bout en bout avec du mocking : plus rapide, plus
# robuste (independant de l'edition de testthat utilisee), et ca cible
# exactement la logique de dispatch reellement utilisee par
# calculate_aci() (pas une reimplementation parallele dans le test).
# ---------------------------------------------------------------------------

test_that(".resolve_component_functions() choisit les bonnes fonctions selon engine", {
  base_fns <- .resolve_component_functions("base")
  expect_identical(base_fns$temperature, temperature_component)
  expect_identical(base_fns$wind, wind_component)

  terra_fns <- .resolve_component_functions("terra")
  expect_identical(terra_fns$temperature, temperature_component_terra)
  expect_identical(terra_fns$wind, wind_component_terra)
})

test_that(".resolve_component_functions() utilise 'base' par defaut", {
  default_fns <- .resolve_component_functions()
  expect_identical(default_fns$temperature, temperature_component)
  expect_identical(default_fns$wind, wind_component)
})

test_that(".resolve_component_functions() rejette un engine invalide", {
  expect_error(.resolve_component_functions("not_a_real_engine"))
})

test_that("calculate_aci() rejette un engine invalide avant tout calcul", {
  # L'erreur doit survenir des match.arg(engine), avant meme la resolution
  # des chemins de fichiers ou tout calcul -- donc pas besoin de fournir de
  # vraies donnees climatiques pour ce test.
  expect_error(
    calculate_aci(
      country_abbrev          = "XX",
      study_period            = c("2001-01-01", "2001-12-31"),
      reference_period        = c("2001-01-01", "2001-12-31"),
      temperature_data_path   = "dummy.nc",
      precipitation_data_path = "dummy.nc",
      wind_u10_data_path      = "dummy.nc",
      wind_v10_data_path      = "dummy.nc",
      engine                  = "not_a_real_engine"
    )
  )
})
