#' @title Actuarial Climate Index (ACI)
#' @description Computes the full Actuarial Climate Index by combining all
#'   five components: temperature (T90 and T10), precipitation, drought, wind,
#'   and sea level.
#' @name aci
NULL

#' Resolve which component functions to use based on the compute engine
#'
#' Isolates the \code{engine} dispatch logic used by \code{calculate_aci()}
#' so it can be unit-tested directly (via \code{expect_identical()}) without
#' needing to mock or run the full \code{calculate_aci()} pipeline.
#'
#' @param engine \code{"base"} (default, original ncdf4-based loading) or
#'   \code{"terra"} (memory-safe, for long/high-resolution historical
#'   series -- see \code{component_terra.R}). Covers temperature, wind,
#'   drought, and precipitation; \code{sealevel_component()} is not yet
#'   available in a terra-based form (its primary input is tide-gauge CSV
#'   data, not gridded NetCDF, so the memory issue doesn't apply the same
#'   way -- see follow-up note).
#' @return A list with elements \code{temperature}, \code{wind},
#'   \code{drought}, \code{precipitation}, each a function with the same
#'   signature as its base-R counterpart.
#' @keywords internal
.resolve_component_functions <- function(engine = c("base", "terra")) {
  engine <- match.arg(engine)
  if (engine == "terra") {
    list(temperature  = temperature_component_terra,
         wind          = wind_component_terra,
         drought       = drought_component_terra,
         precipitation = precipitation_component_terra)
  } else {
    list(temperature  = temperature_component,
         wind          = wind_component,
         drought       = drought_component,
         precipitation = precipitation_component)
  }
}

#' Compute the ACI at grid-cell level
#'
#' All component inputs must be lists with a \code{data} array
#' \code{[nl x nw x nt]} and matching \code{lon}, \code{lat}, \code{time}.
#'
#' @param comp_t_high  Temperature high component (list with \code{data}).
#' @param comp_t_low   Temperature low component.
#' @param comp_prec    Precipitation component.
#' @param comp_drought Drought component.
#' @param comp_wind    Wind component.
#' @param comp_sl      Sea-level component (list with \code{data}), or
#'   \code{NULL} if unavailable.
#' @param col_high     Column name for temperature high (e.g. \code{"t90"}).
#' @param col_low      Column name for temperature low  (e.g. \code{"t10"}).
#' @return A list with arrays \code{[nl x nw x nt]} for each component and
#'   \code{ACI}, plus \code{lon}, \code{lat}, \code{time}.
#' @keywords internal
.compute_aci_grid <- function(comp_t_high, comp_t_low,
                              comp_prec, comp_drought, comp_wind,
                              comp_sl = NULL,
                              col_high = "t90", col_low = "t10") {
  nl <- dim(comp_t_high$data)[1]
  nw <- dim(comp_t_high$data)[2]
  nt <- dim(comp_t_high$data)[3]

  # alpha(i,j) : 1 si la cellule a un signal sealevel, 0 sinon
  if (!is.null(comp_sl)) {
    # Utilise le premier pas de temps pour détecter les cellules côtières
    alpha <- ifelse(is.na(comp_sl$data[, , 1L]), 0, 1)  # [nl x nw]
  } else {
    alpha <- matrix(0, nrow = nl, ncol = nw)
  }

  aci_data <- array(NA_real_, c(nl, nw, nt))
  sl_data  <- if (!is.null(comp_sl)) comp_sl$data else
    array(0, c(nl, nw, nt))

  for (t in seq_len(nt)) {
    # NB: en R, `0 * NA` vaut `NA`, pas `0`. Or `alpha` est precisement
    # defini comme 0 la ou `sl_data` est NA (cellules non cotieres / sans
    # signal sealevel). Sans neutraliser ces NA, `alpha * sl_data` reste NA
    # meme quand alpha = 0, et cette NA contamine `num` (et donc `ACI`)
    # POUR TOUTES LES CELLULES, y compris celles loin de toute cote -
    # potentiellement rendant l'array ACI entierement NA si sealevel est
    # NA partout (pas de stations marégraphiques chargees/disponibles).
    sl_t <- sl_data[, , t]
    sl_t[is.na(sl_t)] <- 0

    num <- comp_t_high$data[, , t] -
      comp_t_low$data[, , t]  +
      comp_prec$data[, , t]   +
      comp_drought$data[, , t]+
      alpha * sl_t            +
      comp_wind$data[, , t]
    den <- 5 + alpha   # [nl x nw], soit 5 soit 6
    aci_data[, , t] <- num / den
  }

  out <- list(
    ACI           = aci_data,
    lon           = comp_t_high$lon,
    lat           = comp_t_high$lat,
    time          = comp_t_high$time
  )
  out[[col_high]]      <- comp_t_high$data
  out[[col_low]]       <- comp_t_low$data
  out[["precipitation"]] <- comp_prec$data
  out[["drought"]]       <- comp_drought$data
  out[["wind"]]          <- comp_wind$data
  out[["sealevel"]]      <- sl_data
  out
}

#' Calculate the Actuarial Climate Index
#'
#' This is the main entry point of the package. It instantiates all five
#' climate components, standardises them over the reference period, and
#' combines them into a single ACI time series:
#'
#' \deqn{ACI = \frac{T_{high} - T_{low} + P + D + \alpha \cdot SL + W}{5 + \alpha}}
#'
#' where \eqn{\alpha} is the sea-level erosion \code{factor} (coastal fraction,
#' default \code{1/5}). For administrative units without coastal stations,
#' \eqn{\alpha = 0} and the denominator becomes \code{5}. At grid-cell level,
#' \eqn{\alpha \in \{0, 1\}} depending on whether the cell lies within
#' \code{max_dist_km} of a tide-gauge station.
#'
#' @param country_abbrev Three-letter ISO 3166-1 alpha-3 country code
#'   (e.g. \code{"FRA"}). Used to build default file paths and to download
#'   PSMSL tide-gauge data.
#' @param study_period            Character vector of length 2:
#'   \code{c("YYYY-MM-DD", "YYYY-MM-DD")} defining the study window.
#' @param reference_period        Character vector of length 2:
#'   \code{c("YYYY-MM-DD", "YYYY-MM-DD")} defining the climatological
#'   reference period used for standardisation.
#' @param years Integer vector of years covered by the NetCDF files
#'   (e.g. \code{2011:2015}). Used to build default file paths when any
#'   \code{*_data_path} argument is \code{NULL}. Not needed if all paths
#'   are supplied explicitly.
#' @param temperature_data_path   Path to the hourly 2-m temperature NetCDF
#'   (\code{t2m} variable). If \code{NULL} (default), built automatically
#'   from \code{country_abbrev} and \code{years}.
#' @param precipitation_data_path Path to the precipitation NetCDF
#'   (\code{tp} variable). If \code{NULL}, built automatically.
#' @param wind_u10_data_path      Path to the u-component wind NetCDF
#'   (\code{u10} variable). If \code{NULL}, built automatically.
#' @param wind_v10_data_path      Path to the v-component wind NetCDF
#'   (\code{v10} variable). If \code{NULL}, built automatically.
#' @param mask_data_path          Path to the country mask NetCDF
#'   (\code{country} variable). If \code{NULL}, built automatically.
#' @param sealevel_dir Character or \code{NULL}. Path to a directory of
#'   already-downloaded PSMSL \code{.txt} files. If \code{NULL} (default),
#'   data are downloaded automatically to \code{"data/psmsl/<country_abbrev>"}.
#' @param percentile_high Numeric. Upper percentile used for the hot temperature
#'   component. Default \code{90}. The corresponding column in the output will
#'   be named \code{t<percentile_high>} (e.g. \code{t90}).
#' @param percentile_low  Numeric. Lower percentile used for the cold temperature
#'   component. Default \code{10}. The corresponding column in the output will
#'   be named \code{t<percentile_low>} (e.g. \code{t10}).
#' @param granularity             Temporal aggregation level. One of
#'   \code{"month"} (default), \code{"season"}, \code{"semester"},
#'   \code{"year"}. Seasons follow meteorological convention: DJF (Dec-Feb),
#'   MAM (Mar-May), JJA (Jun-Aug), SON (Sep-Nov). December is attributed to
#'   the following year's winter (e.g. December 2010 -> "2011-DJF").
#' @param area Logical. Only used when \code{admin_level = NULL}. If
#'   \code{TRUE} (default), returns a national scalar time series (spatial
#'   mean over the country). If \code{FALSE}, returns the full grid-cell-level
#'   ACI and all components as arrays \code{[lon x lat x time]}, suitable
#'   for mapping or custom aggregation.
#'   Ignored when \code{admin_level} is not \code{NULL}.
#' @param factor                  Numeric in \code{[0, 1]}. Weight of the
#'   sea-level component at the national level, representing the fraction of
#'   coastal area. Default \code{1/5}. At the administrative unit level,
#'   this argument is ignored: the coastal fraction is computed automatically
#'   per unit by \code{assign_sealevel_to_admin()}. At grid-cell level,
#'   \eqn{\alpha} is set to 1 for cells within \code{max_dist_km} of a
#'   tide-gauge station, and 0 otherwise.
#' @param max_dist_km Numeric. Maximum distance (km) from a tide-gauge station
#'   for a grid cell to receive a sea-level signal. Cells beyond this threshold
#'   receive \code{NA} for sea level and \eqn{\alpha = 0}. Only used when
#'   \code{area = FALSE} and \code{admin_level = NULL}. Default \code{500}.
#' @param admin_level             Integer or \code{NULL}. If \code{NULL}
#'   (default), returns a single national index. Otherwise computes the ACI
#'   per administrative unit at the given level (1 = regions, 2 = departments,
#'   etc.). The sea-level component is automatically excluded (\eqn{\alpha = 0})
#'   for units without coastal tide-gauge stations.
#' @param crs_metric              Integer. EPSG code of a metric CRS
#'   appropriate for the country, used for accurate area and length
#'   calculations in \code{build_admin_mask()} and
#'   \code{assign_sealevel_to_admin()}. Only used when \code{admin_level} is
#'   not \code{NULL}. Default \code{4326} (WGS84, not recommended for
#'   production — prefer a local CRS such as \code{2154} for France or
#'   \code{27700} for the UK).
#' @param save      Logical. If \code{TRUE}, saves the grid-cell-level object
#'   to \code{save_dir} before aggregation. Default \code{FALSE}.
#' @param save_dir  Character. Directory for the cached \code{.rds} file.
#'   Created if it does not exist. Default \code{"results/<country_abbrev>"}.
#' @param load_dir  Character. Directory from which to reload previously saved
#'   \code{.rds} component files when \code{computed_components = TRUE}.
#'   Default \code{"results/<country_abbrev>"}.
#' @param computed_components Logical. If \code{TRUE}, rds files storing results
#'   of the computations of ACI components at grid cell level are reused.
#'   Default \code{FALSE}.
#' @param engine Character. \code{"base"} (default) uses the original
#'   ncdf4-based loading for all components. \code{"terra"} switches
#'   \code{temperature_component()}, \code{wind_component()},
#'   \code{drought_component()}, and \code{precipitation_component()} to
#'   their memory-safe terra-based equivalents (see
#'   \code{.resolve_component_functions()}), recommended for long,
#'   high-resolution historical series (40+ years hourly) that would
#'   otherwise saturate RAM. \code{sealevel_component()} is unaffected by
#'   this argument in either case.
#' @param cores Positive integer, default \code{1}. Only used when
#'   \code{engine = "terra"}, and only by \code{temperature_component_terra()}
#'   (passed through to \code{calculate_percentiles_terra()}'s own
#'   \code{cores} -- see its performance note: the rolling-window percentile
#'   step has no built-in parallelism in terra itself, so this is where
#'   parallelizing across spatial tiles helps most). Silently ignored for
#'   \code{engine = "base"} and for the other three components, which don't
#'   have a \code{cores} argument.
#' @return
#'   \describe{
#'     \item{National scalar (\code{area = TRUE}, \code{admin_level = NULL})}{
#'       A \code{data.frame} with columns \code{t<percentile_high>},
#'       \code{t<percentile_low>}, \code{precipitation}, \code{drought},
#'       \code{wind}, \code{sealevel}, and \code{ACI}, indexed by dates at
#'       the chosen granularity.}
#'     \item{Grid-cell (\code{area = FALSE}, \code{admin_level = NULL})}{
#'       A named list with one array \code{[lon x lat x nt]} per component
#'       (\code{ACI}, \code{t<percentile_high>}, \code{t<percentile_low>},
#'       \code{precipitation}, \code{drought}, \code{wind}, \code{sealevel}),
#'       plus \code{lon}, \code{lat}, and \code{time} (character vector of
#'       period labels at the chosen granularity).}
#'     \item{Administrative (\code{admin_level} integer)}{
#'       A \code{data.frame} indexed by dates at the chosen granularity,
#'       with one \code{<component>_<unit>} column per administrative unit
#'       FOR EACH component (\code{ACI}, \code{t<percentile_high>},
#'       \code{t<percentile_low>}, \code{precipitation}, \code{drought},
#'       \code{wind}, \code{sealevel}) -- e.g. \code{ACI_<unit>},
#'       \code{t90_<unit>}, \code{precipitation_<unit>}, etc. This mirrors
#'       the grid-cell mode's per-component structure, and lets
#'       \code{plot_aci_map()} plot any individual component at admin level,
#'       not just \code{ACI} itself.}
#'   }
#'
#' @examples
#' \dontrun{
#' # National ACI, annual granularity
#' result <- calculate_aci(
#'   temperature_data_path   = "data/era5/FRA/t2m_1960-2020.nc",
#'   precipitation_data_path = "data/era5/FRA/tp_1960-2020.nc",
#'   wind_u10_data_path      = "data/era5/FRA/u10_1960-2020.nc",
#'   wind_v10_data_path      = "data/era5/FRA/v10_1960-2020.nc",
#'   country_abbrev          = "FRA",
#'   mask_data_path          = "data/era5/FRA/mask_FRA.nc",
#'   study_period            = c("1980-01-01", "2020-12-31"),
#'   reference_period        = c("1961-01-01", "1990-12-31"),
#'   granularity             = "year",
#'   area                    = TRUE,
#'   factor                  = 1/5
#' )
#'
#' # Grid-cell ACI (full spatial output)
#' grid <- calculate_aci(
#'   temperature_data_path   = "data/era5/FRA/t2m_1960-2020.nc",
#'   precipitation_data_path = "data/era5/FRA/tp_1960-2020.nc",
#'   wind_u10_data_path      = "data/era5/FRA/u10_1960-2020.nc",
#'   wind_v10_data_path      = "data/era5/FRA/v10_1960-2020.nc",
#'   country_abbrev          = "FRA",
#'   mask_data_path          = "data/era5/FRA/mask_FRA.nc",
#'   study_period            = c("1980-01-01", "2020-12-31"),
#'   reference_period        = c("1961-01-01", "1990-12-31"),
#'   granularity             = "year",
#'   area                    = FALSE,
#'   max_dist_km             = 500
#' )
#'
#' # ACI with custom percentiles (T95 / T5)
#' result_custom <- calculate_aci(
#'   temperature_data_path   = "data/era5/FRA/t2m_1960-2020.nc",
#'   precipitation_data_path = "data/era5/FRA/tp_1960-2020.nc",
#'   wind_u10_data_path      = "data/era5/FRA/u10_1960-2020.nc",
#'   wind_v10_data_path      = "data/era5/FRA/v10_1960-2020.nc",
#'   country_abbrev          = "FRA",
#'   mask_data_path          = "data/era5/FRA/mask_FRA.nc",
#'   study_period            = c("1980-01-01", "2020-12-31"),
#'   reference_period        = c("1961-01-01", "1990-12-31"),
#'   granularity             = "year",
#'   percentile_high         = 95,
#'   percentile_low          = 5
#' )
#'
#' # ACI by department (admin level 2), using Lambert-93 for France
#' result_dept <- calculate_aci(
#'   temperature_data_path   = "data/era5/FRA/t2m_1960-2020.nc",
#'   precipitation_data_path = "data/era5/FRA/tp_1960-2020.nc",
#'   wind_u10_data_path      = "data/era5/FRA/u10_1960-2020.nc",
#'   wind_v10_data_path      = "data/era5/FRA/v10_1960-2020.nc",
#'   country_abbrev          = "FRA",
#'   mask_data_path          = "data/era5/FRA/mask_FRA.nc",
#'   study_period            = c("1980-01-01", "2020-12-31"),
#'   reference_period        = c("1961-01-01", "1990-12-31"),
#'   granularity             = "year",
#'   area                    = FALSE,
#'   admin_level             = 2,
#'   crs_metric              = 2154
#' )
#' }
#' @export
calculate_aci <- function(country_abbrev,
                          study_period,
                          reference_period,
                          years                   = NULL,
                          temperature_data_path   = NULL,
                          precipitation_data_path = NULL,
                          wind_u10_data_path      = NULL,
                          wind_v10_data_path      = NULL,
                          mask_data_path          = NULL,
                          sealevel_dir            = NULL,
                          percentile_high         = 90,
                          percentile_low          = 10,
                          granularity             = "month",
                          area                    = TRUE,
                          factor                  = 1 / 5,
                          max_dist_km             = 500,
                          admin_level             = NULL,
                          crs_metric              = 4326,
                          save                    = FALSE,
                          save_dir                = paste0("results/", country_abbrev),
                          load_dir                = paste0("results/", country_abbrev),
                          computed_components     = FALSE,
                          engine                  = c("base", "terra"),
                          cores                   = 1L) {

  # engine = "terra" : bascule temperature_component()/wind_component()/
  # drought_component()/precipitation_component() vers leurs equivalents
  # memory-safe (component_terra.R et fichiers *_terra.R associes), utiles
  # pour de longs historiques haute-resolution (40+ ans horaire) qui
  # saturent la RAM avec le chargement ncdf4 classique.
  # sealevel_component() n'est PAS concernee par ce parametre (voir note de
  # suivi dans .resolve_component_functions()).
  #
  # cores : uniquement utilise quand engine = "terra", et seulement par
  # temperature_component_terra() -- transmis a calculate_percentiles_terra(),
  # dont l'etape de quantile glissant (terra::roll()) est de loin la plus
  # couteuse du pipeline terra et n'a aucune parallelisation native (voir
  # ?calculate_percentiles_terra). Ignore silencieusement pour engine = "base"
  # (temperature_component() de base n'a pas ce parametre).
  engine            <- match.arg(engine)
  component_fns     <- .resolve_component_functions(engine)
  temperature_fun    <- component_fns$temperature
  wind_fun           <- component_fns$wind
  drought_fun        <- component_fns$drought
  precipitation_fun  <- component_fns$precipitation

  # --- Column name helpers ---
  col_high <- sprintf("t%d", as.integer(percentile_high))
  col_low  <- sprintf("t%d", as.integer(percentile_low))

  # Shorthand : TRUE uniquement en mode national scalaire ERA5
  grid_cell_mode <- is.null(admin_level) && !area

  # --- Resolve file paths ---
  if (any(is.null(c(temperature_data_path, precipitation_data_path,
                    wind_u10_data_path, wind_v10_data_path,
                    mask_data_path)))) {
    if (is.null(years))
      stop("Provide 'years' so that file paths can be built automatically, ",
           "or supply all *_data_path arguments explicitly.")
    paths <- .build_era5_paths(country_abbrev, years)
    if (is.null(temperature_data_path))   temperature_data_path   <- paths$t2m
    if (is.null(precipitation_data_path)) precipitation_data_path <- paths$tp
    if (is.null(wind_u10_data_path))      wind_u10_data_path      <- paths$u10
    if (is.null(wind_v10_data_path))      wind_v10_data_path      <- paths$v10
    if (is.null(mask_data_path))          mask_data_path          <- paths$mask
  }

  # --- Spatial masks (in case of administrative level specified) ---
  admin_mask       <- NULL
  admin_assignment <- NULL

  if (!is.null(admin_level)) {
    # Noms des fichiers de cache admin
    admin_tag <- sprintf("%s_L%d", country_abbrev, admin_level)
    mask_path_rds       <- file.path(save_dir,
                                     paste0("admin_mask_",       admin_tag, ".rds"))
    assignment_path_rds <- file.path(save_dir,
                                     paste0("admin_assignment_", admin_tag, ".rds"))

    if (computed_components &&
        file.exists(mask_path_rds) && file.exists(assignment_path_rds)) {
      message("Loading cached admin mask and sealevel assignment...")
      admin_mask       <- readRDS(mask_path_rds)
      admin_assignment <- readRDS(assignment_path_rds)
    } else {
      message("Building administrative mask (this may take a moment)...")
      if (engine == "terra") {
        tmp_r       <- load_component_terra(temperature_data_path, "t2m", mask_data_path)
        tmp_dataset <- .spatraster_to_list(tmp_r[[1]])   # 1 seule couche suffit pour lon/lat
      } else {
        tmp_dataset <- load_component(temperature_data_path, "t2m", mask_data_path)
      }
      admin_mask  <- build_admin_mask(
        lon            = tmp_dataset$lon,
        lat            = tmp_dataset$lat,
        country_abbrev = country_abbrev,
        admin_level    = admin_level,
        crs_metric     = crs_metric
      )
      admin_assignment <- assign_sealevel_to_admin(
        country_abbrev = country_abbrev,
        admin_level    = admin_level,
        crs_metric     = crs_metric
      )
      rm(tmp_dataset)

      if (save) {
        dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
        saveRDS(admin_mask,       mask_path_rds)
        saveRDS(admin_assignment, assignment_path_rds)
        message("Admin mask and sealevel assignment saved to: ", save_dir)
      }
    }
  }

  # ------------------------------------------------------------------ #
  # Calcul ou chargement des composantes                                #
  # ------------------------------------------------------------------ #
  if (!computed_components) {

    message("Computing drought component...")
    comp_drought <- drought_fun(
      country_abbrev = country_abbrev,
      precipitation_data_path = precipitation_data_path,
      mask_path               = mask_data_path,
      reference_period        = reference_period,
      study_period            = study_period,
      area                    = is.null(admin_level) && area,
      admin_mask              = admin_mask,
      save                    = save,
      save_dir                = save_dir
    )

    message("Computing wind component...")
    comp_wind <- wind_fun(
      country_abbrev = country_abbrev,
      wind_u10_data_path = wind_u10_data_path,
      wind_v10_data_path = wind_v10_data_path,
      mask_path          = mask_data_path,
      reference_period   = reference_period,
      study_period       = study_period,
      area               = is.null(admin_level) && area,
      admin_mask         = admin_mask,
      save               = save,
      save_dir           = save_dir
    )

    message("Computing precipitation component...")
    comp_prec <- precipitation_fun(
      country_abbrev = country_abbrev,
      precipitation_data_path = precipitation_data_path,
      mask_path               = mask_data_path,
      reference_period        = reference_period,
      study_period            = study_period,
      area                    = is.null(admin_level) && area,
      admin_mask              = admin_mask,
      save                    = save,
      save_dir                = save_dir
    )

    message(sprintf("Computing temperature T%d component...", percentile_low))
    t_low_args <- list(
      country_abbrev = country_abbrev,
      temperature_data_path = temperature_data_path,
      mask_path             = mask_data_path,
      reference_period      = reference_period,
      study_period          = study_period,
      percentile            = percentile_low,
      extremum              = "min",
      above_thresholds      = FALSE,
      area                  = is.null(admin_level) && area,
      admin_mask            = admin_mask,
      save                  = save,
      save_dir              = save_dir
    )
    # cores : uniquement transmis pour engine = "terra" -- temperature_component()
    # (base) n'a pas ce parametre, do.call() erreurait sinon ("unused argument").
    if (engine == "terra") t_low_args$cores <- cores
    comp_t_low <- do.call(temperature_fun, t_low_args)

    message(sprintf("Computing temperature T%d component...", percentile_high))
    t_high_args <- list(
      country_abbrev = country_abbrev,
      temperature_data_path = temperature_data_path,
      mask_path             = mask_data_path,
      reference_period      = reference_period,
      study_period          = study_period,
      percentile            = percentile_high,
      extremum              = "max",
      above_thresholds      = TRUE,
      area                  = is.null(admin_level) && area,
      admin_mask            = admin_mask,
      save                  = save,
      save_dir              = save_dir
    )
    if (engine == "terra") t_high_args$cores <- cores
    comp_t_high <- do.call(temperature_fun, t_high_args)

    message("Computing sea-level component...")
    comp_sl <- sealevel_component(
      country_abbrev      = country_abbrev,
      study_period        = study_period,
      reference_period    = reference_period,
      mask_path           = mask_data_path,
      area                = area,   # TRUE if area=FALSE + admin_level=NULL
      max_dist_km         = max_dist_km,
      sealevel_dir        = sealevel_dir,
      admin_level         = admin_level,
      admin_assignment    = admin_assignment,
      crs_metric          = crs_metric,
      computed_components = FALSE,
      save                = save,
      save_dir            = save_dir
    )

  } else {

    study_tag <- paste(substr(study_period[1], 1, 4),
                       substr(study_period[2], 1, 4), sep = "_")

    .load_rds <- function(name) {
      path <- file.path(load_dir, paste0(name, "_", study_tag, ".rds"))
      if (!file.exists(path))
        stop("Cached file not found: ", path,
             "\nRun calculate_aci() with save = TRUE first.")
      readRDS(path)
    }

    drought_raw <- .load_rds("drought")
    wind_raw    <- .load_rds("wind")
    prec_raw    <- .load_rds("precipitation")
    t_low_raw   <- .load_rds(paste0("temperature_t", as.integer(percentile_low)))
    t_high_raw  <- .load_rds(paste0("temperature_t", as.integer(percentile_high)))
    sl_raw      <- .load_rds("sealevel")  # list(data, coords)

    if (is.null(admin_mask)) {
      comp_drought <- standardize_metric(drought_raw, reference_period, area = area)
      comp_wind    <- standardize_metric(wind_raw,    reference_period, area = area)
      comp_prec    <- standardize_metric(prec_raw,    reference_period, area = area)
      comp_t_low   <- standardize_metric(t_low_raw,  reference_period, area = area)
      comp_t_high  <- standardize_metric(t_high_raw, reference_period, area = area)

      # Trois cas mutuellement exclusifs et exhaustifs
      comp_sl <- if (grid_cell_mode) {
        interpolate_sealevel_to_grid(sl_raw,
                                     lon         = comp_drought$lon,
                                     lat         = comp_drought$lat,
                                     max_dist_km = max_dist_km)
      } else {
        # Mode national scalaire : admin_mask est NULL et grid_cell_mode est FALSE
        reduce_sealevel_over_region(sl_raw$data)
      }

    } else {
      comp_drought <- reduce_dataarray_to_dataframe(
        standardize_metric(drought_raw, reference_period, area = FALSE),
        column_name = "drought",       admin_mask = admin_mask)
      comp_wind    <- reduce_dataarray_to_dataframe(
        standardize_metric(wind_raw,    reference_period, area = FALSE),
        column_name = "wind",          admin_mask = admin_mask)
      comp_prec    <- reduce_dataarray_to_dataframe(
        standardize_metric(prec_raw,    reference_period, area = FALSE),
        column_name = "precipitation", admin_mask = admin_mask)
      comp_t_low   <- reduce_dataarray_to_dataframe(
        standardize_metric(t_low_raw,  reference_period, area = FALSE),
        column_name = col_low,         admin_mask = admin_mask)
      comp_t_high  <- reduce_dataarray_to_dataframe(
        standardize_metric(t_high_raw, reference_period, area = FALSE),
        column_name = col_high,        admin_mask = admin_mask)

      # Mode administratif : agrégation par unité depuis les stations
      comp_sl <- reduce_sealevel_over_region(sl_raw$data,
                                             admin_assignment = admin_assignment)
    }

  }

  # ------------------------------------------------------------------ #
  # Mode grid-cell : calcul ACI + agrégation temporelle                 #
  # ------------------------------------------------------------------ #
  if (grid_cell_mode) {
    grid_aci <- .compute_aci_grid(
      comp_t_high  = comp_t_high,
      comp_t_low   = comp_t_low,
      comp_prec    = comp_prec,
      comp_drought = comp_drought,
      comp_wind    = comp_wind,
      comp_sl      = comp_sl,
      col_high     = col_high,
      col_low      = col_low
    )

    # Agrégation temporelle de chaque composante + ACI
    components <- c("ACI", col_high, col_low,
                    "precipitation", "drought", "wind", "sealevel")
    out <- list(lon = grid_aci$lon, lat = grid_aci$lat)
    agg_time <- NULL
    for (comp in components) {
      agg      <- .aggregate_granularity_array(grid_aci[[comp]],
                                               grid_aci$time,
                                               granularity)
      out[[comp]] <- agg$data
      agg_time    <- agg$time
    }
    out$time <- agg_time

    # Attache lon/lat/time/country_abbrev directement sur chaque array de
    # composante, pour que ces arrays soient auto-suffisants une fois extraits
    # de la liste (ex: grid$t90 utilisable seul, sans passer par grid).
    for (comp in components) {
      out[[comp]] <- .attach_spatial_attrs(
        out[[comp]],
        lon            = out$lon,
        lat            = out$lat,
        time           = out$time,
        country_abbrev = country_abbrev
      )
    }
    return(out)
  }

  # ------------------------------------------------------------------ #
  # Mode national scalaire                                              #
  # ------------------------------------------------------------------ #
  if (is.null(admin_level)) {
    to_df <- function(x, col) {
      df <- data.frame(as.numeric(x), row.names = names(x))
      colnames(df) <- col
      df
    }

    df_t_high  <- to_df(comp_t_high, col_high)
    df_t_low   <- to_df(comp_t_low,  col_low)
    df_prec    <- to_df(comp_prec,   "precipitation")
    df_drought <- to_df(comp_drought,"drought")
    df_wind    <- to_df(comp_wind,   "wind")

    monthly_aci <- merge_dataframes(
      list(df_drought, df_wind, df_prec, df_t_low, df_t_high)
    )
    monthly_aci <- merge(monthly_aci, comp_sl,
                         by = "row.names", all = FALSE)
    rownames(monthly_aci) <- monthly_aci$Row.names
    monthly_aci$Row.names <- NULL

    monthly_aci$ACI <- (
      monthly_aci[[col_high]]
      - monthly_aci[[col_low]]
      + monthly_aci$precipitation
      + monthly_aci$drought
      + factor * monthly_aci$sealevel
      + monthly_aci$wind
    ) / (5 + factor)

    return(aggregate_granularity(monthly_aci, granularity))
  }

  # ------------------------------------------------------------------ #
  # Mode administratif                                                  #
  # ------------------------------------------------------------------ #

  # Nom de fichier cache pour les composantes deja agregees par unite admin.
  # Le cache depend du pays, du niveau, ET de la periode de reference (car la
  # standardisation utilise reference_period). Il ne depend PAS de study_period
  # ni de granularity : ces deux dimensions sont appliquees apres le cache
  # (aggregate_granularity() est appele en aval).
  ref_tag_admin <- paste(substr(reference_period[1], 1, 4),
                         substr(reference_period[2], 1, 4), sep = "_")
  admin_comp_rds <- file.path(
    save_dir,
    sprintf("admin_components_%s_L%d_%s.rds",
            country_abbrev, admin_level, ref_tag_admin)
  )

  if (computed_components && file.exists(admin_comp_rds)) {
    message("Loading cached admin-level components from: ", admin_comp_rds)
    monthly_aci <- readRDS(admin_comp_rds)

  } else {
    units <- admin_mask$units

    per_unit <- lapply(units, function(u) {

      get_col <- function(df, prefix) {
        col <- paste0(prefix, "_", u)
        if (!col %in% colnames(df))
          return(rep(NA_real_, nrow(df)))
        df[[col]]
      }

      t_high_u  <- get_col(comp_t_high,  col_high)
      t_low_u   <- get_col(comp_t_low,   col_low)
      prec_u    <- get_col(comp_prec,    "precipitation")
      drought_u <- get_col(comp_drought, "drought")
      wind_u    <- get_col(comp_wind,    "wind")

      sl_col       <- paste0("sealevel_", u)
      has_sea      <- sl_col %in% colnames(comp_sl)
      dates        <- rownames(comp_t_high)
      sl_u_aligned <- if (has_sea) {
        comp_sl[[sl_col]][match(dates, rownames(comp_sl))]
      } else {
        rep(NA_real_, length(dates))
      }
      alpha <- if (has_sea) admin_assignment$factors[[u]] else 0

      # Meme piege qu'au niveau grid-cell : `alpha * sl_u_aligned` vaut NA des
      # que `sl_u_aligned` est NA, MEME quand `alpha = 0` (non cotier), car en
      # R `0 * NA` vaut `NA` et non `0`. Sans ce garde-fou, `ACI` devient NA
      # pour toutes les unites non cotieres (et pour toute date sans
      # alignement sealevel quand alpha = 0).
      sl_term <- if (alpha == 0) rep(0, length(dates)) else alpha * sl_u_aligned

      aci <- (t_high_u - t_low_u + prec_u + drought_u +
                sl_term + wind_u) / (5 + alpha)

      # On conserve aussi chaque composante individuelle (pas seulement ACI),
      # pour pouvoir tracer plot_aci_map(admin_df, variable = "t90") etc. au
      # meme titre que l'ACI lui-meme, a n'importe quel niveau administratif.
      comp_df <- data.frame(
        ACI         = aci,
        t_high_u,
        t_low_u,
        precipitation = prec_u,
        drought       = drought_u,
        wind          = wind_u,
        sealevel      = sl_u_aligned,
        row.names     = dates,
        check.names   = FALSE
      )
      colnames(comp_df)[2:3] <- c(col_high, col_low)
      colnames(comp_df) <- paste0(colnames(comp_df), "_", u)
      comp_df
    })

    monthly_aci <- do.call(cbind, per_unit)

    if (save) {
      dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      saveRDS(monthly_aci, admin_comp_rds)
      message("Admin-level components saved to: ", admin_comp_rds)
    }
  }
  result <- aggregate_granularity(monthly_aci, granularity)

  # Attache country_abbrev/admin_level/crs_metric en attributs, pour que
  # plot_aci_map() puisse les retrouver automatiquement sans que l'utilisateur
  # ait à les re-spécifier.
  .attach_spatial_attrs(
    result,
    country_abbrev = country_abbrev,
    admin_level    = admin_level,
    crs_metric     = crs_metric
  )
}
