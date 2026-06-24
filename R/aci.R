#' @title Actuarial Climate Index (ACI)
#' @description Computes the full Actuarial Climate Index by combining all
#'   five components: temperature (T90 and T10), precipitation, drought, wind,
#'   and sea level.
#' @name aci
NULL

#' Calculate the Actuarial Climate Index
#'
#' This is the main entry point of the package. It instantiates all five
#' climate components, standardises them over the reference period, and
#' combines them into a single ACI time series:
#'
#' \deqn{ACI = \frac{T_{90} - T_{10} + P + D + \alpha \cdot SL + W}{5 + \alpha}}
#'
#' where \eqn{\alpha} is the sea-level erosion \code{factor} (coastal fraction,
#' default \code{1/5}). For administrative units without coastal stations,
#' \eqn{\alpha = 0} and the denominator becomes \code{5}.
#'
#' @param temperature_data_path   Path to the hourly 2-m temperature NetCDF
#'   (\code{t2m} variable).
#' @param precipitation_data_path Path to the hourly/daily precipitation NetCDF
#'   (\code{tp} variable).
#' @param wind_u10_data_path      Path to the u-component of 10-m wind NetCDF
#'   (\code{u10} variable).
#' @param wind_v10_data_path      Path to the v-component of 10-m wind NetCDF
#'   (\code{v10} variable).
#' @param country_abbrev          Three-letter ISO country code used to
#'   download PSMSL tide-gauge data (e.g. \code{"FRA"}).
#' @param mask_data_path          Path to the country mask NetCDF file
#'   (\code{country} variable, values in [0, 1]).
#' @param study_period            Character vector of length 2:
#'   \code{c("YYYY-MM-DD", "YYYY-MM-DD")} defining the study window.
#' @param reference_period        Character vector of length 2:
#'   \code{c("YYYY-MM-DD", "YYYY-MM-DD")} defining the climatological
#'   reference period used for standardisation.
#' @param granularity             Temporal aggregation level. One of
#'   \code{"month"} (default), \code{"season"}, \code{"semester"},
#'   \code{"year"}. Seasons follow meteorological convention: DJF (Dec-Feb),
#'   MAM (Mar-May), JJA (Jun-Aug), SON (Sep-Nov). December is attributed to
#'   the following year's winter (e.g. December 2010 -> "2011-DJF").
#' @param factor                  Numeric in \code{[0, 1]}. Weight of the
#'   sea-level component at the national level, representing the fraction of
#'   coastal area. Default \code{1/5}. At the administrative unit level,
#'   this argument is ignored: the coastal fraction is computed automatically
#'   per unit by \code{assign_sealevel_to_admin()}.
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
#'
#' @return If \code{admin_level = NULL}: a \code{data.frame} with columns
#'   \code{t90}, \code{t10}, \code{precipitation}, \code{drought},
#'   \code{wind}, \code{sealevel}, and \code{ACI}, indexed by dates at the
#'   chosen granularity.
#'   If \code{admin_level} is an integer: a \code{data.frame} with one
#'   \code{ACI_<unit>} column per administrative unit, indexed by dates at
#'   the chosen granularity.
#'
#' @examples
#' \dontrun{
#' # National ACI, annual granularity
#' result <- calculate_aci(
#'   temperature_data_path   = "data/t2m_1960-2020.nc",
#'   precipitation_data_path = "data/tp_1960-2020.nc",
#'   wind_u10_data_path      = "data/u10_1960-2020.nc",
#'   wind_v10_data_path      = "data/v10_1960-2020.nc",
#'   country_abbrev          = "FRA",
#'   mask_data_path          = "data/mask_france.nc",
#'   study_period            = c("1980-01-01", "2020-12-31"),
#'   reference_period        = c("1961-01-01", "1990-12-31"),
#'   granularity             = "year",
#'   factor                  = 1/5
#' )
#'
#' # ACI by department (admin level 2), using Lambert-93 for France
#' result_dept <- calculate_aci(
#'   temperature_data_path   = "data/t2m_1960-2020.nc",
#'   precipitation_data_path = "data/tp_1960-2020.nc",
#'   wind_u10_data_path      = "data/u10_1960-2020.nc",
#'   wind_v10_data_path      = "data/v10_1960-2020.nc",
#'   country_abbrev          = "FRA",
#'   mask_data_path          = "data/mask_france.nc",
#'   study_period            = c("1980-01-01", "2020-12-31"),
#'   reference_period        = c("1961-01-01", "1990-12-31"),
#'   granularity             = "year",
#'   admin_level             = 2,
#'   crs_metric              = 2154
#' )
#' }
#' @export
calculate_aci <- function(temperature_data_path,
                          precipitation_data_path,
                          wind_u10_data_path,
                          wind_v10_data_path,
                          country_abbrev,
                          mask_data_path,
                          study_period,
                          reference_period,
                          granularity = "month",
                          factor      = 1 / 5,
                          admin_level = NULL,
                          crs_metric  = 4326) {

  # --- Spatial masks (in case of administrative level specified) ---
  admin_mask       <- NULL
  admin_assignment <- NULL

  if (!is.null(admin_level)) {
    message("Building administrative mask (this may take a moment)...")
    tmp_dataset <- load_component(temperature_data_path, "t2m", mask_data_path)
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
  }

  # --- Computation of components (monthly) ---
  message("Computing drought component...")
  comp_drought <- drought_component(
    precipitation_data_path = precipitation_data_path,
    mask_path               = mask_data_path,
    reference_period        = reference_period,
    area                    = is.null(admin_level),
    admin_mask              = admin_mask
  )

  message("Computing wind component...")
  comp_wind <- wind_component(
    wind_u10_data_path = wind_u10_data_path,
    wind_v10_data_path = wind_v10_data_path,
    mask_path          = mask_data_path,
    reference_period   = reference_period,
    area               = is.null(admin_level),
    admin_mask         = admin_mask
  )

  message("Computing precipitation component...")
  comp_prec <- precipitation_component(
    precipitation_data_path = precipitation_data_path,
    mask_path               = mask_data_path,
    reference_period        = reference_period,
    area                    = is.null(admin_level),
    admin_mask              = admin_mask
  )

  message("Computing temperature T10 component...")
  comp_t10 <- temperature_component(
    temperature_data_path = temperature_data_path,
    mask_path             = mask_data_path,
    reference_period      = reference_period,
    percentile            = 10,
    extremum              = "min",
    above_thresholds      = FALSE,
    area                  = is.null(admin_level),
    admin_mask            = admin_mask
  )

  message("Computing temperature T90 component...")
  comp_t90 <- temperature_component(
    temperature_data_path = temperature_data_path,
    mask_path             = mask_data_path,
    reference_period      = reference_period,
    percentile            = 90,
    extremum              = "max",
    above_thresholds      = TRUE,
    area                  = is.null(admin_level),
    admin_mask            = admin_mask
  )

  message("Computing sea-level component...")
  comp_sl <- sealevel_component(
    country_abbrev   = country_abbrev,
    study_period     = study_period,
    reference_period = reference_period,
    admin_assignment = admin_assignment
  )

  # --- Country level (default behaviour) ---
  if (is.null(admin_level)) {

    to_df <- function(x, col) {
      df <- data.frame(as.numeric(x), row.names = names(x))
      colnames(df) <- col
      df
    }

    df_t90     <- to_df(comp_t90,     "t90")
    df_t10     <- to_df(comp_t10,     "t10")
    df_prec    <- to_df(comp_prec,    "precipitation")
    df_drought <- to_df(comp_drought, "drought")
    df_wind    <- to_df(comp_wind,    "wind")

    monthly_aci <- merge_dataframes(
      list(df_drought, df_wind, df_prec, df_t10, df_t90)
    )
    monthly_aci <- merge(monthly_aci, comp_sl,
                         by = "row.names", all = FALSE)
    rownames(monthly_aci) <- monthly_aci$Row.names
    monthly_aci$Row.names <- NULL

    # ACI formula with adaptive denominator
    monthly_aci$ACI <- (
      monthly_aci$t90
      - monthly_aci$t10
      + monthly_aci$precipitation
      + monthly_aci$drought
      + factor * monthly_aci$sealevel
      + monthly_aci$wind
    ) / (5 + factor)

    return(aggregate_granularity(monthly_aci, granularity))
  }

  # --- Specified administrative level ---
  units <- admin_mask$units

  aci_list <- lapply(units, function(u) {

    # Extract the right column (unit) from each ERA5 component
    get_col <- function(df, prefix) {
      col <- paste0(prefix, "_", u)
      if (!col %in% colnames(df))
        return(rep(NA_real_, nrow(df)))
      df[[col]]
    }

    t90_u     <- get_col(comp_t90,     "t90")
    t10_u     <- get_col(comp_t10,     "t10")
    prec_u    <- get_col(comp_prec,    "precipitation")
    drought_u <- get_col(comp_drought, "drought")
    wind_u    <- get_col(comp_wind,    "wind")

    # Sea level : NA if no coast in the administrative unit
    sl_col  <- paste0("sealevel_", u)
    has_sea <- sl_col %in% colnames(comp_sl)

    # Align temporal indexes
    dates <- rownames(comp_t90)
    sl_u_aligned <- if (has_sea) {
      comp_sl[[sl_col]][match(dates, rownames(comp_sl))]
    } else {
      rep(NA_real_, length(dates))
    }

    # ACI formula : alpha = coastal fraction if coast, else 0
    alpha <- if (has_sea) admin_assignment$factors[[u]] else 0

    aci <- (
      t90_u
      - t10_u
      + prec_u
      + drought_u
      + alpha * sl_u_aligned
      + wind_u
    ) / (5 + alpha)

    data.frame(aci, row.names = dates)
  })

  monthly_aci           <- do.call(cbind, aci_list)
  colnames(monthly_aci) <- paste0("ACI_", units)

  aggregate_granularity(monthly_aci, granularity)
}
