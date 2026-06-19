#' @title Actuarial Climate Index (ACI)
#' @description Computes the full Actuarial Climate Index by combining all
#'   five components: temperature (T90 and T10), precipitation, drought, wind,
#'   and sea level.
#' @name aci
NULL

#' Calculate the Actuarial Climate Index
#'
#' This is the main entry point of the package.  It instantiates all five
#' climate components, standardises them over the reference period, and
#' combines them into a single monthly ACI time series:
#'
#' \deqn{ACI = \frac{T_{90} - T_{10} + P + D + \alpha \cdot SL + W}{6}}
#'
#' where \eqn{\alpha} is the erosion \code{factor} (default 1).
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
#' @param factor                  Numeric erosion factor applied to the sea-
#'   level component. Default \code{1}.
#'
#' @return A \code{data.frame} with columns \code{t90}, \code{t10},
#'   \code{precipitation}, \code{drought}, \code{wind}, \code{sealevel}, and
#'   \code{ACI}, indexed by month-end dates.
#'
#' @examples
#' \dontrun{
#' result <- calculate_aci(
#'   temperature_data_path   = "data/t2m_1960-2020.nc",
#'   precipitation_data_path = "data/tp_1960-2020.nc",
#'   wind_u10_data_path      = "data/u10_1960-2020.nc",
#'   wind_v10_data_path      = "data/v10_1960-2020.nc",
#'   country_abbrev          = "FRA",
#'   mask_data_path          = "data/mask_france.nc",
#'   study_period            = c("1980-01-01", "2020-12-31"),
#'   reference_period        = c("1961-01-01", "1990-12-31")
#' )
#' head(result)
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
                           factor = 1) {

  message("Computing drought component...")
  comp_drought <- drought_component(
    precipitation_data_path = precipitation_data_path,
    mask_path               = mask_data_path,
    reference_period        = reference_period,
    area                    = TRUE
  )

  message("Computing wind component...")
  comp_wind <- wind_component(
    wind_u10_data_path = wind_u10_data_path,
    wind_v10_data_path = wind_v10_data_path,
    mask_path          = mask_data_path,
    reference_period   = reference_period,
    area               = TRUE
  )

  message("Computing precipitation component...")
  comp_prec <- precipitation_component(
    precipitation_data_path = precipitation_data_path,
    mask_path               = mask_data_path,
    reference_period        = reference_period,
    area                    = TRUE
  )

  message("Computing temperature T10 component...")
  comp_t10 <- temperature_component(
    temperature_data_path = temperature_data_path,
    mask_path             = mask_data_path,
    reference_period      = reference_period,
    percentile            = 10,
    extremum              = "min",
    above_thresholds      = FALSE,
    area                  = TRUE
  )

  message("Computing temperature T90 component...")
  comp_t90 <- temperature_component(
    temperature_data_path = temperature_data_path,
    mask_path             = mask_data_path,
    reference_period      = reference_period,
    percentile            = 90,
    extremum              = "max",
    above_thresholds      = TRUE,
    area                  = TRUE
  )

  message("Computing sea-level component...")
  comp_sl <- sealevel_component(
    country_abbrev   = country_abbrev,
    study_period     = study_period,
    reference_period = reference_period
  )

  # Convert named vectors to data.frames
  to_df <- function(x, col) {
    df <- data.frame(as.numeric(x), row.names = names(x))
    colnames(df) <- col
    df
  }

  df_t90    <- to_df(comp_t90,    "t90")
  df_t10    <- to_df(comp_t10,    "t10")
  df_prec   <- to_df(comp_prec,   "precipitation")
  df_drought <- to_df(comp_drought, "drought")
  df_wind   <- to_df(comp_wind,   "wind")

  # Merge all components
  composites <- merge_dataframes(
    list(df_drought, df_wind, df_prec, df_t10, df_t90)
  )
  composites <- merge(composites, comp_sl,
                      by = "row.names", all = FALSE)
  rownames(composites) <- composites$Row.names
  composites$Row.names <- NULL

  # ACI formula
  composites$ACI <- (
    composites$t90
    - composites$t10
    + composites$precipitation
    + composites$drought
    + factor * composites$sealevel
    + composites$wind
  ) / 6

  composites
}
