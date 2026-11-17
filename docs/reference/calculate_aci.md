# Calculate the Actuarial Climate Index

This is the main entry point of the package. It instantiates all five
climate components, standardises them over the reference period, and
combines them into a single ACI time series:

## Usage

``` r
calculate_aci(
  country_abbrev,
  study_period,
  reference_period,
  years = NULL,
  temperature_data_path = NULL,
  precipitation_data_path = NULL,
  wind_u10_data_path = NULL,
  wind_v10_data_path = NULL,
  mask_data_path = NULL,
  sealevel_dir = NULL,
  percentile_high = 90,
  percentile_low = 10,
  granularity = "month",
  area = TRUE,
  factor = 1/5,
  max_dist_km = 500,
  admin_level = NULL,
  crs_metric = 4326,
  save = FALSE,
  save_dir = NULL,
  load_dir = NULL,
  computed_components = FALSE,
  engine = c("base", "terra"),
  cores = 1L
)
```

## Arguments

- country_abbrev:

  Three-letter ISO 3166-1 alpha-3 country code (e.g. `"FRA"`). Used to
  build default file paths and to download PSMSL tide-gauge data.

- study_period:

  Character vector of length 2: `c("YYYY-MM-DD", "YYYY-MM-DD")` defining
  the study window.

- reference_period:

  Character vector of length 2: `c("YYYY-MM-DD", "YYYY-MM-DD")` defining
  the climatological reference period used for standardisation.

- years:

  Integer vector of years covered by the NetCDF files (e.g.
  `2011:2015`). Used to build default file paths when any `*_data_path`
  argument is `NULL`. Not needed if all paths are supplied explicitly.

- temperature_data_path:

  Path to the hourly 2-m temperature NetCDF (`t2m` variable). If `NULL`
  (default), built automatically from `country_abbrev` and `years`.

- precipitation_data_path:

  Path to the precipitation NetCDF (`tp` variable). If `NULL`, built
  automatically.

- wind_u10_data_path:

  Path to the u-component wind NetCDF (`u10` variable). If `NULL`, built
  automatically.

- wind_v10_data_path:

  Path to the v-component wind NetCDF (`v10` variable). If `NULL`, built
  automatically.

- mask_data_path:

  Path to the country mask NetCDF (`country` variable). If `NULL`, built
  automatically.

- sealevel_dir:

  Character or `NULL`. Path to a directory of already-downloaded PSMSL
  `.txt` files. If `NULL` (default), data are downloaded automatically
  to a sub-directory of
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- percentile_high:

  Numeric. Upper percentile used for the hot temperature component.
  Default `90`. The corresponding column in the output will be named
  `t<percentile_high>` (e.g. `t90`).

- percentile_low:

  Numeric. Lower percentile used for the cold temperature component.
  Default `10`. The corresponding column in the output will be named
  `t<percentile_low>` (e.g. `t10`).

- granularity:

  Temporal aggregation level. One of `"month"` (default), `"season"`,
  `"semester"`, `"year"`. Seasons follow meteorological convention: DJF
  (Dec-Feb), MAM (Mar-May), JJA (Jun-Aug), SON (Sep-Nov). December is
  attributed to the following year's winter (e.g. December 2010 -\>
  "2011-DJF").

- area:

  Logical. Only used when `admin_level = NULL`. If `TRUE` (default),
  returns a national scalar time series (spatial mean over the country).
  If `FALSE`, returns the full grid-cell-level ACI and all components as
  arrays `[lon x lat x time]`, suitable for mapping or custom
  aggregation. Ignored when `admin_level` is not `NULL`.

- factor:

  Numeric in `[0, 1]`. Weight of the sea-level component at the national
  level, representing the fraction of coastal area. Default `1/5`. At
  the administrative unit level, this argument is ignored: the coastal
  fraction is computed automatically per unit by
  [`assign_sealevel_to_admin()`](assign_sealevel_to_admin.md). At
  grid-cell level, \\\alpha\\ is set to 1 for cells within `max_dist_km`
  of a tide-gauge station, and 0 otherwise.

- max_dist_km:

  Numeric. Maximum distance (km) from a tide-gauge station for a grid
  cell to receive a sea-level signal. Cells beyond this threshold
  receive `NA` for sea level and \\\alpha = 0\\. Only used when
  `area = FALSE` and `admin_level = NULL`. Default `500`.

- admin_level:

  Integer or `NULL`. If `NULL` (default), returns a single national
  index. Otherwise computes the ACI per administrative unit at the given
  level (1 = regions, 2 = departments, etc.). The sea-level component is
  automatically excluded (\\\alpha = 0\\) for units without coastal
  tide-gauge stations.

- crs_metric:

  Integer. EPSG code of a metric CRS appropriate for the country, used
  for accurate area and length calculations in
  [`build_admin_mask()`](build_admin_mask.md) and
  [`assign_sealevel_to_admin()`](assign_sealevel_to_admin.md). Only used
  when `admin_level` is not `NULL`. Default `4326` (WGS84, not
  recommended for production — prefer a local CRS such as `2154` for
  France or `27700` for the UK).

- save:

  Logical. If `TRUE`, saves the grid-cell-level object to `save_dir`
  before aggregation. Default `FALSE`.

- save_dir:

  Character. Directory for the cached `.rds` file. Created if it does
  not exist. Default `NULL`, which resolves to a sub-directory of
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- load_dir:

  Character. Directory from which to reload previously saved `.rds`
  component files when `computed_components = TRUE`. Default `NULL`,
  which resolves to a sub-directory of
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- computed_components:

  Logical. If `TRUE`, rds files storing results of the computations of
  ACI components at grid cell level are reused. Default `FALSE`.

- engine:

  Character. `"base"` (default) uses the original ncdf4-based loading
  for all components. `"terra"` switches
  [`temperature_component()`](temperature_component.md),
  [`wind_component()`](wind_component.md),
  [`drought_component()`](drought_component.md), and
  [`precipitation_component()`](precipitation_component.md) to their
  memory-safe terra-based equivalents (see
  [`.resolve_component_functions()`](dot-resolve_component_functions.md)),
  recommended for long, high-resolution historical series (40+ years
  hourly) that would otherwise saturate RAM.
  [`sealevel_component()`](sealevel_component.md) is unaffected by this
  argument in either case.

- cores:

  Positive integer, default `1`. Only used when `engine = "terra"`, and
  only by
  [`temperature_component_terra()`](temperature_component_terra.md)
  (passed through to
  [`calculate_percentiles_terra()`](calculate_percentiles_terra.md)'s
  own `cores` – see its performance note: it controls how many
  memory-safe spatial tiles are processed concurrently, which is where
  parallelizing helps most). Silently ignored for `engine = "base"` and
  for the other three components, which don't have a `cores` argument.

## Value

- National scalar (`area = TRUE`, `admin_level = NULL`):

  A `data.frame` with columns `t<percentile_high>`, `t<percentile_low>`,
  `precipitation`, `drought`, `wind`, `sealevel`, and `ACI`, indexed by
  dates at the chosen granularity.

- Grid-cell (`area = FALSE`, `admin_level = NULL`):

  A named list with one array `[lon x lat x nt]` per component (`ACI`,
  `t<percentile_high>`, `t<percentile_low>`, `precipitation`, `drought`,
  `wind`, `sealevel`), plus `lon`, `lat`, and `time` (character vector
  of period labels at the chosen granularity).

- Administrative (`admin_level` integer):

  A `data.frame` indexed by dates at the chosen granularity, with one
  `<component>_<unit>` column per administrative unit FOR EACH component
  (`ACI`, `t<percentile_high>`, `t<percentile_low>`, `precipitation`,
  `drought`, `wind`, `sealevel`) – e.g. `ACI_<unit>`, `t90_<unit>`,
  `precipitation_<unit>`, etc. This mirrors the grid-cell mode's
  per-component structure, and lets [`plot_aci_map()`](plot_aci_map.md)
  plot any individual component at admin level, not just `ACI` itself.

## Details

\$\$ACI = \frac{T\_{high} - T\_{low} + P + D + \alpha \cdot SL + W}{5 +
\alpha}\$\$

where \\\alpha\\ is the sea-level erosion `factor` (coastal fraction,
default `1/5`). For administrative units without coastal stations,
\\\alpha = 0\\ and the denominator becomes `5`. At grid-cell level,
\\\alpha \in \\0, 1\\\\ depending on whether the cell lies within
`max_dist_km` of a tide-gauge station.

## Examples

The four examples below all require real ERA5 NetCDF files (temperature,
precipitation, wind, country mask), obtained beforehand via
[`download_era5`](download_era5.md)/[`download_mask`](download_mask.md)
and a Copernicus CDS API key (see [`cds_set_key`](cds_set_key.md)). They
cannot be run automatically by `R CMD check` and are therefore wrapped
in `dontrun`: (1) national ACI at annual granularity; (2) grid-cell ACI
(full spatial output); (3) ACI with custom percentiles (T95/T5); and (4)
ACI by department (admin level 2), using Lambert-93 for France.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- calculate_aci(
  temperature_data_path   = "t2m_1960-2020.nc",
  precipitation_data_path = "tp_1960-2020.nc",
  wind_u10_data_path      = "u10_1960-2020.nc",
  wind_v10_data_path      = "v10_1960-2020.nc",
  country_abbrev          = "FRA",
  mask_data_path          = "mask_FRA.nc",
  study_period            = c("1980-01-01", "2020-12-31"),
  reference_period        = c("1961-01-01", "1990-12-31"),
  granularity             = "year",
  area                    = TRUE,
  factor                  = 1/5
)

grid <- calculate_aci(
  temperature_data_path   = "t2m_1960-2020.nc",
  precipitation_data_path = "tp_1960-2020.nc",
  wind_u10_data_path      = "u10_1960-2020.nc",
  wind_v10_data_path      = "v10_1960-2020.nc",
  country_abbrev          = "FRA",
  mask_data_path          = "mask_FRA.nc",
  study_period            = c("1980-01-01", "2020-12-31"),
  reference_period        = c("1961-01-01", "1990-12-31"),
  granularity             = "year",
  area                    = FALSE,
  max_dist_km             = 500
)

result_custom <- calculate_aci(
  temperature_data_path   = "t2m_1960-2020.nc",
  precipitation_data_path = "tp_1960-2020.nc",
  wind_u10_data_path      = "u10_1960-2020.nc",
  wind_v10_data_path      = "v10_1960-2020.nc",
  country_abbrev          = "FRA",
  mask_data_path          = "mask_FRA.nc",
  study_period            = c("1980-01-01", "2020-12-31"),
  reference_period        = c("1961-01-01", "1990-12-31"),
  granularity             = "year",
  percentile_high         = 95,
  percentile_low          = 5
)

result_dept <- calculate_aci(
  temperature_data_path   = "t2m_1960-2020.nc",
  precipitation_data_path = "tp_1960-2020.nc",
  wind_u10_data_path      = "u10_1960-2020.nc",
  wind_v10_data_path      = "v10_1960-2020.nc",
  country_abbrev          = "FRA",
  mask_data_path          = "mask_FRA.nc",
  study_period            = c("1980-01-01", "2020-12-31"),
  reference_period        = c("1961-01-01", "1990-12-31"),
  granularity             = "year",
  area                    = FALSE,
  admin_level             = 2,
  crs_metric              = 2154
)
} # }
```
