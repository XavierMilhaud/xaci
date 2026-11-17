# Calculate the full temperature component of the ACI

Combines day and night half-day components (equal weighting), then
standardises relative to the reference period.

## Usage

``` r
temperature_component(
  temperature_data_path,
  country_abbrev,
  reference_period,
  study_period,
  mask_path = NULL,
  percentile = 90,
  extremum = "max",
  above_thresholds = TRUE,
  area = FALSE,
  admin_level = NULL,
  admin_mask = NULL,
  crs_metric = 4326,
  computed_components = FALSE,
  save = FALSE,
  save_dir = NULL,
  load_dir = NULL,
  window_days = 5L
)
```

## Arguments

- temperature_data_path:

  Path to the hourly `t2m` NetCDF file.

- country_abbrev:

  Three-letter ISO country code.

- reference_period:

  Character vector `c("start", "end")`. Climatological baseline used for
  standardisation.

- study_period:

  Character vector `c("start", "end")`. Full period covered by the
  study; used to name the cached grid-cell-level `.rds` file (e.g.
  `"temperature_t90_1980_2020.rds"`), so that distinct runs over
  different study windows don't collide or get mixed up.

- mask_path:

  Path to the country mask NetCDF file.

- percentile:

  Percentile for the threshold. Default `90`.

- extremum:

  `"max"` (hot) or `"min"` (cold).

- above_thresholds:

  Logical. Default `TRUE`.

- area:

  Logical. Default `FALSE`.

- admin_level:

  Integer or `NULL`.

- admin_mask:

  Output of [`build_admin_mask()`](build_admin_mask.md), or `NULL`.

- crs_metric:

  EPSG code. Default `4326`.

- computed_components:

  Logical. Default `FALSE`.

- save:

  Logical. Default `FALSE`.

- save_dir:

  Character. Default `NULL`, which resolves to a sub-directory of
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- load_dir:

  Character. Default `NULL`, which resolves to a sub-directory of
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- window_days:

  Passed to [`calculate_percentiles()`](calculate_percentiles.md):
  width, in days, of the rolling window used to compute the percentile
  threshold. Default `5L`.

## Value

Named numeric vector, standardised list, or `data.frame` per admin unit.
