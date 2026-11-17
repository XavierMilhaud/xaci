# Calculate the precipitation component of the ACI

Computes the standardised anomaly of maximum monthly precipitation over
a rolling 5-day window.

## Usage

``` r
precipitation_component(
  precipitation_data_path,
  country_abbrev,
  reference_period,
  study_period,
  mask_path = NULL,
  var_name = "tp",
  window_size = 5L,
  area = FALSE,
  admin_level = NULL,
  admin_mask = NULL,
  crs_metric = 4326,
  computed_components = FALSE,
  save = FALSE,
  save_dir = NULL,
  load_dir = NULL
)
```

## Arguments

- precipitation_data_path:

  Path to the precipitation NetCDF file.

- country_abbrev:

  Three-letter ISO country code.

- reference_period:

  Character vector `c("start", "end")`.

- study_period:

  Character vector `c("start", "end")`. Full period covered by the
  study; used to name the cached grid-cell-level `.rds` file (e.g.
  `"precipitation_1980_2020.rds"`).

- mask_path:

  Path to the country mask NetCDF file, or `NULL`.

- var_name:

  Variable name in the NetCDF. Default `"tp"`.

- window_size:

  Rolling window in days. Default `5`.

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

## Value

Named numeric vector, standardised list, or `data.frame` per admin unit.
