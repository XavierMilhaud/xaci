# Calculate the wind component of the ACI

Calculate the wind component of the ACI

## Usage

``` r
wind_component(
  wind_u10_data_path,
  wind_v10_data_path,
  country_abbrev,
  reference_period,
  study_period,
  mask_path = NULL,
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

- wind_u10_data_path:

  Path to the u10 NetCDF file.

- wind_v10_data_path:

  Path to the v10 NetCDF file.

- country_abbrev:

  Three-letter ISO country code.

- reference_period:

  Character vector `c("start", "end")`. Climatological baseline used for
  standardisation.

- study_period:

  Character vector `c("start", "end")`. Full period covered by the
  study; used to name the cached grid-cell-level `.rds` file (e.g.
  `"wind_1980_2020.rds"`).

- mask_path:

  Path to the country mask NetCDF file, or `NULL`.

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
