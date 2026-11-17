# Calculate the wind component of the ACI (terra version)

Drop-in, memory-safe replacement for
[`wind_component()`](wind_component.md), intended for full-resolution,
grid-cell-level ("area = FALSE") workflows over long historical periods
(40+ years hourly).

## Usage

``` r
wind_component_terra(
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
  load_dir = NULL,
  target_chunk_gb = 1
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

- target_chunk_gb:

  Passed to [`resample_daily_terra()`](resample_daily_terra.md) (see its
  memory note) for memory-safe temporal chunking of large hourly series.
  Lower this if you still hit memory errors (e.g.
  [`mem.maxVSize()`](https://rdrr.io/r/base/memlimits.html) on macOS);
  raise it only if you have RAM headroom to spare.

## Value

Same as [`wind_component()`](wind_component.md).
