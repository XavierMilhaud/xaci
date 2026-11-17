# Calculate the drought component of the ACI (terra version)

Drop-in, memory-safe replacement for
[`drought_component()`](drought_component.md).

## Usage

``` r
drought_component_terra(
  precipitation_data_path,
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

- precipitation_data_path:

  Path to the precipitation NetCDF file.

- country_abbrev:

  Three-letter ISO country code (e.g. `"FRA"`). Used to build
  `save_dir`/`load_dir` defaults and to construct the admin mask when
  `admin_level` is not `NULL`.

- reference_period:

  Character vector `c("start", "end")`.

- study_period:

  Character vector `c("start", "end")`. Full period covered by the
  study; used to name the cached grid-cell-level `.rds` file (e.g.
  `"drought_1980_2020.rds"`).

- mask_path:

  Path to the country mask NetCDF file, or `NULL`.

- area:

  Logical. If `TRUE` return national spatial mean as a named numeric
  vector. Ignored when `admin_level` or `admin_mask` is not `NULL`.
  Default `FALSE`.

- admin_level:

  Integer or `NULL`. If not `NULL`, builds the admin mask internally and
  returns one column per unit. Ignored when `admin_mask` is supplied
  directly.

- admin_mask:

  Output of [`build_admin_mask()`](build_admin_mask.md), or `NULL`. When
  supplied, takes precedence over `admin_level` (no rebuild).

- crs_metric:

  EPSG code for the metric CRS used when building the admin mask.
  Default `4326`.

- computed_components:

  Logical. If `TRUE`, reloads a previously saved `.rds` file from
  `load_dir` instead of recomputing. Default `FALSE`.

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

Same as [`drought_component()`](drought_component.md).
