# Compute the temperature ACI component (terra version)

Drop-in, memory-safe replacement for
[`temperature_component()`](temperature_component.md), intended for
full-resolution, grid-cell-level ("area = FALSE") workflows over long
historical periods (40+ years hourly).

## Usage

``` r
temperature_component_terra(
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
  cores = 1L,
  computed_components = FALSE,
  save = FALSE,
  save_dir = NULL,
  load_dir = NULL,
  target_chunk_gb = 1,
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

- cores:

  Passed through to
  [`calculate_halfday_component_terra()`](calculate_halfday_component_terra.md)
  / [`calculate_percentiles_terra()`](calculate_percentiles_terra.md)
  (see its performance note on
  [`terra::roll()`](https://rspatial.github.io/terra/reference/roll.html)
  having no built-in parallelism). Default `1`.

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

- window_days:

  Passed to
  [`calculate_percentiles_terra()`](calculate_percentiles_terra.md):
  width, in days, of the rolling window used to compute the percentile
  threshold. Default `5L`.

## Value

Same as [`temperature_component()`](temperature_component.md): a
standardized metric list, or a data frame if `admin_mask`/`admin_level`
is provided.
