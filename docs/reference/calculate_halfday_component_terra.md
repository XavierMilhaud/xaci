# Calculate the half-day (day or night) temperature component (terra version)

Calculate the half-day (day or night) temperature component (terra
version)

## Usage

``` r
calculate_halfday_component_terra(
  r,
  reference_period,
  part_of_day,
  extremum,
  percentile,
  above_thresholds,
  mask_path = NULL,
  threshold = 0.8,
  cores = 1L,
  target_chunk_gb = 1,
  window_days = 5L
)
```

## Arguments

- r:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  hourly resolution, temperature already in the desired unit (e.g.
  Celsius), with
  [`terra::time()`](https://rspatial.github.io/terra/reference/time.html)
  set. **Non masque** : le masquage se fait ici, une fois les donnees
  reduites (voir note ci-dessous).

- reference_period:

  Character vector `c("YYYY-MM-DD", "YYYY-MM-DD")`.

- part_of_day:

  `"day"` or `"night"`.

- extremum:

  `"min"` or `"max"`.

- percentile:

  Numeric percentile (e.g. 90 or 10).

- above_thresholds:

  Logical. `TRUE` counts days above the threshold (hot extremes);
  `FALSE` counts days below (cold extremes).

- mask_path:

  Path to the mask NetCDF file, or `NULL` (no masking).

- threshold:

  Numeric threshold for the mask. Default `0.8`.

- cores:

  Passed to
  [`calculate_percentiles_terra()`](calculate_percentiles_terra.md)'s
  own `cores` (see its performance note) – the rolling-window percentile
  step is by far the most expensive part of this function. Default `1`
  (sequential, identical to previous behaviour).

- target_chunk_gb:

  Passed to [`temp_extremum_terra()`](temp_extremum_terra.md) /
  [`resample_daily_terra()`](resample_daily_terra.md) (see its memory
  note) for memory-safe temporal chunking of large hourly series.

- window_days:

  Passed to
  [`calculate_percentiles_terra()`](calculate_percentiles_terra.md):
  width, in days, of the rolling window used to compute the percentile
  threshold. Default `5L`.

## Value

Same structure as
[`calculate_halfday_component()`](calculate_halfday_component.md):
`list(data, time, lon, lat)`.
