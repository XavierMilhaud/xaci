# Calculate monthly wind exceedance frequency (terra version)

Drop-in, memory-safe replacement for
[`calculate_period_wind_exceedance_frequency()`](calculate_period_wind_exceedance_frequency.md).
Only the initial [`wind_power_terra()`](wind_power_terra.md) call
differs from the base-R version; the threshold, crossing, and
monthly-aggregation steps reuse the exact same helpers, since they
already operate on small, daily-resolution data.

## Usage

``` r
calculate_period_wind_exceedance_frequency_terra(
  u10_r,
  v10_r,
  reference_period,
  mask_path = NULL,
  threshold = 0.8,
  target_chunk_gb = 1
)
```

## Arguments

- u10_r, v10_r:

  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  hourly resolution, non masque.

- reference_period:

  Character vector `c("start", "end")`.

- mask_path:

  Path to the mask NetCDF file, or `NULL` (no masking).

- threshold:

  Numeric threshold for the mask. Default `0.8`.

- target_chunk_gb:

  Passed to [`resample_daily_terra()`](resample_daily_terra.md) (see its
  memory note) for memory-safe temporal chunking of large hourly series.

## Value

Same as
[`calculate_period_wind_exceedance_frequency()`](calculate_period_wind_exceedance_frequency.md).
