# Compute daily temperature extremum (min or max) for day or night hours (terra version)

Equivalent of [`temp_extremum()`](temp_extremum.md). Filters layers by
hour-of-day (a cheap, lazy operation on a SpatRaster – no data is read),
then reduces to daily resolution via
[`resample_daily_terra()`](resample_daily_terra.md).

## Usage

``` r
temp_extremum_terra(r, extremum, period, filename = "", target_chunk_gb = 1)
```

## Arguments

- r:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  hourly resolution, with
  [`terra::time()`](https://rspatial.github.io/terra/reference/time.html)
  set.

- extremum:

  `"min"` or `"max"`.

- period:

  `"day"` (hours 6-21) or `"night"` (hours 0-5 and 22-23).

- filename:

  Optional output path (see
  [`resample_daily_terra()`](resample_daily_terra.md)).

- target_chunk_gb:

  Passed to [`resample_daily_terra()`](resample_daily_terra.md) (see its
  memory note) for memory-safe temporal chunking of large hourly series.

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with one layer per day.
