# Calculate maximum precipitation over a rolling window (terra version)

Calculate maximum precipitation over a rolling window (terra version)

## Usage

``` r
calculate_maximum_precipitation_over_window_terra(
  r,
  var_name = "tp",
  window_size = 5L,
  mask_path = NULL,
  threshold = 0.8,
  target_chunk_gb = 1
)
```

## Arguments

- r:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  hourly (or sub-daily) resolution precipitation, with
  [`terra::time()`](https://rspatial.github.io/terra/reference/time.html)
  set. **Non masque** : le masquage se fait ici, une fois les donnees
  reduites a la resolution journaliere (voir note ci-dessous).

- var_name:

  Variable name in `dataset`. Default `"tp"`, inherited from ERA5
  (variable name in the NetCDF).

- window_size:

  Rolling window in DAYS. Default `5`.

- mask_path:

  Path to the mask NetCDF file, or `NULL` (no masking).

- threshold:

  Numeric threshold for the mask. Default `0.8`.

- target_chunk_gb:

  Passed to [`resample_daily_terra()`](resample_daily_terra.md) (see its
  memory note) for memory-safe temporal chunking of large hourly series.

## Value

Same structure as
[`calculate_maximum_precipitation_over_window()`](calculate_maximum_precipitation_over_window.md).
