# Calculate maximum consecutive dry days (terra version)

Calculate maximum consecutive dry days (terra version)

## Usage

``` r
max_consecutive_dry_days_terra(
  r,
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

- mask_path:

  Path to the mask NetCDF file, or `NULL` (no masking).

- threshold:

  Numeric threshold for the mask. Default `0.8`.

- target_chunk_gb:

  Passed to [`resample_daily_terra()`](resample_daily_terra.md) (see its
  memory note) for memory-safe temporal chunking of large hourly series.

## Value

Same structure as
[`max_consecutive_dry_days()`](max_consecutive_dry_days.md).
