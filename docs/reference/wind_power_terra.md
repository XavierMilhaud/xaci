# Calculate daily wind power from u10 and v10 components (terra version)

Terra-based equivalent of [`wind_power()`](wind_power.md): computes
daily mean u/v via [`resample_daily_terra()`](resample_daily_terra.md)
(block-by-block, never loading the full hourly cube in RAM), then wind
speed/power arithmetic directly on the (now daily, small) SpatRasters
before converting back to the package's list format.

## Usage

``` r
wind_power_terra(
  u10_r,
  v10_r,
  reference_period = NULL,
  mask_path = NULL,
  threshold = 0.8,
  target_chunk_gb = 1
)
```

## Arguments

- u10_r, v10_r:

  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  hourly resolution, **non masque** – le masquage se fait ici, une fois
  `wp_r` reduit a la resolution journaliere (voir note ci-dessous), et
  une seule fois plutot que separement sur u10_r et v10_r.

- reference_period:

  Optional character vector `c("start","end")`. If provided, only the
  reference sub-period is returned.

- mask_path:

  Path to the mask NetCDF file, or `NULL` (no masking).

- threshold:

  Numeric threshold for the mask. Default `0.8`.

- target_chunk_gb:

  Passed to [`resample_daily_terra()`](resample_daily_terra.md) (see its
  memory note) for memory-safe temporal chunking of large hourly series.

## Value

Same structure as [`wind_power()`](wind_power.md):
`list(data, time, lon, lat)`.
