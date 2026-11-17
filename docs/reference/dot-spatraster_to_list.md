# Convert a (small) SpatRaster back to the package's plain-list format

Once data has been reduced to daily (or coarser) resolution, it is small
enough to hand off to the rest of the existing base-R pipeline
([`resample_monthly()`](resample_monthly.md),
[`standardize_metric()`](standardize_metric.md),
[`.compute_aci_grid()`](dot-compute_aci_grid.md), etc.) unchanged. This
is the bridge between the terra-based loading/reduction steps and that
pipeline.

## Usage

``` r
.spatraster_to_list(r)
```

## Arguments

- r:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  reasonably small (daily resolution or coarser – NOT intended for
  hourly data).

## Value

A list with elements `data` (\[lon x lat x time\] array), `lon`, `lat`,
`time` – the same structure produced by
[`load_netcdf()`](load_netcdf.md).
