# Reorient a terra array to the package's \[lon x lat x layer\] convention

[`terra::as.array()`](https://rspatial.github.io/terra/reference/coerce.html)
returns `[nrow x ncol x nlyr]` with rows (latitude) in decreasing order.
The rest of the package expects `[lon x lat x layer]` with latitude
increasing, matching [`load_netcdf()`](load_netcdf.md). Shared by
[`.spatraster_to_list()`](dot-spatraster_to_list.md) and by
[`calculate_percentiles_terra()`](calculate_percentiles_terra.md)'s
day-of-year output (which has no real time dimension, so
[`.spatraster_to_list()`](dot-spatraster_to_list.md) doesn't apply).

## Usage

``` r
.spatraster_to_array_only(r)
```

## Arguments

- r:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).

## Value

A numeric array `[ncol x nrow x nlyr]` = `[lon x lat x layer]`.
