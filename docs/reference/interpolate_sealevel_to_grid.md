# Interpolate tide-gauge sea-level values onto an ERA5 grid using IDW

Interpolate tide-gauge sea-level values onto an ERA5 grid using IDW

## Usage

``` r
interpolate_sealevel_to_grid(raw, lon, lat, max_dist_km = 500, power = 2)
```

## Arguments

- raw:

  List returned by [`sealevel_process()`](sealevel_process.md), with
  fields `data` (standardised `data.frame` `[time x stations]`) and
  `coords` (`data.frame` with `station_id`, `lon`, `lat`).

- lon:

  Numeric vector of grid longitudes (length nl).

- lat:

  Numeric vector of grid latitudes (length nw).

- max_dist_km:

  Numeric. Cells farther than this from every station receive `NA`.
  Default `500`.

- power:

  Numeric. IDW power parameter. Default `2`.

## Value

A list with `data` (array `[nl x nw x nt]`), `lon`, `lat`, `time`
(POSIXct).
