# Compute monthly threshold-crossing frequency from daily extrema

Shared helper behind
[`calculate_halfday_component()`](calculate_halfday_component.md)
(base-R) and
[`calculate_halfday_component_terra()`](calculate_halfday_component_terra.md)
(terra). Kept independent of how `daily_ext`/`thresholds_day` were
produced, so both loading paths reuse the exact same, already-tested
logic.

## Usage

``` r
.crossing_frequency(daily_ext, thresholds_day, above_thresholds, lon, lat)
```

## Arguments

- daily_ext:

  List with `data` \[lon x lat x days\] and daily `time`, as returned by
  [`temp_extremum()`](temp_extremum.md)/[`temp_extremum_terra()`](temp_extremum_terra.md)
  (converted to list form).

- thresholds_day:

  Array `[lon x lat x 366]`, as returned by
  [`calculate_percentiles()`](calculate_percentiles.md)/[`calculate_percentiles_terra()`](calculate_percentiles_terra.md)
  (converted to array form).

- above_thresholds:

  Logical. `TRUE` counts days above the threshold (hot extremes);
  `FALSE` counts days below (cold extremes).

- lon, lat:

  Coordinate vectors, carried through to the output.

## Value

A list with `data` \[lon x lat x months\] and monthly `time`.
