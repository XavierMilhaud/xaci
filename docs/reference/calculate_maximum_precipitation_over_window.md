# Calculate maximum precipitation over a rolling window

Computes the rolling `window_size`-day sum of precipitation, then takes
the monthly maximum.

## Usage

``` r
calculate_maximum_precipitation_over_window(
  dataset,
  var_name = "tp",
  window_size = 5L
)
```

## Arguments

- dataset:

  List returned by [`load_component()`](load_component.md) for `tp`. May
  be sub-daily (e.g. hourly ERA5 data); it is always resampled to daily
  totals first.

- var_name:

  Variable name in `dataset`. Default `"tp"`, inherited from ERA5
  (variable name in the NetCDF).

- window_size:

  Rolling window in DAYS. Default `5`.

## Value

A list with `data` \[lon x lat x months\] and `time`.
