# Compute monthly max rolling precipitation from an already-daily series

Shared helper behind
[`calculate_maximum_precipitation_over_window()`](calculate_maximum_precipitation_over_window.md)
(base-R) and its terra equivalent. Operates exclusively on
DAILY-resolution precipitation totals (small, even for 40+ years of
data).

## Usage

``` r
.max_precipitation_from_daily(daily, var_name, window_size)
```

## Arguments

- daily:

  List `list(data, time, lon, lat)`, daily resolution (e.g. from
  `resample_daily(dataset, FUN = sum)`).

- var_name:

  Passed through to
  [`calculate_rolling_sum()`](calculate_rolling_sum.md).

- window_size:

  Rolling window in days.

## Value

A list with `data` \[lon x lat x months\] and `time`.
