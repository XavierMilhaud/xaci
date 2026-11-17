# Calculate the half-day (day or night) temperature component

Calculate the half-day (day or night) temperature component

## Usage

``` r
calculate_halfday_component(
  dataset,
  reference_period,
  part_of_day,
  extremum,
  percentile,
  above_thresholds,
  window_days = 5L
)
```

## Arguments

- dataset:

  Full sub-daily `t2m` dataset (list).

- reference_period:

  Character vector `c("YYYY-MM-DD", "YYYY-MM-DD")`.

- part_of_day:

  `"day"` or `"night"`.

- extremum:

  `"min"` or `"max"`.

- percentile:

  Numeric percentile (e.g. 90 or 10).

- above_thresholds:

  Logical. `TRUE` counts days above the threshold (hot extremes);
  `FALSE` counts days below (cold extremes).

- window_days:

  Passed to [`calculate_percentiles()`](calculate_percentiles.md):
  width, in days, of the rolling window used to compute the percentile
  threshold. Default `5L`.

## Value

A list with `data` \[lon × lat × months\] and monthly `time`.
