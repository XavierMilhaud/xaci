# Calculate days with wind power above the 90th-percentile threshold

Calculate days with wind power above the 90th-percentile threshold

## Usage

``` r
days_above_wind_thresholds(u10_dataset, v10_dataset, reference_period)
```

## Arguments

- u10_dataset:

  List for `u10`.

- v10_dataset:

  List for `v10`.

- reference_period:

  Character vector `c("start", "end")`.

## Value

A list with binary `data` \[lon x lat x days\] and `time`.
