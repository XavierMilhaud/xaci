# Calculate wind power thresholds (90th percentile per day-of-year)

Calculate wind power thresholds (90th percentile per day-of-year)

## Usage

``` r
wind_thresholds(u10_dataset, v10_dataset, reference_period)
```

## Arguments

- u10_dataset:

  List for `u10` variable.

- v10_dataset:

  List for `v10` variable.

- reference_period:

  Character vector `c("start", "end")`.

## Value

A list with `data` \[lon x lat x all_days\] giving the threshold for
each day of the full series, and `time`.
