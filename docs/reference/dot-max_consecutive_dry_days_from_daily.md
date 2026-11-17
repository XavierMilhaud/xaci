# Compute annual max consecutive dry days from an already-daily series

Shared helper behind
[`max_consecutive_dry_days()`](max_consecutive_dry_days.md) (base-R) and
its terra equivalent. Operates exclusively on DAILY-resolution
precipitation totals (small, even for 40+ years of data).

## Usage

``` r
.max_consecutive_dry_days_from_daily(daily)
```

## Arguments

- daily:

  List `list(data, time, lon, lat)`, daily resolution (e.g. from
  `resample_daily(dataset, FUN = sum)`).

## Value

A list with `data` \[lon x lat x years\] and annual `time`.
