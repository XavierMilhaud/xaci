# Compute per-day-of-year wind power thresholds from an already-daily series

Shared helper behind [`wind_thresholds()`](wind_thresholds.md) (base-R)
and the terra pipeline ([`wind_power_terra()`](wind_power_terra.md) +
this same function). Operates exclusively on DAILY-resolution wind power
(already small, even for 40+ years of data), never on raw hourly arrays.

## Usage

``` r
.wind_thresholds_from_wp(wp, reference_period, lon, lat)
```

## Arguments

- wp:

  A list `list(data, time)`, daily resolution wind power (e.g. from
  [`wind_power()`](wind_power.md), called without `reference_period`).

- reference_period:

  Character vector `c("start", "end")`.

- lon, lat:

  Coordinate vectors, carried through to the output.

## Value

A list with `data` \[lon x lat x all_days\] and `time`.
