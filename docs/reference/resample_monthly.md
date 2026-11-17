# Resample a 3-D or 1-D climate series to monthly resolution

Applies `FUN` within each calendar month.

## Usage

``` r
resample_monthly(dataset, FUN = mean)
```

## Arguments

- dataset:

  List returned by [`load_component()`](load_component.md) or any
  component function, with fields `data` (3-D array `[lon x lat x time]`
  or numeric vector) and `time` (POSIXct vector).

- FUN:

  Aggregation function. Default `mean`.

## Value

A list with `data` (monthly aggregates) and `time` (first day of each
month as POSIXct).
