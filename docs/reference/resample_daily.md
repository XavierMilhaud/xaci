# Resample a 3-D climate array to daily resolution

Aggregates sub-daily data to daily values using the supplied function
(`sum` or `mean`).

## Usage

``` r
resample_daily(dataset, FUN = sum)
```

## Arguments

- dataset:

  List returned by [`load_component()`](load_component.md) (may have
  sub-daily time steps).

- FUN:

  Aggregation function: `sum` or `mean`.

## Value

A new list with daily `time` and aggregated `data`.
