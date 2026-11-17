# Aggregate a 3-D climate array along the time dimension

Aggregate a 3-D climate array along the time dimension

## Usage

``` r
.aggregate_granularity_array(data, time, granularity = "month", FUN = mean)
```

## Arguments

- data:

  Numeric array `[nl x nw x nt]`.

- time:

  POSIXct vector of length `nt`.

- granularity:

  One of `"month"`, `"season"`, `"semester"`, `"year"`.

- FUN:

  Aggregation function. Default `mean`.

## Value

A list with `data` (array `[nl x nw x nt_agg]`), and `time` (character
vector of period labels, same convention as
[`aggregate_granularity()`](aggregate_granularity.md)).
