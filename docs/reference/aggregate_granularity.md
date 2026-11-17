# Aggregate a monthly data frame to a given temporal granularity

Aggregate a monthly data frame to a given temporal granularity

## Usage

``` r
aggregate_granularity(df, granularity = "month")
```

## Arguments

- df:

  A data.frame with "YYYY-MM-01" Date row names (monthly).

- granularity:

  One of "month", "season", "semester", "year".

## Value

A data.frame with aggregated values and appropriate row names.
