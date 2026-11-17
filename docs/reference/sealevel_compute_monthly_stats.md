# Compute monthly reference statistics for sea-level data

Compute monthly reference statistics for sea-level data

## Usage

``` r
sealevel_compute_monthly_stats(df, reference_period, stats)
```

## Arguments

- df:

  Clean `data.frame` (row names = `"YYYY-MM-DD"`).

- reference_period:

  Character vector `c("start", "end")`.

- stats:

  `"means"` or `"std"`.

## Value

A numeric matrix, 12 rows (calendar months `"1"`-`"12"` as row names) x
one column per station (station names as returned by `colnames(df)`).
Each station is standardised against its own monthly reference
statistics, independently of the other stations – consistent with how
the other ACI components (ERA5 grid cells, administrative units) are
each standardised against their own reference, not a value pooled across
the whole spatial domain.
