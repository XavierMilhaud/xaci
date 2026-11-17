# Standardise sea-level data over the study period

Standardise sea-level data over the study period

## Usage

``` r
sealevel_standardize_data(df, monthly_means, monthly_std_devs, study_period)
```

## Arguments

- df:

  Clean `data.frame`.

- monthly_means:

  Numeric matrix, 12 rows (months `"1"`-`"12"`) x one column per
  station, as returned by
  `sealevel_compute_monthly_stats(..., stats = "means")`.

- monthly_std_devs:

  Same shape as `monthly_means`, for `stats = "std"`.

- study_period:

  Character vector `c("start", "end")`.

## Value

A `data.frame` of standardised anomalies for the study period, with rows
containing all-NA removed. Each station's values are standardised
against its own monthly reference (see
[`sealevel_compute_monthly_stats()`](sealevel_compute_monthly_stats.md)),
not a value pooled across stations.
