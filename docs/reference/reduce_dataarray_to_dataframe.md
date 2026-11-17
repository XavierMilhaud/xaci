# Reduce a standardised spatial metric to a data frame

If `admin_mask` is `NULL`, averages over all non-NA cells (national
mean). Otherwise computes a weighted mean per administrative unit using
surface-fraction weights.

## Usage

``` r
reduce_dataarray_to_dataframe(metric, column_name = "value", admin_mask = NULL)
```

## Arguments

- metric:

  List with fields `data` \[lon x lat x time\] and `time`.

- column_name:

  Character. Column name prefix. Default `"value"`.

- admin_mask:

  Output of [`build_admin_mask()`](build_admin_mask.md), or `NULL`
  (default) for national mean.

## Value

A `data.frame` with one column per administrative unit (or one column
for the national mean), indexed by month-start dates.
