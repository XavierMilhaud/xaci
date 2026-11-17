# Reduce sea-level data to a single column or one column per administrative unit

Reduce sea-level data to a single column or one column per
administrative unit

## Usage

``` r
reduce_sealevel_over_region(df, admin_assignment = NULL)
```

## Arguments

- df:

  A `data.frame` of standardised tide-gauge values with `"YYYY-MM-01"`
  row names and one column per station.

- admin_assignment:

  Output of [`assign_sealevel_to_admin()`](assign_sealevel_to_admin.md),
  or `NULL` (default) for national mean. Must be a list with elements
  `station_ids` (named list of PSMSL IDs per unit) and `factors` (named
  numeric vector of coastal fractions per unit).

## Value

A `data.frame` named `"sealevel"` (national) or `"sealevel_<unit>"` (per
admin unit). Units without coastal stations have `NA` values.
