# Calculate maximum consecutive dry days (CDD) per year

A dry day is defined as total daily precipitation \< 1 mm. The function
returns the annual maximum number of consecutive dry days for each grid
cell.

## Usage

``` r
max_consecutive_dry_days(dataset)
```

## Arguments

- dataset:

  List returned by [`load_component()`](load_component.md) for `tp`
  (total precipitation), with at least daily time resolution.

## Value

A list with `data` \[lon × lat × years\] and annual `time`.
