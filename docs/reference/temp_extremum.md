# Compute daily temperature extremum (min or max) for day or night hours

Compute daily temperature extremum (min or max) for day or night hours

## Usage

``` r
temp_extremum(dataset, extremum, period)
```

## Arguments

- dataset:

  List returned by [`load_component()`](load_component.md) for the `t2m`
  variable (sub-daily, hourly data expected).

- extremum:

  `"min"` or `"max"`.

- period:

  `"day"` (hours 6–21) or `"night"` (hours 0–5 and 22–23).

## Value

A list with `data` \[lon × lat × days\] and daily `time`.
