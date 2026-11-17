# Calculate monthly wind exceedance frequency

Calculate monthly wind exceedance frequency

## Usage

``` r
calculate_period_wind_exceedance_frequency(
  u10_dataset,
  v10_dataset,
  reference_period
)
```

## Arguments

- u10_dataset:

  List for `u10`.

- v10_dataset:

  List for `v10`.

- reference_period:

  Character vector `c("start", "end")`.

## Value

A list with `data` \[lon x lat x periods\] and `time`.
