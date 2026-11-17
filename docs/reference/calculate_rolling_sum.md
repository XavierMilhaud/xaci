# Calculate the rolling sum of a climate variable

Computes a rolling (sliding-window) sum along the time dimension of a
3-D climate array.

## Usage

``` r
calculate_rolling_sum(dataset, var_name, window_size)
```

## Arguments

- dataset:

  List returned by [`load_component()`](load_component.md).

- var_name:

  Variable name (for documentation; the array in `dataset$data` is used
  directly).

- window_size:

  Integer number of time steps for the rolling window.

## Value

A list with the same structure as `dataset` but with `data` replaced by
the rolling sum (leading incomplete windows are `NA`).
