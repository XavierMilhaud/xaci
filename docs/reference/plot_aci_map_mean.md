# Plot the temporal mean of an ACI variable over a sub-period

Convenience wrapper around `plot_aci_map` that computes the mean over a
user-defined date range rather than a single time index.

## Usage

``` r
plot_aci_map_mean(
  data,
  variable = "ACI",
  period = NULL,
  country_abbrev = NULL,
  admin_level = NULL,
  crs_metric = NULL,
  var_label = "Standardised anomaly",
  title = NULL,
  palette = "RdBu",
  reverse = TRUE,
  n_breaks = 9,
  borders = TRUE
)
```

## Arguments

- data:

  Bare array, grid-cell list, or admin `data.frame`.

- variable:

  Variable to map. Default `"ACI"`. Ignored when `data` is already a
  bare array.

- period:

  Character vector `c("start", "end")` in the same format as the `time`
  field of `data` (e.g. `c("2000-01", "2010-12")` for monthly). If
  `NULL` (default), uses the full time range.

- country_abbrev:

  Three-letter ISO code.

- admin_level:

  Integer or `NULL`.

- crs_metric:

  EPSG code. Default `NULL` (read from attributes, falling back to
  `4326`).

- var_label:

  Colour-bar label.

- title:

  Plot title.

- palette:

  RColorBrewer palette. Default `"RdBu"`.

- reverse:

  Logical. Default `TRUE`.

- n_breaks:

  Integer. Default `9`.

- borders:

  Logical. Default `TRUE`.

## Value

A `ggplot` object.
