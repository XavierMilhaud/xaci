# Plot the ACI global time series

Draws a line chart of the ACI values returned by
[`calculate_aci`](calculate_aci.md), with an optional smoothed trend and
a horizontal zero reference line.

## Usage

``` r
plot_aci_timeseries(
  aci_df,
  smooth = TRUE,
  span = 0.2,
  title = "Actuarial Climate Index (ACI)",
  colour = "#1F77B4",
  fill_area = TRUE
)
```

## Arguments

- aci_df:

  A `data.frame` returned by [`calculate_aci()`](calculate_aci.md) with
  `area = TRUE`. Must contain at least the column `ACI` and have
  month-end dates as row names.

- smooth:

  Logical. If `TRUE` (default), a LOESS smoothing curve is overlaid in
  red.

- span:

  Numeric span for the LOESS smoother. Default `0.2`.

- title:

  Plot title. Default `"Actuarial Climate Index (ACI)"`.

- colour:

  Line colour for the raw ACI series. Default `"#1F77B4"` (blue).

- fill_area:

  Logical. If `TRUE` (default), fills the area between the ACI curve and
  zero.

## Value

A `ggplot` object.

## Examples

``` r
# Toy monthly ACI data.frame (not from calculate_aci(), just for
# illustration), with the columns and "YYYY-MM" row names that
# calculate_aci(area = TRUE) would return.
n     <- 60L
dates <- format(seq(as.Date("2015-01-01"), by = "month", length.out = n),
                "%Y-%m")
aci_df <- data.frame(
  t90           = 20 + stats::rnorm(n, sd = 2),
  t10           = 5  + stats::rnorm(n, sd = 2),
  precipitation = stats::rnorm(n, sd = 1),
  drought       = stats::rnorm(n, sd = 1),
  wind          = stats::rnorm(n, sd = 1),
  sealevel      = stats::rnorm(n, sd = 1),
  row.names     = dates
)
aci_df$ACI <- with(aci_df, (t90 - t10 + precipitation + drought +
                               wind + sealevel) / 5)
plot_aci_timeseries(aci_df)

```
