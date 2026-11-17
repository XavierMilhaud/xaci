# Plot a summary dashboard of the ACI results

Arranges the four core plots (time series, component facets, boxplots
and density) into a single patchwork figure. Requires the patchwork
package to be installed.

## Usage

``` r
plot_aci_dashboard(aci_df, title = "ACI Summary Dashboard")
```

## Arguments

- aci_df:

  A `data.frame` returned by [`calculate_aci()`](calculate_aci.md) with
  `area = TRUE`.

- title:

  Overall figure title. Default `"ACI Summary Dashboard"`.

## Value

A patchwork object (inherits from `ggplot`).

## Examples

``` r
# Toy monthly ACI data.frame (see plot_aci_timeseries() for details)
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
plot_aci_dashboard(aci_df)

```
