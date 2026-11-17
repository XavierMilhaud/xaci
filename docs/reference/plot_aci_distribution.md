# Plot the distribution of ACI components

Displays the statistical distribution of each standardised component
(and optionally the global ACI) as **boxplots** (`type = "boxplot"`),
**violin plots** (`type = "violin"`), or **density curves**
(`type = "density"`).

## Usage

``` r
plot_aci_distribution(
  aci_df,
  components = NULL,
  include_aci = FALSE,
  type = c("boxplot", "violin", "density"),
  title = "ACI Component Distributions",
  subtitle = NULL
)
```

## Arguments

- aci_df:

  A `data.frame` returned by [`calculate_aci()`](calculate_aci.md) with
  `area = TRUE`.

- components:

  Character vector of component columns to include.

- include_aci:

  Logical. If `TRUE`, adds the composite `ACI` column to the
  distribution plot alongside the components. Default `FALSE`.

- type:

  `"boxplot"` (default), `"violin"`, or `"density"`.

- title:

  Plot title.

- subtitle:

  Character or `NULL`. Optional subtitle displayed below the title.
  Default `NULL`.

## Value

A `ggplot` object.

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
plot_aci_distribution(aci_df, type = "violin")

plot_aci_distribution(aci_df, type = "density")

```
