# Animate the ACI or a component over time

Produces a GIF animation (via gganimate) showing the evolution of a
spatial variable at each time step. Requires gganimate and gifski (or
png) to be installed.

## Usage

``` r
animate_aci_map(
  data,
  variable = "ACI",
  country_abbrev = NULL,
  admin_level = NULL,
  crs_metric = 4326,
  var_label = "Standardised anomaly",
  title = "ACI over time",
  palette = "RdBu",
  reverse = TRUE,
  n_breaks = 9,
  borders = TRUE,
  fps = 4,
  width = 800,
  height = 600,
  save_path = NULL
)
```

## Arguments

- data:

  Grid-cell list or admin `data.frame`.

- variable:

  Variable to animate. Default `"ACI"`.

- country_abbrev:

  Three-letter ISO code.

- admin_level:

  Integer or `NULL`.

- crs_metric:

  EPSG code, used for admin choropleth projection only. Default `4326`.
  Ignored (with a warning if explicitly set to something else) in raster
  (grid-cell) mode, where the basemap is always reprojected to EPSG:4326
  to match the fixed lon/lat raster grid.

- var_label:

  Colour-bar label. Default `"Standardised anomaly"`.

- title:

  Plot title. Default `"ACI over time"`.

- palette:

  RColorBrewer palette. Default `"RdBu"`.

- reverse:

  Logical. Default `TRUE`.

- n_breaks:

  Integer. Default `9`.

- borders:

  Logical. Default `TRUE`.

- fps:

  Frames per second. Default `4`.

- width:

  Output width in pixels. Default `800`.

- height:

  Output height in pixels. Default `600`.

- save_path:

  File path for the output GIF. If `NULL` (default), the animation is
  rendered to the viewer but not saved.

## Value

A gganimate animation object (invisibly).

## Why `dontrun`

This example is wrapped in `dontrun` because it depends on the optional
gganimate and gifski packages (declared in `Suggests`, not guaranteed to
be installed on every machine running `R CMD check`), and because
rendering a multi-frame GIF takes well over 5 seconds, making it
unsuitable for `donttest` as well.

## Examples

``` r
if (FALSE) { # \dontrun{
lon <- seq(-5, 8, length.out = 10)
lat <- seq(42, 51, length.out = 8)
arr <- array(stats::rnorm(10 * 8 * 5), dim = c(10, 8, 5))
grid <- list(ACI = arr, lon = lon, lat = lat,
            time = seq_len(5))

animate_aci_map(grid, variable = "ACI", country_abbrev = "FRA",
                fps = 2, save_path = file.path(tempdir(), "aci_animation.gif"))
} # }
```
