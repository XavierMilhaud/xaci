# Calculate the sea-level component of the ACI

Calculate the sea-level component of the ACI

## Usage

``` r
sealevel_component(
  country_abbrev,
  study_period,
  reference_period,
  mask_path = NULL,
  area = TRUE,
  max_dist_km = 500,
  sealevel_dir = NULL,
  admin_level = NULL,
  admin_assignment = NULL,
  crs_metric = 4326,
  computed_components = FALSE,
  save = FALSE,
  save_dir = NULL,
  load_dir = NULL
)
```

## Arguments

- country_abbrev:

  Three-letter country code (e.g. `"FRA"`).

- study_period:

  Character vector `c("start", "end")`. Required to filter and/or
  download PSMSL tide-gauge data (unlike ERA5-based components which
  read the full NetCDF file and filter in downstream steps).

- reference_period:

  Character vector `c("start", "end")`.

- mask_path:

  Path to the country mask NetCDF. Used to extract the ERA5 grid when
  `area = FALSE`.

- area:

  Logical. Mirrors the `area` argument of all other component functions:
  if `FALSE` (default), interpolates the standardised anomalies onto the
  ERA5 grid and returns a list with a `[lon x lat x t]` array —
  consistent with the grid-cell output of
  [`temperature_component()`](temperature_component.md),
  [`precipitation_component()`](precipitation_component.md), etc. If
  `TRUE`, returns a `data.frame` of aggregated anomalies (national or
  per admin unit, depending on `admin_level`).

- max_dist_km:

  Numeric. Maximum distance (km) for IDW interpolation. Only used when
  `area = FALSE`. Default `500`.

- sealevel_dir:

  Character or `NULL`. Path to the directory containing PSMSL `.txt`
  files. Named `sealevel_dir` for consistency with the `sealevel_dir`
  argument of [`calculate_aci()`](calculate_aci.md). If `NULL` or the
  directory does not exist, data are downloaded automatically.

- admin_level:

  Integer or `NULL`. If not `NULL`, returns one column per admin unit.
  Ignored when `admin_assignment` is supplied.

- admin_assignment:

  Output of [`assign_sealevel_to_admin()`](assign_sealevel_to_admin.md),
  or `NULL`. When supplied, takes precedence over `admin_level`.

- crs_metric:

  EPSG code used when building `admin_assignment` internally. Default
  `4326`.

- computed_components:

  Logical. If `TRUE`, reloads a previously saved `.rds` file from
  `load_dir`. Default `FALSE`.

- save:

  Logical. If `TRUE`, saves the processed result to `save_dir`. Default
  `FALSE`.

- save_dir:

  Character. Directory for saving results. Default `NULL`, which
  resolves to a sub-directory of
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- load_dir:

  Character. Directory from which to reload a cached result when
  `computed_components = TRUE`. Default `NULL`, which resolves to a
  sub-directory of [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

## Value

If `area = FALSE`: a list with a `[lon x lat x t]` array, `lon`, `lat`,
`time` — same structure as other grid-cell components. If `area = TRUE`:
a `data.frame` of station anomalies (national or per admin unit).
