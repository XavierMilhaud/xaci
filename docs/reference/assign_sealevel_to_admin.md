# Assign PSMSL tide-gauge stations to administrative units and compute coastal factors

Performs a spatial join between PSMSL station coordinates and
administrative unit polygons, and computes for each unit the fraction of
its perimeter that is coastline. The coastline layer is cropped to the
country bounding box before intersection to speed up computation. All
geometries are projected to `crs_metric` before length calculations to
ensure results are in metres.

## Usage

``` r
assign_sealevel_to_admin(country_abbrev, admin_level = 1, crs_metric = 4326)
```

## Arguments

- country_abbrev:

  ISO-3 country code (e.g. `"FRA"`).

- admin_level:

  Integer \>= 0. Administrative level fetched via GADM (see
  [`.load_admin_sf()`](dot-load_admin_sf.md) in `utils.R`): `0` for the
  national boundary, `1` for regions, `2` for departments, etc. Default
  `1`.

- crs_metric:

  Integer. EPSG code of a metric CRS appropriate for the country, used
  for accurate length calculations. Default `4326` (WGS84, not
  recommended for production — prefer a local CRS such as `2154` for
  France or `27700` for the UK).

## Value

A list with two elements:

- `station_ids`:

  Named list: keys are administrative unit names, values are integer
  vectors of PSMSL station IDs within that unit.

- `factors`:

  Named numeric vector: keys are administrative unit names, values are
  the coastal fraction (coastline length / total perimeter) in `[0, 1]`.
  Zero for landlocked units.
