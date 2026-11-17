# Build a spatial weight matrix mapping ERA5 grid cells to administrative units

For each ERA5 grid cell, computes the fraction of its surface area that
falls within each administrative unit polygon (using `sf` geometry
intersection). All geometries are projected to `crs_metric` before area
calculations to ensure results are in metres squared. The result can be
computationally expensive for fine grids and large countries — consider
caching it with [`saveRDS()`](https://rdrr.io/r/base/readRDS.html).

## Usage

``` r
build_admin_mask(
  lon,
  lat,
  country_abbrev,
  admin_level = 1,
  resolution = 0.25,
  crs_metric = 4326,
  cache_dir = .gadm_cache_dir()
)
```

## Arguments

- lon:

  Numeric vector of ERA5 longitudes.

- lat:

  Numeric vector of ERA5 latitudes.

- country_abbrev:

  ISO-3 country code (e.g. `"FRA"`).

- admin_level:

  Integer \>= 0. Administrative level fetched via GADM: `0` for the
  national boundary, `1` for regions, `2` for departments, etc.
  (availability depends on how finely GADM subdivides the given
  country). Default `1`.

- resolution:

  Numeric. Half-width of ERA5 grid cells in degrees. Default `0.25`.

- crs_metric:

  Integer. EPSG code of a metric CRS appropriate for the country, used
  for accurate area calculations. Default `4326` (WGS84, not recommended
  for production — prefer a local CRS such as `2154` for France or
  `27700` for the UK).

- cache_dir:

  Directory used to cache the downloaded GADM administrative boundaries.
  Default: a persistent per-user cache directory (see
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)).

## Value

A list with elements:

- `weights`:

  A named list of length `length(lon) * length(lat)`. Each element is a
  named numeric vector of (unit -\> fractional area weight) pairs.

- `units`:

  Character vector of all administrative unit names.

- `lon`:

  Input longitudes.

- `lat`:

  Input latitudes.
