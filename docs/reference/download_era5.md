# Download an ERA5 variable from the Copernicus CDS

Downloads hourly ERA5 single-level data for one variable and one or
several years, for a given geographical bounding box. Files are
downloaded year by year and then optionally merged into a single NetCDF
using the ncdf4 package.

## Usage

``` r
download_era5(
  variable = c("t2m", "tp", "u10", "v10"),
  years,
  area = c(51.5, -5.5, 41, 10),
  country_abbrev = NULL,
  dest_dir = NULL,
  merge = TRUE,
  overwrite = FALSE
)
```

## Arguments

- variable:

  Character. Short name of the ERA5 variable: one of `"t2m"`, `"tp"`,
  `"u10"`, `"v10"`.

- years:

  Integer vector. Years to download (e.g. `1961:1990`).

- area:

  Numeric vector of length 4: `c(north, west, south, east)` in decimal
  degrees. Default is metropolitan France: `c(51.5, -5.5, 41.0, 10.0)`.

- country_abbrev:

  Character. Three-letter ISO 3166-1 alpha-3 country code (e.g.
  `"FRA"`). Used to build the default output path under
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) when `dest_dir` is
  not supplied. Ignored when `dest_dir` is supplied explicitly. One of
  `country_abbrev` or `dest_dir` must be provided.

- dest_dir:

  Character. Directory where files will be saved. If `NULL` (default),
  resolves to a sub-directory of
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) built from
  `country_abbrev`. Created if it does not exist. Pass your own path for
  a location that persists across sessions.

- merge:

  Logical. If `TRUE` (default) and more than one year is requested, the
  individual yearly files are merged into a single file named
  `<variable>_<first_year>_<last_year>.nc` using
  [`nc_open`](https://rdrr.io/pkg/ncdf4/man/nc_open.html) /
  [`ncvar_get`](https://rdrr.io/pkg/ncdf4/man/ncvar_get.html) and a
  time-concatenation loop (no external tool required).

- overwrite:

  Logical. If `FALSE` (default) skip years whose output file already
  exists.

## Value

Invisibly, the path to the final NetCDF file (merged if `merge = TRUE`,
otherwise the directory containing yearly files).

## Details

The CDS Personal Access Token must be stored beforehand with
[`cds_set_key`](cds_set_key.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Store your token once
cds_set_key("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

# Automatic path: <tempdir()>/xaci_era5/FRA/tp_1961_1990.nc
download_era5(
  variable       = "tp",
  years          = 1961:1990,
  area           = c(51.5, -5.5, 41.0, 10.0),
  country_abbrev = "FRA"
)

# Or explicit path:
download_era5(
  variable = "tp",
  years    = 1961:1990,
  area     = c(51.5, -5.5, 41.0, 10.0),
  dest_dir = "my/custom/path"
)
} # }
```
