# Download all ERA5 variables required by the xaci package

Convenience wrapper that calls [`download_era5`](download_era5.md) four
times (one per variable: `t2m`, `tp`, `u10`, `v10`).

## Usage

``` r
download_era5_all(
  years,
  area = c(51.5, -5.5, 41, 10),
  country_abbrev = NULL,
  dest_dir = NULL,
  merge = TRUE,
  overwrite = FALSE
)
```

## Arguments

- years:

  Integer vector of years to download.

- area:

  Bounding box `c(north, west, south, east)`. Default: metropolitan
  France.

- country_abbrev:

  Character. ISO-3 country code (e.g. `"FRA"`). Used to build the
  default output path. One of `country_abbrev` or `dest_dir` must be
  provided.

- dest_dir:

  Character. Output directory. If `NULL` (default), resolves to a
  sub-directory of [`tempdir()`](https://rdrr.io/r/base/tempfile.html).
  Pass your own path for a location that persists across sessions.

- merge:

  Logical. Merge yearly files. Default `TRUE`.

- overwrite:

  Logical. Overwrite existing files. Default `FALSE`.

## Value

Invisibly, a named character vector of output paths (one per variable).

## Examples

``` r
if (FALSE) { # \dontrun{
cds_set_key("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

# Produces (by default): <tempdir()>/xaci_era5/FRA/t2m_1961_1990.nc, tp_..., u10_..., v10_...
download_era5_all(
  years          = 1961:1990,
  area           = c(51.5, -5.5, 41.0, 10.0),
  country_abbrev = "FRA"
)
} # }
```
