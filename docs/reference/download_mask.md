# Download the country mask from the Copernicus CDS

Downloads the ERA5 land-sea mask for the given bounding box, then
rescales it to `[0, 1]` and saves it as `mask_<country_abbrev>.nc` in
`dest_dir`. The mask variable is renamed to `country` so it is directly
compatible with [`apply_mask`](apply_mask.md).

## Usage

``` r
download_mask(
  country_abbrev = "FRA",
  area = c(51.5, -5.5, 41, 10),
  dest_dir = NULL,
  overwrite = FALSE
)
```

## Arguments

- country_abbrev:

  Three-letter ISO 3166-1 alpha-3 country code (e.g. `"FRA"`).

- area:

  Numeric vector `c(north, west, south, east)`. Default: metropolitan
  France.

- dest_dir:

  Character. Directory for the output file. Default `NULL`, which
  resolves to a sub-directory of
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

- overwrite:

  Logical. Overwrite if the file already exists. Default `FALSE`.

## Value

Invisibly, the path to the mask NetCDF file.

## Examples

``` r
if (FALSE) { # \dontrun{
cds_set_key("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

# Produces (by default): <tempdir()>/xaci_era5/FRA/mask_FRA.nc
download_mask(
  country_abbrev = "FRA",
  area      = c(51.5, -5.5, 41.0, 10.0)
)
} # }
```
