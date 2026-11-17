# Download PSMSL tide-gauge data for a country

Reads the bundled `psmsl_data.csv` to identify stations for the given
country abbreviation, downloads the corresponding data files from the
PSMSL website, and stores them locally.

## Usage

``` r
request_sealevel_data(country_abbrev, dest_dir = NULL)
```

## Arguments

- country_abbrev:

  Three-letter ISO country code (e.g. `"FRA"`).

- dest_dir:

  Destination directory. If `NULL` (default), resolves to a
  sub-directory of [`tempdir()`](https://rdrr.io/r/base/tempfile.html).

## Value

Invisibly, the path to the destination directory.
