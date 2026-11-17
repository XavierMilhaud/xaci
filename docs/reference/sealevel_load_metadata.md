# Load PSMSL station coordinates from the global station list CSV

Reads the PSMSL station metadata CSV (columns: Station Name, ID, Lat.,
Lon., GLOSS ID, Country, Date, Coastline, Station) and returns
coordinates for the requested stations.

## Usage

``` r
sealevel_load_metadata(meta_path = NULL, station_ids = NULL)
```

## Arguments

- meta_path:

  Optional explicit path to the CSV file. If `NULL` (default), the
  metadata CSV bundled with the package (`inst/extdata/psmsl_data.csv`,
  located via
  [`system.file()`](https://rdrr.io/r/base/system.file.html)) is used.

- station_ids:

  Integer vector of PSMSL station IDs to keep. If `NULL` (default), all
  stations in the CSV are returned.

## Value

A `data.frame` with columns `station_id` (character, e.g.
`"Measurement_1"`), `lon`, `lat`.
