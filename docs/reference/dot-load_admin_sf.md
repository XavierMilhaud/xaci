# Load administrative boundary polygons for a country, at a given level

Replaces the former
[`rnaturalearth::ne_states()`](https://docs.ropensci.org/rnaturalearth/reference/ne_states.html)-based
approach, which only ever exposed a single fixed administrative level
per country (so `admin_level` had no real effect downstream, regardless
of the value supplied). Uses GADM (via
[`geodata::gadm()`](https://rspatial.github.io/geodata/reference/gadm.html)),
which provides a genuine multi-level administrative hierarchy for
virtually every country (level 0 = national boundary, level 1 =
states/regions, level 2 = departments/counties, etc., depending on how
finely each country is subdivided in GADM).

## Usage

``` r
.load_admin_sf(country_abbrev, admin_level = 1, cache_dir = .gadm_cache_dir())
```

## Arguments

- country_abbrev:

  ISO-3 country code (e.g. `"FRA"`).

- admin_level:

  Integer \>= 0. `0` = country boundary, `1` = first administrative
  subdivision, etc. Must not exceed what GADM provides for that country
  (GADM returns an error/empty result otherwise).

- cache_dir:

  Directory used to cache the downloaded GADM data. Default: a
  persistent per-user cache directory.

## Value

An `sf` object with columns `name` (administrative unit name at the
requested level) and `geometry`, in EPSG:4326.

## Details

GADM keys countries directly by their ISO-3 code, so no name-lookup
table is needed (unlike
[`rnaturalearth::ne_states()`](https://docs.ropensci.org/rnaturalearth/reference/ne_states.html),
which expected English country names and required maintaining
`.iso3_to_ne_name`).

Downloads are cached on disk (`cache_dir`, by default a persistent
per-user cache via
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)), so
repeated calls for the same country/level across
[`build_admin_mask()`](build_admin_mask.md),
[`assign_sealevel_to_admin()`](assign_sealevel_to_admin.md), and the
plotting helpers do not re-download the data.
