# Convert an ISO-3 country code to its English name for rnaturalearth

Internal helper, kept for backward compatibility. As of the GADM-based
[`.load_admin_sf()`](dot-load_admin_sf.md), administrative polygons are
no longer fetched via
[`rnaturalearth::ne_states()`](https://docs.ropensci.org/rnaturalearth/reference/ne_states.html)
(which only exposes ONE fixed administrative level per country, ignoring
`admin_level`), so this lookup is no longer used internally for that
purpose. `ne_coastline()` (a separate, level-independent worldwide
coastline layer used in
[`assign_sealevel_to_admin()`](assign_sealevel_to_admin.md)) does not
need it either.

## Usage

``` r
.iso3_to_country_name(iso3)
```

## Arguments

- iso3:

  A single ISO-3 character string (case-insensitive).

## Value

The corresponding English country name.
