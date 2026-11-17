# Build standard ERA5 file paths from country and years

Internal helper that reconstructs the paths produced by
[`download_era5_all()`](download_era5_all.md) and
[`download_mask()`](download_mask.md).

## Usage

``` r
.build_era5_paths(country_abbrev, years, base_dir = NULL)
```

## Arguments

- country_abbrev:

  ISO-3 country code (e.g. `"FRA"`).

- years:

  Integer vector of years (e.g. `2011:2015`).

- base_dir:

  Root data directory. If `NULL` (default), matches the
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html)-based fallback
  used by [`download_era5`](download_era5.md) and
  [`download_mask`](download_mask.md) when they are also called without
  an explicit `dest_dir`.

## Value

A named list with elements `t2m`, `tp`, `u10`, `v10`, and `mask`.
