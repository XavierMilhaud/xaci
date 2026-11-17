# Resolve a caching/output directory, defaulting to a session tempdir()

CRAN policy forbids writing to the user's home filespace (including the
package directory and [`getwd()`](https://rdrr.io/r/base/getwd.html)) by
default. All `save_dir`, `load_dir`, `sealevel_dir` and `dest_dir`
arguments across the package therefore default to `NULL`, and are
resolved here to a sub-directory of
[`tempdir()`](https://rdrr.io/r/base/tempfile.html) when the user does
not supply an explicit path. Users who want persistent caching across
sessions should pass their own directory explicitly.

## Usage

``` r
.resolve_cache_dir(dir, subdir = NULL)
```

## Arguments

- dir:

  Character or `NULL`. User-supplied directory.

- subdir:

  Character or `NULL`. Sub-path appended to
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) when `dir` is
  `NULL`, to namespace the fallback location (e.g. by
  country/component).

## Value

Character. The resolved directory path.
