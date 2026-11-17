# Compute temperature percentile thresholds for each day of year (terra version)

Equivalent of [`calculate_percentiles()`](calculate_percentiles.md).

## Usage

``` r
calculate_percentiles_terra(
  r_daily,
  n,
  reference_period,
  window_days = 5L,
  filename = "",
  cores = 1L
)
```

## Arguments

- r_daily:

  A
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
  **daily** resolution (typically the output of
  [`temp_extremum_terra()`](temp_extremum_terra.md)), with
  [`terra::time()`](https://rspatial.github.io/terra/reference/time.html)
  set. **Not** the raw hourly raster.

- n:

  Percentile (e.g. 90 or 10).

- reference_period:

  Character vector `c("YYYY-MM-DD", "YYYY-MM-DD")`.

- window_days:

  Width, in days, of the window used to pool neighbouring calendar days
  before taking the percentile (e.g. 5 pools \\\pm\\2 days around each
  calendar day). Default `5L`, matching the standard CLIMDEX/ACI
  convention.

- filename:

  Optional output path for the final thresholds.

- cores:

  How many spatial tiles to process IN PARALLEL (a ceiling, further
  capped by `.safe_cores_terra()` based on RAM available and a single
  tile's size). Default `1` (sequential – tiles are still used for
  memory safety even at `cores = 1`, just processed one after another
  instead of concurrently; see performance note above). **Note:** the
  NUMBER of tiles is decided independently of `cores`, purely to keep a
  single
  [`terra::roll()`](https://rspatial.github.io/terra/reference/roll.html)
  call's memory footprint bounded (see
  `.calculate_percentiles_terra_tiled()`) – `cores` only controls how
  many of those (already memory-safe) tiles run at once.

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with 366 layers (day-of-year 1-366).

## Details

**Bug fix \#1 (see NEWS):** this function used to take the raw
**hourly** raster, filter it to day/night hours, and compute the
rolling-window percentile directly on those hourly values (with
`window_size` expressed in hours: 80 for day, 40 for night) – while
[`calculate_halfday_component_terra()`](calculate_halfday_component_terra.md)
compared the resulting threshold to a **daily** max/min. Because the
daily extremum of ~16 (or 8) hourly values almost always crosses the
90th (or 10th) percentile of the hourly distribution, this made the
exceedance frequency during the reference period itself massively higher
than the intended ~10 already-reduced **daily** extremum raster (the
output of [`temp_extremum_terra()`](temp_extremum_terra.md)) as input,
with `window_days` expressed in **days** (default 5), so the threshold
and the tested variable are computed from the same daily quantity.

**Bug fix \#2 (see NEWS):** even after bug fix \#1, the threshold was
still computed as a rolling-window `n`-th percentile
([`terra::roll()`](https://rspatial.github.io/terra/reference/roll.html))
followed by a **second** `n`-th percentile of those already-extreme
rolled values, grouped by day-of-year
([`terra::tapp()`](https://rspatial.github.io/terra/reference/tapp.html)).
This two-stage procedure systematically under-counts the true exceedance
frequency (observed ~5.6 applied) – see
[`calculate_percentiles()`](calculate_percentiles.md) (base-R engine)
for the full explanation and numeric illustration, which applies
identically here. `calculate_percentiles_terra()` now instead pools the
RAW daily values within `window_days` of a given calendar day, across
ALL years of the reference period, and takes a SINGLE
[`terra::app()`](https://rspatial.github.io/terra/reference/app.html)
quantile of that pooled sample per cell, per calendar day – the standard
CLIMDEX/ACI methodology. This also removes the
[`terra::roll()`](https://rspatial.github.io/terra/reference/roll.html)/[`terra::tapp()`](https://rspatial.github.io/terra/reference/tapp.html)
dependency for this function entirely (simpler, and no longer sensitive
to
[`terra::roll()`](https://rspatial.github.io/terra/reference/roll.html)'s
argument names changing across terra versions).

**Performance AND memory note:** looping over 366 calendar days and
calling
[`terra::app()`](https://rspatial.github.io/terra/reference/app.html) on
a (typically ~100-200 layer) subset of the reference period for each is
comparable in total workload to the single whole-series
[`terra::roll()`](https://rspatial.github.io/terra/reference/roll.html)
call used before bug fix \#2, but touches far fewer layers per call. A
single such call over a whole country's grid could still, in principle,
use substantial memory for a large grid – as a precaution the raster is
still ALWAYS split into small spatial tiles sized to a conservative,
fixed memory target (see `target_tile_gb` in
`.calculate_percentiles_terra_tiled()`), regardless of `cores` – `cores`
only controls how many of those already-memory-safe tiles run
concurrently (default `1`: one at a time).
