# Compute temperature percentile thresholds for each day of year

Uses a rolling window (in **days**) over the reference period, followed
by a group-by-day-of-year percentile. Matches the CLIMDEX / ACI
methodology (see `ACI (2018)`, Appendix A of Garrido et al.): the
threshold for a given calendar day is the `n`-th percentile of the
**daily** TX/TN values falling within a window of `window_days` days
(default 5, i.e. \\\pm\\2 days) around that calendar day, pooled across
all years of the reference period.

## Usage

``` r
calculate_percentiles(daily_dataset, n, reference_period, window_days = 5L)
```

## Arguments

- daily_dataset:

  List with `data` \[lon x lat x days\] and daily `time`, as returned by
  [`temp_extremum()`](temp_extremum.md) – i.e. the daily TX (for T90) or
  TN (for T10) series, **not** the raw hourly dataset.

- n:

  Percentile (e.g. 90 or 10).

- reference_period:

  Character vector `c("YYYY-MM-DD", "YYYY-MM-DD")`.

- window_days:

  Width, in days, of the window used to pool neighbouring calendar days
  before taking the percentile (e.g. 5 pools \\\pm\\2 days around each
  calendar day). Default `5L`, matching the standard CLIMDEX/ACI
  convention.

## Value

A numeric array `[lon x lat x 366]` (day-of-year 1-366).

## Details

**Bug fix \#1 (see NEWS):** earlier versions of this function computed
the rolling window/percentile on **hourly** `t2m` values (with
`window_size` expressed in hours: 80 for day, 40 for night), then
compared the resulting threshold to a **daily** extremum in
[`.crossing_frequency()`](dot-crossing_frequency.md). Since the daily
max (or min) of ~16 (or 8) hourly values almost always exceeds (or falls
below) the 90th (or 10th) percentile of the marginal hourly
distribution, this made the exceedance frequency during the reference
period itself massively higher than the intended ~10 the reference
frequency approximately by construction. `calculate_percentiles()` now
takes the already-reduced **daily** extremum series (the output of
[`temp_extremum()`](temp_extremum.md)) as input, so the threshold and
the tested variable are computed from the same daily quantity.

**Bug fix \#2 (see NEWS):** even after bug fix \#1, the threshold was
still computed as a **percentile of rolling percentiles**: a
`window_days`-wide rolling `n`-th percentile was first applied along the
daily series, and a **second** `n`-th percentile was then taken of those
already-extreme rolled values, grouped by day-of-year across reference
years. This two-stage procedure is systematically biased: taking the
`n`-th percentile of a set of local `n`-th percentiles pushes the final
threshold further into the tail than a single, direct percentile would,
so it under-counts the true exceedance frequency (observed ~5.6 even
with bug fix \#1 alone applied). The standard CLIMDEX/ACI methodology
instead **pools the raw daily values** that fall within `window_days` of
a given calendar day, across **all** years of the reference period, into
a single sample, and takes **one** percentile of that pooled sample.
`calculate_percentiles()` now follows this pooling approach (no more
rolling window / `zoo` dependency for this function), which empirically
recovers the intended ~10
