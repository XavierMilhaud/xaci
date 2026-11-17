# Full sea-level processing pipeline

Full sea-level processing pipeline

## Usage

``` r
sealevel_process(directory, study_period, reference_period)
```

## Arguments

- directory:

  Path to the directory with PSMSL `.txt` files.

- study_period:

  Character vector `c("start", "end")`.

- reference_period:

  Character vector `c("start", "end")`.

## Value

A named list with:

- `data`:

  Standardised `data.frame` of anomalies `[time x stations]`, row names
  `"YYYY-MM-DD"`. Stations listed in `excluded_stations` are entirely
  absent from this data.frame (all-`NA` columns are dropped), not merely
  NA.

- `coords`:

  A `data.frame` with columns `station_id`, `lon`, `lat`, one row per
  station present in `data` (excluded stations are also dropped here).

- `excluded_stations`:

  Character vector of station column names (possibly empty) that have
  ZERO observations in `reference_period` for at least one calendar
  month, and are therefore impossible to standardise against Eq. A.12
  (no reference mean is definable for that station/month) – see the "Bug
  fix" note below. Use this to know which stations (e.g. ones installed
  after 1990) silently contribute nothing, so this isn't found out by
  surprise downstream.

## Bug fix (see NEWS)

Stations with literally no data during `reference_period` (e.g. a tide
gauge installed in 2018, for a 1961-1990 reference period) used to get a
reference mean of `NaN` (not `NA`), which propagated through
[`sealevel_standardize_data()`](sealevel_standardize_data.md) to make
**the entire standardised series of that station `NaN`** – including
during years where the station DOES have perfectly good data. This had
no visible effect only because
[`reduce_sealevel_over_region()`](reduce_sealevel_over_region.md)'s
`rowMeans(..., na.rm = TRUE)` happens to also drop `NaN` (R treats `NaN`
as a kind of `NA`), so the station was silently and permanently excluded
from every national/regional average it could have contributed to, with
no warning. For the French stations listed in Garrido et al.'s Table
B.2, 12 of the 40 stations (30%) have zero overlap with 1961-1990 and
were affected. This function now (a) produces clean `NA` instead of
`NaN` for this case, and more importantly (b) explicitly detects and
reports these stations via
[`warning()`](https://rdrr.io/r/base/warning.html) and the
`excluded_stations` return value, instead of relying on an implicit,
easy-to-miss floating-point coincidence. Excluding such stations is not
itself a choice this function makes – there is no principled reference
baseline to compute for a station that did not exist during the
reference period – but that exclusion is now visible rather than silent.
