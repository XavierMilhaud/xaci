# Standardise a monthly metric relative to a reference period

For each calendar month, computes the mean and standard deviation over
the reference period, then returns (x - mean) / sd. Optionally averages
over the spatial dimensions first.

## Usage

``` r
standardize_metric(metric, reference_period, area = FALSE)
```

## Arguments

- metric:

  A list with fields `data` \[lon x lat x time\] or \[time\], `time`,
  and optionally `lon`/`lat`.

- reference_period:

  Character vector of length 2: start and end dates (`"YYYY-MM-DD"`).

- area:

  Logical. If `TRUE` the spatial mean is computed before standardising
  and a named numeric vector is returned. Default `FALSE`.

## Value

If `area = TRUE`: a named numeric vector (names = dates). Otherwise: a
list with the same structure as `metric` but standardised values.
