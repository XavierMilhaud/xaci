# Correct PSMSL float date format to Date objects

PSMSL encodes dates as `YYYY.fraction` where the fraction identifies the
month.

## Usage

``` r
sealevel_correct_date_format(df)
```

## Arguments

- df:

  `data.frame` with numeric row names (PSMSL date format).

## Value

The same `data.frame` with `Date` row names (`"YYYY-MM-01"`), rows with
unrecognised dates removed.
