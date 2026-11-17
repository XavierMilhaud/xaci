# Linearly interpolate annual CDD to monthly resolution

For month \\j \in 1..11\\ of year \\k\\, the monthly CDD interpolates
between the ANNUAL values of year \\k-1\\ and year \\k\\ (each annual
value being anchored at December 31 of its own year): \$\$CDD_j(k) =
\frac{12-j}{12} \cdot CDD\_{k-1} + \frac{j}{12} \cdot CDD_k\$\$ and
\\CDD\_{12}(k) = CDD_k\\ exactly (December of year \\k\\ coincides with
that year's own annual anchor point). This matches Eq. (A.6) of Garrido
et al. The first year, having no \\k-1\\ to interpolate from, repeats
its own annual value for all 12 months.

## Usage

``` r
drought_interpolate(cdd_annual)
```

## Arguments

- cdd_annual:

  List returned by
  [`max_consecutive_dry_days()`](max_consecutive_dry_days.md).

## Value

A list with `data` \[lon × lat × months\] and monthly `time`.

## Details

**Bug fix (see NEWS):** earlier versions of this function computed this
same interpolation formula correctly, but assigned the result to the
WRONG calendar year – labelling month \\j\\ of year
\\\text{years}\[k\]\\ as `w1 * data[,,k] + w2 * data[,,k+1]`, which is
actually month \\j\\ of year \\\text{years}\[k+1\]\\ per Eq. (A.6)
(whose December value must equal that year's OWN annual value, not the
following year's). Verified numerically: with annual CDD values
10/20/30/40 for 2000-2003, December 2000 came out as 20 (year 2001's
annual value) instead of 10 (year 2000's own value). The interpolation
arithmetic itself was correct; only the year label was off by one. As a
consequence, the "repeat own value for all 12 months" edge-case handling
now applies to the FIRST year (which has no preceding year to
interpolate from) instead of the last (which, under the corrected
labelling, has no such issue: Eq. A.6 only ever needs year \\k-1\\ to
compute year \\k\\'s months, so the last year is computed normally).
