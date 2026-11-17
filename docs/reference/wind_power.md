# Calculate daily wind power from u10 and v10 components

Wind speed = sqrt(u10^2 + v10^2), wind power = 0.5 \* rho \* ws^3.

## Usage

``` r
wind_power(u10_dataset, v10_dataset, reference_period = NULL)
```

## Arguments

- u10_dataset:

  List returned by [`load_component()`](load_component.md) for `u10`.

- v10_dataset:

  List returned by [`load_component()`](load_component.md) for `v10`.

- reference_period:

  Optional character vector `c("start","end")`. If provided, only the
  reference sub-period is returned.

## Value

A list with `data` \[lon x lat x days\] (wind power in W/m²) and daily
`time`.

## Details

**Bug fix (see NEWS):** earlier versions averaged `u10` and `v10` to
daily means SEPARATELY first, then took the magnitude of that daily-mean
vector (`sqrt(mean(u)^2 + mean(v)^2)`) as the "daily mean wind speed".
This "vector averaging" systematically UNDER-estimates the true daily
mean wind speed whenever wind direction varies over the day, because
opposing (or merely differing) hourly components partially cancel out
before the magnitude is taken – e.g. a wind blowing at a constant 8 m/s
whose direction oscillates by only +/-60 degrees over the day averages
out to a vector magnitude of ~6 m/s, a ~58% error on the resulting wind
power (0.5\*rho\*w^3), verified numerically. The standard meteorological
convention for a "daily mean wind speed" is instead to average the
INSTANTANEOUS scalar speed `sqrt(u^2+v^2)` computed at each timestep,
which is always positive and does not suffer from this cancellation;
that is what this function now does.
