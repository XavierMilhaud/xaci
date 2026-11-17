# Terra-based Precipitation Component

Memory-safe equivalents of
[`calculate_maximum_precipitation_over_window()`](calculate_maximum_precipitation_over_window.md)
and [`precipitation_component()`](precipitation_component.md). Only the
hourly-to-daily reduction runs through terra; the rolling-sum and
monthly-max logic already operates on small, daily-resolution data,
reused as-is via the shared helper
[`.max_precipitation_from_daily()`](dot-max_precipitation_from_daily.md)
(defined in `precipitation.R`) – no logic is duplicated.
