# Terra-based Wind Component

Memory-safe equivalents of [`wind_power()`](wind_power.md) and
[`wind_component()`](wind_component.md). Only
[`wind_power_terra()`](wind_power_terra.md) touches raw hourly data (via
terra); [`wind_thresholds()`](wind_thresholds.md)'s internal logic,
threshold-crossing, and monthly aggregation already operate on DAILY
resolution data (small even for 40+ years), so they are reused as-is via
the shared helpers
[`.wind_thresholds_from_wp()`](dot-wind_thresholds_from_wp.md),
[`.days_above_from_wp()`](dot-days_above_from_wp.md),
[`.monthly_frequency_from_binary()`](dot-monthly_frequency_from_binary.md)
defined in `wind.R` – no logic is duplicated.
