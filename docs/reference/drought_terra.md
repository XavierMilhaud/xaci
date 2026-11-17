# Terra-based Drought Component

Memory-safe equivalents of
[`max_consecutive_dry_days()`](max_consecutive_dry_days.md) and
[`drought_component()`](drought_component.md). Only the hourly-to-daily
reduction runs through terra;
[`drought_interpolate()`](drought_interpolate.md) and the
consecutive-dry-day scan already operate on small,
annual/daily-resolution data, so they are reused as-is via the shared
helper
[`.max_consecutive_dry_days_from_daily()`](dot-max_consecutive_dry_days_from_daily.md)
(defined in `drought.R`) – no logic is duplicated.
