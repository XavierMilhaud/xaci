# Terra-based Loading and Hourly-to-Daily Reduction

Memory-safe alternatives to
[`load_netcdf()`](load_netcdf.md)/[`apply_mask()`](apply_mask.md) and to
the hourly-resolution steps of the temperature, precipitation and wind
pipelines ([`temp_extremum()`](temp_extremum.md),
[`calculate_percentiles()`](calculate_percentiles.md), the daily
resampling inside [`wind_power()`](wind_power.md)).

**Why this file exists:** for a whole country at hourly resolution over
40+ years,
[`ncdf4::ncvar_get()`](https://rdrr.io/pkg/ncdf4/man/ncvar_get.html)
materialises the full `[lon x lat x time]` array in RAM in one shot
(tens of GB). `terra` reads NetCDF lazily (via GDAL) and processes
computations block-by-block, writing results to disk instead of
accumulating them in memory.

Only the hourly-scale steps are ported here. Once data has been reduced
to DAILY resolution (via
[`resample_daily_terra()`](resample_daily_terra.md),
[`temp_extremum_terra()`](temp_extremum_terra.md), ...), the resulting
object is small enough (~40 years x 365 days) to convert back to a plain
array with [`.spatraster_to_list()`](dot-spatraster_to_list.md) and hand
off, unchanged, to the rest of the existing (already tested) pipeline –
[`resample_monthly()`](resample_monthly.md),
[`standardize_metric()`](standardize_metric.md),
[`.compute_aci_grid()`](dot-compute_aci_grid.md), etc.
