# Terra-based Temperature Component

Memory-safe equivalents of
[`calculate_halfday_component()`](calculate_halfday_component.md) and
[`temperature_component()`](temperature_component.md), for
full-resolution, grid-cell-level workflows over long historical periods
(see `R/component_terra.R` for the underlying loading/reduction
primitives).

Only the hourly-scale steps
([`temp_extremum_terra()`](temp_extremum_terra.md),
[`calculate_percentiles_terra()`](calculate_percentiles_terra.md)) run
through terra. Once reduced to daily/monthly resolution, the exact same
[`.crossing_frequency()`](dot-crossing_frequency.md) helper used by the
base-R pipeline takes over – no logic is duplicated.
