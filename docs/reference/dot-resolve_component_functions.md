# Resolve which component functions to use based on the compute engine

Isolates the `engine` dispatch logic used by
[`calculate_aci()`](calculate_aci.md) so it can be unit-tested directly
(via `expect_identical()`) without needing to mock or run the full
[`calculate_aci()`](calculate_aci.md) pipeline.

## Usage

``` r
.resolve_component_functions(engine = c("base", "terra"))
```

## Arguments

- engine:

  `"base"` (default, original ncdf4-based loading) or `"terra"`
  (memory-safe, for long/high-resolution historical series – see
  `component_terra.R`). Covers temperature, wind, drought, and
  precipitation; [`sealevel_component()`](sealevel_component.md) is not
  yet available in a terra-based form (its primary input is tide-gauge
  CSV data, not gridded NetCDF, so the memory issue doesn't apply the
  same way – see follow-up note).

## Value

A list with elements `temperature`, `wind`, `drought`, `precipitation`,
each a function with the same signature as its base-R counterpart.
