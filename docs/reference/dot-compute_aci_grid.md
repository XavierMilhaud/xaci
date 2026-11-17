# Compute the ACI at grid-cell level

All component inputs must be lists with a `data` array `[nl x nw x nt]`
and matching `lon`, `lat`, `time`.

## Usage

``` r
.compute_aci_grid(
  comp_t_high,
  comp_t_low,
  comp_prec,
  comp_drought,
  comp_wind,
  comp_sl = NULL,
  col_high = "t90",
  col_low = "t10"
)
```

## Arguments

- comp_t_high:

  Temperature high component (list with `data`).

- comp_t_low:

  Temperature low component.

- comp_prec:

  Precipitation component.

- comp_drought:

  Drought component.

- comp_wind:

  Wind component.

- comp_sl:

  Sea-level component (list with `data`), or `NULL` if unavailable.

- col_high:

  Column name for temperature high (e.g. `"t90"`).

- col_low:

  Column name for temperature low (e.g. `"t10"`).

## Value

A list with arrays `[nl x nw x nt]` for each component and `ACI`, plus
`lon`, `lat`, `time`.
