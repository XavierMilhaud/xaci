# Load a NetCDF variable and optionally apply a country mask (terra version)

Drop-in, memory-safe replacement for
[`load_component()`](load_component.md) intended for full-resolution,
grid-cell-level ("area = FALSE") workflows over long historical periods.

## Usage

``` r
load_component_terra(
  data_path,
  var_name,
  mask_path = NULL,
  threshold = 0.8,
  chunk_size = 20000
)
```

## Arguments

- data_path:

  Path to the NetCDF file.

- var_name:

  Name of the variable to extract.

- mask_path:

  Path to the mask NetCDF file, or `NULL` (default).

- threshold:

  Numeric threshold for the mask. Default `0.8`.

- chunk_size:

  Passe a [`apply_mask_terra()`](apply_mask_terra.md) (voir sa doc) pour
  le cas ou `nlyr(r)` depasse la limite de 65535 couches.

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html).
