# Apply a country mask to a NetCDF data array

Sets grid cells to NA where the mask value is below `threshold`.

## Usage

``` r
apply_mask(dataset, mask_path, var_name, threshold = 0.8)
```

## Arguments

- dataset:

  List returned by [`load_netcdf()`](load_netcdf.md).

- mask_path:

  Path to the mask NetCDF file (variable: `country`).

- var_name:

  Name of the variable inside `dataset`.

- threshold:

  Numeric threshold; cells with mask \< threshold become NA. Default
  0.8.

## Value

The dataset list with `data` masked in-place.
