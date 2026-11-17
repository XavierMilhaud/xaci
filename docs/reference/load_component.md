# Load a NetCDF dataset and optionally apply a country mask

This function is a constructor. It reads a NetCDF variable and, if
`mask_path` is provided, zeroes out (sets to `NA`) all grid cells where
the mask value is below `threshold`.

## Usage

``` r
load_component(data_path, var_name, mask_path = NULL, threshold = 0.8)
```

## Arguments

- data_path:

  Path to the NetCDF data file.

- var_name:

  Name of the primary variable to load.

- mask_path:

  Path to the mask NetCDF file, or `NULL` (default).

- threshold:

  Numeric threshold for the mask. Default `0.8`.

## Value

A list with elements `data` (3-D array \[lon × lat × time\]), `lon`,
`lat`, `time` (POSIXct vector), and `var_name`.
