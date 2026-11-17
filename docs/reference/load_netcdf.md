# Load a NetCDF variable as a 3D array \[lon x lat x time\]

Load a NetCDF variable as a 3D array \[lon x lat x time\]

## Usage

``` r
load_netcdf(path, var_name)
```

## Arguments

- path:

  Path to the NetCDF file.

- var_name:

  Name of the variable to extract.

## Value

A list with fields: `data` (3D array), `lon`, `lat`, `time`.
