# Lazily load a NetCDF variable as a terra SpatRaster

Equivalent of [`load_netcdf()`](load_netcdf.md), but never materialises
pixel values in RAM: only metadata (dimensions, CRS, time) is read up
front.

## Usage

``` r
load_netcdf_terra(path, var_name)
```

## Arguments

- path:

  Path to the NetCDF file.

- var_name:

  Name of the variable to extract.

## Value

A
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
with one layer per time step and
[`terra::time()`](https://rspatial.github.io/terra/reference/time.html)
attached.
