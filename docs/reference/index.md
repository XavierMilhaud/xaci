# Package index

## All functions

- [`aci`](aci.md) : Actuarial Climate Index (ACI)
- [`aggregate_granularity()`](aggregate_granularity.md) : Aggregate a
  monthly data frame to a given temporal granularity
- [`animate_aci_map()`](animate_aci_map.md) : Animate the ACI or a
  component over time
- [`apply_mask()`](apply_mask.md) : Apply a country mask to a NetCDF
  data array
- [`apply_mask_terra()`](apply_mask_terra.md) : Apply a country mask to
  a SpatRaster
- [`assign_sealevel_to_admin()`](assign_sealevel_to_admin.md) : Assign
  PSMSL tide-gauge stations to administrative units and compute coastal
  factors
- [`build_admin_mask()`](build_admin_mask.md) : Build a spatial weight
  matrix mapping ERA5 grid cells to administrative units
- [`calculate_aci()`](calculate_aci.md) : Calculate the Actuarial
  Climate Index
- [`calculate_halfday_component()`](calculate_halfday_component.md) :
  Calculate the half-day (day or night) temperature component
- [`calculate_halfday_component_terra()`](calculate_halfday_component_terra.md)
  : Calculate the half-day (day or night) temperature component (terra
  version)
- [`calculate_maximum_precipitation_over_window()`](calculate_maximum_precipitation_over_window.md)
  : Calculate maximum precipitation over a rolling window
- [`calculate_maximum_precipitation_over_window_terra()`](calculate_maximum_precipitation_over_window_terra.md)
  : Calculate maximum precipitation over a rolling window (terra
  version)
- [`calculate_percentiles()`](calculate_percentiles.md) : Compute
  temperature percentile thresholds for each day of year
- [`calculate_percentiles_terra()`](calculate_percentiles_terra.md) :
  Compute temperature percentile thresholds for each day of year (terra
  version)
- [`calculate_period_wind_exceedance_frequency()`](calculate_period_wind_exceedance_frequency.md)
  : Calculate monthly wind exceedance frequency
- [`calculate_period_wind_exceedance_frequency_terra()`](calculate_period_wind_exceedance_frequency_terra.md)
  : Calculate monthly wind exceedance frequency (terra version)
- [`calculate_rolling_sum()`](calculate_rolling_sum.md) : Calculate the
  rolling sum of a climate variable
- [`cds_set_key()`](cds_set_key.md) : Set your CDS Personal Access Token
- [`component`](component.md) : Base Climate Component Helpers
- [`component_terra`](component_terra.md) : Terra-based Loading and
  Hourly-to-Daily Reduction
- [`days_above_wind_thresholds()`](days_above_wind_thresholds.md) :
  Calculate days with wind power above the 90th-percentile threshold
- [`.spatraster_to_list()`](dot-spatraster_to_list.md) : Convert a
  (small) SpatRaster back to the package's plain-list format
- [`download`](download.md) : Download ERA5 and Country Mask Data
- [`download_era5()`](download_era5.md) : Download an ERA5 variable from
  the Copernicus CDS
- [`download_era5_all()`](download_era5_all.md) : Download all ERA5
  variables required by the xaci package
- [`download_mask()`](download_mask.md) : Download the country mask from
  the Copernicus CDS
- [`drought`](drought.md) : Drought Component of the ACI
- [`drought_component()`](drought_component.md) : Calculate the drought
  component of the ACI
- [`drought_component_terra()`](drought_component_terra.md) : Calculate
  the drought component of the ACI (terra version)
- [`drought_interpolate()`](drought_interpolate.md) : Linearly
  interpolate annual CDD to monthly resolution
- [`drought_terra`](drought_terra.md) : Terra-based Drought Component
- [`load_component()`](load_component.md) : Load a NetCDF dataset and
  optionally apply a country mask
- [`load_component_terra()`](load_component_terra.md) : Load a NetCDF
  variable and optionally apply a country mask (terra version)
- [`load_netcdf_terra()`](load_netcdf_terra.md) : Lazily load a NetCDF
  variable as a terra SpatRaster
- [`load_psmsl_data()`](load_psmsl_data.md) : Load the bundled PSMSL
  station metadata
- [`max_consecutive_dry_days()`](max_consecutive_dry_days.md) :
  Calculate maximum consecutive dry days (CDD) per year
- [`max_consecutive_dry_days_terra()`](max_consecutive_dry_days_terra.md)
  : Calculate maximum consecutive dry days (terra version)
- [`plot_aci_components()`](plot_aci_components.md) : Plot the ACI
  component decomposition
- [`plot_aci_dashboard()`](plot_aci_dashboard.md) : Plot a summary
  dashboard of the ACI results
- [`plot_aci_distribution()`](plot_aci_distribution.md) : Plot the
  distribution of ACI components
- [`plot_aci_map()`](plot_aci_map.md) : Plot a spatial map of the ACI or
  one of its components
- [`plot_aci_map_mean()`](plot_aci_map_mean.md) : Plot the temporal mean
  of an ACI variable over a sub-period
- [`plot_aci_timeseries()`](plot_aci_timeseries.md) : Plot the ACI
  global time series
- [`precipitation`](precipitation.md) : Precipitation Component of the
  ACI
- [`precipitation_component()`](precipitation_component.md) : Calculate
  the precipitation component of the ACI
- [`precipitation_component_terra()`](precipitation_component_terra.md)
  : Calculate the precipitation component of the ACI (terra version)
- [`precipitation_terra`](precipitation_terra.md) : Terra-based
  Precipitation Component
- [`reduce_dataarray_to_dataframe()`](reduce_dataarray_to_dataframe.md)
  : Reduce a standardised spatial metric to a data frame
- [`request_sealevel_data()`](request_sealevel_data.md) : Download PSMSL
  tide-gauge data for a country
- [`resample_daily_terra()`](resample_daily_terra.md) : Resample a
  SpatRaster to daily resolution (terra version)
- [`sealevel`](sealevel.md) : Sea Level Component of the ACI
- [`sealevel_clean_data()`](sealevel_clean_data.md) : Replace PSMSL
  sentinel values (-99999) with NA
- [`sealevel_component()`](sealevel_component.md) : Calculate the
  sea-level component of the ACI
- [`sealevel_compute_monthly_stats()`](sealevel_compute_monthly_stats.md)
  : Compute monthly reference statistics for sea-level data
- [`sealevel_correct_date_format()`](sealevel_correct_date_format.md) :
  Correct PSMSL float date format to Date objects
- [`sealevel_load_data()`](sealevel_load_data.md) : Load sea-level txt
  files from a directory
- [`sealevel_process()`](sealevel_process.md) : Full sea-level
  processing pipeline
- [`sealevel_standardize_data()`](sealevel_standardize_data.md) :
  Standardise sea-level data over the study period
- [`standardize_metric()`](standardize_metric.md) : Standardise a
  monthly metric relative to a reference period
- [`temp_extremum()`](temp_extremum.md) : Compute daily temperature
  extremum (min or max) for day or night hours
- [`temp_extremum_terra()`](temp_extremum_terra.md) : Compute daily
  temperature extremum (min or max) for day or night hours (terra
  version)
- [`temperature`](temperature.md) : Temperature Component of the ACI
- [`temperature_component()`](temperature_component.md) : Calculate the
  full temperature component of the ACI
- [`temperature_component_terra()`](temperature_component_terra.md) :
  Compute the temperature ACI component (terra version)
- [`temperature_terra`](temperature_terra.md) : Terra-based Temperature
  Component
- [`utils`](utils.md) : Utility Functions for ACI Package
- [`visualization`](visualization.md) : Visualization Functions for the
  ACI Package
- [`wind`](wind.md) : Wind Component of the ACI
- [`wind_component()`](wind_component.md) : Calculate the wind component
  of the ACI
- [`wind_component_terra()`](wind_component_terra.md) : Calculate the
  wind component of the ACI (terra version)
- [`wind_power()`](wind_power.md) : Calculate daily wind power from u10
  and v10 components
- [`wind_power_terra()`](wind_power_terra.md) : Calculate daily wind
  power from u10 and v10 components (terra version)
- [`wind_terra`](wind_terra.md) : Terra-based Wind Component
- [`wind_thresholds()`](wind_thresholds.md) : Calculate wind power
  thresholds (90th percentile per day-of-year)
