# xaci — Actuarial Climate Index (R package)

Implements the computation of the **Actuarial Climate Index (ACI)** and its six
components from gridded climate data (NetCDF ERA5) and tide gauge stations (PSMSL).

> Reference: Garrido, Milhaud & Olympio (2026). *The definition of a French
> actuarial climate index; one more step towards a European index.* ⟨hal-04491982⟩

---

## Installation

```r
# From local sources
install.packages(".", repos = NULL, type = "source")

# Or with devtools
# install.packages("devtools")
devtools::install(".")
```

### Dependencies

| Package          | Role                                          |
|------------------|-----------------------------------------------|
| `ncdf4`          | Read NetCDF files                             |
| `dplyr`          | Manipulate tables                             |
| `zoo`            | Sliding sums / means                          |
| `readr`          | Read the PSMSL CSV                            |
| `ggplot2`        | Visualisation                                 |
| `tidyr`          | Data reshaping                                |
| `sf`             | Spatial operations                            |
| `rnaturalearth`  | Administrative boundaries                     |

Suggested (not required for core use):

| Package             | Role                                       |
|---------------------|--------------------------------------------|
| `ecmwfr`            | Download ERA5 data from Copernicus CDS     |
| `rnaturalearthdata` | High-resolution natural earth data         |
| `patchwork`         | Combine ggplot2 panels                     |
| `testthat`          | Unit tests                                 |

---

## Required data

Climate data are extracted from the
[Copernicus Climate Data Store (ERA5)](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels).  
Tide gauge data are automatically downloaded from [PSMSL](https://www.psmsl.org/data/obtaining/).

Necessary NetCDF variables:

| Variable  | Description                          |
|-----------|--------------------------------------|
| `t2m`     | 2-m air temperature (hourly)         |
| `tp`      | Total precipitation (hourly)         |
| `u10`     | U-component of 10-m wind (hourly)    |
| `v10`     | V-component of 10-m wind (hourly)    |
| `country` | Country land-sea mask (values in [0,1]) |

> **Note:** The examples below require NetCDF files downloaded from the
> Copernicus CDS. Replace the paths with the actual locations of your files
> before running them.

---

## Download ERA5 data and country mask

The package includes functions to download directly from the
[Copernicus CDS](https://cds.climate.copernicus.eu) via the
[`ecmwfr`](https://bluegreen-labs.github.io/ecmwfr/) package (v2.0+,
Personal Access Token required).

### 1 · Store your CDS token (once per machine)

```r
install.packages("ecmwfr")
library(xaci)

# Your token: https://cds.climate.copernicus.eu → profile → Personal Access Token
cds_set_key("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
```

### 2 · Download ERA5 variables

This step can take hours/days depending on the number of years and variables requested.

```r
# Download t2m (temperature), tp (precipitation), u10 and v10 (wind) for France, 1960–2024
# One NetCDF per variable is produced in data/era5/
download_era5_all(
  years    = 1960:2024,
  area     = c(51.5, -5.5, 41.0, 10.0),   # N, W, S, E
  dest_dir = "data/era5/FRA/1960_2024"
)

# Or download a single variable:
download_era5(
  variable = "v10",
  years    = 1970:1980,
  area     = c(51.5, -5.5, 41.0, 10.0), # area for France
  dest_dir = "data/era5/FRA"
)
```

### 3 · Download the country mask

```r
download_mask(
  country_abbrev = "FRA",
  area      = c(51.5, -5.5, 41.0, 10.0),
  dest_dir  = "data/era5/FRA"
)
# → data/era5/FRA/mask_FRA.nc  (variable: 'country', values in [0, 1])
```

---

## The `country_abbrev` argument

All functions that refer to a country accept a single format for `country_abbrev`:
a **three-letter ISO 3166-1 alpha-3 code** (e.g. `"FRA"`, `"GBR"`, `"DEU"`).
The internal conversion to the format required by each library (PSMSL CSV,
`rnaturalearth`) is handled automatically.

---

## Quick use

### National ACI (scalar time series)

This calculation can take several hours depending on the platform.

```r
library(xaci)

result <- calculate_aci(
  country_abbrev          = "FRA",
  study_period            = c("2011-01-01", "2015-12-31"),
  reference_period        = c("2011-01-01", "2013-12-31"),
  mask_data_path          = "data/era5/FRA/mask_FRA.nc",
  temperature_data_path   = "data/era5/FRA/t2m_2011_2015.nc",
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  wind_u10_data_path      = "data/era5/FRA/u10_2011_2015.nc",
  wind_v10_data_path      = "data/era5/FRA/v10_2011_2015.nc",
  granularity             = "month",   # "month", "season", "semester", "year"
  area                    = TRUE,      # aggregate at national level
  factor                  = 0.2,       # proportion of country coastal line 
  admin_level             = NULL,
  crs_metric              = 2154,
  save                    = TRUE       # cache grid-cell objects to results/
)

head(result)
#            drought  wind  precipitation   t10   t90  sealevel   ACI
# 2011-01  ...
```

### Saving and reloading intermediate results

Long ERA5 computations produce grid-cell-level objects that can be saved once
and reloaded to study the ACI at different spatial or temporal resolutions
without re-running the heavy calculations.

**Step 1 — compute once and save (heavy computations)**

```r
calculate_aci(
  country_abbrev = "FRA",
  study_period = c("2011-01-01", "2015-12-31"),
  reference_period = c("2011-01-01", "2013-12-31"),
  years = 2011:2015,
  granularity = "month",
  area = TRUE,
  factor = 1 / 5,
  admin_level = NULL,
  crs_metric  = 2154,
  save = TRUE,            # save grid-cell .rds files
  save_dir = paste("results/", country_abbrev, sep==""),
  computed_components = FALSE
)

# Produces:
#   results/FRA/temperature_highs_2011_2013.rds
#   results/FRA/temperature_lows_2011_2013.rds
#   results/FRA/precipitation_2011_2013.rds
#   results/FRA/drought_2011_2013.rds
#   results/FRA/wind_2011_2013.rds
#   results/FRA/sealevel_2011_2013.rds
# (period tag derived from reference_period)
```

In addition to the computation of the ACI, all computations have been saved for each component at the ERA5 grid cell level 
(defined from downloaded data), at a monthly time step. It is therefore not interesting to run once again the computations for 
each component. However, the computations can be performed component-wise instead of for the whole set of components, if necessary.


**Step 2 — reload and re-aggregate freely**

Once obtained the objects corresponding to monthly calculations of ACI components at the grid cell level, it is quite
straightforward to perform the computation of the ACI or its components, whatever the time aggregation or spatial 
aggregation specified by the user.

```r
# Monthly national index, no re-computation
monthly_national_aci_FRA <- calculate_aci(
  country_abbrev = "FRA",
  study_period = c("2011-01-01", "2015-12-31"),
  reference_period = c("2011-01-01", "2013-12-31"),
  years = 2011:2015,
  temperature_data_path = NULL,
  precipitation_data_path = NULL,
  wind_u10_data_path = NULL,
  wind_v10_data_path = NULL,
  mask_data_path = NULL,
  sealevel_dir = NULL,
  percentile_high = 90,
  percentile_low = 10,
  granularity = "month",
  area = TRUE,
  factor = 0.2,
  max_dist_km = 500,
  admin_level = NULL,
  crs_metric = 2154,
  save = FALSE,
  load_dir = paste0("results/", country_abbrev),
  computed_components = TRUE
)

plot_aci_timeseries(aci_df = monthly_national_aci_FRA, smooth = TRUE, span = 0.2, fill_area = TRUE)
plot_aci_components(monthly_national_aci_FRA, type = "bar")
plot_aci_components(monthly_national_aci_FRA, type = "bar", components = c("t90","t10"))
plot_aci_components(monthly_national_aci_FRA, type = "stacked", components = "sealevel")
plot_aci_distribution(monthly_national_aci_FRA, type = "violin")
plot_aci_distribution(monthly_national_aci_FRA, type = "density")

# Seasonal national index, no re-computation
seasonal_national_aci_FRA <- calculate_aci(
  country_abbrev = "FRA",
  study_period = c("2011-01-01", "2015-12-31"),
  reference_period = c("2011-01-01", "2013-12-31"),
  years = 2011:2015,
  granularity = "season",
  area = TRUE,
  factor = 0.2,
  load_dir = paste0("results/", country_abbrev),
  computed_components = TRUE
)

plot_aci_timeseries(aci_df = seasonal_national_aci_FRA, smooth = TRUE, span = 0.2, fill_area = TRUE)
plot_aci_components(seasonal_national_aci_FRA, type = "bar")
plot_aci_components(seasonal_national_aci_FRA, type = "bar", components = c("t90"))
plot_aci_components(seasonal_national_aci_FRA, type = "stacked", components = "sealevel")
plot_aci_distribution(seasonal_national_aci_FRA, components = c("t90"), type = "boxplot", include_aci = TRUE)
plot_aci_distribution(seasonal_national_aci_FRA, type = "violin")


# By department, no re-computation
calculate_aci(
  ...,
  computed_components = TRUE,
  load_dir            = "results",
  admin_level         = 2,
  crs_metric          = 2154
)
```

### Grid-cell level output (for mapping)

Setting `area = FALSE` with `admin_level = NULL` returns a named list of
standardised grid-cell objects instead of a scalar time series. This is the
entry point for cartographic visualisation.

```r
monthlyACI_gridCell <- calculate_aci(
  country_abbrev = "FRA",
  study_period = c("2011-01-01", "2015-12-31"),
  reference_period = c("2011-01-01", "2013-12-31"),
  years = 2011:2015,
  granularity = "month",
  area = FALSE,
  factor = 0.2,
  max_dist_km = 500,
  admin_level = NULL,
  crs_metric = 2154,
  save = FALSE,
  load_dir = paste0("results/", country_abbrev),
  computed_components = TRUE
)

# monthlyACI_gridCell$lon          : longitude vector
# monthlyACI_gridCell$lat          : latitude vector
# monthlyACI_gridCell$t10          : array [lon x lat x time]
# monthlyACI_gridCell$t90          : array [lon x lat x time]
# monthlyACI_gridCell$precipitation: array [lon x lat x time]
# monthlyACI_gridCell$drought      : array [lon x lat x time]
# monthlyACI_gridCell$wind         : array [lon x lat x time]
# monthlyACI_gridCell$sealevel     : array [lon x lat x time]
# monthlyACI_gridCell$ACI          : array [lon x lat x time]
```

Visualisation:

```r
# Maps precipitation from observed ERA5 data:
ds <- load_component("data/era5/FRA/t2m_2011_2015.nc", "t2m",
                     mask_path = "data/era5/FRA/mask_FRA.nc")
plot_aci_map(ds, time_index = "mean", var_label = "Temperature (K)", title = "Mean temperature")
# hourly data from Copernicus: 5 years x 365,25 days x 24 hours = 43830
plot_aci_map(ds, time_index = 43000, var_label = "Temperature (K)", title = "Mean temperature")

# Maps precipitation from the computed precipitation component of the ACI:
plot_aci_map(monthlyACI_gridCell, variable = "ACI", time_index = "mean", var_label = "ACI", title = "Mean ACI")
plot_aci_map(monthlyACI_gridCell$ACI, time_index = "mean", var_label = "ACI", title = "Mean ACI")
# Now map for the two last months of the study period (59th and 60th):
plot_aci_map(monthlyACI_gridCell, variable = "t90", time_index = 60, var_label = "Temperature (C)", title = "t90")
```

---

## Computing individual components

Each component can be called independently. All accept `save = TRUE` /
`save_dir` to cache their grid-cell object, and `area = FALSE` to return the
full spatial array.

```r
precipitation_component(
  country_abbrev = "FRA",
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  mask_path = "data/era5/FRA/mask_FRA.nc",
  reference_period = c("2011-01-01", "2013-12-31"),
  var_name = "tp", window_size = 5L,
  area = FALSE, admin_level = NULL, admin_mask = NULL, crs_metric = 2154,
  computed_components = FALSE, save = TRUE, save_dir = paste0("results/", country_abbrev)
)

drought_component(
  country_abbrev = "FRA",
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  mask_path = "data/era5/FRA/mask_FRA.nc",
  reference_period = c("2011-01-01", "2013-12-31"),
  area = FALSE, admin_level = NULL, admin_mask = NULL, crs_metric = 2154,
  computed_components = FALSE, save = TRUE, save_dir = paste0("results/", country_abbrev)
)

wind_component(
  country_abbrev = "FRA",
  wind_u10_data_path = "data/era5/FRA/u10_2011_2015.nc",
  wind_v10_data_path = "data/era5/FRA/v10_2011_2015.nc",
  mask_path = "data/era5/FRA/mask_FRA.nc",
  reference_period = c("2011-01-01", "2013-12-31"),
  area = FALSE, admin_level = NULL, admin_mask = NULL, crs_metric = 2154,
  computed_components = FALSE, save = TRUE, save_dir = paste0("results/", country_abbrev)
)

temperature_component(
  country_abbrev = "FRA",
  temperature_data_path = "data/era5/FRA/t2m_1960_2024.nc",
  mask_path = "data/era5/FRA/mask_FRA.nc",
  reference_period = c("2061-01-01", "1990-12-31"),
  percentile = 90, extremum = "max", above_thresholds = TRUE,
  area = FALSE, admin_level = NULL, admin_mask = NULL, crs_metric = 2154,
  computed_components   = FALSE, save = TRUE, save_dir = paste0("results/", country_abbrev)
)

temperature_component(
  country_abbrev = "FRA",
  temperature_data_path = "data/era5/FRA/t2m_2011_2015.nc",
  mask_path = "data/era5/FRA/mask_FRA.nc",
  reference_period = c("2011-01-01", "2013-12-31"),
  percentile = 10, extremum = "min", above_thresholds = FALSE,
  area = FALSE, admin_level = NULL, admin_mask = NULL, crs_metric = 2154,
  computed_components   = FALSE, save = TRUE, save_dir = paste0("results/", country_abbrev)
)

sealevel_component(
  country_abbrev = "FRA",
  study_period = c("2011-01-01", "2015-12-31"),
  reference_period = c("2011-01-01", "2013-12-31"),
  mask_path = "data/era5/FRA/mask_FRA.nc",
  grid_cell = TRUE,
  max_dist_km = 500,
  data_dir = NULL, 
  admin_level = NULL, admin_assignment = NULL, crs_metric = 2154,
  computed_components = FALSE, save = TRUE, save_dir = paste0("results/", country_abbrev)
)
```


### National scalar output (`area = TRUE`)

```r
prec <- precipitation_component(
  country_abbrev = "FRA",
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  mask_path = "data/era5/FRA/mask_FRA.nc",
  reference_period = c("2011-01-01", "2013-12-31"),
  var_name = "tp", window_size = 5L,
  area = TRUE, admin_level = NULL, admin_mask = NULL, crs_metric = 2154,
  computed_components = TRUE, save = FALSE, load_dir = paste0("results/", country_abbrev)
)

drought <- drought_component(
  country_abbrev = "FRA",
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  mask_path = "data/era5/FRA/mask_FRA.nc",
  reference_period = c("2011-01-01", "2013-12-31"),
  area = TRUE, admin_level = NULL, admin_mask = NULL, crs_metric = 2154,
  computed_components = TRUE, save = FALSE, load_dir = paste0("results/", country_abbrev)
)

wind <- wind_component(
  country_abbrev = "FRA",
  wind_u10_data_path = "data/era5/FRA/u10_2011_2015.nc",
  wind_v10_data_path = "data/era5/FRA/v10_2011_2015.nc",
  mask_path = "data/era5/FRA/mask_FRA.nc",
  reference_period = c("2011-01-01", "2013-12-31"),
  area = TRUE, admin_level = NULL, admin_mask = NULL, crs_metric = 2154,
  computed_components = TRUE, save = FALSE, load_dir = paste0("results/", country_abbrev)
)

# T90 (hot days)
t_hot <- temperature_component(
  country_abbrev = "FRA",
  temperature_data_path = "data/era5/FRA/t2m_2011_2015.nc",
  mask_path = NULL,
  reference_period = c("2011-01-01", "2013-12-31"),
  percentile = 90, extremum = "max", above_thresholds = TRUE,
  area = TRUE, admin_level = NULL, admin_mask = NULL, crs_metric = 2154,
  computed_components = TRUE, save = FALSE, load_dir = paste0("results/", country_abbrev)
)

# T10 (cold nights)
t_low <- temperature_component(
  country_abbrev = "FRA",
  temperature_data_path = "data/era5/FRA/t2m_2011_2015.nc",
  mask_path = NULL,
  reference_period = c("2011-01-01", "2013-12-31"),
  percentile = 10, extremum = "min", above_thresholds = FALSE,
  area = TRUE, admin_level = NULL, admin_mask = NULL, crs_metric = 2154,
  computed_components = TRUE, save = FALSE, load_dir = paste0("results/", country_abbrev)
)

sealevel <- sealevel_component(
  country_abbrev = "FRA",
  study_period = c("2011-01-01", "2015-12-31"),
  reference_period = c("2011-01-01", "2013-12-31"),
  mask_path = "data/era5/FRA/mask_FRA.nc",
  grid_cell = FALSE,
  max_dist_km = 500,
  data_dir = NULL, 
  admin_level = NULL, admin_assignment = NULL, crs_metric = 2154,
  computed_components = TRUE, save = FALSE, load_dir = paste0("results/", country_abbrev)
)
```

### Grid-cell level output (`area = FALSE`)

```r
# Full spatial array — suitable for mapping
prec_grid <- precipitation_component(
  country_abbrev = "FRA",
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  mask_path = "data/era5/FRA/mask_FRA.nc",
  reference_period = c("2011-01-01", "2013-12-31"),
  var_name = "tp", window_size = 5L,
  area = FALSE, admin_level = NULL, admin_mask = NULL, crs_metric = 2154,
  computed_components = TRUE, save = FALSE, load_dir = paste0("results/", country_abbrev)
)
plot_aci_map(...)

drought_grid <- drought_component(
  country_abbrev = "FRA",
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  mask_path = "data/era5/FRA/mask_FRA.nc",
  reference_period = c("2011-01-01", "2013-12-31"),
  area = FALSE, admin_level = NULL, admin_mask = NULL, crs_metric = 2154,
  computed_components = TRUE, save = FALSE, load_dir = paste0("results/", country_abbrev)
)

wind_grid <- wind_component(
  country_abbrev = "FRA",
  wind_u10_data_path = "data/era5/FRA/u10_2011_2015.nc",
  wind_v10_data_path = "data/era5/FRA/v10_2011_2015.nc",
  mask_path = "data/era5/FRA/mask_FRA.nc",
  reference_period = c("2011-01-01", "2013-12-31"),
  area = FALSE, admin_level = NULL, admin_mask = NULL, crs_metric = 2154,
  computed_components = TRUE, save = FALSE, load_dir = paste0("results/", country_abbrev)
)

# T90 (hot days)
t_hot_grid <- temperature_component(
  country_abbrev = "FRA",
  temperature_data_path = "data/era5/FRA/t2m_2011_2015.nc",
  mask_path = NULL,
  reference_period = c("2011-01-01", "2013-12-31"),
  percentile = 90, extremum = "max", above_thresholds = TRUE,
  area = FALSE, admin_level = NULL, admin_mask = NULL, crs_metric = 2154,
  computed_components = TRUE, save = FALSE, load_dir = paste0("results/", country_abbrev)
)

# T10 (cold nights)
t_low_grid <- temperature_component(
  country_abbrev = "FRA",
  temperature_data_path = "data/era5/FRA/t2m_2011_2015.nc",
  mask_path = NULL,
  reference_period = c("2011-01-01", "2013-12-31"),
  percentile = 10, extremum = "min", above_thresholds = FALSE,
  area = FALSE, admin_level = NULL, admin_mask = NULL, crs_metric = 2154,
  computed_components = TRUE, save = FALSE, load_dir = paste0("results/", country_abbrev)
)

sealevel_grid <- sealevel_component(
  country_abbrev = "FRA",
  study_period = c("2011-01-01", "2015-12-31"),
  reference_period = c("2011-01-01", "2013-12-31"),
  mask_path = "data/era5/FRA/mask_FRA.nc",
  grid_cell = TRUE,
  max_dist_km = 500,
  data_dir = NULL, 
  admin_level = NULL, admin_assignment = NULL, crs_metric = 2154,
  computed_components = TRUE, save = FALSE, load_dir = paste0("results/", country_abbrev)
)
```

### Administrative unit level

Build the spatial mask once, then pass it to any component:

```r
prec_administrativeLevel1 <- precipitation_component(
  country_abbrev = "FRA",
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  mask_path = "data/era5/FRA/mask_FRA.nc",
  reference_period = c("2011-01-01", "2013-12-31"),
  var_name = "tp", window_size = 5L,
  area = FALSE, admin_level = 1, admin_mask = NULL, crs_metric = 2154,
  computed_components = TRUE, save = FALSE, load_dir = paste0("results/", country_abbrev)
)
plot_aci_map(prec_administrativeLevel1)


prec_administrativeLevel2 <- precipitation_component(
  country_abbrev = "FRA",
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  mask_path = "data/era5/FRA/mask_FRA.nc",
  reference_period = c("2011-01-01", "2013-12-31"),
  var_name = "tp", window_size = 5L,
  area = FALSE, admin_level = 2, admin_mask = NULL, crs_metric = 2154,
  computed_components = TRUE, save = FALSE, load_dir = paste0("results/", country_abbrev)
)



# Sea level assignment for administrative units
dept_assignment <- assign_sealevel_to_admin(
  country_abbrev = "FRA",
  admin_level    = 1,
  crs_metric     = 2154
)
sealevel_component(country_abbrev = "FRA", 
                   study_period = c("2011-01-01","2015-12-31"), 
                   reference_period = c("2011-01-01","2013-12-31"),
                   data_dir = NULL,
                   admin_assignment = dept_assignment,
                   save = FALSE
)
```

### ACI by administrative unit

```r
result_dept <- calculate_aci(
  temperature_data_path   = "data/era5/FRA/t2m_2011_2015.nc",
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  wind_u10_data_path      = "data/era5/FRA/u10_2011_2015.nc",
  wind_v10_data_path      = "data/era5/FRA/v10_2011_2015.nc",
  country_abbrev          = "FRA",
  mask_data_path          = "data/era5/FRA/mask_FRA.nc",
  study_period            = c("2011-01-01", "2015-12-31"),
  reference_period        = c("2011-01-01", "2013-12-31"),
  granularity             = "month",
  admin_level             = 1,
  crs_metric              = 2154,
  save                    = FALSE,
  load_dir                = "results",
  computed_components     = TRUE
)
```

---

## ACI formula

$$ACI = \frac{T_{90} - T_{10} + P + D + \alpha \cdot SL + W}{5 + \alpha}$$

| Symbol | Component                                   |
|--------|---------------------------------------------|
| T₉₀   | Frequency of hot days (percentile 90)        |
| T₁₀   | Frequency of cold nights (percentile 10)     |
| P      | Maximum sliding precipitation (5-day window) |
| D      | Consecutive dry days (CDD)                   |
| SL     | Standardised sea level                       |
| W      | Wind power above 90th percentile             |
| α      | Coastal fraction (default = 1/5 nationally; computed per unit at administrative level) |

At the administrative level, α is computed automatically per unit as the ratio
of coastal length to total perimeter. Units without tide-gauge stations use
α = 0 and a denominator of 5.

---

## Package structure

```
xaci/
├── R/
│   ├── aci.R            # Main function calculate_aci()
│   ├── component.R      # Base helpers (loading, mask, resampling)
│   ├── temperature.R    # Temperature component (T10 / T90)
│   ├── precipitation.R  # Precipitation component
│   ├── drought.R        # Drought component (CDD)
│   ├── wind.R           # Wind component
│   ├── sealevel.R       # Sea-level component (PSMSL)
│   ├── utils.R          # Shared utilities (standardisation, aggregation, …)
│   ├── download.R       # ERA5 and mask download helpers
│   └── visualization.R  # Plotting functions
├── inst/
│   └── extdata/
│       └── psmsl_data.csv   # PSMSL tide-gauge station metadata (bundled)
├── tests/
│   └── testthat/
│       └── test-aci.R
├── DESCRIPTION
├── NAMESPACE
└── LICENSE
```

---

## Tests

```r
devtools::test()
# or
testthat::test_dir("tests/testthat")
```

---

## References

- Garrido J., Milhaud X., Olympio A. (2026). *The definition of a French actuarial climate index*. ⟨hal-04491982⟩
- American Academy of Actuaries et al. (2019). *ACI: Actuaries Climate Index Development and Design v1.1*.
- Hersbach et al. (2023). *ERA5 hourly data on single levels*. Copernicus C3S. DOI: 10.24381/cds.adbb2d47
- PSMSL (2023). *Tide Gauge Data*. http://www.psmsl.org/data/obtaining/
