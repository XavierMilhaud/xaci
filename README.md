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
# Download t2m, tp, u10, v10 for France, 1960–2024
# One NetCDF per variable is produced in data/era5/
download_era5_all(
  years    = 1960:2024,
  area     = c(51.5, -5.5, 41.0, 10.0),   # N, W, S, E
  dest_dir = "data/era5/FRA/1960_2024"
)

# Or download a single variable:
download_era5(
  variable = "t2m",
  years    = 1960:2024,
  area     = c(51.5, -5.5, 41.0, 10.0), # area for France
  dest_dir = "data/era5/FRA/1960_2024"
)
```

### 3 · Download the country mask

```r
download_mask(
  country_abbrev = "FRA",
  area      = c(51.5, -5.5, 41.0, 10.0),
  dest_dir  = "data/era5/FRA"
)
# → data/era5/mask_FRA.nc  (variable: 'country', values in [0, 1])
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
  temperature_data_path   = "data/era5/FRA/t2m_2011_2015.nc",
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  wind_u10_data_path      = "data/era5/FRA/u10_2011_2015.nc",
  wind_v10_data_path      = "data/era5/FRA/v10_2011_2015.nc",
  country_abbrev          = "FRA",
  mask_data_path          = "data/era5/FRA/mask_FRA.nc",
  study_period            = c("2011-01-01", "2015-12-31"),
  reference_period        = c("2011-01-01", "2013-12-31"),
  granularity             = "month",   # "month", "season", "semester", "year"
  area                    = TRUE,      # TRUE = national scalar (default)
  factor                  = 0.2,
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

**Step 2 — reload and re-aggregate freely**

```r
# Monthly national index, no re-computation
monthly_national_aci_FRA <- calculate_aci(
  country_abbrev = "FRA",
  study_period = c("2011-01-01", "2015-12-31"),
  reference_period = c("2011-01-01", "2013-12-31"),
  years = 2011:2015,
  granularity = "month",
  area = TRUE,
  factor = 1 / 5,
  admin_level = NULL,
  crs_metric  = 2154,
  save = FALSE,
  load_dir = paste("results/",country_abbrev,sep=""),
  computed_components = TRUE
)

plot_aci_timeseries(aci_df = monthly_national_aci_FRA, smooth = TRUE, span = 0.2,
  title = "Actuarial Climate Index (ACI)", colour = "#1F77B4", fill_area = TRUE)
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
  factor = 1 / 5,
  admin_level = NULL,
  crs_metric  = 2154,
  save = FALSE,
  load_dir = paste("results/",country_abbrev,sep=""),
  computed_components = TRUE
)

plot_aci_timeseries(aci_df = seasonal_national_aci_FRA, smooth = TRUE, span = 0.2,
  title = "Actuarial Climate Index (ACI)", colour = "#1F77B4", fill_area = TRUE)
plot_aci_components(seasonal_national_aci_FRA, type = "bar")
plot_aci_components(seasonal_national_aci_FRA, type = "bar", components = c("t90"))
plot_aci_components(seasonal_national_aci_FRA, type = "stacked", components = "sealevel")
plot_aci_distribution(seasonal_national_aci_FRA, components = c("t90"), type = "boxplot", include_aci = TRUE)
plot_aci_distribution(seasonal_national_aci_FRA, type = "violin")
plot_aci_distribution(seasonal_national_aci_FRA, type = "density")

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
grid <- calculate_aci(
  country_abbrev = "FRA",
  study_period = c("2011-01-01", "2015-12-31"),
  reference_period = c("2011-01-01", "2013-12-31"),
  years = 2011:2015,
  granularity = "month",
  area = FALSE,         # keep full [lon x lat x time] arrays
  admin_level = NULL,
  crs_metric  = 2154,
  save = FALSE,
  load_dir = paste("results/",country_abbrev,sep=""),
  computed_components = TRUE
)

# grid$t90$data          : array [lon x lat x time]
# grid$t90$lon           : longitude vector
# grid$t90$lat           : latitude vector
# grid$t90$time          : POSIXct time vector
# grid$precipitation$data: array [lon x lat x time]
# grid$sealevel          : data.frame of tide-gauge stations

dim(grid$t90$data)
```

J'aimerais que l'objet 'grid' contienne egalement les valeurs de l'ACI par grid cell et son evolution dans le temps.
Donc j'aimerais que l'ACI apparaisse aussi sous forme d'array [lon x lat x time]. Ce qui implique peut-etre de gerer
le niveau de la mer sous la meme forme [lon x lat x time]?
Visualisation:

```r
# Maps precipitation from observed ERA5 data:
ds <- load_component("data/era5/FRA/t2m_2011_2015.nc", "t2m",
                     mask_path = "data/era5/FRA/mask_FRA.nc")
plot_aci_map(ds, time_index = "mean", var_label = "Temperature (K)", title = "Mean temperature")
# Or maps precipitation from the computed precipitation component of the ACI:
plot_aci_map(grid$t90, time_index = "mean", var_label = "Temperature (C)", title = "Mean t90")
# Now map for the two last months of the study period (59th and 60th):
plot_aci_map(grid$t90, time_index = 56, var_label = "Temperature (C)", title = "t90")
plot_aci_map(grid$t90, time_index = 60, var_label = "Temperature (C)", title = "t90")
```

---

## Computing individual components

Each component can be called independently. All accept `save = TRUE` /
`save_dir` to cache their grid-cell object, and `area = FALSE` to return the
full spatial array.

### National scalar output (`area = TRUE`)

```r
precipitation_component(
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  mask_path               = "data/era5/FRA/mask_FRA.nc",
  reference_period        = c("2011-01-01", "2013-12-31"),
  area                    = TRUE,
  save                    = TRUE
)

drought_component(
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  mask_path               = "data/era5/FRA/mask_FRA.nc",
  reference_period        = c("2011-01-01", "2013-12-31"),
  area                    = TRUE,
  save                    = TRUE
)

wind_component(
  wind_u10_data_path = "data/era5/FRA/u10_2011_2015.nc",
  wind_v10_data_path = "data/era5/FRA/v10_2011_2015.nc",
  mask_path          = "data/era5/mask_FRA.nc",
  reference_period   = c("2011-01-01", "2013-12-31"),
  area               = TRUE,
  save               = TRUE
)

# T90 (hot days)
temperature_component(
  temperature_data_path = "data/era5/FRA/t2m_1960_2024.nc",
  mask_path             = "data/era5/FRA/mask_FRA.nc",
  reference_period      = c("1960-01-01", "1990-12-31"),
  percentile            = 90,
  extremum              = "max",
  above_thresholds      = TRUE,
  area                  = TRUE,
  save                  = TRUE
)

# T10 (cold nights)
temperature_component(
  temperature_data_path = "data/era5/FRA/t2m_2011_2015.nc",
  mask_path             = "data/era5/FRA/mask_FRA.nc",
  reference_period      = c("2011-01-01", "2013-12-31"),
  percentile            = 10,
  extremum              = "min",
  above_thresholds      = FALSE,
  area                  = TRUE,
  save                  = TRUE
)

sealevel_component(
  country_abbrev   = "FRA",
  study_period     = c("2011-01-01", "2015-12-31"),
  reference_period = c("2011-01-01", "2013-12-31"),
  save             = TRUE
)
```

### Grid-cell level output (`area = FALSE`)

```r
# Full spatial array — suitable for mapping
prec_grid <- precipitation_component(
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  mask_path               = "data/era5/FRA/mask_FRA.nc",
  reference_period        = c("2011-01-01", "2013-12-31"),
  area                    = FALSE,
  save                    = TRUE
)
dim(prec_grid$data)   # [lon x lat x time]
```

### Administrative unit level

Build the spatial mask once, then pass it to any component:

```r
tmp <- load_component("data/era5/FRA/tp_2011_2015.nc", "tp", "data/era5/FRA/mask_FRA.nc")
dept_mask <- build_admin_mask(
  lon            = tmp$lon,
  lat            = tmp$lat,
  country_abbrev = "FRA",   # ISO-3 code
  admin_level    = 2,       # 1 = regions, 2 = departments, …
  crs_metric     = 2154     # Lambert-93 for France
)
region_mask <- build_admin_mask(
  lon            = tmp$lon,
  lat            = tmp$lat,
  country_abbrev = "FRA",   # ISO-3 code
  admin_level    = 1,       # 1 = regions, 2 = departments, …
  crs_metric     = 2154     # Lambert-93 for France
)
rm(tmp)

precipitation_component(
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  mask_path               = "data/era5/FRA/mask_FRA.nc",
  reference_period        = c("2011-01-01", "2013-12-31"),
  area                    = FALSE,
  admin_mask              = dept_mask,
  save                    = TRUE
)
precipitation_component(
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  mask_path               = "data/era5/FRA/mask_FRA.nc",
  reference_period        = c("2011-01-01", "2013-12-31"),
  area                    = FALSE,
  admin_mask              = region_mask,
  save                    = TRUE
)
# On a le meme resultat que l'on ait mis le region_mask ou le dept_mask!!!



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
