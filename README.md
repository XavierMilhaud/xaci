# ACI — Actuarial Climate Index (R package)

Implements the computation of the **Actuarial Climate Index (ACI)** and its six components from gridded climate data (NetCDF ERA5) tide gauge stations (PSMSL).

> Reference: Garrido, Milhaud & Olympio (2026). *The definition of a French actuarial climate index; one more step towards a European index.* ⟨hal-04491982⟩

---

## Installation

```r
# Depuis les sources (dossier local)
install.packages(".", repos = NULL, type = "source")

# Ou avec devtools
# install.packages("devtools")
devtools::install(".")
```

### Dependences

| Package   | Role                            |
|-----------|---------------------------------|
| `ncdf4`   | Read NetCDF file                |
| `lubridate` | Manipulate dates              |
| `dplyr`   | Manipulate tables               |
| `zoo`     | Sum / sliding mean              |
| `readr`   | Read the CSV PSMSL              |

---

## Required data

Climate data are extracted from [Climate Data Store of Copernicus (ERA5)](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels).  
Tide gauge data are automatically downloaded from [PSMSL](https://www.psmsl.org/data/obtaining/).

Necessary NetCDF variables:

| Variable | Description                        |
|----------|------------------------------------|
| `t2m`    | Temperature at 2 m (hourly)        |
| `tp`     | Total precipitation (hourly)       |
| `u10`    | Wind U at 10 m (hourly)            |
| `v10`    | Wind V at 10 m (hourly)            |
| `country`| Country mask (values in [0,1])     |

> **Note:** The examples below require NetCDF files downloaded from the Copernicus CDS.  
> Replace the paths with the actual locations of your files before running them.


---

## Download ERA5 data and country mask

The package includes functions to download directly from the
[Copernicus CDS](https://cds.climate.copernicus.eu) via the
[`ecmwfr`](https://bluegreen-labs.github.io/ecmwfr/) package (v2.0+,
new API — Personal Access Token only).

### 1 · Install ecmwfr and store your token (once per machine)

```r
install.packages("ecmwfr")
library(xaci)

# Your token is at: https://cds.climate.copernicus.eu → profile → Personal Access Token
cds_set_key("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
```

### 2 · Download the ERA5 variables

This step is really time-consuming. Can take hours depending on how many years of data and how many variables one downloads.

```r
# Downloads t2m, tp, u10, v10 for France, 2020–2022
# One NetCDF per variable is produced in data/era5/
download_era5_all(
  years    = 2020:2022,
  area     = c(51.5, -5.5, 41.0, 10.0),   # N, W, S, E
  dest_dir = "data/era5"
)

# Or download a single variable:
download_era5(
  variable = "v10",
  years    = 2020:2022,
  area     = c(51.5, -5.5, 41.0, 10.0),
  dest_dir = "data/era5"
)
```

### 3 · Download the country mask

Usually, this step is much faster and should at most take a few minutes.

```r
download_mask(
  area_name = "france",
  area      = c(51.5, -5.5, 41.0, 10.0),
  dest_dir  = "data/era5"
)
# → data/era5/mask_france.nc  (variable: 'country', values in [0,1])
```

---

## Quick use

### Complete calculation of the ACI at the country level

This calculation can take hours depending on the platform on which it is performed.

```r
library(xaci)

# Adapt these paths to your local NetCDF files
result <- calculate_aci(
  temperature_data_path   = "/data/era5/t2m_2020_2022.nc",
  precipitation_data_path = "/data/era5/tp_2020_2022.nc",
  wind_u10_data_path      = "/data/era5/u10_2020_2022.nc",
  wind_v10_data_path      = "/data/era5/v10_2020_2022.nc",
  country_abbrev          = "FRA",
  mask_data_path          = "/data/era5/mask_france.nc",
  study_period            = c("2020-01-01", "2022-12-31"),
  reference_period        = c("2020-01-01", "2021-12-31")
)

head(result)
#         drought  wind  precipitation   t10   t90  sealevel   ACI
# 2020-01-01  ...
```

### Calculation of one given individual component at the country level

```r
library(xaci)

# Adapt these paths to your local NetCDF files
prec <- precipitation_component(
  precipitation_data_path = "./data/era5/tp_2020_2022.nc",
  mask_path               = "./data/era5/mask_france.nc",
  reference_period        = c("2020-01-01", "2021-12-31"),
  area                    = TRUE  # spatial mean
)
saveRDS(prec, "data/components_country_level/prec.rds")
#readRDS("data/components_country_level/prec.rds")

drought <- drought_component(
  precipitation_data_path = "./data/era5/tp_2020_2022.nc",
  mask_path               = "./data/era5/mask_france.nc",
  reference_period        = c("2020-01-01", "2021-12-31"),
  area                    = TRUE
)
saveRDS(drought, "data/components_country_level/drought.rds")

wind <- wind_component(
  wind_u10_data_path = "./data/era5/u10_2020_2022.nc",
  wind_v10_data_path = "./data/era5/v10_2020_2022.nc",
  mask_path               = "./data/era5/mask_france.nc",
  reference_period        = c("2020-01-01", "2021-12-31"),
  area                    = TRUE
)
saveRDS(wind, "data/components_country_level/wind.rds")

temperature_highs <- temperature_component(
  temperature_data_path = "./data/era5/t2m_2020_2022.nc",
  mask_path               = "./data/era5/mask_france.nc",
  reference_period        = c("2020-01-01", "2021-12-31"),
  percentile = 90,
  extremum = "max",
  above_thresholds = TRUE,
  area                    = TRUE
)
saveRDS(temperature_highs, "data/components_country_level/temperature_highs.rds")

temperature_lows <- temperature_component(
  temperature_data_path = "./data/era5/t2m_2020_2022.nc",
  mask_path               = "./data/era5/mask_france.nc",
  reference_period        = c("2020-01-01", "2021-12-31"),
  percentile = 10,
  extremum = "min",
  above_thresholds = FALSE,
  area                    = TRUE
)
saveRDS(temperature_lows, "data/components_country_level/temperature_lows.rds")

seaLevel <- sealevel_component(
  country_abbrev = "FRA", 
  study_period = c("2020-01-01", "2022-12-31"), 
  reference_period = c("2020-01-01", "2021-12-31"))
saveRDS(seaLevel, "data/components_country_level/sea_level.rds")
```

### Complete calculation of the ACI at high-level resolution within a country:

What to do with the sea level on grid cells? Exclude it from the ACI calculation?


### Calculation of one given individual component at high-level resolution

```r
library(xaci)

# Adapt these paths to your local NetCDF files
prec <- precipitation_component(
  precipitation_data_path = "./data/era5/tp_2020_2022.nc",
  mask_path               = "./data/era5/mask_france.nc",
  reference_period        = c("2020-01-01", "2021-12-31"),
  area                    = FALSE  # grid-cell level
)
saveRDS(prec, "data/components_gridCell_level/prec.rds")

drought <- drought_component(
  precipitation_data_path = "./data/era5/tp_2020_2022.nc",
  mask_path               = "./data/era5/mask_france.nc",
  reference_period        = c("2020-01-01", "2021-12-31"),
  area                    = FALSE  # grid-cell level
)
saveRDS(drought, "data/components_gridCell_level/drought.rds")

wind <- wind_component(
  wind_u10_data_path = "./data/era5/u10_2020_2022.nc",
  wind_v10_data_path = "./data/era5/v10_2020_2022.nc",
  mask_path               = "./data/era5/mask_france.nc",
  reference_period        = c("2020-01-01", "2021-12-31"),
  area                    = FALSE  # grid-cell level
)
saveRDS(wind, "data/components_gridCell_level/wind.rds")

temperature_highs <- temperature_component(
  temperature_data_path = "./data/era5/t2m_2020_2022.nc",
  mask_path               = "./data/era5/mask_france.nc",
  reference_period        = c("2020-01-01", "2021-12-31"),
  percentile = 90,
  extremum = "max",
  above_thresholds = TRUE,
  area                    = FALSE  # grid-cell level
)
saveRDS(temperature_highs, "data/components_gridCell_level/temperature_highs.rds")

temperature_lows <- temperature_component(
  temperature_data_path = "./data/era5/t2m_2020_2022.nc",
  mask_path               = "./data/era5/mask_france.nc",
  reference_period        = c("2020-01-01", "2021-12-31"),
  percentile = 10,
  extremum = "min",
  above_thresholds = FALSE,
  area                    = FALSE  # grid-cell level
)
saveRDS(temperature_lows, "data/components_gridCell_level/temperature_lows.rds")
```


---

## Package structure

```
ACI/
├── R/
│   ├── aci.R            # Main function calculate_aci()
│   ├── component.R      # Base helpers (loading, mask, resampling)
│   ├── temperature.R    # Temperature component (T10 / T90)
│   ├── precipitation.R  # Precipitation component
│   ├── drought.R        # Drought component (CDD)
│   ├── wind.R           # Wind component
│   ├── sealevel.R       # Sea level component (PSMSL)
│   └── utils.R          # Useful functions (standardization, merging, etc.)
├── inst/
│   └── extdata/
│       └── psmsl_data.csv   # Metadata tide-gauge stations PSMSL (included)
├── tests/
│   └── testthat/
│       └── test-aci.R   # Unit tests (testthat)
├── DESCRIPTION
├── NAMESPACE
└── LICENSE
```

---

## ACI formula

$$ACI = \frac{T_{90} - T_{10} + P + D + \alpha \cdot SL + W}{6}$$

| Symbol | Component                                |
|---------|-----------------------------------------|
| T₉₀    | Frequency of hot days (percentile 90)    |
| T₁₀    | Frequency of cold nights (percentile 10) |
| P       | Maximum sliding rainfalls (5 days)      |
| D       | Consecutive dry days (CDD)              |
| SL      | Standardized sea level                  |
| W       | Wind power above 90th percentile        |
| α       | Coastal erosion factor (default = 1)    |

---

## Tests

```r
devtools::test()
# ou
testthat::test_dir("tests/testthat")
```

---

## References

- Garrido J., Milhaud X., Olympio A. (2026). *The definition of a French actuarial climate index*. ⟨hal-04491982⟩  
- American Academy of Actuaries et al. (2019). *ACI: Actuaries Climate Index Development and Design v1.1*.  
- Hersbach et al. (2023). *ERA5 hourly data on single levels*. Copernicus C3S. DOI: 10.24381/cds.adbb2d47  
- PSMSL (2023). *Tide Gauge Data*. http://www.psmsl.org/data/obtaining/
