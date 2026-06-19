# ACI — Actuarial Climate Index (R package)

Related to the Python package [`ACI-Python`](https://github.com/your-org/ACI-Python).  
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
| `terra`   | Manipulate rasters (optional)   |
| `lubridate` | Manipulate dates              |
| `dplyr`   | Manipulate tables               |
| `tidyr`   | Transform data                  |
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

---

## Quick use

### Complete calculation of the ACI

```r
library(ACI)

result <- calculate_aci(
  temperature_data_path   = "data/t2m_1960-2020.nc",
  precipitation_data_path = "data/tp_1960-2020.nc",
  wind_u10_data_path      = "data/u10_1960-2020.nc",
  wind_v10_data_path      = "data/v10_1960-2020.nc",
  country_abbrev          = "FRA",
  mask_data_path          = "data/mask_france.nc",
  study_period            = c("1980-01-01", "2020-12-31"),
  reference_period        = c("1961-01-01", "1990-12-31")
)

head(result)
#         drought  wind  precipitation   t10   t90  sealevel   ACI
# 1980-01-31  ...
```

### Calculation of one given individual component

```r
# Composante précipitations uniquement
prec <- precipitation_component(
  precipitation_data_path = "data/tp_1960-2020.nc",
  mask_path               = "data/mask_france.nc",
  reference_period        = c("1961-01-01", "1990-12-31"),
  area                    = TRUE   # moyenne spatiale
)

# Composante sécheresse
drought <- drought_component(
  precipitation_data_path = "data/tp_1960-2020.nc",
  mask_path               = "data/mask_france.nc",
  reference_period        = c("1961-01-01", "1990-12-31"),
  area                    = TRUE
)
```

---

## Package structure

```
ACI/
├── R/
│   ├── aci.R            # Fonction principale calculate_aci()
│   ├── component.R      # Helpers de base (chargement, masque, resample)
│   ├── temperature.R    # Composante température (T10 / T90)
│   ├── precipitation.R  # Composante précipitations
│   ├── drought.R        # Composante sécheresse (CDD)
│   ├── wind.R           # Composante vent
│   ├── sealevel.R       # Composante niveau marin (PSMSL)
│   └── utils.R          # Utilitaires (standardisation, fusion, etc.)
├── inst/
│   └── extdata/
│       └── psmsl_data.csv   # Métadonnées stations PSMSL (embarquées)
├── tests/
│   └── testthat/
│       └── test-aci.R   # Tests unitaires (testthat)
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

- Garrido J., Milhaud X., Olympio A. (2023). *The definition of a French actuarial climate index*. ⟨hal-04491982⟩  
- American Academy of Actuaries et al. (2019). *ACI: Actuaries Climate Index Development and Design v1.1*.  
- Hersbach et al. (2023). *ERA5 hourly data on single levels*. Copernicus C3S. DOI: 10.24381/cds.adbb2d47  
- PSMSL (2023). *Tide Gauge Data*. http://www.psmsl.org/data/obtaining/
