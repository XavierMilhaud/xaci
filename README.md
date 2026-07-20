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
| `sf`             | Spatial operations (masks, admin polygons)    |
| `rnaturalearth`  | Worldwide coastline layer (sea-level factors) |
| `geodata`        | Multi-level administrative boundaries (GADM)  |
| `terra`          | Required by `geodata`; also powers the memory-safe `engine = "terra"` option (see [Large historical periods](#large-historical-periods-the-terra-engine)) |
| `units`          | Unit handling in spatial distance calculations|

Suggested (not required for core use):

| Package             | Role                                       |
|---------------------|--------------------------------------------|
| `ecmwfr`            | Download ERA5 data from Copernicus CDS     |
| `rnaturalearthdata` | High-resolution natural earth data         |
| `patchwork`         | Combine ggplot2 panels (`plot_aci_dashboard()`) |
| `gganimate`, `gifski` | Animated maps (`animate_aci_map()`)      |
| `testthat`          | Unit tests                                 |

---

## The `country_abbrev` argument

All functions that refer to a country accept a single format for `country_abbrev`:
a **three-letter ISO 3166-1 alpha-3 code** (e.g. `"FRA"`, `"GBR"`, `"DEU"`).
The internal conversion to the format required by each library (PSMSL CSV,
GADM via `geodata::gadm()`) is handled automatically.

---

## Get started (first use of the package)

Setting up `xaci` for a new country follows **four sequential steps**. Steps 1–3
are one-off, heavy computations; step 4 is the fast, repeatable step you'll use
day to day once steps 1–3 are done.

```
Step 1 (long)   Step 2 (short)   Step 3 (long)              Step 4 (fast, repeatable)
ERA5 download → Country mask  → Grid-cell components     →  ACI / components at any
(data/)          (data/)         (results/, cached .rds)     temporal & spatial level
```

### Step 1 · Download ERA5 data (long — can take hours to days)

Climate data are extracted from the
[Copernicus Climate Data Store (ERA5)](https://cds.climate.copernicus.eu/datasets/reanalysis-era5-single-levels)
via the [`ecmwfr`](https://bluegreen-labs.github.io/ecmwfr/) package (v2.0+,
Personal Access Token required). Files are saved to `data/era5/<country_abbrev>/`.

```r
install.packages("ecmwfr")
library(xaci)

# 1. Store your CDS token once per machine
# (https://cds.climate.copernicus.eu -> profile -> Personal Access Token)
cds_set_key("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")

# 2. Download all four ERA5 variables (t2m, tp, u10, v10) for France, 1960-2024
# One NetCDF per variable is produced in data/era5/FRA/
download_era5_all(
  years          = 2010:2020,
  area           = c(51.5, -5.5, 41.0, 10.0),   # N, W, S, E (metropolitan France)
  country_abbrev = "FRA"
)

# Or download a single variable:
download_era5(
  variable       = "u10",
  years          = 1960:2024,
  area           = c(51.5, -5.5, 41.0, 10.0),
  country_abbrev = "FRA"
)
```

Required NetCDF variables:

| Variable | Description                       |
|----------|------------------------------------|
| `t2m`    | 2-m air temperature (hourly)       |
| `tp`     | Total precipitation (hourly)       |
| `u10`    | U-component of 10-m wind (hourly)  |
| `v10`    | V-component of 10-m wind (hourly)  |

### Step 2 · Download the country mask (short)

```r
download_mask(
  country_abbrev = "FRA",
  area           = c(51.5, -5.5, 41.0, 10.0),
  dest_dir       = "data/era5/FRA"
)
# -> data/era5/FRA/mask_FRA.nc  (variable: 'country', values in [0, 1])
```

The mask is used by `apply_mask()` / `load_component()` to restrict grid cells
to the country's land area (threshold on the land fraction, default `0.8`).

### Step 3 · Compute components at grid-cell level (long, one-off per period)

This is the heavy step: for each of the five components (temperature high/low,
precipitation, drought, wind — sea level is handled separately from PSMSL
data), `xaci` computes standardised anomalies at the **finest possible
granularity**: individual ERA5 grid cells, monthly time steps. The resulting
objects are cached as `.rds` files in `results/<country_abbrev>/`, so this
step never needs to be repeated for a given `country_abbrev` /
`reference_period` combination — later re-aggregation (step 4) simply reloads
them.

The simplest way to run step 3 is via `calculate_aci()` itself, with `save = TRUE`:

```r
calculate_aci(
  country_abbrev    = "FRA",
  study_period      = c("2011-01-01", "2015-12-31"),
  reference_period  = c("2011-01-01", "2013-12-31"),
  years             = 2011:2015,        # used to build default ERA5 file paths
  granularity       = "month",
  area              = TRUE,
  factor            = 1 / 5,
  admin_level       = NULL,
  save              = TRUE,             # <- cache grid-cell .rds files
  save_dir          = "results/FRA",
  computed_components = FALSE           # <- force (re)computation
)

# Produces, in results/FRA/:
#   temperature_t90_startStudyYear_endStudyYear.rds
#   temperature_t10_startStudyYear_endStudyYear.rds
#   precipitation_startStudyYear_endStudyYear.rds
#   drought_startStudyYear_endStudyYear.rds
#   wind_startStudyYear_endStudyYear.rds
#   sealevel_startStudyYear_endStudyYear.rds
```

Components can also be computed one at a time (e.g. to parallelise the work,
or recompute a single component after a data update) — see
[Computing individual components](#computing-individual-components) below.

> **Working with 40+ years of hourly data for a whole country?** The default
> loading (`ncdf4`-based) reads the full hourly cube into memory before any
> computation, which can exceed available RAM at that scale. See
> [Large historical periods: the `terra` engine](#large-historical-periods-the-terra-engine)
> for a memory-safe, drop-in alternative.

### Step 4 · Deploy the computations at any granularity (fast, repeatable)

Once step 3's `.rds` objects exist in `results/<country_abbrev>/`, any
subsequent call with `computed_components = TRUE` **reloads** them instead of
recomputing from the NetCDF files, and re-aggregates on the fly to whichever
temporal granularity (`"month"`, `"season"`, `"semester"`, `"year"`) and
spatial level (national, grid-cell, administrative unit) you ask for. This is
the step you'll run repeatedly while exploring the data.

```r
# Monthly national ACI, no re-computation
monthly_national_aci_FRA <- calculate_aci(
  country_abbrev      = "FRA",
  years               = 2011:2015,
  study_period        = c("2011-01-01", "2015-12-31"),
  reference_period    = c("2011-01-01", "2013-12-31"),
  granularity         = "month",
  area                = TRUE,
  factor              = 1 / 5,
  admin_level         = NULL,
  load_dir            = "results/FRA",
  computed_components = TRUE            # <- reload cached components
)

head(monthly_national_aci_FRA)
#            drought  wind  precipitation   t10   t90  sealevel   ACI
# 2011-01  ...

# Same underlying data, different aggregation - no recomputation needed:
seasonal_national_aci_FRA <- calculate_aci(
  country_abbrev      = "FRA",
  years               = 2011:2015,
  study_period        = c("2011-01-01", "2015-12-31"),
  reference_period    = c("2011-01-01", "2013-12-31"),
  granularity         = "season",
  area                = TRUE,
  load_dir            = "results/FRA",
  computed_components = TRUE
)
head(seasonal_national_aci_FRA)

# By administrative unit (department, admin_level = 2) instead of national:
dept_aci_FRA <- calculate_aci(
  country_abbrev      = "FRA",
  years               = 2011:2015,
  study_period        = c("2011-01-01", "2015-12-31"),
  reference_period    = c("2011-01-01", "2013-12-31"),
  granularity         = "month",
  admin_level         = 2,
  crs_metric          = 2154,           # Lambert-93 for France
  load_dir            = "results/FRA",
  computed_components = TRUE
)
dim(dept_aci_FRA)  # 60 months (5 years), 96 departments over 7 variables (ACI and its 6 components) = 672 columns
class(dept_aci_FRA)
dept_aci_FRA[1:3, 1:10]

# Full spatial grid, for mapping (area = FALSE, admin_level = NULL):
grid_aci_FRA <- calculate_aci(
  country_abbrev      = "FRA",
  years = 2011:2015,
  study_period        = c("2011-01-01", "2015-12-31"),
  reference_period    = c("2011-01-01", "2013-12-31"),
  granularity         = "month",
  area                = FALSE,
  admin_level         = NULL,
  max_dist_km         = 500,
  load_dir            = "results/FRA",
  computed_components = TRUE
)
# grid_aci_FRA$lon / $lat        : coordinate vectors
# grid_aci_FRA$t90, $t10,
#   $precipitation, $drought,
#   $wind, $sealevel, $ACI       : arrays [lon x lat x time]
# dim(grid_aci_FRA$t90)
# dim(grid_aci_FRA$sealevel)

class(grid_aci_FRA)
attributes(grid_aci_FRA)
class(grid_aci_FRA$lon)
dim(grid_aci_FRA$lon)     # 63 grid cells on longitude
dim(grid_aci_FRA$lat)     # 43 grid cells on latitude
length(grid_aci_FRA$time) # studied months
dim(grid_aci_FRA$ACI)     # ACI values for each month on each grid cell
grid_aci_FRA$ACI[ , ,60]  # many NA values (coming from NA sea level component?)

plot_aci_timeseries(monthly_national_aci_FRA, smooth = TRUE, span = 0.2)
plot_aci_components(monthly_national_aci_FRA, type = "bar")
plot_aci_map(grid_aci_FRA, variable = "ACI", time_index = "mean")  # strangely maps also a part of Spain, and other neighboors
plot_aci_map(dept_aci_FRA, variable = "ACI", time_index = "mean")
```

> **Note:** the examples above require NetCDF files downloaded in steps 1–2,
> or cached `.rds` files from step 3. Replace paths and periods with your own.

---

## ACI formula

$$ACI = \frac{T_{90} - T_{10} + P + D + \alpha \cdot SL + W}{5 + \alpha}$$

| Symbol | Component                                   |
|--------|-----------------------------------------------|
| T₉₀    | Frequency of hot days (percentile 90)         |
| T₁₀    | Frequency of cold nights (percentile 10)      |
| P      | Maximum sliding precipitation (5-day window)  |
| D      | Consecutive dry days (CDD)                    |
| SL     | Standardised sea level                        |
| W      | Wind power above 90th percentile              |
| α      | Coastal fraction (default = 1/5 nationally; computed per unit at administrative level) |

At the administrative level, α is computed automatically per unit as the ratio
of coastal length to total perimeter (`assign_sealevel_to_admin()`). Units
without tide-gauge stations use α = 0 and a denominator of 5. At grid-cell
level, α ∈ {0, 1} depending on whether a cell lies within `max_dist_km` of a
tide-gauge station.

---

## Computing individual components

Each component can be called independently — useful to recompute a single
component after a data update, or to inspect one component's output without
running the full `calculate_aci()` pipeline. All accept `save = TRUE` /
`save_dir` to cache their grid-cell object (step 3), and `computed_components
= TRUE` / `load_dir` to reload it (step 4). All accept `area = FALSE` to
return the full spatial array, or `admin_level` to aggregate per
administrative unit.

```r
# --- Step 3 equivalent: compute and cache one component ---
precipitation_component(
  country_abbrev          = "FRA",
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  mask_path               = "data/era5/FRA/mask_FRA.nc",
  reference_period        = c("2011-01-01", "2013-12-31"),
  study_period            = c("2011-01-01", "2015-12-31"),
  var_name                = "tp", window_size = 5L,
  area                    = FALSE, admin_level = NULL,
  save                    = TRUE, save_dir = "results/FRA"
)

drought_component(
  country_abbrev          = "FRA",
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  mask_path               = "data/era5/FRA/mask_FRA.nc",
  reference_period        = c("2011-01-01", "2013-12-31"),
  study_period            = c("2011-01-01", "2015-12-31"),
  area                    = FALSE, admin_level = NULL,
  save                    = TRUE, save_dir = "results/FRA"
)

wind_component(
  country_abbrev     = "FRA",
  wind_u10_data_path = "data/era5/FRA/u10_2011_2015.nc",
  wind_v10_data_path = "data/era5/FRA/v10_2011_2015.nc",
  mask_path          = "data/era5/FRA/mask_FRA.nc",
  reference_period   = c("2011-01-01", "2013-12-31"),
  study_period       = c("2011-01-01", "2015-12-31"),
  area               = FALSE, admin_level = NULL,
  save               = TRUE, save_dir = "results/FRA"
)

# T90 (hot days) and T10 (cold nights) both come from temperature_component(),
# with different percentile/extremum/above_thresholds arguments:
temperature_component(
  country_abbrev         = "FRA",
  temperature_data_path  = "data/era5/FRA/t2m_2011_2015.nc",
  mask_path              = "data/era5/FRA/mask_FRA.nc",
  reference_period       = c("2011-01-01", "2013-12-31"),
  study_period           = c("2011-01-01", "2015-12-31"),
  percentile = 90, extremum = "max", above_thresholds = TRUE,
  area = FALSE, admin_level = NULL,
  save = TRUE, save_dir = "results/FRA"
)
temperature_component(
  country_abbrev         = "FRA",
  temperature_data_path  = "data/era5/FRA/t2m_2011_2015.nc",
  mask_path              = "data/era5/FRA/mask_FRA.nc",
  reference_period       = c("2011-01-01", "2013-12-31"),
  study_period           = c("2011-01-01", "2015-12-31"),
  percentile = 10, extremum = "min", above_thresholds = FALSE,
  area = FALSE, admin_level = NULL,
  save = TRUE, save_dir = "results/FRA"
)

# Sea level: area = TRUE returns station/admin anomalies, area = FALSE
# interpolates onto the ERA5 grid via IDW (needs mask_path for lon/lat).
sealevel_component(
  country_abbrev   = "FRA",
  study_period     = c("2011-01-01", "2015-12-31"),
  reference_period = c("2011-01-01", "2013-12-31"),
  mask_path        = "data/era5/FRA/mask_FRA.nc",
  area             = FALSE,
  max_dist_km      = 500,
  sealevel_dir     = NULL,     # -> auto-download from PSMSL
  save             = TRUE, save_dir = "results/FRA"
)

# --- Step 4 equivalent: reload cached components (fast) ---
prec_national <- precipitation_component(
  country_abbrev          = "FRA",
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  mask_path               = "data/era5/FRA/mask_FRA.nc",
  reference_period        = c("2011-01-01", "2013-12-31"),
  study_period            = c("2011-01-01", "2015-12-31"),
  area                    = TRUE,
  computed_components     = TRUE, load_dir = "results/FRA"
)
prec_national

sealevel_national <- sealevel_component(
  country_abbrev   = "FRA",
  study_period     = c("2011-01-01", "2015-12-31"),
  reference_period = c("2011-01-01", "2013-12-31"),
  area             = TRUE,
  computed_components = TRUE, load_dir = "results/FRA"
)
sealevel_national
```

### Administrative unit level

Build the spatial mask once, then pass it to any component to avoid
rebuilding it on every call:

```r
admin_mask_L1 <- build_admin_mask(
  lon = grid_aci_FRA$lon, lat = grid_aci_FRA$lat,
  country_abbrev = "FRA", admin_level = 1, crs_metric = 2154
)

prec_admin_L1 <- precipitation_component(
  country_abbrev          = "FRA",
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  mask_path               = "data/era5/FRA/mask_FRA.nc",
  reference_period        = c("2011-01-01", "2013-12-31"),
  study_period            = c("2011-01-01", "2015-12-31"),
  area                    = FALSE,
  admin_mask              = admin_mask_L1,
  computed_components     = TRUE, load_dir = "results/FRA"
)
plot_aci_map(prec_admin_L1, variable = "precipitation")

# Sea-level factors (coastal fraction) per administrative unit:
dept_assignment <- assign_sealevel_to_admin(
  country_abbrev = "FRA", admin_level = 1, crs_metric = 2154
)
sealevel_component(
  country_abbrev   = "FRA",
  study_period     = c("2011-01-01", "2015-12-31"),
  reference_period = c("2011-01-01", "2013-12-31"),
  area             = TRUE,
  admin_assignment = dept_assignment
)
```

---

## Large historical periods: the `terra` engine

ERA5 data at hourly resolution, for a whole country, over several decades
(e.g. 1980–2020), can be too large to load fully into memory with the
default pipeline (`ncdf4`-based `load_component()`, used internally by
`temperature_component()`, `wind_component()`, `drought_component()`, and
`precipitation_component()`). Loading the raw hourly cube for a single
variable can require tens of GB of RAM before any computation even starts.

`xaci` provides a **memory-safe alternative** built on the
[`terra`](https://rspatial.github.io/terra/) package: it reads NetCDF files
lazily (metadata only, no pixel values loaded up front) and processes the
hourly-to-daily reduction step block by block, writing intermediate results
to disk rather than accumulating them in RAM. Once data has been reduced to
daily resolution — a few hundred KB to a few MB even for 40+ years — the
rest of the pipeline (percentile thresholds, monthly aggregation,
standardisation) is identical to the default engine: same functions, same
results, verified by a parity test suite (`tests/testthat/`) comparing both
engines on synthetic and NetCDF-backed data.

### Via `calculate_aci()`

Pass `engine = "terra"` — everything else stays the same:

```r
calculate_aci(
  country_abbrev      = "FRA",
  study_period        = c("2000-01-01", "2024-12-31"),
  reference_period    = c("2000-01-01", "2012-12-31"),
  years               = 2000:2024,
  granularity         = "month",
  area                = TRUE,
  factor              = 1 / 5,
  admin_level         = NULL,
  save                = TRUE,
  save_dir            = "results/FRA",
  computed_components = FALSE,
  engine              = "terra",    # <- memory-safe loading & reduction
  cores               = 12          # <- transmis a temperature_component_terra()
)
```

`engine = "base"` (the default) reproduces the original behaviour exactly;
switch to `engine = "terra"` when the default pipeline runs out of memory —
typically a whole-country, hourly, multi-decade `study_period`.

### Via individual components

Each memory-safe component has a `_terra`-suffixed, drop-in equivalent, used
exactly like its base-R counterpart (same arguments, same
`save`/`computed_components` caching logic — see
[Computing individual components](#computing-individual-components) above):

| Base-R function               | Terra equivalent                   |
|--------------------------------|--------------------------------------|
| `temperature_component()`      | `temperature_component_terra()`      |
| `wind_component()`             | `wind_component_terra()`             |
| `drought_component()`          | `drought_component_terra()`          |
| `precipitation_component()`    | `precipitation_component_terra()`    |

```r
temperature_component_terra(
  country_abbrev        = "FRA",
  temperature_data_path  = "data/era5/FRA/t2m_2010_2020.nc",
  mask_path              = "data/era5/FRA/mask_FRA.nc",
  reference_period       = c("2000-01-01", "2012-12-31"),
  study_period           = c("2000-01-01", "2024-12-31"),
  percentile = 10, extremum = "min", above_thresholds = FALSE,
  area = FALSE, admin_level = NULL,
  cores = 2, save = TRUE, save_dir = "results/FRA"
)

temperature_component_terra(
  country_abbrev        = "FRA",
  temperature_data_path  = "data/era5/FRA/t2m_2000_2024.nc",
  mask_path              = "data/era5/FRA/mask_FRA.nc",
  reference_period       = c("2000-01-01", "2012-12-31"),
  study_period           = c("2000-01-01", "2024-12-31"),
  percentile = 90, extremum = "max", above_thresholds = TRUE,
  area = FALSE, admin_level = NULL,
  cores = 8, save = TRUE, save_dir = "results/FRA"
)

sealevel_component(
  country_abbrev   = "FRA",
  study_period     = c("2000-01-01", "2024-12-31"),
  reference_period = c("2000-01-01", "2010-12-31"),
  mask_path        = "data/era5/FRA/mask_FRA.nc",
  area             = FALSE,
  max_dist_km      = 500,
  sealevel_dir     = NULL,     # -> auto-download from PSMSL
  save             = TRUE, save_dir = "results/FRA"
)

dry <- drought_component(
  country_abbrev          = "FRA",
  precipitation_data_path = "data/era5/FRA/tp_2011_2015.nc",
  mask_path               = "data/era5/FRA/mask_FRA.nc",
  reference_period        = c("2011-01-01", "2013-12-31"),
  study_period            = c("2011-01-01", "2015-12-31"),
  area                    = FALSE, admin_level = NULL,
  save                    = FALSE, load_dir = "results/FRA"
)
plot_aci_map(dry, time_index = "mean")
plot_aci_map(dry, time_index = 60)
animate_aci_map(dry, country_abbrev = "FRA",
                 fps = 2, save_path = "aci_animation.gif")
```

> **Note:** `sealevel_component()` is unaffected by `engine` — its primary
> input is tide-gauge CSV data (PSMSL), not gridded NetCDF, so the same
> memory bottleneck doesn't apply.

Cached `.rds` files are named identically whether produced by the base or
terra engine (based on `study_period`, see
[Step 3](#step-3--compute-components-at-grid-cell-level-long-one-off-per-period)),
so a later call with `computed_components = TRUE` reloads them the same way
regardless of which engine originally computed them.

---

## Visualisation

```r
plot_aci_timeseries(monthly_national_aci_FRA, smooth = TRUE, span = 0.2, fill_area = TRUE)
plot_aci_components(monthly_national_aci_FRA, type = "bar")
plot_aci_components(monthly_national_aci_FRA, type = "stacked", components = "sealevel")
plot_aci_distribution(monthly_national_aci_FRA, type = "violin")
plot_aci_distribution(monthly_national_aci_FRA, type = "boxplot", components = "t90", include_aci = TRUE)

# Grid-cell / raster map (mean over the whole period, or a single time slice):
plot_aci_map(grid_aci_FRA, variable = "ACI", time_index = "mean",
             var_label = "ACI", title = "Mean ACI, France")
plot_aci_map(grid_aci_FRA$ACI, time_index = "mean")   # bare array also works
plot_aci_map(grid_aci_FRA, variable = "t90", time_index = 60)

# Administrative choropleth (same function, dispatches on data.frame input):
plot_aci_map(dept_aci_FRA, variable = "ACI", time_index = "mean")

# Combined dashboard (requires the 'patchwork' package):
plot_aci_dashboard(monthly_national_aci_FRA)

# Animated map over time (requires 'gganimate' and 'gifski'):
animate_aci_map(grid_aci_FRA, variable = "ACI")
```

---

## Package structure

```
xaci/
├── R/
│   ├── aci.R                 # Main function calculate_aci() (engine = "base"/"terra")
│   ├── component.R           # Base helpers (loading, mask, resampling)
│   ├── component_terra.R     # Memory-safe loading & hourly->daily reduction (terra)
│   ├── temperature.R         # Temperature component (T10 / T90)
│   ├── temperature_terra.R   # Terra equivalent of temperature.R
│   ├── precipitation.R       # Precipitation component
│   ├── precipitation_terra.R # Terra equivalent of precipitation.R
│   ├── drought.R             # Drought component (CDD)
│   ├── drought_terra.R       # Terra equivalent of drought.R
│   ├── wind.R                # Wind component
│   ├── wind_terra.R          # Terra equivalent of wind.R
│   ├── sealevel.R            # Sea-level component (PSMSL, not engine-dependent)
│   ├── utils.R                # Shared utilities (standardisation, aggregation, masks...)
│   ├── download.R            # ERA5 and mask download helpers
│   └── visualization.R       # Plotting functions
├── inst/
│   └── extdata/
│       └── psmsl_data.csv   # PSMSL tide-gauge station metadata (bundled)
├── tests/
│   └── testthat/
│       ├── test-aci.R
│       └── ...               # terra parity & NetCDF integration tests
├── DESCRIPTION
├── NAMESPACE
└── LICENSE
```

Locally, computations are organised into two directories (both git-ignored,
see `.gitignore`):

| Directory   | Content                                                        |
|-------------|-----------------------------------------------------------------|
| `data/`     | Downloaded ERA5 NetCDFs (steps 1–2) and PSMSL tide-gauge files |
| `results/`  | Cached grid-cell-level component objects, `.rds` (step 3)      |

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
