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
| `terra`          | Required by `geodata`; also powers the memory-safe `engine = "terra"` option (see `vignette("xaci-terra-engine")`) |
| `units`          | Unit handling in spatial distance calculations|

Suggested (not required for core use):

| Package             | Role                                       |
|---------------------|--------------------------------------------|
| `ecmwfr`            | Download ERA5 data from Copernicus CDS     |
| `rnaturalearthdata` | High-resolution natural earth data         |
| `patchwork`         | Combine ggplot2 panels (`plot_aci_dashboard()`) |
| `gganimate`, `gifski` | Animated maps (`animate_aci_map()`)      |
| `knitr`, `rmarkdown` | Build the vignettes                       |
| `testthat`          | Unit tests                                 |

---

## Documentation

The full walkthrough lives in the package vignettes, each runnable end to end
(most on a small self-contained synthetic dataset, so they execute without any
download or CDS account):

```r
browseVignettes("xaci")
# or, individually:
vignette("xaci-intro", package = "xaci")
```

| Vignette                | Covers |
|-------------------------|--------|
| `xaci-intro`            | What the ACI is, the four-step workflow, a first minimal example |
| `xaci-components`       | Computing each of the six components individually (temperature, precipitation, drought, wind, sea level), and the `save` / `computed_components` caching pattern |
| `xaci-full-pipeline`    | Running `calculate_aci()` end to end, reading its output, national vs. grid-cell mode |
| `xaci-visualization`    | All six plotting functions: time series, component breakdowns, distributions, maps, dashboard, animated maps |
| `xaci-terra-engine`     | The memory-safe `engine = "terra"` option for whole-country, multi-decade, hourly historical periods |
| `xaci-admin-levels`     | Aggregating components and the ACI at the administrative-unit level (e.g. French departments) |

This README stays intentionally short: it covers installation, the overall
data flow, and reference material (the ACI formula, package layout) that
doesn't belong in any single vignette. For actual usage examples, see the
table above.

---

## The `country_abbrev` argument

All functions that refer to a country accept a single format for `country_abbrev`:
a **three-letter ISO 3166-1 alpha-3 code** (e.g. `"FRA"`, `"GBR"`, `"DEU"`).
The internal conversion to the format required by each library (PSMSL CSV,
GADM via `geodata::gadm()`) is handled automatically.

---

## Overall data flow

Setting up `xaci` for a new country follows **four sequential steps**. Steps 1–3
are one-off, heavy computations; step 4 is the fast, repeatable step you'll use
day to day once steps 1–3 are done. See `vignette("xaci-intro")` for the full
explanation and a first worked example, and `vignette("xaci-components")` /
`vignette("xaci-full-pipeline")` for everything past step 2.

```
Step 1 (long)   Step 2 (short)   Step 3 (long)              Step 4 (fast, repeatable)
ERA5 download → Country mask  → Grid-cell components     →  ACI / components at any
(data/)          (data/)         (results/, cached .rds)     temporal & spatial level
```

* **Step 1** downloads the four required ERA5 variables (`t2m`, `tp`, `u10`,
  `v10`) via `download_era5_all()` / `download_era5()` — requires a free
  [Copernicus CDS](https://cds.climate.copernicus.eu) account and the
  `ecmwfr` package.
* **Step 2** downloads a land/sea country mask with `download_mask()`
  (`data/era5/<country_abbrev>/mask_<country_abbrev>.nc`, variable `"country"`,
  values in `[0, 1]`) — used by `apply_mask()` to restrict grid cells to the
  country's land area (threshold on the land fraction, default `0.8`).
* **Step 3** computes each component at grid-cell / monthly resolution and
  caches it to `results/<country_abbrev>/*.rds` (`save = TRUE`) — the
  expensive step, done once per country and reference period.
* **Step 4** reloads the cached components (`computed_components = TRUE`)
  and re-aggregates them on the fly to any temporal granularity (`"month"`,
  `"season"`, `"semester"`, `"year"`) and spatial level (national, grid-cell,
  administrative unit) — the step you repeat while exploring the data.

Locally, computations are organised into two git-ignored directories:

| Directory   | Content                                                        |
|-------------|-----------------------------------------------------------------|
| `data/`     | Downloaded ERA5 NetCDFs (steps 1–2) and PSMSL tide-gauge files |
| `results/`  | Cached grid-cell-level component objects, `.rds` (step 3)      |

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
tide-gauge station. See `vignette("xaci-admin-levels")` for a worked example.

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
├── vignettes/                # See "Documentation" above
├── tests/
│   └── testthat/
│       ├── test-aci.R
│       └── ...               # terra parity & NetCDF integration tests
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
