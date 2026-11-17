# Running calculate_aci() End to End

``` r

library(xaci)
```

[`calculate_aci()`](../reference/calculate_aci.md) is the main entry
point of the package: it computes all six components (see
[`vignette("xaci-components")`](../articles/xaci-components.md)) and
combines them into the index itself, in a single call. This vignette
runs it end to end on the same kind of synthetic dataset used elsewhere
in this package’s vignettes, and explains the shape of its output.

## Building the synthetic dataset

``` r

build_synthetic_nc <- function(path, var, unit, lon, lat, time_vec, origin, vals) {
  time_hours <- as.numeric(difftime(time_vec, origin, units = "hours"))
  dim_lon  <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  dim_lat  <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  dim_time <- ncdf4::ncdim_def(
    "time", paste0("hours since ", format(origin, "%Y-%m-%d %H:%M:%S")),
    time_hours, unlim = TRUE
  )
  ncvar <- ncdf4::ncvar_def(var, unit, list(dim_lon, dim_lat, dim_time),
                            missval = NA, prec = "double")
  nc <- ncdf4::nc_create(path, list(ncvar))
  ncdf4::ncvar_put(nc, ncvar, vals)
  ncdf4::nc_close(nc)
  invisible(path)
}

build_synthetic_mask <- function(path, lon, lat) {
  dim_lon <- ncdf4::ncdim_def("longitude", "degrees_east", lon)
  dim_lat <- ncdf4::ncdim_def("latitude", "degrees_north", lat)
  var_mask <- ncdf4::ncvar_def("country", "1", list(dim_lon, dim_lat),
                               missval = NA, prec = "double")
  nc <- ncdf4::nc_create(path, list(var_mask))
  ncdf4::ncvar_put(nc, var_mask, matrix(1, length(lon), length(lat)))
  ncdf4::nc_close(nc)
  invisible(path)
}

set.seed(7)
lon      <- c(-1, 0, 1)
lat      <- c(43, 44, 45)
origin   <- as.POSIXct("1900-01-01 00:00:00", tz = "UTC")
time_vec <- seq(as.POSIXct("2011-01-01 00:00", tz = "UTC"),
                as.POSIXct("2014-12-31 23:00", tz = "UTC"), by = "hour")
nlo <- length(lon); nla <- length(lat); nt <- length(time_vec)

# A mild warming/drying trend, so 2014 (the one out-of-reference year, see
# below) shows genuine anomalies rather than pure noise.
trend <- seq_len(nt) / nt

seasonal_t <- 288 + 10 * sin(2 * pi * seq_len(nt) / (24 * 365)) + 0.6 * trend
t2m_vals <- array(NA_real_, c(nlo, nla, nt))
for (i in seq_len(nlo)) for (j in seq_len(nla))
  t2m_vals[i, j, ] <- seasonal_t + (i + j) + rnorm(nt, sd = 1.5)

tp_vals <- array(0, c(nlo, nla, nt))
for (i in seq_len(nlo)) for (j in seq_len(nla)) {
  rain_hours <- rbinom(nt, 1, 0.08 * (1 - 0.3 * trend))
  tp_vals[i, j, ] <- rain_hours * rexp(nt, rate = 800)
}

u10_vals <- array(rnorm(nlo * nla * nt, mean = 3, sd = 4), c(nlo, nla, nt))
v10_vals <- array(rnorm(nlo * nla * nt, mean = 1, sd = 4), c(nlo, nla, nt))

t2m_file  <- tempfile(fileext = ".nc")
tp_file   <- tempfile(fileext = ".nc")
u10_file  <- tempfile(fileext = ".nc")
v10_file  <- tempfile(fileext = ".nc")
mask_file <- tempfile(fileext = ".nc")

build_synthetic_nc(t2m_file, "t2m", "K",     lon, lat, time_vec, origin, t2m_vals)
build_synthetic_nc(tp_file,  "tp",  "m",     lon, lat, time_vec, origin, tp_vals)
build_synthetic_nc(u10_file, "u10", "m s-1", lon, lat, time_vec, origin, u10_vals)
build_synthetic_nc(v10_file, "v10", "m s-1", lon, lat, time_vec, origin, v10_vals)
build_synthetic_mask(mask_file, lon, lat)

month_frac <- c("0417", "125", "2083", "2917", "375", "4583",
                "5417", "625", "7083", "7917", "875", "9583")
build_synthetic_psmsl_station <- function(dir, station_id, years,
                                          base_level, trend_mm_per_year) {
  lines <- character(0)
  for (y in years) {
    for (m in seq_len(12)) {
      level <- base_level + trend_mm_per_year * (y - years[1]) + rnorm(1, sd = 15)
      lines <- c(lines, sprintf("%d.%s;%.1f;0;000", y, month_frac[m], level))
    }
  }
  writeLines(lines, file.path(dir, paste0(station_id, ".txt")))
}

psmsl_dir <- tempfile("psmsl_")
dir.create(psmsl_dir)
build_synthetic_psmsl_station(psmsl_dir, 1,  2011:2014, base_level = 7020, trend_mm_per_year = 3)
build_synthetic_psmsl_station(psmsl_dir, 61, 2011:2014, base_level = 6980, trend_mm_per_year = 4)

reference_period <- c("2011-01-01", "2013-12-31")  # 3 years
study_period      <- c("2011-01-01", "2014-12-31")  # 4 years
```

`reference_period` spans 3 full years rather than 2 or fewer. With only
1 year, standardisation would divide by an undefined per-month standard
deviation, and give an empty sea-level result — see
[`vignette("xaci-components")`](../articles/xaci-components.md). With
exactly 2, it would run into a subtler issue: standardising *n*
reference observations against their own mean/sd always satisfies two
constraints (they sum to 0, their squares sum to *n* - 1), leaving only
*n* - 2 “degrees of freedom” for the data itself. At `n = 2` that’s zero
— every month’s anomaly is forced to exactly `+1/sqrt(2)` or
`-1/sqrt(2)` (≈ ±0.71), for *every* component, regardless of the
underlying data. 3 reference years (1 degree of freedom per month) is
enough to break that shared artifact and keep this vignette quick to
build — a deliberate compromise, not a fully realistic reference sample
(with real, multi-decade data this is a non-issue either way).
`study_period` extends one year beyond it (2014), which isn’t
constrained this way at all and shows unambiguously genuine anomalies
instead, driven by the warming/drying trend built into the synthetic
data.

We use `"FRA"` as the `country_abbrev` here (rather than a fictitious
code) because the sea-level component looks up stations by matching this
value against the bundled PSMSL metadata’s `Country` column — see
[`vignette("xaci-components")`](../articles/xaci-components.md) for
details.

## Running the pipeline

With explicit `*_data_path` arguments,
[`calculate_aci()`](../reference/calculate_aci.md) skips the
ERA5-path-building step (normally driven by `years`) and goes straight
to computing every component, then combining them:

``` r

monthly_national_aci <- calculate_aci(
  country_abbrev           = "FRA",
  study_period               = study_period,
  reference_period            = reference_period,
  temperature_data_path      = t2m_file,
  precipitation_data_path    = tp_file,
  wind_u10_data_path          = u10_file,
  wind_v10_data_path          = v10_file,
  mask_data_path               = mask_file,
  sealevel_dir                  = psmsl_dir,
  granularity                    = "month",
  area                            = TRUE,
  factor                          = 1 / 5,
  admin_level                     = NULL,
  save                             = FALSE,
  computed_components              = FALSE
)

head(monthly_national_aci, 3)   # inside reference_period (2011-2013)
#>           drought wind precipitation       t10        t90   sealevel        ACI
#> 2011-01 -1.037071    0     0.2090946 0.8899792 -0.8430475 -0.5283510 -0.5128218
#> 2011-02 -1.039315    0     0.7125336 1.0498105 -0.9059700  0.9525916 -0.4023162
#> 2011-03 -1.041648    0     0.4047044 0.9271726 -1.0545163 -0.5195323 -0.5235652
tail(monthly_national_aci, 3)   # 2014, outside reference_period
#>           drought      wind precipitation       t10      t90   sealevel       ACI
#> 2014-10 1.0449282 0.2293907     -1.765073 -3.092075 1.334810 -0.4112348 0.7411315
#> 2014-11 0.9819440 0.2185185     -2.073301 -1.403643 1.668883 23.7643976 1.3370322
#> 2014-12 0.9149914 0.2007168     -1.883702 -1.183858 1.960784 -3.1812422 0.3346922
```

## Reading the output

For national, monthly output,
[`calculate_aci()`](../reference/calculate_aci.md) returns a
`data.frame` with one row per month (`"YYYY-MM"` row names) and one
column per component plus `ACI` itself:

``` r

colnames(monthly_national_aci)
#> [1] "drought"       "wind"          "precipitation" "t10"           "t90"           "sealevel"      "ACI"
class(monthly_national_aci)
#> [1] "data.frame"
```

Each component column is a standardised anomaly (roughly, number of
standard deviations from the reference-period mean for that calendar
month); `ACI` is their combination via the formula introduced in
[`vignette("xaci-intro")`](../articles/xaci-intro.md).

## Changing granularity without recomputing

Because the underlying grid-cell computation is independent from the
temporal aggregation step, you can request a different `granularity`
without paying the cost of recomputing every component — in real usage
this is best done via the `save` / `computed_components` caching pattern
shown in
[`vignette("xaci-components")`](../articles/xaci-components.md); here,
for the small synthetic example, we simply call
[`calculate_aci()`](../reference/calculate_aci.md) again:

``` r

seasonal_national_aci <- calculate_aci(
  country_abbrev           = "FRA",
  study_period               = study_period,
  reference_period            = reference_period,
  temperature_data_path      = t2m_file,
  precipitation_data_path    = tp_file,
  wind_u10_data_path          = u10_file,
  wind_v10_data_path          = v10_file,
  mask_data_path               = mask_file,
  sealevel_dir                  = psmsl_dir,
  granularity                    = "season",
  area                            = TRUE
)

seasonal_national_aci
#>              drought      wind precipitation          t10         t90    sealevel         ACI
#> 2011-DJF -1.03819317 0.0000000    0.46081409  0.969894853 -0.87450877  0.21212035 -0.45756897
#> 2011-JJA -1.05199807 0.0000000    0.25351878  1.108970234 -1.01137077 -0.01068346 -0.56172250
#> 2011-MAM -1.04410644 0.0000000    0.74793613  1.010036731 -1.07520863 -0.06092608 -0.46030786
#> 2011-SON -1.06092614 0.0000000    0.23764905  0.951405025 -0.96584416 -0.98209795 -0.56479728
#> 2012-DJF -0.30158844 0.0000000    0.74026939  0.297286437 -0.47628068 -0.54686923 -0.08543462
#> 2012-JJA  0.11374658 0.0000000    0.03022988 -0.499934012  0.50022588 -0.35847384  0.20623877
#> 2012-MAM  0.09501163 0.0000000    0.08630348 -0.040987960  0.25528758  0.22676501  0.10056609
#> 2012-SON  0.13575482 0.0000000   -0.03836602  0.003980472  0.03965165  0.20567830  0.03349916
#> 2013-DJF  0.68872008 0.0000000   -0.69087648 -0.571106166  0.72595319  0.31860023  0.26127365
#> 2013-JJA  0.93825149 0.0000000   -0.28374866 -0.609036222  0.51114490  0.36915730  0.35548373
#> 2013-MAM  0.94909481 0.0000000   -0.83423960 -0.969048771  0.81992105 -0.16583893  0.35974178
#> 2013-SON  0.92517132 0.0000000   -0.19928304 -0.955385496  0.92619251  0.77641966  0.53129812
#> 2014-DJF  1.27763927 0.1331712   -1.68680186 -1.205350954  1.51899049  1.26086507  0.51933136
#> 2014-JJA  1.21229289 0.2163282   -3.08625290 -3.170720717  3.87861529  5.99626705  1.26749184
#> 2014-MAM  1.35519305 0.2186778   -0.82412900 -1.785200078  2.61115904  0.75944621  1.01884428
#> 2014-SON  1.04371673 0.2258463   -1.61425154 -2.319881289  1.78284039  8.20681539  1.03834543
#> 2015-DJF  0.91499142 0.2007168   -1.88370235 -1.183857700  1.96078431 -3.18124219  0.33469221
```

## Grid-cell mode: output for mapping

Setting `area = FALSE` with `admin_level = NULL` switches to **grid-cell
mode**: instead of a national scalar per month, you get the full spatial
field for each component and for `ACI` itself, ready for mapping (see
[`vignette("xaci-visualization")`](../articles/xaci-visualization.md)):

``` r

grid_aci <- calculate_aci(
  country_abbrev           = "FRA",
  study_period               = study_period,
  reference_period            = reference_period,
  temperature_data_path      = t2m_file,
  precipitation_data_path    = tp_file,
  wind_u10_data_path          = u10_file,
  wind_v10_data_path          = v10_file,
  mask_data_path               = mask_file,
  sealevel_dir                  = psmsl_dir,
  granularity                    = "month",
  area                            = FALSE,
  admin_level                     = NULL,
  # See the note in vignette("xaci-components") on why this toy example
  # widens max_dist_km beyond its 500 km default.
  max_dist_km                     = 800
)

names(grid_aci)
#>  [1] "lon"           "lat"           "ACI"           "t90"           "t10"           "precipitation" "drought"      
#>  [8] "wind"          "sealevel"      "time"
dim(grid_aci$ACI)   # [lon x lat x time]
#> [1]  3  3 48
```

`grid_aci$lon` / `grid_aci$lat` are the coordinate vectors,
`grid_aci$time` the (aggregated) time steps, and `grid_aci$ACI`,
`grid_aci$t90`, … are `[lon x lat x time]` arrays, each self-sufficient
(carrying its own `lon`/`lat`/`time`/`country_abbrev` attributes) once
extracted from the list.

For aggregation at the level of administrative units (e.g. French
departments) instead of nationally or on the raw grid, see
[`vignette("xaci-admin-levels")`](../articles/xaci-admin-levels.md).
