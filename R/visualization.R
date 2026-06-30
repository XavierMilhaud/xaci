#' @title Visualization Functions for the ACI Package
#' @description A collection of ggplot2-based plotting functions to visualize
#'   the Actuarial Climate Index (ACI) and its components. Four families of
#'   plots are provided:
#'   \enumerate{
#'     \item \strong{Time series} of the global ACI (\code{plot_aci_timeseries})
#'     \item \strong{Component decomposition} stacked / faceted chart
#'       (\code{plot_aci_components})
#'     \item \strong{Geographic maps} from a NetCDF-derived array
#'       (\code{plot_aci_map})
#'     \item \strong{Distribution plots} - boxplots and density ridges
#'       (\code{plot_aci_distribution})
#'   }
#' @name visualization
NULL


utils::globalVariables(c("value", "period", "component", "lon", "lat", "ACI"))

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Validate that the ACI result data.frame has the expected columns
#' @noRd
.check_aci_df <- function(df) {
  required_fixed <- c("precipitation", "drought", "wind", "sealevel", "ACI")
  missing <- setdiff(required_fixed, colnames(df))
  if (length(missing) > 0L)
    stop("The data.frame is missing column(s): ",
         paste(missing, collapse = ", "))
  # Check that there is at least 2 columns of temperature (t<n>)
  t_cols <- grep("^t\\d+$", colnames(df), value = TRUE)
  if (length(t_cols) < 2L)
    stop("The data.frame must contain at least two temperature columns (e.g. t90, t10).")
  invisible(TRUE)
}

#' Parse row names of an ACI data.frame into Date objects
#' @noRd
.parse_dates <- function(df) {
  rn <- rownames(df)

  # Detect format for first element
  first <- rn[1]

  if (grepl("^\\d{4}-\\d{2}$", first)) {
    # Monthly : "2010-01" -> "2010-01-01"
    as.Date(paste0(rn, "-01"))

  } else if (grepl("^\\d{4}$", first)) {
    # Annually : "2010" -> "2010-01-01"
    as.Date(paste0(rn, "-01-01"))

  } else if (grepl("^\\d{4}-S\\d$", first)) {
    # Biannually : "2010-S1" -> "2010-01-01", "2010-S2" -> "2010-07-01"
    year <- as.integer(sub("-S\\d$", "", rn))
    sem  <- as.integer(sub("^\\d{4}-S", "", rn))
    month <- ifelse(sem == 1L, "01", "07")
    as.Date(sprintf("%d-%s-01", year, month))

  } else if (grepl("^\\d{4}-(DJF|MAM|JJA|SON)$", first)) {
    # Quarterly : "2010-DJF" -> "2010-01-01", etc.
    season_month <- c(DJF = "01", MAM = "03", JJA = "06", SON = "09")
    year   <- as.integer(sub("-(DJF|MAM|JJA|SON)$", "", rn))
    season <- sub("^\\d{4}-", "", rn)
    as.Date(sprintf("%d-%s-01", year, season_month[season]))

  } else {
    stop("Unrecognised row name format in ACI data.frame: '", first, "'.\n",
         "Expected one of: 'YYYY-MM', 'YYYY', 'YYYY-Sn', 'YYYY-DJF/MAM/JJA/SON'.")
  }
}

#' Default component palette (colour-blind-friendly)
#' @noRd
.component_colours <- function(col_high = "t90", col_low = "t10") {
  colours <- c(
    "#D62728",   # red   -> temperature high
    "#1F77B4",   # blue  -> temperature low
    "#2CA02C",   # green -> precipitation
    "#FF7F0E",   # orange -> drought
    "#9467BD",   # purple -> wind
    "#8C564B",   # brown  -> sealevel
    "grey30"     # grey -> ACI
  )
  names(colours) <- c(col_high, col_low,
                      "precipitation", "drought", "wind", "sealevel","ACI")
  colours
}

#' Infer high and low temperature column names from an ACI data.frame
#' @noRd
.infer_t_cols <- function(df) {
  t_cols <- grep("^t\\d+$", colnames(df), value = TRUE)
  if (length(t_cols) < 2L)
    stop("The data.frame must contain at least two temperature columns (e.g. t90, t10).")
  t_cols <- t_cols[order(as.integer(sub("^t", "", t_cols)))]
  list(low = t_cols[1], high = t_cols[2])
}

#' Extraire un data.frame lon/lat/value depuis un objet grid-cell ou admin
#'
#' Accepte soit un array nu portant ses propres attributs \code{lon}/
#' \code{lat}/\code{time} (ex: \code{grid$t90}, auto-suffisant depuis
#' \code{calculate_aci()}), soit une liste parent (\code{grid}) avec
#' \code{variable} indiquant le champ à extraire.
#' @noRd
.slice_to_df <- function(dataset, variable = NULL, time_index) {
  if (is.array(dataset)) {
    # Array nu et auto-suffisant (attributs lon/lat/time attaches par
    # calculate_aci() au niveau grid-cell).
    arr  <- dataset
    lons <- attr(dataset, "lon")
    lats <- attr(dataset, "lat")
    time <- attr(dataset, "time")
    if (is.null(lons) || is.null(lats))
      stop("This array has no `lon`/`lat` attributes attached. ",
           "Pass the parent list with `variable` instead, or use an array ",
           "returned by calculate_aci().")
  } else if (is.list(dataset)) {
    # Liste parent : soit la sortie grid-cell complete de calculate_aci()
    # (`variable` selectionne le champ a tracer), soit la sortie brute de
    # load_component() (un seul champ $data, sans nom de variable).
    if (!is.null(dataset[[variable]]) && is.array(dataset[[variable]])) {
      arr <- dataset[[variable]]
    } else if (!is.null(dataset$data) && is.array(dataset$data)) {
      arr <- dataset$data
    } else {
      stop("Variable '", variable, "' not found or not an array in dataset.")
    }
    lons <- if (!is.null(dataset$lon)) dataset$lon else attr(arr, "lon")
    lats <- if (!is.null(dataset$lat)) dataset$lat else attr(arr, "lat")
    time <- if (!is.null(dataset$time)) dataset$time else attr(arr, "time")
  } else {
    stop("`data` must be an array (with lon/lat attributes) or a list ",
         "(grid-cell output of calculate_aci()).")
  }

  if (identical(time_index, "mean")) {
    slice      <- apply(arr, c(1L, 2L), mean, na.rm = TRUE)
    time_label <- "temporal mean"
  } else {
    t <- as.integer(time_index)
    if (t < 1L || t > dim(arr)[3L])
      stop("`time_index` must be in [1, ", dim(arr)[3L], "] or \"mean\".")
    slice      <- arr[, , t]
    time_label <- if (!is.null(time)) as.character(time[t]) else as.character(t)
  }

  df <- data.frame(
    lon   = rep(lons, times = length(lats)),
    lat   = rep(lats, each  = length(lons)),
    value = as.vector(slice)
  )
  df <- df[!is.na(df$value), ]
  if (nrow(df) == 0L)
    stop("No non-NA value to plot: every grid cell is NA for this ",
         "variable/time_index. This points to an upstream issue in the ",
         "data itself (e.g. the whole component array is NA), not a ",
         "plotting issue. Inspect e.g. `sum(!is.na(<array>))` and ",
         "`dim(<array>)` before plotting.")
  list(df = df, time_label = time_label)
}

#' Construire le fond de carte du pays (frontiere nationale, GADM niveau 0)
#' @noRd
.country_basemap <- function(country_abbrev) {
  .load_admin_sf(country_abbrev, admin_level = 0)
}

#' Common ggplot2 theme layer to all maps (theming only, no coord)
#'
#' NB: deliberately does NOT include \code{coord_sf()}. ggplot2 silently
#' re-adds its own default \code{coord_sf()} (with no \code{xlim}/\code{ylim})
#' whenever a \code{geom_sf()} layer is added to a plot, which would override
#' any limits set here if \code{coord_sf()} were added before that layer (as
#' happens when a basemap is overlaid on a raster). Callers must add
#' \code{ggplot2::coord_sf(xlim = ..., ylim = ...)} themselves, as the LAST
#' element of the plot, i.e. after any \code{geom_sf()} layer.
#' @noRd
.map_theme <- function() {
  list(
    ggplot2::theme_minimal(base_size = 12),
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold"),
      panel.grid       = ggplot2::element_blank(),
      panel.border     = ggplot2::element_rect(colour = "grey40",
                                               fill = NA, linewidth = 0.4),
      axis.text        = ggplot2::element_text(size = 8),
      legend.position  = "right"
    )
  )
}

#' Compute a padded bounding box from lon/lat vectors
#' @noRd
.bbox_with_margin <- function(lon, lat, margin_frac = 0.05) {
  lon_rng <- range(lon, na.rm = TRUE)
  lat_rng <- range(lat, na.rm = TRUE)
  lon_pad <- diff(lon_rng) * margin_frac
  lat_pad <- diff(lat_rng) * margin_frac
  list(
    xlim = c(lon_rng[1] - lon_pad, lon_rng[2] + lon_pad),
    ylim = c(lat_rng[1] - lat_pad, lat_rng[2] + lat_pad)
  )
}

#' Standard diverging colour scale
#' @noRd
.diverging_scale <- function(var_label, palette, reverse, n_breaks) {
  ggplot2::scale_fill_distiller(
    palette   = palette,
    direction = if (reverse) -1L else 1L,
    n.breaks  = n_breaks,
    name      = var_label,
    na.value  = "grey90"
  )
}

#' Join ACI values per administrative units to sf geometry
#' @noRd
.admin_df_to_sf <- function(aci_admin_df, variable, time_index,
                            country_abbrev, admin_level, crs_metric) {
  # Repli sur les attributs attaches par calculate_aci() (mode admin) si
  # les arguments correspondants ne sont pas fournis explicitement.
  if (is.null(country_abbrev)) country_abbrev <- attr(aci_admin_df, "country_abbrev")
  if (is.null(admin_level))    admin_level    <- attr(aci_admin_df, "admin_level")
  if (is.null(crs_metric))     crs_metric     <- attr(aci_admin_df, "crs_metric")
  if (is.null(crs_metric))     crs_metric     <- 4326

  if (is.null(country_abbrev) || is.null(admin_level))
    stop("`country_abbrev` and `admin_level` could not be determined.\n",
         "Provide them explicitly, or use a data.frame produced by ",
         "calculate_aci(admin_level = ...), which carries them as attributes.")

  # aci_admin_df : data.frame with columns ACI_<unit>, rownames = periods
  # Extract the values for specified time_index
  col_pattern <- paste0("^", variable, "_")
  cols <- grep(col_pattern, colnames(aci_admin_df), value = TRUE)
  if (length(cols) == 0L)
    stop("No column matching '", variable, "_*' in the admin data.frame.")

  if (identical(time_index, "mean")) {
    vals       <- colMeans(aci_admin_df[, cols, drop = FALSE], na.rm = TRUE)
    time_label <- "temporal mean"
  } else {
    t <- as.integer(time_index)
    if (t < 1L || t > nrow(aci_admin_df))
      stop("`time_index` must be in [1, ", nrow(aci_admin_df), "] or \"mean\".")
    vals       <- unlist(aci_admin_df[t, cols, drop = FALSE])
    time_label <- rownames(aci_admin_df)[t]
  }

  # Name of units (without prefix "ACI_" etc.)
  units <- sub(col_pattern, "", cols)
  value_df <- data.frame(name = units, value = as.numeric(vals))

  # Polygons via GADM (genuinely respects admin_level, contrairement a
  # l'ancien appel rnaturalearth::ne_states() qui l'ignorait silencieusement)
  admin_sf <- .load_admin_sf(country_abbrev, admin_level)
  admin_sf <- sf::st_transform(admin_sf, crs_metric)
  merged   <- merge(admin_sf, value_df, by = "name", all.x = TRUE)

  list(sf = merged, time_label = time_label)
}

# ---------------------------------------------------------------------------
# 1. ACI : Time-series plot
# ---------------------------------------------------------------------------

#' Plot the ACI global time series
#'
#' Draws a line chart of the monthly ACI values returned by
#' \code{\link{calculate_aci}}, with an optional smoothed
#' trend and a horizontal zero reference line.
#'
#' @param aci_df     A \code{data.frame} returned by \code{calculate_aci()}
#'   with \code{area = TRUE}. Must contain at least the column \code{ACI} and
#'   have month-end dates as row names.
#' @param smooth     Logical. If \code{TRUE} (default), a LOESS smoothing
#'   curve is overlaid in red.
#' @param span       Numeric span for the LOESS smoother. Default \code{0.2}.
#' @param title      Plot title. Default \code{"Actuarial Climate Index (ACI)"}.
#' @param colour     Line colour for the raw ACI series.
#'   Default \code{"#1F77B4"} (blue).
#' @param fill_area  Logical. If \code{TRUE} (default), fills the area between
#'   the ACI curve and zero.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' #result <- calculate_aci(...)
#' #plot_aci_timeseries(result)
#' aci_df <- readRDS("results/FRA/aci_df_example")
#' plot_aci_timeseries(aci_df)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_hline geom_ribbon geom_line
#'   geom_smooth scale_x_date labs theme_minimal theme element_text
#' @export
plot_aci_timeseries <- function(aci_df,
                                smooth     = TRUE,
                                span       = 0.2,
                                title      = "Actuarial Climate Index (ACI)",
                                colour     = "#1F77B4",
                                fill_area  = TRUE) {
  .check_aci_df(aci_df)

  granularity_label <- if (grepl("^\\d{4}$", rownames(aci_df)[1])) "Annual" else
    if (grepl("^\\d{4}-S", rownames(aci_df)[1])) "Semester" else
      if (grepl("(DJF|MAM|JJA|SON)$", rownames(aci_df)[1])) "Seasonal" else
        "Monthly"

  df <- data.frame(
    date = .parse_dates(aci_df),
    ACI  = aci_df$ACI
  )

  p <- ggplot2::ggplot(df, ggplot2::aes(x = date, y = ACI)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60",
                        linetype = "dashed", linewidth = 0.4)

  if (fill_area) {
    p <- p +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = pmin(ACI, 0), ymax = pmax(ACI, 0)),
        fill  = colour,
        alpha = 0.15
      )
  }

  p <- p +
    ggplot2::geom_line(colour = colour, linewidth = 0.7)

  if (smooth) {
    p <- p +
      ggplot2::geom_smooth(
        method  = "loess",
        formula = y ~ x,
        span    = span,
        colour  = "#D62728",
        se      = TRUE,
        fill    = "#D62728",
        alpha   = 0.15,
        linewidth = 1
      )
  }

  p +
    ggplot2::scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
    ggplot2::labs(
      title    = title,
      subtitle = paste(granularity_label, "values with LOESS trend"),
      x        = NULL,
      y        = "Standardised ACI"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      axis.text.x   = ggplot2::element_text(angle = 30, hjust = 1),
      panel.grid.minor = ggplot2::element_blank()
    )
}

# ---------------------------------------------------------------------------
# 2. Time series: component decomposition plot
# ---------------------------------------------------------------------------

#' Plot the ACI component decomposition
#'
#' Produces either a \strong{bar chart} (one panel per component,
#' \code{type = "bar"}) or a \strong{stacked area chart}
#' (\code{type = "stacked"}) showing the six standardised components that
#' make up the ACI.
#'
#' @param aci_df  A \code{data.frame} returned by \code{calculate_aci()}
#'   with \code{area = TRUE}
#' @param type    \code{"facet"} (default) or \code{"stacked"}.
#' @param components Character vector of component names to include.
#'   Default: all six \code{c("t90","t10","precipitation","drought","wind","sealevel")}.
#' @param title   Plot title.
#' @param subtitle Character or \code{NULL}. Optional subtitle displayed below
#'   the title. Default \code{NULL}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' plot_aci_components(result, type = "bar")
#' plot_aci_components(result, type = "stacked", components = c("t90","t10"))
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_area geom_line facet_wrap scale_fill_manual
#'   scale_colour_manual scale_x_date labs theme_minimal theme element_text
#'   geom_hline
#' @importFrom tidyr pivot_longer
#' @export
plot_aci_components <- function(aci_df,
                                type       = c("stacked", "line", "bar"),
                                components = NULL,
                                title      = "ACI Component Contributions",
                                subtitle   = NULL) {
  type <- match.arg(type)
  .check_aci_df(aci_df)

  # Infer temperature columns
  t_cols     <- .infer_t_cols(aci_df)
  col_high   <- t_cols$high
  col_low    <- t_cols$low
  all_components <- c(col_high, col_low,
                      "precipitation", "drought", "wind", "sealevel")

  # Default names of components
  if (is.null(components)) components <- all_components

  # Validate specified components
  unknown <- setdiff(components, all_components)
  if (length(unknown) > 0L)
    stop("Unknown component(s): ", paste(unknown, collapse = ", "),
         "\nAvailable: ", paste(all_components, collapse = ", "))

  colours <- .component_colours(col_high, col_low)

  # Labels dynamiques
  labels_map <- c(
    setNames(
      c(paste0("T", sub("^t", "", col_high), " (hot)"),
        paste0("T", sub("^t", "", col_low),  " (cold)")),
      c(col_high, col_low)
    ),
    precipitation = "Precipitation",
    drought       = "Drought",
    wind          = "Wind",
    sealevel      = "Sea level"
  )

  df_long <- data.frame(
    date      = rep(.parse_dates(aci_df), times = length(components)),
    component = rep(components, each = nrow(aci_df)),
    value     = unlist(aci_df[, components, drop = FALSE])
  )
  df_long$component <- factor(df_long$component, levels = components)

  p <- ggplot2::ggplot(df_long,
                       ggplot2::aes(x = date, y = value,
                                    fill    = if (type != "line") component else NULL,
                                    colour  = if (type == "line") component else NULL,
                                    group   = component))

  if (type == "stacked") {
    p <- p +
      ggplot2::geom_area(alpha = 0.8, position = "stack") +
      ggplot2::scale_fill_manual(values = colours[components],
                                 labels = labels_map[components],
                                 name   = "Component")
  } else if (type == "line") {
    p <- p +
      ggplot2::geom_line(linewidth = 0.8) +
      ggplot2::scale_colour_manual(values = colours[components],
                                   labels = labels_map[components],
                                   name   = "Component")
  } else {
    p <- p +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::scale_fill_manual(values = colours[components],
                                 labels = labels_map[components],
                                 name   = "Component")
  }

  p +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    ggplot2::labs(
      title    = title,
      subtitle = subtitle,
      x        = NULL,
      y        = "Standardised anomaly"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")
}

# ---------------------------------------------------------------------------
# 4. Distribution / boxplot
# ---------------------------------------------------------------------------

#' Plot the distribution of ACI components
#'
#' Displays the statistical distribution of each standardised component (and
#' optionally the global ACI) as \strong{boxplots} (\code{type = "boxplot"}),
#' \strong{violin plots} (\code{type = "violin"}), or
#' \strong{density curves} (\code{type = "density"}).
#'
#' @param aci_df     A \code{data.frame} returned by \code{calculate_aci()}.
#' @param components Character vector of component columns to include.
#' @param include_aci Logical. If \code{TRUE}, adds the composite \code{ACI}
#'   column to the distribution plot alongside the components. Default
#'   \code{FALSE}.
#' @param type       \code{"boxplot"} (default), \code{"violin"}, or
#'   \code{"density"}.
#' @param title      Plot title.
#' @param subtitle Character or \code{NULL}. Optional subtitle displayed below
#'   the title. Default \code{NULL}.
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' plot_aci_distribution(result, type = "violin")
#' plot_aci_distribution(result, type = "density")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_boxplot geom_violin geom_jitter
#'   geom_density scale_fill_manual scale_colour_manual scale_x_discrete
#'   labs theme_minimal theme element_text geom_hline after_stat
#' @export
plot_aci_distribution <- function(aci_df,
                                  components   = NULL,
                                  include_aci  = FALSE,
                                  type         = c("boxplot", "violin", "density"),
                                  title        = "ACI Component Distributions",
                                  subtitle     = NULL) {
  type <- match.arg(type)
  .check_aci_df(aci_df)

  # Infer temperature columns
  t_cols     <- .infer_t_cols(aci_df)
  col_high   <- t_cols$high
  col_low    <- t_cols$low
  all_components <- c(col_high, col_low,
                      "precipitation", "drought", "wind", "sealevel")

  if (is.null(components)) components <- all_components

  unknown <- setdiff(components, all_components)
  if (length(unknown) > 0L)
    stop("Unknown component(s): ", paste(unknown, collapse = ", "),
         "\nAvailable: ", paste(all_components, collapse = ", "))

  if (include_aci) components <- c(components, "ACI")

  colours <- .component_colours(col_high, col_low)

  labels_map <- c(
    setNames(
      c(paste0("T", sub("^t", "", col_high), "\n(hot)"),
        paste0("T", sub("^t", "", col_low),  "\n(cold)")),
      c(col_high, col_low)
    ),
    precipitation = "Precip.\n",
    drought       = "Drought\n",
    wind          = "Wind\n",
    sealevel      = "Sea\nlevel",
    ACI           = "ACI"
  )

  df_long <- data.frame(
    component = rep(components, each = nrow(aci_df)),
    value     = unlist(aci_df[, components, drop = FALSE])
  )
  df_long$component <- factor(df_long$component,
                              levels   = components,
                              labels   = labels_map[components])

  p <- ggplot2::ggplot(df_long,
                       ggplot2::aes(x = component, y = value,
                                    fill = component, colour = component))

  if (type == "boxplot") {
    p <- p + ggplot2::geom_boxplot(alpha = 0.6, outlier.size = 1)
  } else if (type == "violin") {
    p <- p + ggplot2::geom_violin(alpha = 0.6, draw_quantiles = c(0.25, 0.5, 0.75))
  } else {
    p <- p + ggplot2::geom_density(alpha = 0.4)
  }

  # df_long$component porte les libelles d'affichage (labels_map), pas les
  # noms bruts des composantes : on renomme le vecteur de couleurs en
  # consequence pour que scale_fill/colour_manual matchent les niveaux reels.
  colours_disp <- colours[components]
  names(colours_disp) <- labels_map[components]

  p +
    ggplot2::scale_fill_manual(values   = colours_disp,   guide = "none") +
    ggplot2::scale_colour_manual(values = colours_disp,   guide = "none") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
    ggplot2::labs(
      title    = title,
      subtitle = subtitle,
      x        = NULL,
      y        = "Standardised anomaly"
    ) +
    ggplot2::theme_minimal(base_size = 12)
}

# ---------------------------------------------------------------------------
# 5. Convenience wrapper - dashboard of all plots
# ---------------------------------------------------------------------------

#' Plot a summary dashboard of the ACI results
#'
#' Arranges the four core plots (time series, component facets, boxplots and
#' density) into a single patchwork figure.  Requires the \pkg{patchwork}
#' package to be installed.
#'
#' @param aci_df  A \code{data.frame} returned by \code{calculate_aci()}.
#' @param title   Overall figure title. Default \code{"ACI Summary Dashboard"}.
#'
#' @return A \pkg{patchwork} object (inherits from \code{ggplot}).
#'
#' @examples
#' \dontrun{
#' plot_aci_dashboard(result)
#' }
#'
#' @export
plot_aci_dashboard <- function(aci_df,
                               title = "ACI Summary Dashboard") {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required for plot_aci_dashboard().\n",
         "Install it with: install.packages(\"patchwork\")")
  }

  p1 <- plot_aci_timeseries(aci_df, title = "ACI Time Series")
  p2 <- plot_aci_components(aci_df, type = "facet",
                            title = "Components")
  p3 <- plot_aci_distribution(aci_df, type = "boxplot",
                              title = "Distributions")
  p4 <- plot_aci_distribution(aci_df, type = "density",
                              title = "Distributions")

  patchwork::wrap_plots(p1, p2, p3, p4, ncol = 2) +
    patchwork::plot_annotation(
      title = title,
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(size = 16, face = "bold",
                                           hjust = 0.5)
      )
    )
}


#' Plot a spatial map of the ACI or one of its components
#'
#' Handles three types of input, at any spatial aggregation level produced
#' by \code{calculate_aci()} (grid-cell, admin level 1, admin level 2, ...):
#' \itemize{
#'   \item \strong{Bare array} (e.g. \code{grid$t90}, \code{grid$ACI}): a
#'     single component array as stored in the grid-cell list returned by
#'     \code{calculate_aci(area = FALSE)}. Self-describing via its attached
#'     \code{lon}/\code{lat}/\code{time}/\code{country_abbrev} attributes, so
#'     it can be passed on its own. Renders a raster map. Since \code{ACI}
#'     and its components share the exact same array structure, this works
#'     identically whether \code{variable} is a component or the ACI itself.
#'   \item \strong{Grid-cell list} (the full output of
#'     \code{calculate_aci(area = FALSE)}, or of \code{load_component()}):
#'     \code{variable} selects which field of the list to map. Renders a
#'     raster map.
#'   \item \strong{Administrative} \code{data.frame} (output of
#'     \code{calculate_aci(admin_level = N)}): renders a choropleth map.
#'     \code{country_abbrev}/\code{admin_level}/\code{crs_metric} are read
#'     from the data.frame's attributes when not supplied explicitly.
#' }
#'
#' @param data         A bare array (component or ACI, with attributes), a
#'   named list (grid-cell / raw dataset), or a \code{data.frame} (admin
#'   output).
#' @param variable     Name of the variable to map when \code{data} is a
#'   list. For grid-cell output use e.g. \code{"ACI"}, \code{"drought"},
#'   \code{"t90"}. For admin output use \code{"ACI"} (columns must be named
#'   \code{ACI_<unit>}). Ignored when \code{data} is already a bare array.
#'   Default \code{"ACI"}.
#' @param time_index   Integer (1-based time slice) or \code{"mean"} (default)
#'   to plot the temporal average.
#' @param country_abbrev Three-letter ISO code. Required for admin maps and
#'   for overlaying country/region borders on raster maps. If \code{NULL}
#'   and \code{data} carries a \code{country_abbrev} attribute (as set by
#'   \code{calculate_aci()}), that value is used automatically.
#' @param admin_level  Integer or \code{NULL}. Required for admin maps; read
#'   from \code{data}'s attributes when not supplied.
#' @param crs_metric   EPSG code used for admin choropleth projection only.
#'   If \code{NULL}, read from \code{data}'s attributes, falling back to
#'   \code{4326}. Ignored (with a warning if explicitly set) in raster
#'   (grid-cell) mode: the grid-cell data stays in its native EPSG:4326
#'   (lon/lat), since reprojecting a fixed regular raster to a different
#'   CRS would distort/break it; the basemap is reprojected to 4326 to
#'   match it instead.
#' @param var_label    Colour-bar label. Default \code{"Standardised anomaly"}.
#' @param title        Plot title. Default \code{"ACI Spatial Distribution"}.
#' @param palette      RColorBrewer palette. Default \code{"RdBu"}.
#' @param reverse      Logical. Reverse colour scale? Default \code{TRUE}.
#' @param n_breaks     Number of colour breaks. Default \code{9}.
#' @param borders      Logical. Overlay administrative borders? Default
#'   \code{TRUE}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' grid <- calculate_aci(..., area = FALSE)
#'
#' # Bare array, self-describing thanks to its attributes
#' plot_aci_map(grid$t90, time_index = "mean")
#'
#' # Equivalent, via the parent list and `variable`
#' plot_aci_map(grid, variable = "t90", time_index = "mean")
#'
#' # Admin choropleth: country_abbrev/admin_level read from attributes
#' admin <- calculate_aci(..., admin_level = 1)
#' plot_aci_map(admin, variable = "ACI", time_index = 1)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_raster geom_sf coord_sf labs
#' @importFrom sf st_transform
#' @export
plot_aci_map <- function(data,
                         variable      = "ACI",
                         time_index    = "mean",
                         country_abbrev = NULL,
                         admin_level   = NULL,
                         crs_metric    = NULL,
                         var_label     = "Standardised anomaly",
                         title         = "ACI Spatial Distribution",
                         palette       = "RdBu",
                         reverse       = TRUE,
                         n_breaks      = 9,
                         borders       = TRUE) {

  # ---- Mode administratif : choropleth ----
  if (is.data.frame(data)) {
    res <- .admin_df_to_sf(data, variable, time_index,
                           country_abbrev, admin_level, crs_metric)
    p <- ggplot2::ggplot(res$sf) +
      ggplot2::geom_sf(ggplot2::aes(fill = value), colour = "white",
                       linewidth = 0.3) +
      .diverging_scale(var_label, palette, reverse, n_breaks) +
      ggplot2::labs(title    = title,
                    subtitle = paste0("Time slice: ", res$time_label),
                    x = NULL, y = NULL) +
      .map_theme() +
      ggplot2::coord_sf(expand = FALSE)
    return(p)
  }

  # ---- Mode grid-cell : raster (array nu ou liste parente + `variable`) ----
  res <- .slice_to_df(data, variable, time_index)

  # Repli sur l'attribut country_abbrev (array nu auto-suffisant), pour la
  # superposition des frontieres administratives.
  if (is.null(country_abbrev)) {
    country_abbrev <- if (is.array(data)) attr(data, "country_abbrev")
    else if (is.list(data)) attr(data[[variable]], "country_abbrev")
    else NULL
  }

  # En mode raster, les donnees sont figees sur une grille reguliere en
  # lon/lat (EPSG:4326, le CRS natif d'ERA5) : geom_raster() exige une
  # grille reguliere, donc on ne reprojette JAMAIS les donnees elles-memes.
  # `crs_metric` ne s'applique qu'au choropleth administratif ci-dessus, ou
  # c'est la geometrie (et non un raster fige) qui est reprojetee. Ici, on
  # force systematiquement le fond de carte dans le meme CRS que le raster
  # (4326) pour garantir l'alignement, et on avertit si l'utilisateur avait
  # explicitement demande une autre projection.
  if (!is.null(crs_metric) && !isTRUE(all.equal(crs_metric, 4326))) {
    warning("`crs_metric` (", crs_metric, ") is ignored in raster mode: ",
            "the grid-cell data stays in its native EPSG:4326 (lon/lat), ",
            "and the basemap is reprojected to match it, to keep both ",
            "layers aligned. `crs_metric` only applies to admin choropleths.")
  }
  basemap_crs <- 4326

  # Borne l'etendue de la carte a la bbox des donnees (pas a celle du fond de
  # carte), pour eviter que des territoires eloignes (ex: DOM-TOM francais,
  # inclus dans la frontiere nationale GADM niveau 0) n'etirent le panneau.
  bbox <- .bbox_with_margin(res$df$lon, res$df$lat)

  p <- ggplot2::ggplot(res$df,
                       ggplot2::aes(x = lon, y = lat, fill = value)) +
    ggplot2::geom_raster(interpolate = TRUE) +
    .diverging_scale(var_label, palette, reverse, n_breaks) +
    ggplot2::labs(title    = title,
                  subtitle = paste0("Time slice: ", res$time_label),
                  x = "Longitude", y = "Latitude") +
    .map_theme()

  # Superposition of admin frontiers
  if (borders && !is.null(country_abbrev)) {
    basemap <- .country_basemap(country_abbrev)
    basemap <- sf::st_transform(basemap, basemap_crs)
    p <- p +
      ggplot2::geom_sf(data = basemap, fill = NA, colour = "grey30",
                       linewidth = 0.25, inherit.aes = FALSE)
  }

  # `coord_sf()` DOIT etre ajoute en tout dernier : ggplot2 reinjecte
  # silencieusement un coord_sf() par defaut (sans xlim/ylim) des qu'une
  # couche geom_sf() est ajoutee, ce qui ecraserait nos bornes si elles
  # etaient fixees avant la couche basemap ci-dessus (c'etait le bug:
  # malgre la bbox calculee, la carte continuait a inclure les DOM-TOM
  # car coord_sf() etait (re)defini sans limites lors de l'ajout du
  # fond de carte).
  p + ggplot2::coord_sf(xlim = bbox$xlim, ylim = bbox$ylim, expand = FALSE)
}

#' Plot the temporal mean of an ACI variable over a sub-period
#'
#' Convenience wrapper around \code{plot_aci_map} that computes the mean
#' over a user-defined date range rather than a single time index.
#'
#' @param data         Bare array, grid-cell list, or admin \code{data.frame}.
#' @param variable     Variable to map. Default \code{"ACI"}. Ignored when
#'   \code{data} is already a bare array.
#' @param period       Character vector \code{c("start", "end")} in the same
#'   format as the \code{time} field of \code{data} (e.g.
#'   \code{c("2000-01", "2010-12")} for monthly). If \code{NULL} (default),
#'   uses the full time range.
#' @param country_abbrev Three-letter ISO code.
#' @param admin_level  Integer or \code{NULL}.
#' @param crs_metric   EPSG code. Default \code{NULL} (read from attributes,
#'   falling back to \code{4326}).
#' @param var_label    Colour-bar label.
#' @param title        Plot title.
#' @param palette      RColorBrewer palette. Default \code{"RdBu"}.
#' @param reverse      Logical. Default \code{TRUE}.
#' @param n_breaks     Integer. Default \code{9}.
#' @param borders      Logical. Default \code{TRUE}.
#'
#' @return A \code{ggplot} object.
#' @export
plot_aci_map_mean <- function(data,
                              variable       = "ACI",
                              period         = NULL,
                              country_abbrev = NULL,
                              admin_level    = NULL,
                              crs_metric     = NULL,
                              var_label      = "Standardised anomaly",
                              title          = NULL,
                              palette        = "RdBu",
                              reverse        = TRUE,
                              n_breaks       = 9,
                              borders        = TRUE) {

  # Filter on subperiod if asked
  if (!is.null(period)) {
    if (is.data.frame(data)) {
      rn   <- rownames(data)
      keep <- rn >= period[1] & rn <= period[2]
      data <- data[keep, , drop = FALSE]
    } else if (is.array(data)) {
      # Array nu auto-suffisant : filtre via son propre attribut `time`,
      # puis ré-attache les attributs sur le sous-array resultant.
      time_chr <- as.character(attr(data, "time"))
      keep     <- time_chr >= period[1] & time_chr <= period[2]
      filtered <- data[, , keep, drop = FALSE]
      data <- .attach_spatial_attrs(
        filtered,
        lon            = attr(data, "lon"),
        lat            = attr(data, "lat"),
        time           = attr(data, "time")[keep],
        country_abbrev = attr(data, "country_abbrev")
      )
    } else {
      time_chr <- as.character(data$time)
      keep     <- time_chr >= period[1] & time_chr <= period[2]
      # Sous-ensemble de chaque array variable
      for (nm in names(data)) {
        if (is.array(data[[nm]]) && length(dim(data[[nm]])) == 3L) {
          data[[nm]] <- data[[nm]][, , keep, drop = FALSE]
        }
      }
      data$time <- data$time[keep]
    }
  }

  ttl <- if (is.null(title)) {
    lbl <- if (is.null(period)) "full period" else
      paste(period, collapse = " - ")
    paste0(variable, " - mean over ", lbl)
  } else title

  plot_aci_map(data,
               variable       = variable,
               time_index     = "mean",
               country_abbrev = country_abbrev,
               admin_level    = admin_level,
               crs_metric     = crs_metric,
               var_label      = var_label,
               title          = ttl,
               palette        = palette,
               reverse        = reverse,
               n_breaks       = n_breaks,
               borders        = borders)
}

#' Animate the ACI or a component over time
#'
#' Produces a GIF animation (via \pkg{gganimate}) showing the evolution of
#' a spatial variable at each time step. Requires \pkg{gganimate} and
#' \pkg{gifski} (or \pkg{png}) to be installed.
#'
#' @param data         Grid-cell list or admin \code{data.frame}.
#' @param variable     Variable to animate. Default \code{"ACI"}.
#' @param country_abbrev Three-letter ISO code.
#' @param admin_level  Integer or \code{NULL}.
#' @param crs_metric   EPSG code, used for admin choropleth projection only.
#'   Default \code{4326}. Ignored (with a warning if explicitly set to
#'   something else) in raster (grid-cell) mode, where the basemap is always
#'   reprojected to EPSG:4326 to match the fixed lon/lat raster grid.
#' @param var_label    Colour-bar label. Default \code{"Standardised anomaly"}.
#' @param title        Plot title. Default \code{"ACI over time"}.
#' @param palette      RColorBrewer palette. Default \code{"RdBu"}.
#' @param reverse      Logical. Default \code{TRUE}.
#' @param n_breaks     Integer. Default \code{9}.
#' @param borders      Logical. Default \code{TRUE}.
#' @param fps          Frames per second. Default \code{4}.
#' @param width        Output width in pixels. Default \code{800}.
#' @param height       Output height in pixels. Default \code{600}.
#' @param save_path    File path for the output GIF. If \code{NULL} (default),
#'   the animation is rendered to the viewer but not saved.
#'
#' @return A \pkg{gganimate} animation object (invisibly).
#'
#' @examples
#' \dontrun{
#' grid <- calculate_aci(..., area = FALSE)
#' animate_aci_map(grid, variable = "ACI", country_abbrev = "FRA",
#'                 fps = 2, save_path = "aci_animation.gif")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_raster geom_sf labs
#' @export
animate_aci_map <- function(data,
                            variable       = "ACI",
                            country_abbrev = NULL,
                            admin_level    = NULL,
                            crs_metric     = 4326,
                            var_label      = "Standardised anomaly",
                            title          = "ACI over time",
                            palette        = "RdBu",
                            reverse        = TRUE,
                            n_breaks       = 9,
                            borders        = TRUE,
                            fps            = 4,
                            width          = 800,
                            height         = 600,
                            save_path      = NULL) {

  for (pkg in c("gganimate", "gifski")) {
    if (!requireNamespace(pkg, quietly = TRUE))
      stop("Package '", pkg, "' is required for animate_aci_map().\n",
           "Install with: install.packages(\"", pkg, "\")")
  }

  # ---- Build data.frame long (all periods) ----
  if (is.data.frame(data)) {
    # admin mode : one line per period, one column per unit
    if (is.null(country_abbrev) || is.null(admin_level))
      stop("For admin maps, provide `country_abbrev` and `admin_level`.")

    nt     <- nrow(data)
    frames <- lapply(seq_len(nt), function(t) {
      res    <- .admin_df_to_sf(data, variable, t,
                                country_abbrev, admin_level, crs_metric)
      sf_t   <- res$sf
      sf_t$period <- res$time_label
      sf_t
    })
    df_all <- do.call(rbind, frames)

    p <- ggplot2::ggplot(df_all) +
      ggplot2::geom_sf(ggplot2::aes(fill = value), colour = "white",
                       linewidth = 0.3) +
      .diverging_scale(var_label, palette, reverse, n_breaks) +
      ggplot2::labs(title    = paste0(title, " - {closest_state}"),
                    x = NULL, y = NULL) +
      .map_theme() +
      ggplot2::coord_sf(expand = FALSE) +
      gganimate::transition_states(period, transition_length = 1,
                                   state_length = 2) +
      gganimate::ease_aes("linear")

  } else {
    # Mode grid-cell
    arr  <- data[[variable]]
    if (!is.array(arr) || length(dim(arr)) != 3L)
      stop("Variable '", variable, "' must be a 3D array [lon x lat x time].")

    nt   <- dim(arr)[3L]
    lons <- data$lon
    lats <- data$lat

    # Construire le data.frame long [lon x lat x time]
    frames <- lapply(seq_len(nt), function(t) {
      slice  <- arr[, , t]
      period <- as.character(data$time[t])
      df     <- data.frame(
        lon    = rep(lons, times = length(lats)),
        lat    = rep(lats, each  = length(lons)),
        value  = as.vector(slice),
        period = period
      )
      df[!is.na(df$value), ]
    })
    df_all <- do.call(rbind, frames)

    bbox <- .bbox_with_margin(df_all$lon, df_all$lat)

    p <- ggplot2::ggplot(df_all,
                         ggplot2::aes(x = lon, y = lat, fill = value)) +
      ggplot2::geom_raster(interpolate = TRUE) +
      .diverging_scale(var_label, palette, reverse, n_breaks) +
      ggplot2::labs(title    = paste0(title, " - {closest_state}"),
                    x = "Longitude", y = "Latitude") +
      .map_theme() +
      gganimate::transition_states(period, transition_length = 1,
                                   state_length = 2) +
      gganimate::ease_aes("linear")

    if (!is.null(crs_metric) && !isTRUE(all.equal(crs_metric, 4326))) {
      warning("`crs_metric` (", crs_metric, ") is ignored in raster mode: ",
              "the grid-cell data stays in its native EPSG:4326 (lon/lat), ",
              "and the basemap is reprojected to match it, to keep both ",
              "layers aligned. `crs_metric` only applies to admin choropleths.")
    }

    if (borders && !is.null(country_abbrev)) {
      basemap <- .country_basemap(country_abbrev)
      basemap <- sf::st_transform(basemap, 4326)
      p <- p +
        ggplot2::geom_sf(data = basemap, fill = NA, colour = "grey30",
                         linewidth = 0.25, inherit.aes = FALSE)
    }

    # coord_sf() ajoute en dernier, apres la couche geom_sf du fond de
    # carte (cf. plot_aci_map pour l'explication detaillee du bug evite).
    p <- p + ggplot2::coord_sf(xlim = bbox$xlim, ylim = bbox$ylim,
                               expand = FALSE)
  }

  # ---- Rendu ----
  anim <- gganimate::animate(p, fps = fps, width = width, height = height,
                             renderer = gganimate::gifski_renderer())
  if (!is.null(save_path)) {
    gganimate::anim_save(save_path, animation = anim)
    message("Animation saved to: ", save_path)
  }
  invisible(anim)
}
