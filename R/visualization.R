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
#'     \item \strong{Distribution plots} – boxplots and density ridges
#'       (\code{plot_aci_distribution})
#'   }
#' @name visualization
NULL


utils::globalVariables(c("value", "component", "lon", "lat", "ACI"))

# ---------------------------------------------------------------------------
# Internal helpers
# ---------------------------------------------------------------------------

#' Validate that the ACI result data.frame has the expected columns
#' @noRd
.check_aci_df <- function(df) {
  required <- c("t90", "t10", "precipitation", "drought", "wind",
                "sealevel", "ACI")
  missing  <- setdiff(required, colnames(df))
  if (length(missing) > 0L) {
    stop("The data.frame is missing column(s): ",
         paste(missing, collapse = ", "),
         ".\nExpected output from calculate_aci().")
  }
  invisible(TRUE)
}

#' Parse row names of an ACI data.frame into Date objects
#' @noRd
.parse_dates <- function(df) {
  as.Date(rownames(df))
}

#' Default component palette (colour-blind-friendly)
#' @noRd
.component_colours <- function() {
  c(
    t90           = "#D62728",   # red
    t10           = "#1F77B4",   # blue
    precipitation = "#2CA02C",   # green
    drought       = "#FF7F0E",   # orange
    wind          = "#9467BD",   # purple
    sealevel      = "#17BECF"    # cyan
  )
}

# ---------------------------------------------------------------------------
# 1. Time-series plot
# ---------------------------------------------------------------------------

#' Plot the ACI global time series
#'
#' Draws a line chart of the monthly ACI values returned by
#' \code{\link{calculate_aci}}, with an optional smoothed trend and a
#' horizontal zero reference line.
#'
#' @param aci_df     A \code{data.frame} returned by \code{calculate_aci()}.
#'   Must contain at least the column \code{ACI} and have month-end dates as
#'   row names.
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
#' aci_df <- readRDS("data/aci_df_example")
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
      subtitle = "Monthly values with LOESS trend",
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
# 2. Component decomposition plot
# ---------------------------------------------------------------------------

#' Plot the ACI component decomposition
#'
#' Produces either a \strong{faceted line chart} (one panel per component,
#' \code{type = "facet"}) or a \strong{stacked area chart}
#' (\code{type = "stacked"}) showing the six standardised components that
#' make up the ACI.
#'
#' @param aci_df  A \code{data.frame} returned by \code{calculate_aci()}.
#' @param type    \code{"facet"} (default) or \code{"stacked"}.
#' @param title   Plot title.
#' @param components Character vector of component names to include.
#'   Default: all six \code{c("t90","t10","precipitation","drought","wind","sealevel")}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' plot_aci_components(result, type = "facet")
#' plot_aci_components(result, type = "stacked")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_area geom_line facet_wrap scale_fill_manual
#'   scale_colour_manual scale_x_date labs theme_minimal theme element_text
#'   geom_hline
#' @importFrom tidyr pivot_longer
#' @export
plot_aci_components <- function(aci_df,
                                type       = c("facet", "stacked"),
                                title      = "ACI Component Decomposition",
                                components = c("t90", "t10", "precipitation",
                                               "drought", "wind", "sealevel")) {
  .check_aci_df(aci_df)
  type <- match.arg(type)

  colours <- .component_colours()
  components <- intersect(components, names(colours))

  df_long <- data.frame(
    date = rep(.parse_dates(aci_df), times = length(components)),
    component = rep(components, each = nrow(aci_df)),
    value     = unlist(lapply(components, function(cn) aci_df[[cn]]))
  )
  df_long$component <- factor(df_long$component, levels = components)

  if (type == "facet") {
    p <- ggplot2::ggplot(df_long,
                         ggplot2::aes(x = date, y = value,
                                      colour = component)) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey70",
                          linetype = "dashed", linewidth = 0.3) +
      ggplot2::geom_line(linewidth = 0.6) +
      ggplot2::facet_wrap(~ component, ncol = 2, scales = "free_y") +
      ggplot2::scale_colour_manual(values = colours, guide = "none") +
      ggplot2::scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
      ggplot2::labs(
        title    = title,
        subtitle = "Each panel shows one standardised component",
        x        = NULL, y = "Standardised value"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        plot.title    = ggplot2::element_text(face = "bold"),
        axis.text.x   = ggplot2::element_text(angle = 30, hjust = 1, size = 8),
        panel.grid.minor = ggplot2::element_blank(),
        strip.text    = ggplot2::element_text(face = "bold")
      )

  } else {
    # Stacked area — shift values to be non-negative for stacking clarity
    p <- ggplot2::ggplot(df_long,
                         ggplot2::aes(x = date, y = value,
                                      fill = component)) +
      ggplot2::geom_area(position = "stack", alpha = 0.8) +
      ggplot2::scale_fill_manual(
        values = colours,
        name   = "Component",
        labels = c(t90 = "T90 (hot)", t10 = "T10 (cold)",
                   precipitation = "Precipitation",
                   drought = "Drought", wind = "Wind",
                   sealevel = "Sea Level")
      ) +
      ggplot2::scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
      ggplot2::labs(
        title    = title,
        subtitle = "Stacked standardised components",
        x        = NULL, y = "Cumulative standardised value"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title   = ggplot2::element_text(face = "bold"),
        axis.text.x  = ggplot2::element_text(angle = 30, hjust = 1),
        panel.grid.minor = ggplot2::element_blank(),
        legend.position  = "bottom"
      )
  }

  p
}

# ---------------------------------------------------------------------------
# 3. Geographic map
# ---------------------------------------------------------------------------

#' Plot a geographic map of a climate variable from a xaci dataset list
#'
#' Renders a spatial snapshot (one time slice or the time-mean) of any
#' variable stored in a xaci \emph{dataset list} (as returned by
#' \code{\link{load_component}} or a component function with \code{area = FALSE}).
#' Requires the \pkg{ggplot2} and \pkg{sf} packages (only \pkg{ggplot2} is
#' hard-required; the function works without \pkg{sf} using
#' \code{coord_quickmap}).
#'
#' @param dataset    A list with elements \code{data} (3-D array
#'   \code{[lon × lat × time]}), \code{lon}, \code{lat}, and \code{time}.
#' @param time_index Either an integer (time slice index) or \code{"mean"}
#'   (default) to plot the temporal average.
#' @param var_label  Label for the colour bar. Default \code{"Value"}.
#' @param title      Plot title.
#' @param palette    RColorBrewer palette name or a vector of colours.
#'   Default \code{"RdBu"} (diverging).
#' @param reverse    Logical. Reverse the colour scale? Default \code{TRUE}
#'   (so that blue = cold/low, red = warm/high for "RdBu").
#' @param n_breaks   Number of colour-scale breaks. Default \code{9}.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' \dontrun{
#' # Either maps precipitation from original dataset:
#' #ds <- load_component("data/era5/tp_2011_2015.nc", "tp",
#' #                     mask_path = "data/era5/mask_france.nc")
#' # Or maps precipitation from the created precipitation component of the ACI:
#' ds <- readRDS("data/components_gridCell_level/prec.rds")
#' plot_aci_map(ds, time_index = "mean", var_label = "Precipitation (unit?)",
#'              title = "Mean precipitation")
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_raster scale_fill_distiller
#'   coord_quickmap labs theme_minimal theme element_text element_blank
#'   element_rect
#' @export
plot_aci_map <- function(dataset,
                         time_index = "mean",
                         var_label  = "Value",
                         title      = "Spatial Distribution",
                         palette    = "RdBu",
                         reverse    = TRUE,
                         n_breaks   = 9) {

  if (!is.list(dataset) || is.null(dataset$data) ||
      is.null(dataset$lon) || is.null(dataset$lat)) {
    stop("`dataset` must be a list with elements $data, $lon, and $lat.\n",
         "Use load_component() or a component function with area = FALSE.")
  }

  arr  <- dataset$data
  lons <- dataset$lon
  lats <- dataset$lat

  # Build the 2-D slice to plot
  if (identical(time_index, "mean")) {
    slice <- apply(arr, c(1, 2), mean, na.rm = TRUE)
    time_label <- "temporal mean"
  } else {
    if (!is.numeric(time_index) || time_index < 1 ||
        time_index > dim(arr)[3]) {
      stop("`time_index` must be an integer in [1, ", dim(arr)[3],
           "] or \"mean\".")
    }
    slice <- arr[, , as.integer(time_index)]
    t_val <- dataset$time[as.integer(time_index)]
    time_label <- format(t_val, "%Y-%m-%d")
  }

  # Convert to a long data.frame
  grid_df <- data.frame(
    lon   = rep(lons, times = length(lats)),
    lat   = rep(lats, each  = length(lons)),
    value = as.vector(slice)
  )
  grid_df <- grid_df[!is.na(grid_df$value), ]

  ggplot2::ggplot(grid_df,
                  ggplot2::aes(x = lon, y = lat, fill = value)) +
    ggplot2::geom_raster(interpolate = TRUE) +
    ggplot2::scale_fill_distiller(
      palette  = palette,
      direction = if (reverse) -1L else 1L,
      n.breaks = n_breaks,
      name     = var_label,
      na.value = "grey90"
    ) +
    ggplot2::coord_quickmap(expand = FALSE) +
    ggplot2::labs(
      title    = title,
      subtitle = paste0("Time slice: ", time_label),
      x        = "Longitude",
      y        = "Latitude"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold"),
      legend.position = "right",
      panel.grid    = ggplot2::element_blank(),
      panel.border  = ggplot2::element_rect(colour = "grey40",
                                            fill = NA, linewidth = 0.4),
      axis.text     = ggplot2::element_text(size = 8)
    )
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
#' @param type       \code{"boxplot"} (default), \code{"violin"}, or
#'   \code{"density"}.
#' @param include_aci Logical. Include the global ACI series in the plot?
#'   Default \code{TRUE}.
#' @param title      Plot title.
#' @param components Character vector of component columns to include.
#'
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
                                  type         = c("boxplot", "violin", "density"),
                                  include_aci  = TRUE,
                                  title        = "Distribution of ACI Components",
                                  components   = c("t90", "t10",
                                                   "precipitation", "drought",
                                                   "wind", "sealevel")) {
  .check_aci_df(aci_df)
  type <- match.arg(type)

  colours <- .component_colours()
  if (include_aci) {
    all_cols <- c(components, "ACI")
    colours  <- c(colours, ACI = "#333333")
  } else {
    all_cols <- components
  }
  all_cols <- intersect(all_cols, colnames(aci_df))

  # Long format
  df_long <- data.frame(
    component = rep(all_cols, each = nrow(aci_df)),
    value     = unlist(lapply(all_cols, function(cn) aci_df[[cn]]))
  )
  df_long$component <- factor(df_long$component, levels = all_cols)

  labels_map <- c(
    t90 = "T90\n(hot)", t10 = "T10\n(cold)",
    precipitation = "Precip.", drought = "Drought",
    wind = "Wind", sealevel = "Sea Level",
    ACI = "ACI\n(global)"
  )

  p_base <- ggplot2::ggplot(df_long,
                             ggplot2::aes(x = component, y = value,
                                          fill = component,
                                          colour = component)) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey60",
                        linetype = "dashed", linewidth = 0.4) +
    ggplot2::scale_fill_manual(values   = colours, guide = "none") +
    ggplot2::scale_colour_manual(values = colours, guide = "none") +
    ggplot2::scale_x_discrete(labels = labels_map) +
    ggplot2::labs(
      title = title,
      x     = NULL,
      y     = "Standardised value"
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title       = ggplot2::element_text(face = "bold"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank()
    )

  if (type == "boxplot") {
    p <- p_base +
      ggplot2::geom_boxplot(alpha = 0.7, outlier.size = 1,
                             outlier.alpha = 0.5, width = 0.6) +
      ggplot2::labs(subtitle = "Monthly distributions per component (boxplots)")

  } else if (type == "violin") {
    p <- p_base +
      ggplot2::geom_violin(alpha = 0.6, trim = FALSE, linewidth = 0.5) +
      ggplot2::geom_boxplot(width = 0.08, fill = "white",
                             colour = "grey30", outlier.shape = NA) +
      ggplot2::labs(subtitle = "Monthly distributions per component (violin + IQR)")

  } else {   # density
    # For density we switch to a rotated view with one density per component
    p <- ggplot2::ggplot(df_long,
                         ggplot2::aes(x = value, fill = component,
                                      colour = component)) +
      ggplot2::geom_density(alpha = 0.35, linewidth = 0.7) +
      ggplot2::geom_vline(xintercept = 0, colour = "grey50",
                          linetype = "dashed", linewidth = 0.4) +
      ggplot2::scale_fill_manual(
        values = colours,
        name   = "Component",
        labels = labels_map
      ) +
      ggplot2::scale_colour_manual(
        values = colours,
        name   = "Component",
        labels = labels_map
      ) +
      ggplot2::labs(
        title    = title,
        subtitle = "Density curves of monthly component values",
        x        = "Standardised value",
        y        = "Density"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.title     = ggplot2::element_text(face = "bold"),
        legend.position = "right",
        panel.grid.minor = ggplot2::element_blank()
      )
  }

  p
}

# ---------------------------------------------------------------------------
# 5. Convenience wrapper — dashboard of all plots
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
