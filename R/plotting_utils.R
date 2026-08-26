# ---- Plotting Utilities: Single-Dataset Chart Builders ----
# Core, single-dataset plot builders (ternary/correlation/histogram/boxplot/
# scatter-matrix), saving, theming, and color-palette helpers.
#
# Related chart builders live in sibling modules, split out for size:
#   plotting_utils_compare.R   - per-dataset / compare-both chart builders
#   plotting_utils_multifile.R - multi-file overlay chart builders

#' Create a simple ggplot2 scatter of ternary A/B coordinates
#'
#' Note: plots `A` vs `B` as an ordinary Cartesian scatter, not a true
#' triangular ternary projection - the app's actual ternary diagrams are
#' built via `general_ternary_plot()`/the `Ternary` package instead.
#'
#' @param ternary_points A data frame with numeric columns `A` and `B`.
#' @param color_values Optional numeric vector to map point color to (viridis scale).
#' @param point_size Point size. Default 2.
#' @param alpha Point opacity. Default 0.7.
#' @param color_palette Currently unused; accepted for interface compatibility.
#' @param title Plot title. Default `"Ternary Plot"`.
#' @param subtitle Optional plot subtitle.
#' @return A `ggplot` object.
#' @export
create_ternary_plot <- function(ternary_points, color_values = NULL, point_size = 2,
                               alpha = 0.7, color_palette = "viridis",
                               title = "Ternary Plot", subtitle = NULL) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("rlang package is required for plotting")
  }
  
  # Create base plot
  p <- ggplot2::ggplot(ternary_points, ggplot2::aes(x = A, y = B)) +
    ggplot2::geom_point(size = point_size, alpha = alpha) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = title,
      subtitle = subtitle,
      x = "Component A",
      y = "Component B"
    ) +
    ggplot2::coord_fixed(ratio = 1)
  
  # Add color if provided
  if (!is.null(color_values)) {
    p <- p + ggplot2::aes(color = color_values) +
      ggplot2::scale_color_viridis_c()
  }
  
  return(p)
}

#' Draw a `corrplot` correlation heatmap, base-graphics style
#'
#' Constant (zero-variance) columns are dropped first (their correlation
#' is undefined and would crash `corrplot`'s hierarchical-clustering
#' column ordering); if a pairwise `NA` still remains, falls back to the
#' original column order instead of clustering.
#'
#' @param data A data frame of numeric columns.
#' @param method Correlation method passed to `cor()`. Default `"pearson"`.
#' @param title Plot title.
#' @param color_palette `corrplot::COL2()` palette name. Default `"RdBu"`.
#' @return `TRUE` (invisibly `FALSE` if fewer than 2 non-constant numeric
#'   columns remain). Called for its plotting side effect on the current
#'   graphics device.
#' @export
create_correlation_plot <- function(data, method = "pearson",
                                  title = "Correlation Matrix",
                                  color_palette = "RdBu") {

  if (!requireNamespace("corrplot", quietly = TRUE)) {
    stop("corrplot package is required for correlation plotting")
  }

  # A constant (zero-variance) column has an undefined correlation with
  # anything (division by zero -> NaN). corrplot's order="hclust" step calls
  # stats::hclust() on the correlation matrix, whose underlying Fortran
  # routine rejects any NA/NaN/Inf outright ("NA/NaN/Inf in foreign function
  # call (arg 10)") - so such columns are dropped before plotting rather than
  # crashing the whole heatmap.
  col_sds <- vapply(data, function(x) stats::sd(x, na.rm = TRUE), numeric(1))
  constant_cols <- names(col_sds)[is.na(col_sds) | col_sds == 0]
  if (length(constant_cols) > 0) {
    data <- data[, setdiff(names(data), constant_cols), drop = FALSE]
  }

  if (ncol(data) < 2) {
    plot.new()
    text(0.5, 0.5, "Not enough variable columns to plot a correlation matrix\n(need at least 2 non-constant numeric columns)", cex = 0.9)
    return(invisible(FALSE))
  }

  # Calculate correlation matrix
  cor_matrix <- cor(data, use = "complete.obs", method = method)

  # A pairwise NA can still remain (e.g. two columns share no complete rows
  # in common under "complete.obs") even after dropping constant columns -
  # hclust ordering can't handle that either, so fall back to the original
  # column order rather than crashing.
  order_method <- if (anyNA(cor_matrix)) "original" else "hclust"

  # Reserve extra room in the bottom margin when a note about excluded
  # columns needs to be printed below the plot, so mtext() doesn't overlap
  # the heatmap's own bottom row/labels.
  bottom_margin <- if (length(constant_cols) > 0) 3 else 0

  # Create correlation plot
  corrplot::corrplot(cor_matrix,
    method = "color",
                     type = "upper",
                     order = order_method,
                     tl.cex = 0.7,
    tl.col = "black",
    tl.srt = 45,
                     col = corrplot::COL2(color_palette, 200),
    title = title,
                     mar = c(bottom_margin, 0, 2, 0))

  if (length(constant_cols) > 0) {
    mtext(paste("Excluded constant column(s):", paste(constant_cols, collapse = ", ")),
          side = 1, line = 1.5, cex = 0.6, col = "grey40")
  }

  return(TRUE)
}

#' Create a histogram with an overlaid density curve
#'
#' @param data A data frame.
#' @param column Name of the numeric column to plot.
#' @param bins Number of histogram bins. Default 30.
#' @param title Plot title. Defaults to `"Distribution of <column>"`.
#' @param xlab X-axis label. Defaults to `column`.
#' @param ylab Y-axis label. Default `"Density"`.
#' @return A `ggplot` object.
#' @export
create_histogram_density <- function(data, column, bins = 30,
                                   title = NULL, xlab = NULL, ylab = "Density") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  if (is.null(title)) title <- paste("Distribution of", column)
  if (is.null(xlab)) xlab <- column
  
  # Ensure rlang is available
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("rlang package is required for plotting")
  }
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(column))) +
    ggplot2::geom_histogram(ggplot2::aes(y = after_stat(density)), bins = bins, 
                           fill = "steelblue", alpha = 0.7) +
    ggplot2::geom_density(color = "red", size = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title, x = xlab, y = ylab)
  
  return(p)
}

#' Create a boxplot of one numeric column grouped by another column
#'
#' @param data A data frame.
#' @param x_column Name of the grouping column.
#' @param y_column Name of the numeric column to plot.
#' @param title Plot title. Defaults to `"Boxplot of <y_column> by <x_column>"`.
#' @param xlab X-axis label. Defaults to `x_column`.
#' @param ylab Y-axis label. Defaults to `y_column`.
#' @return A `ggplot` object.
#' @export
create_boxplot <- function(data, x_column, y_column,
                          title = NULL, xlab = NULL, ylab = NULL) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  if (is.null(title)) title <- paste("Boxplot of", y_column, "by", x_column)
  if (is.null(xlab)) xlab <- x_column
  if (is.null(ylab)) ylab <- y_column
  
  # Ensure rlang is available
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("rlang package is required for plotting")
  }
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(x_column), y = !!rlang::sym(y_column))) +
    ggplot2::geom_boxplot(fill = "steelblue", alpha = 0.7) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title, x = xlab, y = ylab) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  return(p)
}

#' Create a `GGally::ggpairs()` scatter plot matrix
#'
#' @param data A data frame.
#' @param columns Character vector of columns to include. Defaults to all
#'   numeric columns of `data`.
#' @param title Plot title. Default `"Scatter Plot Matrix"`.
#' @return A `ggmatrix`/`ggpairs` object.
#' @export
create_scatter_matrix <- function(data, columns = NULL,
                                title = "Scatter Plot Matrix") {
  
  if (!requireNamespace("GGally", quietly = TRUE)) {
    stop("GGally package is required for scatter plot matrices")
  }
  
  if (is.null(columns)) {
    columns <- names(data)[sapply(data, is.numeric)]
  }
  
  data_subset <- data[, columns, drop = FALSE]
  
  p <- GGally::ggpairs(
    data_subset,
    title = title,
    lower = list(continuous = GGally::wrap("smooth", alpha = 0.3)),
    upper = list(continuous = GGally::wrap("cor", size = 3))
  )
  
  return(p)
}

#' Save a ggplot object to one or more file formats
#'
#' @param plot_obj A `ggplot` object.
#' @param filename Base filename (any extension is stripped and replaced per format).
#' @param output_dir Directory to save into; created if it doesn't exist.
#' @param formats Character vector of formats to save: any of `"png"`,
#'   `"pdf"`, `"svg"`. Default `c("png", "pdf")`.
#' @param width Plot width in inches. Default 10.
#' @param height Plot height in inches. Default 8.
#' @param dpi Resolution for raster formats. Default 300.
#' @return Character vector of the file paths written.
#' @export
save_plot <- function(plot_obj, filename, output_dir,
                     formats = c("png", "pdf"),
                     width = 10, height = 8, dpi = 300) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for saving plots")
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Remove file extension from filename
  base_name <- tools::file_path_sans_ext(filename)
  
  saved_files <- character(0)
  
  for (format in formats) {
    file_path <- file.path(output_dir, paste0(base_name, ".", format))
    
    if (format == "png") {
      ggplot2::ggsave(file_path, plot_obj, width = width, height = height, 
                     dpi = dpi, device = "png")
    } else if (format == "pdf") {
      ggplot2::ggsave(file_path, plot_obj, width = width, height = height, 
                     device = "pdf")
    } else if (format == "svg") {
      ggplot2::ggsave(file_path, plot_obj, width = width, height = height, 
                     device = "svg")
    }
    
    saved_files <- c(saved_files, file_path)
  }
  
  return(saved_files)
}

#' Build a ggplot2 title/subtitle/caption text theme
#'
#' @param main_title Currently unused; accepted for interface compatibility.
#' @param subtitle Currently unused; accepted for interface compatibility.
#' @param caption Currently unused; accepted for interface compatibility.
#' @param size Base title font size in points; subtitle/caption are sized
#'   relative to it. Default 14.
#' @return A `ggplot2::theme()` object, to be added to a plot.
#' @export
create_plot_title <- function(main_title, subtitle = NULL,
                            caption = NULL, size = 14) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  title_theme <- ggplot2::theme(
    plot.title = ggplot2::element_text(size = size, face = "bold"),
    plot.subtitle = ggplot2::element_text(size = size - 2, face = "italic"),
    plot.caption = ggplot2::element_text(size = size - 4, hjust = 0)
  )
  
  return(title_theme)
}

#' Apply a named ggplot2 theme to a plot
#'
#' @param plot_obj A `ggplot` object.
#' @param theme_name One of `"minimal"`, `"classic"`, `"bw"`, `"dark"`.
#'   Anything else falls back to `"minimal"`. Default `"minimal"`.
#' @return The plot with the theme added.
#' @export
apply_consistent_theme <- function(plot_obj, theme_name = "minimal") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  if (theme_name == "minimal") {
    plot_obj + ggplot2::theme_minimal()
  } else if (theme_name == "classic") {
    plot_obj + ggplot2::theme_classic()
  } else if (theme_name == "bw") {
    plot_obj + ggplot2::theme_bw()
  } else if (theme_name == "dark") {
    plot_obj + ggplot2::theme_dark()
  } else {
    plot_obj + ggplot2::theme_minimal()
  }
}

#' Generate a vector of colors from a named palette
#'
#' @param n_colors Number of colors to generate.
#' @param palette_name One of `"viridis"`, `"rainbow"`, `"heat"`,
#'   `"terrain"`, `"topo"`. Anything else falls back to `"rainbow"`.
#'   Default `"viridis"`.
#' @return A character vector of `n_colors` hex color codes.
#' @export
create_color_palette <- function(n_colors, palette_name = "viridis") {
  
  if (palette_name == "viridis") {
    if (requireNamespace("viridisLite", quietly = TRUE)) {
    return(viridisLite::viridis(n_colors))
    } else {
      return(rainbow(n_colors))
    }
  } else if (palette_name == "rainbow") {
    return(rainbow(n_colors))
  } else if (palette_name == "heat") {
    return(heat.colors(n_colors))
  } else if (palette_name == "terrain") {
    return(terrain.colors(n_colors))
  } else if (palette_name == "topo") {
    return(topo.colors(n_colors))
  } else {
    return(rainbow(n_colors))
  }
}

#' Summarize a plot object's type and (optionally) its data's dimensions
#'
#' @param plot_obj A plot object, e.g. from `create_ternary_plot()`.
#' @param data Optional data frame the plot was built from, to report row/column counts.
#' @return A list: `type` (`"ggplot"` or `"base_plot"`), plus `layers`
#'   (layer count) and, if `data` was supplied, `data_rows`/`data_cols`.
#' @export
generate_plot_summary <- function(plot_obj, data = NULL) {

  summary_info <- list()

  if (inherits(plot_obj, "ggplot")) {
    summary_info$type <- "ggplot"
    summary_info$layers <- length(plot_obj$layers)

    if (!is.null(data)) {
      summary_info$data_rows <- nrow(data)
      summary_info$data_cols <- ncol(data)
    }
  } else {
    summary_info$type <- "base_plot"
  }

  return(summary_info)
}

# Per-dataset/compare-both chart builders (scatter/histogram/box/violin/
# connected/stacked) moved to plotting_utils_compare.R.
# Multi-file overlay chart builders moved to plotting_utils_multifile.R.
