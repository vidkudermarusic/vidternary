# ---- Plotting Utilities: Single-Dataset Chart Builders ----
# Core, single-dataset plot builders (ternary/correlation/histogram/boxplot/
# scatter-matrix), saving, theming, and color-palette helpers.
#
# Related chart builders live in sibling modules, split out for size:
#   plotting_utils_compare.R   - per-dataset / compare-both chart builders
#   plotting_utils_multifile.R - multi-file overlay chart builders

# Function to create ternary plot with ggplot2
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

# Function to create correlation plot
create_correlation_plot <- function(data, method = "pearson", 
                                  title = "Correlation Matrix", 
                                  color_palette = "RdBu") {
  
  if (!requireNamespace("corrplot", quietly = TRUE)) {
    stop("corrplot package is required for correlation plotting")
  }
  
  # Calculate correlation matrix
  cor_matrix <- cor(data, use = "complete.obs", method = method)
  
  # Create correlation plot
  corrplot::corrplot(cor_matrix, 
    method = "color",
                     type = "upper", 
                     order = "hclust",
                     tl.cex = 0.7,
    tl.col = "black",
    tl.srt = 45,
                     col = corrplot::COL2(color_palette, 200),
    title = title,
                     mar = c(0, 0, 2, 0))
  
  return(TRUE)
}

# Function to create histogram with density
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

# Function to create boxplot
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

# Function to create scatter plot matrix
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

# Function to save plot with multiple formats
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

# Function to create plot title
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

# Function to apply consistent theme across plots
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

# Function to create color palette
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

# Function to generate plot summary
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
