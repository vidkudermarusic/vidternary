# =============================================================================
# vidternary: Plotting Utilities Module
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Comprehensive plotting utilities for creating various plot types
#              including ternary plots, correlation plots, histograms, and
#              statistical visualizations with consistent theming.
# 
# Key Functions:
#   - create_ternary_plot(): ggplot2-based ternary plot creation
#   - create_correlation_plot(): Correlation matrix visualization
#   - create_histogram_density(): Histogram and density plots
#   - create_boxplot(): Box plot creation with statistical summaries
#   - save_plot(): Universal plot saving function
#   - apply_consistent_theme(): Consistent theming across all plots
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - ggplot2, GGally, corrplot, viridisLite, plotly
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

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

# Function to create scatter plots
create_scatter_plots <- function(data, columns, colors = NULL, x_col = NULL, y_col = NULL, 
                               point_size = 0.8, dataset_mode = "dataset1", 
                               add_trendline = FALSE, add_smooth = FALSE, log_x = FALSE, log_y = FALSE) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  # Handle different data input types
  if (is.list(data) && dataset_mode == "both") {
    # Combine datasets for comparison
    df1 <- data[[1]]
    df2 <- data[[2]]
    df1$dataset <- "Dataset 1"
    df2$dataset <- "Dataset 2"
    combined_data <- rbind(df1, df2)
    
    plots <- list()
    
    for (col in columns) {
      tryCatch({
        # Ensure rlang is available
        if (!requireNamespace("rlang", quietly = TRUE)) {
          stop("rlang package is required for plotting")
        }
        p <- ggplot2::ggplot(combined_data, ggplot2::aes(x = dataset, y = !!rlang::sym(col), color = dataset))
        
        p <- p + ggplot2::geom_point(alpha = 0.7, size = point_size) +
          ggplot2::geom_jitter(width = 0.2) +
          ggplot2::theme_minimal() +
          ggplot2::labs(title = paste("Scatter Plot Comparison:", col))
        
        plots[[col]] <- p
      }, error = function(e) {
        # Create a simple error plot if the main plot fails
        plots[[col]] <<- ggplot2::ggplot() + 
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("Error plotting", col), size = 4) +
          ggplot2::theme_void()
      })
    }
    
    return(plots)
  } else {
    # Single dataset
    if (is.list(data)) {
      data <- data[[1]]
    }
    
    plots <- list()
    
    for (col in columns) {
      tryCatch({
        # Ensure rlang is available
        if (!requireNamespace("rlang", quietly = TRUE)) {
          stop("rlang package is required for plotting")
        }
        
        if (!is.null(x_col) && !is.null(y_col)) {
          # Specific x and y columns
          p <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(x_col), y = !!rlang::sym(y_col)))
        } else {
          # Use first column as x, current column as y
          p <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(columns[1]), y = !!rlang::sym(col)))
        }
        
        p <- p + ggplot2::geom_point(alpha = 0.7, size = point_size)
        
        if (add_trendline) {
          p <- p + ggplot2::geom_smooth(method = "lm", se = TRUE, alpha = 0.3)
        }
        
        if (add_smooth) {
          p <- p + ggplot2::geom_smooth(method = "loess", se = TRUE, alpha = 0.3)
        }
        
        p <- p + ggplot2::theme_minimal() +
          ggplot2::labs(title = paste("Scatter Plot:", col))
        
        # Add log scales if requested
        if (log_x) {
          p <- p + ggplot2::scale_x_log10()
        }
        if (log_y) {
          p <- p + ggplot2::scale_y_log10()
        }
        
        plots[[col]] <- p
      }, error = function(e) {
        # Create a simple error plot if the main plot fails
        plots[[col]] <<- ggplot2::ggplot() + 
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("Error plotting", col), size = 4) +
          ggplot2::theme_void()
      })
    }
    
    return(plots)
  }
}

# Function to create histograms
create_histograms <- function(data, columns, breaks = 30, alpha = 0.7, 
                            data_type = "frequency", dataset_mode = "dataset1", log_x = FALSE, log_y = FALSE) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  # Handle different data input types
  if (is.list(data) && dataset_mode == "both") {
    # Combine datasets for comparison
    df1 <- data[[1]]
    df2 <- data[[2]]
    df1$dataset <- "Dataset 1"
    df2$dataset <- "Dataset 2"
    combined_data <- rbind(df1, df2)
    
    plots <- list()
    
    for (col in columns) {
      tryCatch({
        # Ensure rlang is available
        if (!requireNamespace("rlang", quietly = TRUE)) {
          stop("rlang package is required for plotting")
        }
        p <- ggplot2::ggplot(combined_data, ggplot2::aes(x = !!rlang::sym(col), fill = dataset))
        
        if (data_type == "density") {
          p <- p + ggplot2::geom_histogram(ggplot2::aes(y = after_stat(density)), bins = breaks, alpha = alpha, position = "identity")
        } else {
          p <- p + ggplot2::geom_histogram(bins = breaks, alpha = alpha, position = "identity")
        }
        
        p <- p + ggplot2::theme_minimal() +
          ggplot2::labs(title = paste("Histogram Comparison:", col))
        
        plots[[col]] <- p
      }, error = function(e) {
        # Create a simple error plot if the main plot fails
        plots[[col]] <<- ggplot2::ggplot() + 
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("Error plotting", col), size = 4) +
          ggplot2::theme_void()
      })
    }
    
    return(plots)
  } else {
    # Single dataset
    if (is.list(data)) {
      data <- data[[1]]
    }
    
    plots <- list()
    
    for (col in columns) {
      tryCatch({
        # Sanitize column name for ggplot
        safe_col <- make.names(col)
        
        # Use aes() instead of aes_string() for better handling of special characters
        # Ensure rlang is available
        if (!requireNamespace("rlang", quietly = TRUE)) {
          stop("rlang package is required for plotting")
        }
        p <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(col)))
        
        if (data_type == "density") {
          p <- p + ggplot2::geom_histogram(ggplot2::aes(y = after_stat(density)), bins = breaks, fill = "steelblue", alpha = alpha)
        } else {
          p <- p + ggplot2::geom_histogram(bins = breaks, fill = "steelblue", alpha = alpha)
        }
        
        p <- p + ggplot2::theme_minimal() +
          ggplot2::labs(title = paste("Distribution of", col))
        
        # Add log scales if requested
        if (log_x) {
          p <- p + ggplot2::scale_x_log10()
        }
        if (log_y) {
          p <- p + ggplot2::scale_y_log10()
        }
        
        plots[[col]] <- p
      }, error = function(e) {
        # Create a simple error plot if the main plot fails
        plots[[col]] <<- ggplot2::ggplot() + 
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("Error plotting", col), size = 4) +
          ggplot2::theme_void()
      })
    }
    
    return(plots)
  }
}

# Function to create box plots
create_box_plots <- function(data, columns, horizontal = FALSE, notch = FALSE, 
                           show_outliers = TRUE, dataset_mode = "dataset1", violin_overlay = FALSE) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  # Handle different data input types
  if (is.list(data) && dataset_mode == "both") {
    # Combine datasets for comparison
    df1 <- data[[1]]
    df2 <- data[[2]]
    df1$dataset <- "Dataset 1"
    df2$dataset <- "Dataset 2"
    combined_data <- rbind(df1, df2)
    
    # Create comparison plot
    tryCatch({
      # Ensure rlang is available
      if (!requireNamespace("rlang", quietly = TRUE)) {
        stop("rlang package is required for plotting")
      }
      p <- ggplot2::ggplot(combined_data, ggplot2::aes(x = dataset, y = !!rlang::sym(columns[1]))) +
        ggplot2::geom_boxplot(notch = notch, outlier.shape = if (show_outliers) 19 else NA)
      
      if (violin_overlay) {
        p <- p + ggplot2::geom_violin(alpha = 0.3, fill = "lightblue")
      }
      
      p <- p + ggplot2::theme_minimal() +
        ggplot2::labs(title = paste("Box Plot Comparison:", columns[1]))
      
      if (horizontal) {
        p <- p + ggplot2::coord_flip()
      }
      
      return(p)
    }, error = function(e) {
      # Create a simple error plot if the main plot fails
      return(ggplot2::ggplot() + 
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("Error plotting", columns[1]), size = 4) +
        ggplot2::theme_void())
    })
  } else {
    # Single dataset
    if (is.list(data)) {
      data <- data[[1]]
    }
    
    plots <- list()
    
    for (col in columns) {
      tryCatch({
        # Ensure rlang is available
        if (!requireNamespace("rlang", quietly = TRUE)) {
          stop("rlang package is required for plotting")
        }
        p <- ggplot2::ggplot(data, ggplot2::aes(y = !!rlang::sym(col))) +
          ggplot2::geom_boxplot(notch = notch, outlier.shape = if (show_outliers) 19 else NA)
        
        if (violin_overlay) {
          p <- p + ggplot2::geom_violin(alpha = 0.3, fill = "lightblue")
        }
        
        p <- p + ggplot2::theme_minimal() +
          ggplot2::labs(title = paste("Box Plot:", col))
        
        if (horizontal) {
          p <- p + ggplot2::coord_flip()
        }
        
        plots[[col]] <- p
      }, error = function(e) {
        # Create a simple error plot if the main plot fails
        plots[[col]] <<- ggplot2::ggplot() + 
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("Error plotting", col), size = 4) +
          ggplot2::theme_void()
      })
    }
    
    return(plots)
  }
}

# Function to create violin plots
create_violin_plots <- function(data, columns, group_column = NULL, fill_violins = TRUE,
                              add_boxplot = TRUE, show_points = FALSE, scale_method = 1,
                              dataset_mode = "dataset1") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  # Handle different data input types
  if (is.list(data) && dataset_mode == "both") {
    # Combine datasets for comparison
    df1 <- data[[1]]
    df2 <- data[[2]]
    df1$dataset <- "Dataset 1"
    df2$dataset <- "Dataset 2"
    combined_data <- rbind(df1, df2)
    
    # Create comparison plot
    tryCatch({
      # Ensure rlang is available
      if (!requireNamespace("rlang", quietly = TRUE)) {
        stop("rlang package is required for plotting")
      }
      p <- ggplot2::ggplot(combined_data, ggplot2::aes(x = dataset, y = !!rlang::sym(columns[1]))) +
        ggplot2::geom_violin(scale = if (scale_method == 1) "area" else "width", 
                            fill = if (fill_violins) "lightblue" else NA, alpha = 0.7)
      
      if (add_boxplot) {
        p <- p + ggplot2::geom_boxplot(width = 0.1, alpha = 0.5)
      }
      
      if (show_points) {
        p <- p + ggplot2::geom_jitter(width = 0.1, alpha = 0.3)
      }
      
      p <- p + ggplot2::theme_minimal() +
        ggplot2::labs(title = paste("Violin Plot Comparison:", columns[1]))
      
      return(p)
    }, error = function(e) {
      # Create a simple error plot if the main plot fails
      return(ggplot2::ggplot() + 
        ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("Error plotting", columns[1]), size = 4) +
        ggplot2::theme_void())
    })
  } else {
    # Single dataset
    if (is.list(data)) {
      data <- data[[1]]
    }
    
    plots <- list()
    
    for (col in columns) {
      tryCatch({
        # Ensure rlang is available
        if (!requireNamespace("rlang", quietly = TRUE)) {
          stop("rlang package is required for plotting")
        }
        
        if (!is.null(group_column)) {
          p <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(group_column), y = !!rlang::sym(col))) +
            ggplot2::geom_violin(scale = if (scale_method == 1) "area" else "width", 
                                fill = if (fill_violins) "lightblue" else NA, alpha = 0.7)
        } else {
          p <- ggplot2::ggplot(data, ggplot2::aes(y = !!rlang::sym(col))) +
            ggplot2::geom_violin(scale = if (scale_method == 1) "area" else "width", 
                                fill = if (fill_violins) "lightblue" else NA, alpha = 0.7)
        }
        
        if (add_boxplot) {
          p <- p + ggplot2::geom_boxplot(width = 0.1, alpha = 0.5)
        }
        
        if (show_points) {
          p <- p + ggplot2::geom_jitter(width = 0.1, alpha = 0.3)
        }
        
        p <- p + ggplot2::theme_minimal() +
          ggplot2::labs(title = paste("Violin Plot:", col))
        
        plots[[col]] <- p
      }, error = function(e) {
        # Create a simple error plot if the main plot fails
        plots[[col]] <<- ggplot2::ggplot() + 
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = paste("Error plotting", col), size = 4) +
          ggplot2::theme_void()
      })
    }
    
    return(plots)
  }
}

# Function to create connected scatter plots
create_connected_scatter_plots <- function(data, columns, x_column, group_column = NULL,
                                         show_points = TRUE, show_lines = TRUE, smooth_lines = FALSE,
                                         line_width = 1, point_size = 2, dataset_mode = "dataset1") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  # Handle different data input types
  if (is.list(data) && dataset_mode == "both") {
    # Combine datasets for comparison
    df1 <- data[[1]]
    df2 <- data[[2]]
    df1$dataset <- "Dataset 1"
    df2$dataset <- "Dataset 2"
    combined_data <- rbind(df1, df2)
    
    plots <- list()
    
    for (col in columns) {
      p <- ggplot2::ggplot(combined_data, ggplot2::aes_string(x = x_column, y = col, color = "dataset"))
      
      if (show_lines) {
        if (smooth_lines) {
          p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE, size = line_width)
        } else {
          p <- p + ggplot2::geom_line(size = line_width)
        }
      }
      
      if (show_points) {
        p <- p + ggplot2::geom_point(size = point_size)
      }
      
      p <- p + ggplot2::theme_minimal() +
        ggplot2::labs(title = paste("Connected Scatter:", col, "vs", x_column))
      
      plots[[col]] <- p
    }
    
    return(plots)
  } else {
    # Single dataset
    if (is.list(data)) {
      data <- data[[1]]
    }
    
    plots <- list()
    
    for (col in columns) {
      if (!is.null(group_column)) {
        p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_column, y = col, color = group_column))
      } else {
        p <- ggplot2::ggplot(data, ggplot2::aes_string(x = x_column, y = col))
      }
      
      if (show_lines) {
        if (smooth_lines) {
          p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE, size = line_width)
        } else {
          p <- p + ggplot2::geom_line(size = line_width)
        }
      }
      
      if (show_points) {
        p <- p + ggplot2::geom_point(size = point_size)
      }
      
      p <- p + ggplot2::theme_minimal() +
        ggplot2::labs(title = paste("Connected Scatter:", col, "vs", x_column))
      
      plots[[col]] <- p
    }
    
    return(plots)
  }
}

# Function to create stacked bar charts
create_stacked_bar_charts <- function(data, columns, x_column, percentage = FALSE,
                                    horizontal = FALSE, show_labels = FALSE, dataset_mode = "dataset1") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  # Handle different data input types
  if (is.list(data) && dataset_mode == "both") {
    # Combine datasets for comparison
    df1 <- data[[1]]
    df2 <- data[[2]]
    df1$dataset <- "Dataset 1"
    df2$dataset <- "Dataset 2"
    combined_data <- rbind(df1, df2)
    
    # Reshape data for stacked bar chart
    library(reshape2)
    melted_data <- reshape2::melt(combined_data, id.vars = c(x_column, "dataset"), 
                                measure.vars = columns, variable.name = "variable", value.name = "value")
    
    p <- ggplot2::ggplot(melted_data, ggplot2::aes_string(x = x_column, y = "value", fill = "variable")) +
      ggplot2::geom_bar(stat = "identity", position = if (percentage) "fill" else "stack") +
      ggplot2::facet_wrap(~dataset) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Stacked Bar Chart Comparison")
    
    if (show_labels) {
      p <- p + ggplot2::geom_text(ggplot2::aes(label = round(value, 1)), 
                                 position = ggplot2::position_stack(vjust = 0.5))
    }
    
    if (horizontal) {
      p <- p + ggplot2::coord_flip()
    }
    
    return(p)
  } else {
    # Single dataset
    if (is.list(data)) {
      data <- data[[1]]
    }
    
    # Reshape data for stacked bar chart
    library(reshape2)
    melted_data <- reshape2::melt(data, id.vars = x_column, 
                                measure.vars = columns, variable.name = "variable", value.name = "value")
    
    p <- ggplot2::ggplot(melted_data, ggplot2::aes_string(x = x_column, y = "value", fill = "variable")) +
      ggplot2::geom_bar(stat = "identity", position = if (percentage) "fill" else "stack") +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Stacked Bar Chart")
    
    if (show_labels) {
      p <- p + ggplot2::geom_text(ggplot2::aes(label = round(value, 1)), 
                                 position = ggplot2::position_stack(vjust = 0.5))
    }
    
    if (horizontal) {
      p <- p + ggplot2::coord_flip()
    }
    
    return(p)
  }
}

# Function to create multi-file scatter plot
create_multifile_scatter_plot <- function(all_data, column_name, normalize = FALSE) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  # Combine all data with file source
  combined_data <- do.call(rbind, all_data)
  
  # Normalize if requested
  if (normalize) {
    combined_data[[column_name]] <- (combined_data[[column_name]] - min(combined_data[[column_name]], na.rm = TRUE)) / 
                                   (max(combined_data[[column_name]], na.rm = TRUE) - min(combined_data[[column_name]], na.rm = TRUE))
  }
  
  # Create scatter plot
  p <- ggplot2::ggplot(combined_data, ggplot2::aes_string(x = "file_source", y = column_name, color = "file_source")) +
    ggplot2::geom_point(alpha = 0.7, size = 2) +
    ggplot2::geom_jitter(width = 0.2) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Multi-File Comparison:", column_name),
      x = "File Source",
      y = if (normalize) paste(column_name, "(Normalized)") else column_name
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  return(p)
}

# Function to create multi-file histogram plot
create_multifile_histogram_plot <- function(all_data, column_name, normalize = FALSE, breaks = 30, alpha = 0.7, data_type = "frequency") {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  # Combine all data with file source
  combined_data <- do.call(rbind, all_data)
  
  # Normalize if requested
  if (normalize) {
    combined_data[[column_name]] <- (combined_data[[column_name]] - min(combined_data[[column_name]], na.rm = TRUE)) / 
                                   (max(combined_data[[column_name]], na.rm = TRUE) - min(combined_data[[column_name]], na.rm = TRUE))
  }
  
  # Create histogram plot
  p <- ggplot2::ggplot(combined_data, ggplot2::aes_string(x = column_name, fill = "file_source"))
  
  if (data_type == "density") {
    p <- p + ggplot2::geom_histogram(ggplot2::aes(y = after_stat(density)), bins = breaks, alpha = alpha, position = "identity")
  } else {
    p <- p + ggplot2::geom_histogram(bins = breaks, alpha = alpha, position = "identity")
  }
  
  p <- p + ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Multi-File Histogram Comparison:", column_name),
      x = if (normalize) paste(column_name, "(Normalized)") else column_name,
      y = if (data_type == "density") "Density" else "Count"
    )
  
  return(p)
}

# Function to create multi-file boxplot plot
create_multifile_boxplot_plot <- function(all_data, column_name, normalize = FALSE, horizontal = FALSE, notch = FALSE, show_outliers = TRUE, violin_overlay = FALSE, alpha = 0.7) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  # Combine all data with file source
  combined_data <- do.call(rbind, all_data)
  
  # Normalize if requested
  if (normalize) {
    combined_data[[column_name]] <- (combined_data[[column_name]] - min(combined_data[[column_name]], na.rm = TRUE)) / 
                                   (max(combined_data[[column_name]], na.rm = TRUE) - min(combined_data[[column_name]], na.rm = TRUE))
  }
  
  # Create boxplot
  p <- ggplot2::ggplot(combined_data, ggplot2::aes_string(x = "file_source", y = column_name, fill = "file_source")) +
    ggplot2::geom_boxplot(notch = notch, outlier.shape = if (show_outliers) 19 else NA, alpha = alpha)
  
  if (violin_overlay) {
    p <- p + ggplot2::geom_violin(alpha = 0.3, fill = "lightblue")
  }
  
  p <- p + ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Multi-File Box Plot Comparison:", column_name),
      x = "File Source",
      y = if (normalize) paste(column_name, "(Normalized)") else column_name
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  if (horizontal) {
    p <- p + ggplot2::coord_flip()
  }
  
  return(p)
}

# Function to create multi-file violin plot
create_multifile_violin_plot <- function(all_data, column_name, normalize = FALSE, fill_violins = TRUE, add_boxplot = TRUE, show_points = FALSE, scale_method = 1, alpha = 0.7) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  # Combine all data with file source
  combined_data <- do.call(rbind, all_data)
  
  # Normalize if requested
  if (normalize) {
    combined_data[[column_name]] <- (combined_data[[column_name]] - min(combined_data[[column_name]], na.rm = TRUE)) / 
                                   (max(combined_data[[column_name]], na.rm = TRUE) - min(combined_data[[column_name]], na.rm = TRUE))
  }
  
  # Create violin plot
  p <- ggplot2::ggplot(combined_data, ggplot2::aes_string(x = "file_source", y = column_name, fill = "file_source")) +
    ggplot2::geom_violin(scale = if (scale_method == 1) "area" else "width", alpha = alpha)
  
  if (add_boxplot) {
    p <- p + ggplot2::geom_boxplot(width = 0.1, alpha = 0.5)
  }
  
  if (show_points) {
    p <- p + ggplot2::geom_jitter(width = 0.1, alpha = 0.3)
  }
  
  p <- p + ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Multi-File Violin Plot Comparison:", column_name),
      x = "File Source",
      y = if (normalize) paste(column_name, "(Normalized)") else column_name
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  return(p)
}

# Function to create multi-file connected scatter plot
create_multifile_connected_plot <- function(all_data, column_name, normalize = FALSE, show_points = TRUE, show_lines = TRUE, smooth_lines = FALSE, line_width = 1, point_size = 2) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  # Combine all data with file source
  combined_data <- do.call(rbind, all_data)
  
  # Normalize if requested
  if (normalize) {
    combined_data[[column_name]] <- (combined_data[[column_name]] - min(combined_data[[column_name]], na.rm = TRUE)) / 
                                   (max(combined_data[[column_name]], na.rm = TRUE) - min(combined_data[[column_name]], na.rm = TRUE))
  }
  
  # Create connected scatter plot
  p <- ggplot2::ggplot(combined_data, ggplot2::aes_string(x = "file_source", y = column_name, color = "file_source"))
  
  if (show_lines) {
    if (smooth_lines) {
      p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE, size = line_width)
    } else {
      p <- p + ggplot2::geom_line(size = line_width)
    }
  }
  
  if (show_points) {
    p <- p + ggplot2::geom_point(size = point_size)
  }
  
  p <- p + ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Multi-File Connected Scatter:", column_name),
      x = "File Source",
      y = if (normalize) paste(column_name, "(Normalized)") else column_name
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  return(p)
}

# Function to create multi-file stacked bar chart
create_multifile_stacked_plot <- function(all_data, column_name, normalize = FALSE, percentage = FALSE, horizontal = FALSE, show_labels = FALSE, alpha = 0.8) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting")
  }
  
  # Combine all data with file source
  combined_data <- do.call(rbind, all_data)
  
  # Normalize if requested
  if (normalize) {
    combined_data[[column_name]] <- (combined_data[[column_name]] - min(combined_data[[column_name]], na.rm = TRUE)) / 
                                   (max(combined_data[[column_name]], na.rm = TRUE) - min(combined_data[[column_name]], na.rm = TRUE))
  }
  
  # Create stacked bar chart
  p <- ggplot2::ggplot(combined_data, ggplot2::aes_string(x = "file_source", y = column_name, fill = "file_source")) +
    ggplot2::geom_bar(stat = "identity", alpha = alpha) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("Multi-File Stacked Bar Chart:", column_name),
      x = "File Source",
      y = if (normalize) paste(column_name, "(Normalized)") else column_name
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  if (show_labels) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = round(combined_data[[column_name]], 1)), 
                               position = ggplot2::position_stack(vjust = 0.5))
  }
  
  if (horizontal) {
    p <- p + ggplot2::coord_flip()
  }
  
  return(p)
}
