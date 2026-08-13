# ---- Plotting Utilities: Per-Dataset / Compare-Both Chart Builders ----
# Split out of plotting_utils.R: the chart builders used by the "Multiple
# Plot Types" tab for a single dataset or side-by-side dataset comparison
# (as opposed to the true multi-file overlay builders in
# plotting_utils_multifile.R).

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
