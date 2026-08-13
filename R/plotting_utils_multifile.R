# ---- Plotting Utilities: Multi-File Comparison Chart Builders ----
# Split out of plotting_utils.R: chart builders that overlay one column
# across several uploaded files (file_source as the grouping variable),
# used by the "Multi-File Comparison" mode in each Multiple Plot Types tab.

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
