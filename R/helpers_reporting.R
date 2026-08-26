# ---- Helper Functions Module: Reporting & File I/O ----
# Split out of helpers.R: report/dashboard generation, correlation/statistics
# summaries, and centralized file-reading utilities.

#' Write a simple HTML report of statistics and a correlation matrix
#'
#' @param stats An object (e.g. from `generate_stats()`) to `print()` into
#'   the report's Statistics section.
#' @param correlation An object (e.g. from `compute_correlation()`) to
#'   `print()` into the report's Correlation Matrix section.
#' @param plot_files Currently unused; accepted for interface compatibility.
#' @param output_path Path to write the HTML file to. Defaults to
#'   `Ternary_Analysis_Report_<timestamp>.html` in the working directory.
#' @return The path the report was written to.
#' @export
generate_report <- function(stats, correlation, plot_files, output_path = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  if (is.null(output_path)) {
    output_path <- paste0("Ternary_Analysis_Report_", timestamp, ".html")
  }

  # Create a simple HTML report since we don't have the Rmd template
  html_content <- paste0(
    "<!DOCTYPE html>",
    "<html><head><title>Ternary Analysis Report</title></head>",
    "<body><h1>Ternary Analysis Report</h1>",
    "<p>Generated on: ", timestamp, "</p>",
    "<h2>Statistics</h2>",
    "<pre>", capture.output(print(stats)), "</pre>",
    "<h2>Correlation Matrix</h2>",
    "<pre>", capture.output(print(correlation)), "</pre>",
    "</body></html>"
  )

  writeLines(html_content, output_path)
  return(output_path)
}

#' Write an HTML data-quality dashboard for two datasets
#'
#' Used by `run_comprehensive_analysis()`.
#'
#' @param quality_report A result from `check_data_quality()`.
#' @param output_dir Directory to write `data_quality_dashboard.html` into.
#'   Defaults to the working directory.
#' @return The path to the written HTML file.
#' @export
create_quality_dashboard <- function(quality_report, output_dir = NULL) {
  if (is.null(output_dir)) {
    output_dir <- getwd()
  }

  # Create HTML dashboard in parts
  header <- paste0(
    "<!DOCTYPE html>",
    "<html><head>",
    "<title>Data Quality Dashboard</title>",
    "<style>",
    "body { font-family: Arial, sans-serif; margin: 20px; }",
    ".metric { background: #f5f5f5; padding: 15px; margin: 10px 0; border-radius: 5px; }",
    ".score { font-size: 24px; font-weight: bold; }",
    ".grade-A { color: #28a745; }",
    ".grade-B { color: #17a2b8; }",
    ".grade-C { color: #ffc107; }",
    ".grade-D { color: #fd7e14; }",
    ".grade-F { color: #dc3545; }",
    "table { border-collapse: collapse; }",
    "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }",
    "th { background-color: #f2f2f2; }",
    "</style></head><body>",
    "<h1>Data Quality Dashboard</h1>",
    "<p>Generated on: ", Sys.time(), "</p>"
  )

  dataset1_section <- paste0(
    "<h2>Overall Quality Scores</h2>",
    "<div class='metric'>",
    "<h3>Dataset 1</h3>",
    "<div class='score grade-", quality_report$quality_score$data1$grade, "'>",
    "Score: ", quality_report$quality_score$data1$score, "/100 (Grade: ", quality_report$quality_score$data1$grade, ")</div>",
    "<p>Missing: ", quality_report$quality_score$data1$details$missing_penalty, "%, ",
    "Infinite: ", quality_report$quality_score$data1$details$infinite_penalty, "%, ",
    "Zero Variance: ", quality_report$quality_score$data1$details$zero_var_penalty, "%, ",
    "Outliers: ", quality_report$quality_score$data1$details$outlier_penalty, "%</p>",
    "</div>"
  )

  dataset2_section <- paste0(
    "<div class='metric'>",
    "<h3>Dataset 2</h3>",
    "<div class='score grade-", quality_report$quality_score$data2$grade, "'>",
    "Score: ", quality_report$quality_score$data2$score, "/100 (Grade: ", quality_report$quality_score$data2$grade, ")</div>",
    "<p>Missing: ", quality_report$quality_score$data2$details$missing_penalty, "%, ",
    "Infinite: ", quality_report$quality_score$data2$details$infinite_penalty, "%, ",
    "Zero Variance: ", quality_report$quality_score$data2$details$zero_var_penalty, "%, ",
    "Outliers: ", quality_report$quality_score$data2$details$outlier_penalty, "%</p>",
    "</div>"
  )

  missing_values_table <- paste0(
    "<h2>Detailed Analysis</h2>",
    "<h3>Missing Values</h3>",
    "<table><tr><th>Column</th><th>Dataset 1</th><th>Dataset 2</th></tr>",
    paste0("<tr><td>", names(quality_report$missing_values$data1), "</td><td>",
           quality_report$missing_values$data1, "</td><td>",
           quality_report$missing_values$data2, "</td></tr>", collapse = ""),
    "</table>"
  )

  outliers_table <- paste0(
    "<h3>Outliers (IQR Method)</h3>",
    "<table><tr><th>Column</th><th>Dataset 1</th><th>Dataset 2</th></tr>",
    paste0("<tr><td>", names(quality_report$outliers_iqr$data1), "</td><td>",
           quality_report$outliers_iqr$data1, "</td><td>",
           quality_report$outliers_iqr$data2, "</td></tr>", collapse = ""),
    "</table>"
  )

  footer <- "</body></html>"

  # Combine all parts
  dashboard_html <- paste0(header, dataset1_section, dataset2_section, missing_values_table, outliers_table, footer)

  dashboard_file <- file.path(output_dir, "data_quality_dashboard.html")
  writeLines(dashboard_html, dashboard_file)
  cat("Quality dashboard created:", dashboard_file, "\n")
  return(dashboard_file)
}

# Centralized success message helper
create_success_message <- function(operation, filename = NULL, details = NULL) {
  base_message <- paste(operation, "completed successfully!")
  if (!is.null(filename)) {
    base_message <- paste(operation, "exported successfully to", filename)
  }
  if (!is.null(details)) {
    base_message <- paste(base_message, details)
  }
  return(base_message)
}

#' Save a correlation heatmap PNG for a dataset's numeric columns
#'
#' Uses `corrplot::corrplot()` if available, else a base `heatmap()` fallback.
#'
#' @param data A data frame. Needs at least 2 numeric columns.
#' @param output_dir Directory to write the PNG into.
#' @param filename_prefix Filename prefix; the file is
#'   `<filename_prefix>_<timestamp>.png`.
#' @return The path to the written PNG, or `NULL` if there wasn't enough
#'   numeric data or `output_dir` was `NULL`.
#' @export
create_correlation_heatmap <- function(data, output_dir, filename_prefix) {
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }

  # Get numeric columns
  numeric_cols <- sapply(data, is.numeric)
  if (sum(numeric_cols) < 2) {
    return(NULL)
  }

  numeric_data <- data[, numeric_cols, drop = FALSE]

  # Calculate correlation matrix
  cor_matrix <- cor(numeric_data, use = "complete.obs")

  # Create output filename
  if (!is.null(output_dir)) {
    filename <- file.path(output_dir, paste0(filename_prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))

    # Create correlation heatmap
    png(filename, width = 800, height = 600, res = 150)
    tryCatch({
      if (requireNamespace("corrplot", quietly = TRUE)) {
        corrplot::corrplot(cor_matrix, method = "color", type = "upper",
                          order = "hclust", tl.cex = 0.8, tl.col = "black", tl.srt = 45)
      } else {
        # Fallback to base R heatmap
        heatmap(cor_matrix, main = paste("Correlation Heatmap -", filename_prefix))
      }
    }, error = function(e) {
      cat("Correlation heatmap creation failed:", e$message, "\n")
    })
    dev.off()

    return(filename)
  }

  return(NULL)
}

#' Save a grid of per-column histograms PNG for a dataset's numeric columns
#'
#' @param data A data frame. Needs at least 1 numeric column.
#' @param output_dir Directory to write the PNG into.
#' @param filename_prefix Filename prefix; the file is
#'   `<filename_prefix>_<timestamp>.png`.
#' @return The path to the written PNG, or `NULL` if there was no numeric
#'   data or `output_dir` was `NULL`.
#' @export
create_distribution_plots <- function(data, output_dir, filename_prefix) {
  if (is.null(data) || nrow(data) == 0) {
    return(NULL)
  }

  # Get numeric columns
  numeric_cols <- sapply(data, is.numeric)
  if (sum(numeric_cols) == 0) {
    return(NULL)
  }

  numeric_data <- data[, numeric_cols, drop = FALSE]

  # Create output filename
  if (!is.null(output_dir)) {
    filename <- file.path(output_dir, paste0(filename_prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"))

    # Create distribution plots
    png(filename, width = 1200, height = 800, res = 150)
    tryCatch({
      # Calculate number of plots needed
      n_cols <- min(4, ncol(numeric_data))
      n_rows <- ceiling(ncol(numeric_data) / n_cols)

      par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 2, 1))

      for (i in 1:ncol(numeric_data)) {
        col_name <- colnames(numeric_data)[i]
        values <- numeric_data[, i]
        values <- values[!is.na(values)]  # Remove NAs

        if (length(values) > 0) {
          hist(values, main = col_name, xlab = "Value", ylab = "Frequency",
               col = "lightblue", border = "black")
        } else {
          plot(1, 1, type = "n", main = paste(col_name, "(No data)"),
               xlab = "", ylab = "")
        }
      }
    }, error = function(e) {
      cat("Distribution plots creation failed:", e$message, "\n")
    })
    dev.off()

    return(filename)
  }

  return(NULL)
}
