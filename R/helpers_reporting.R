# ---- Helper Functions Module: Reporting & File I/O ----
# Split out of helpers.R: report/dashboard generation, correlation/statistics
# summaries, and centralized file-reading utilities.

# Report Generation Function
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

# Enhanced Data Visualization Functions
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

# Function to generate filtered data for export
generate_filtered_data_for_export <- function(dataset_num, xlsx_file1 = NULL, xlsx_file2 = NULL) {
  # This function should return the filtered data from ternary plots
  # For now, return original data until we implement proper filtered data storage
  tryCatch({
    if (dataset_num == 1 && !is.null(xlsx_file1)) {
      return(openxlsx::read.xlsx(xlsx_file1$datapath, sheet = 1))
    } else if (dataset_num == 2 && !is.null(xlsx_file2)) {
      return(openxlsx::read.xlsx(xlsx_file2$datapath, sheet = 1))
    } else {
      return(data.frame(Message = paste("Dataset", dataset_num, "not available")))
    }
  }, error = function(e) {
    return(data.frame(Error = paste("Error loading dataset", dataset_num, ":", e$message)))
  })
}

# ---- Centralized File Operations ----
# These functions consolidate common file reading and data processing patterns

# File type detection function
detect_file_type <- function(file_path) {
  if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
    return("csv")
  } else if (grepl("\\.xlsx?$", file_path, ignore.case = TRUE)) {
    return("excel")
  } else {
    return("unknown")
  }
}

# Centralized file reading function
read_file_by_type <- function(file_path, sheet = 1) {
  file_type <- detect_file_type(file_path)

  switch(file_type,
    "csv" = read.csv(file_path),
    "excel" = openxlsx::read.xlsx(file_path, sheet = sheet),
    stop("Unsupported file type: ", file_type)
  )
}

# Centralized dataset file reading for Shiny inputs
read_dataset_file <- function(file_input, sheet = 1) {
  if (is.null(file_input)) return(NULL)

  tryCatch({
    return(read_file_by_type(file_input$datapath, sheet))
  }, error = function(e) {
    log_operation("ERROR", "Failed to read dataset file", paste("File:", file_input$name, "Error:", e$message))
    return(NULL)
  })
}

# Centralized numeric column detection
get_numeric_columns <- function(df) {
  if (is.null(df)) return(character(0))
  return(names(df)[sapply(df, is.numeric)])
}

# Centralized statistical summary creation
create_statistical_summary <- function(df, numeric_cols = NULL) {
  if (is.null(df)) return(data.frame(Message = "No data available"))

  if (is.null(numeric_cols)) {
    numeric_cols <- get_numeric_columns(df)
  }

  if (length(numeric_cols) == 0) {
    return(data.frame(Message = "No numeric columns found"))
  }

  stats_df <- data.frame(
    Statistic = c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max"),
    stringsAsFactors = FALSE
  )

  for (col in numeric_cols) {
    col_data <- df[, col]
    col_data <- col_data[!is.na(col_data)]
    if (length(col_data) > 0) {
      stats_df[[col]] <- c(
        min(col_data),
        quantile(col_data, 0.25),
        median(col_data),
        mean(col_data),
        quantile(col_data, 0.75),
        max(col_data)
      )
    } else {
      stats_df[[col]] <- rep(NA, 6)
    }
  }

  return(stats_df)
}

# Centralized safe worksheet creation for Excel exports
safe_add_worksheet <- function(wb, sheet_name, data_func) {
  tryCatch({
    openxlsx::addWorksheet(wb, sheet_name)
    data <- data_func()
    if (!is.null(data) && (is.data.frame(data) && nrow(data) > 0 || is.matrix(data))) {
      openxlsx::writeData(wb, sheet_name, data)
    } else {
      openxlsx::writeData(wb, sheet_name, data.frame(Message = "No data available or insufficient data"))
    }
  }, error = function(e) {
    tryCatch({
      openxlsx::addWorksheet(wb, sheet_name)
      openxlsx::writeData(wb, sheet_name, data.frame(Error = paste("Sheet creation failed:", e$message)))
    }, error = function(e2) {
      log_operation("WARNING", "Could not create worksheet", paste("Sheet:", sheet_name, "Error:", e2$message))
    })
  })
}

# Centralized correlation matrix creation
create_correlation_matrix <- function(df, numeric_cols = NULL) {
  if (is.null(df)) return(data.frame(Message = "No data available"))

  if (is.null(numeric_cols)) {
    numeric_cols <- get_numeric_columns(df)
  }

  if (length(numeric_cols) < 2) {
    return(data.frame(Message = "Insufficient numeric columns for correlation analysis"))
  }

  tryCatch({
    return(cor(df[, numeric_cols, drop = FALSE], use = "complete.obs"))
  }, error = function(e) {
    return(data.frame(Error = paste("Correlation calculation failed:", e$message)))
  })
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

# Correlation heatmap creation function
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

# Distribution plots creation function
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
