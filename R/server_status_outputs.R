# =============================================================================
# vidternary: Shiny Server Module - Status Outputs
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Server-side logic for status outputs and feedback functionality
#              including progress indicators and user notifications.
# 
# Key Functions:
#   - create_server_status_outputs(): Main status outputs server logic
#   - [Status output functions and feedback mechanisms]
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - shiny
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

create_server_status_outputs <- function(input, output, session, rv) {
  
  # ---- Status Output Functions ----
  
  # Application status
  output$status <- renderText({
    paste("Application Status: Ready\n",
          "Dataset 1:", ifelse(is.null(rv$df1), "Not loaded", paste(nrow(rv$df1), "rows,", ncol(rv$df1), "columns")), "\n",
          "Dataset 2:", ifelse(is.null(rv$df2), "Not loaded", paste(nrow(rv$df2), "rows,", ncol(rv$df2), "columns")))
  })
  
  # Multiple ternary plot status
  output$multiple_ternary_status <- renderText({
    if (is.null(rv$multiple_ternary_results)) {
      "No multiple ternary plots created yet. Click 'Create All Ternary Plots' to start."
    } else {
      paste("Multiple ternary plots created successfully!\n",
            "Files processed:", length(rv$multiple_ternary_results), "\n",
            "Status: Ready for saving")
    }
  })
  
  # Cache statistics
  output$cache_stats <- renderText({
    get_cache_stats()
  })
  
  # Project status
  output$project_status <- renderText({
    "Project status: No project loaded"
  })
  
  # Export status
  output$export_status <- renderPrint({
    if (is.null(rv$last_export_results)) {
      cat("=== EXPORT STATUS ===\n")
      cat("Status: No exports performed yet\n")
      cat("Use 'Export All Selected' to start exporting data\n")
    } else {
      cat("=== EXPORT STATUS ===\n")
      cat("Status: Last export completed\n")
      cat("Export folder:", rv$last_export_folder, "\n")
      cat("Files exported:", length(rv$last_export_results), "\n")
      cat("Last export:", names(rv$last_export_results), "\n")
    }
  })
  
  # Filtered data status
  output$filtered_data_status <- renderText({
    status_text <- c()
    
    if (!is.null(rv$filtered_data1)) {
      rows1 <- nrow(rv$filtered_data1)
      original1 <- if (!is.null(rv$df1)) nrow(rv$df1) else "unknown"
      filtered_percentage1 <- if (original1 != "unknown") round((rows1 / original1) * 100, 1) else "unknown"
      status_text <- c(status_text, 
                       paste("Dataset 1: Filtered data available (", rows1, "rows,", ncol(rv$filtered_data1), "columns,", filtered_percentage1, "% retained)"))
    } else {
      status_text <- c(status_text, "Dataset 1: No filtered data available (generate a plot first)")
    }
    
    if (!is.null(rv$filtered_data2)) {
      rows2 <- nrow(rv$filtered_data2)
      original2 <- if (!is.null(rv$df2)) nrow(rv$df2) else "unknown"
      filtered_percentage2 <- if (original2 != "unknown") round((rows2 / original2) * 100, 1) else "unknown"
      status_text <- c(status_text, 
                       paste("Dataset 2: Filtered data available (", rows2, "rows,", ncol(rv$filtered_data2), "columns,", filtered_percentage2, "% retained)"))
    } else {
      status_text <- c(status_text, "Dataset 2: No filtered data available (generate a plot first)")
    }
    
    # Add data quality information
    if (!is.null(rv$filtered_data1) && !is.null(rv$filtered_data2)) {
      common_cols <- intersect(colnames(rv$filtered_data1), colnames(rv$filtered_data2))
      numeric_cols <- common_cols[sapply(rv$filtered_data1[, common_cols, drop = FALSE], is.numeric)]
      if (length(numeric_cols) >= 2) {
        status_text <- c(status_text, 
                         paste("✅ Ready for multivariate analysis (", length(numeric_cols), "common numeric columns)"))
        status_text <- c(status_text, 
                         paste("Available columns: ", paste(numeric_cols[1:min(5, length(numeric_cols))], collapse = ", ")))
        if (length(numeric_cols) > 5) {
          status_text <- c(status_text, "...")
        }
      } else {
        status_text <- c(status_text, "❌ Insufficient common numeric columns for multivariate analysis")
      }
    }
    
    paste(status_text, collapse = "\n")
  })
  
  # Return the module functions (if any are needed externally)
  return(list(
    # This module primarily contains output rendering functions
    # No external functions to return at this time
  ))
}
