# ---- Server Export Module: Status/Report Outputs ----
# Split out of server_export.R: the export status/history renderers and the
# "Export Comprehensive Analysis" button handler - the only export control
# that exists in ui_data_export_tab.R.

register_export_report_handlers <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {

  # Export status display
  output$export_status <- renderPrint({
    if (is.null(rv$last_export_results)) {
      cat("=== EXPORT STATUS ===\n")
      cat("Status: No exports performed yet\n")
      cat("Use 'Export Comprehensive Analysis' to start exporting data\n")
    } else {
      cat("=== EXPORT STATUS ===\n")
      cat("Status: Last export completed\n")
      cat("Export folder:", rv$last_export_folder, "\n")
      cat("Files exported:", length(rv$last_export_results), "\n")
      cat("Last export:", names(rv$last_export_results), "\n")
    }
  })

  # Enhanced export status
  output$export_status_enhanced <- renderText({
    if (length(rv$export_files) == 0) {
      "No exports yet. Select items and click export buttons."
    } else {
      paste("Last export:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
            "\nTotal exports:", length(rv$export_files))
    }
  })

  # Download links display
  output$download_links <- renderUI({
    if (is.null(rv$last_export_results)) {
      div(style = "color: #6c757d;", "No exports available for download")
    } else {
      export_folder <- rv$last_export_folder
      if (!is.null(export_folder) && dir.exists(export_folder)) {
        files <- list.files(export_folder, full.names = TRUE)
        if (length(files) > 0) {
          div(
            h6("Available files:"),
            lapply(files, function(file) {
              filename <- basename(file)
              div(
                style = "margin: 2px 0; padding: 2px; background-color: #f8f9fa; border-radius: 3px;",
                paste("📁", filename)
              )
            })
          )
        } else {
          div(style = "color: #6c757d;", "No files found in export folder")
        }
      } else {
        div(style = "color: #6c757d;", "Export folder not accessible")
      }
    }
  })

  # Export history display
  output$export_history <- renderPrint({
    if (is.null(rv$export_history) || length(rv$export_history) == 0) {
      cat("=== EXPORT HISTORY ===\n")
      cat("No export history available\n")
    } else {
      cat("=== EXPORT HISTORY ===\n")
      for (i in seq_along(rv$export_history)) {
        entry <- rv$export_history[[i]]
        cat("Export", i, ":", entry$timestamp, "\n")
        if (!is.null(entry$results)) {
          cat("  Items:", paste(names(entry$results), collapse = ", "), "\n")
        }
        cat("  ---\n")
      }
    }
  })

  # "Export Comprehensive Analysis" button: builds one workbook (stats +
  # correlation + original data per loaded dataset, plus an analysis
  # report sheet) using the existing, working helpers in
  # helpers_reporting.R, and always saves it.
  observeEvent(input$export_comprehensive_btn, {
    if (!isTRUE(input$export_comprehensive)) {
      show_message("Please enable 'Export Comprehensive Analysis' checkbox first", "warning")
      return()
    }
    if (is.null(input$xlsx_file1)) {
      show_message("Please upload Dataset 1 before exporting", "warning")
      return()
    }

    tryCatch({
      output_dir <- if (!is.null(directory_management)) directory_management$output_dir() else getwd()
      if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      filename <- paste0("comprehensive_analysis_", timestamp, ".xlsx")
      filepath <- file.path(output_dir, filename)

      wb <- openxlsx::createWorkbook()

      safe_add_worksheet(wb, "Stats Dataset 1", function() create_statistical_summary(read_dataset_file(input$xlsx_file1)))
      safe_add_worksheet(wb, "Correlation Dataset 1", function() create_correlation_matrix(read_dataset_file(input$xlsx_file1)))
      safe_add_worksheet(wb, "Original Data 1", function() read_dataset_file(input$xlsx_file1))

      if (!is.null(input$xlsx_file2)) {
        safe_add_worksheet(wb, "Stats Dataset 2", function() create_statistical_summary(read_dataset_file(input$xlsx_file2)))
        safe_add_worksheet(wb, "Correlation Dataset 2", function() create_correlation_matrix(read_dataset_file(input$xlsx_file2)))
        safe_add_worksheet(wb, "Original Data 2", function() read_dataset_file(input$xlsx_file2))
      }

      safe_add_worksheet(wb, "Analysis Report", function() {
        report_lines <- c(
          "=== COMPREHENSIVE ANALYSIS REPORT ===",
          paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          "",
          "=== DATASET INFORMATION ===",
          paste("Dataset 1:", paste("Loaded -", input$xlsx_file1$name)),
          paste("Dataset 2:", if (!is.null(input$xlsx_file2)) paste("Loaded -", input$xlsx_file2$name) else "Not Loaded"),
          ""
        )
        report_lines <- c(report_lines, "=== TERNARY PLOT ELEMENTS ===")
        if (!is.null(input$element_A1) && length(input$element_A1) > 0) {
          report_lines <- c(report_lines, paste("Element A:", paste(input$element_A1, collapse = " + ")))
        }
        if (!is.null(input$element_B1) && length(input$element_B1) > 0) {
          report_lines <- c(report_lines, paste("Element B:", paste(input$element_B1, collapse = " + ")))
        }
        if (!is.null(input$element_C1) && length(input$element_C1) > 0) {
          report_lines <- c(report_lines, paste("Element C:", paste(input$element_C1, collapse = " + ")))
        }
        report_lines <- c(report_lines, "",
          "=== EXPORT INFORMATION ===",
          paste("Export Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          "Export Type: Comprehensive Analysis",
          "Status: Completed Successfully"
        )
        data.frame(Line = report_lines, stringsAsFactors = FALSE)
      })

      openxlsx::saveWorkbook(wb, filepath, overwrite = TRUE)

      export_results <- list(comprehensive = list(filename = filename, path = filepath))
      rv$last_export_results <- export_results
      rv$last_export_folder <- output_dir
      rv$export_history <- c(rv$export_history, list(list(timestamp = timestamp, results = export_results)))

      show_message(paste("Comprehensive analysis exported to:", filename), "success")
      log_operation("SUCCESS", "Comprehensive analysis export completed", paste("File:", filename))
    }, error = function(e) {
      show_message(paste("Error exporting comprehensive analysis:", e$message), "error")
      log_operation("ERROR", "Comprehensive analysis export failed", e$message)
    })
  })
}
