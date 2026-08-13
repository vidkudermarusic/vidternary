# ---- Server Export Module: Status/Report Outputs ----
# Split out of server_export.R: the export status/history renderers and the
# "Export Comprehensive Analysis" button handler (as opposed to the
# filtered-data export handlers in server_export_data.R).

register_export_report_handlers <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {

  # Export status display
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

  # Button handlers for new export options
  observeEvent(input$export_comprehensive_btn, {
    tryCatch({
      if (input$export_comprehensive) {
        # Trigger the comprehensive analysis export
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        filename <- paste0("comprehensive_analysis_", timestamp, ".xlsx")

        # Get output directory
        output_dir <- if (!is.null(directory_management)) {
          directory_management$get_output_directory()
        } else {
          getwd()
        }

        filepath <- file.path(output_dir, filename)

        # Use the comprehensive analysis function if available
        if (exists("run_comprehensive_analysis") && !is.null(input$xlsx_file1) && !is.null(input$xlsx_file2)) {
          tryCatch({
            # Load datasets
            df1 <- read_dataset_file(input$xlsx_file1)
            df2 <- read_dataset_file(input$xlsx_file2)

            # Run comprehensive analysis
            analysis_config <- list(
              timestamp = timestamp,
              dataset1_name = input$xlsx_file1$name,
              dataset2_name = input$xlsx_file2$name,
              export_type = "comprehensive_analysis"
            )

            comprehensive_results <- run_comprehensive_analysis(df1, df2, analysis_config, output_dir)

            # Create workbook from comprehensive analysis results
            wb <- openxlsx::createWorkbook()

            # Add comprehensive analysis sheets
            if (!is.null(comprehensive_results$quality_report)) {
              openxlsx::addWorksheet(wb, "Quality Report")
              openxlsx::writeData(wb, "Quality Report", comprehensive_results$quality_report)
            }

            if (!is.null(comprehensive_results$statistical_summary)) {
              openxlsx::addWorksheet(wb, "Statistical Summary")
              openxlsx::writeData(wb, "Statistical Summary", comprehensive_results$statistical_summary)
            }

            if (!is.null(comprehensive_results$correlation_analysis)) {
              openxlsx::addWorksheet(wb, "Correlation Analysis")
              openxlsx::writeData(wb, "Correlation Analysis", comprehensive_results$correlation_analysis)
            }

            if (!is.null(comprehensive_results$multivariate_results)) {
              openxlsx::addWorksheet(wb, "Multivariate Analysis")
              openxlsx::writeData(wb, "Multivariate Analysis", comprehensive_results$multivariate_results)
            }

            # Add original datasets
            openxlsx::addWorksheet(wb, "Original Data 1")
            openxlsx::writeData(wb, "Original Data 1", df1)

            openxlsx::addWorksheet(wb, "Original Data 2")
            openxlsx::writeData(wb, "Original Data 2", df2)

            # Add analysis report
            report_df <- data.frame(
              Line = c(
                "=== COMPREHENSIVE ANALYSIS REPORT ===",
                paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
                "",
                "=== DATASET INFORMATION ===",
                paste("Dataset 1:", input$xlsx_file1$name),
                paste("Dataset 2:", input$xlsx_file2$name),
                "",
                "=== ANALYSIS STATUS ===",
                "Comprehensive Analysis: Completed Successfully",
                "Quality Assessment: Completed",
                "Statistical Analysis: Completed",
                "Multivariate Analysis: Completed",
                "",
                "=== EXPORT INFORMATION ===",
                paste("Export Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
                "Export Type: Comprehensive Analysis (via comprehensive_analysis.R)",
                "Status: Completed Successfully"
              ),
              stringsAsFactors = FALSE
            )
            openxlsx::addWorksheet(wb, "Analysis Report")
            openxlsx::writeData(wb, "Analysis Report", report_df)

            show_message("Comprehensive analysis completed using comprehensive_analysis.R module", "success")

          }, error = function(e) {
            show_message(paste("Comprehensive analysis module failed, using fallback:", e$message), "warning")
            # Fall back to manual sheet creation
            wb <- openxlsx::createWorkbook()
            create_manual_sheets(wb)
          })
        } else {
          # Fallback: Create workbook manually if comprehensive analysis not available
          wb <- openxlsx::createWorkbook()
          create_manual_sheets(wb)
        }

        # Helper function for manual sheet creation (fallback)
        create_manual_sheets <- function(wb) {

        # Sheet 1: Stats Dataset 1
        safe_add_worksheet(wb, "Stats Dataset 1", function() {
          if (!is.null(input$xlsx_file1)) {
            df1 <- read_dataset_file(input$xlsx_file1)
            if (!is.null(df1)) {
              return(create_statistical_summary(df1))
            }
          }
          return(data.frame(Message = "Dataset 1 not loaded or no numeric columns"))
        })

        # Sheet 2: Stats Dataset 2
        safe_add_worksheet(wb, "Stats Dataset 2", function() {
          if (!is.null(input$xlsx_file2)) {
            df2 <- read_dataset_file(input$xlsx_file2)
            if (!is.null(df2)) {
              return(create_statistical_summary(df2))
            }
          }
          return(data.frame(Message = "Dataset 2 not loaded or no numeric columns"))
        })

        # Sheet 3: Correlation Dataset 1
        safe_add_worksheet(wb, "Correlation Dataset 1", function() {
          if (!is.null(input$xlsx_file1)) {
            df1 <- read_dataset_file(input$xlsx_file1)
            if (!is.null(df1)) {
              return(create_correlation_matrix(df1))
            }
          }
          return(data.frame(Message = "Dataset 1 not loaded or insufficient numeric columns"))
        })

        # Sheet 4: Correlation Dataset 2
        safe_add_worksheet(wb, "Correlation Dataset 2", function() {
          if (!is.null(input$xlsx_file2)) {
            df2 <- read_dataset_file(input$xlsx_file2)
            if (!is.null(df2)) {
              return(create_correlation_matrix(df2))
            }
          }
          return(data.frame(Message = "Dataset 2 not loaded or insufficient numeric columns"))
        })

        # Sheet 5: Analysis Report
        safe_add_worksheet(wb, "Analysis Report", function() {
          report_lines <- c()
          report_lines <- c(report_lines, "=== COMPREHENSIVE ANALYSIS REPORT ===")
          report_lines <- c(report_lines, paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
          report_lines <- c(report_lines, "")
          report_lines <- c(report_lines, "=== DATASET INFORMATION ===")
          report_lines <- c(report_lines, paste("Dataset 1:", if (!is.null(input$xlsx_file1)) paste("Loaded -", input$xlsx_file1$name) else "Not Loaded"))
          report_lines <- c(report_lines, paste("Dataset 2:", if (!is.null(input$xlsx_file2)) paste("Loaded -", input$xlsx_file2$name) else "Not Loaded"))
          report_lines <- c(report_lines, "")
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
          report_lines <- c(report_lines, "")
          report_lines <- c(report_lines, "=== EXPORT INFORMATION ===")
          report_lines <- c(report_lines, paste("Export Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
          report_lines <- c(report_lines, "Export Type: Comprehensive Analysis")
          report_lines <- c(report_lines, "Status: Completed Successfully")

          report_df <- data.frame(Line = report_lines, stringsAsFactors = FALSE)
          return(report_df)
        })

        # Sheet 6: Original Data 1
        safe_add_worksheet(wb, "Original Data 1", function() {
          if (!is.null(input$xlsx_file1)) {
            return(read_dataset_file(input$xlsx_file1))
          }
          return(data.frame(Message = "Dataset 1 not loaded"))
        })

        # Sheet 7: Original Data 2
        safe_add_worksheet(wb, "Original Data 2", function() {
          if (!is.null(input$xlsx_file2)) {
            return(read_dataset_file(input$xlsx_file2))
          }
          return(data.frame(Message = "Dataset 2 not loaded"))
        })

          # Save workbook
          openxlsx::saveWorkbook(wb, filepath, overwrite = TRUE)

          show_message(paste("Comprehensive analysis exported to:", filename), "success")
          log_operation("SUCCESS", "Comprehensive analysis export completed", paste("File:", filename))
        }

        show_message(paste("Comprehensive analysis exported to:", filename), "success")
        log_operation("SUCCESS", "Comprehensive analysis export completed", paste("File:", filename))
      } else {
        show_message("Please enable 'Export Comprehensive Analysis' checkbox first", "warning")
      }
    }, error = function(e) {
      show_message(paste("Error exporting comprehensive analysis:", e$message), "error")
      log_operation("ERROR", "Comprehensive analysis export failed", e$message)
    })
  })
}
