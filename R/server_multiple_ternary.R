# =============================================================================
# vidternary: Shiny Server Module - Multiple Ternary Creator
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Server-side logic for multiple ternary creator functionality including
#              file handling, column updates, and batch processing logic.
# 
# Key Functions:
#   - create_server_multiple_ternary(): Main multiple ternary creator server logic
#   - [Multiple ternary creator functions and batch processing]
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - shiny, openxlsx, fs
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

create_server_multiple_ternary <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {
  
  # ---- Multiple Ternary Creator Functionality ----
  
  # Reactive values for multiple ternary processing
  multiple_ternary_results <- reactiveValues(
    plots = NULL,
    status = NULL,
    file_names = NULL
  )
  
  # Update column choices for multiple ternary plots when files are selected
  observeEvent(input$multiple_xlsx_files, {
    req(input$multiple_xlsx_files)
    
    tryCatch({
      # Read the first file to get column names
      first_file <- input$multiple_xlsx_files$datapath[1]
      df <- openxlsx::read.xlsx(first_file, sheet = 1)
      
      # Get all column names
      all_columns <- colnames(df)
      
      # Update column choices for all element types
      updateSelectizeInput(session, "multiple_element_A", choices = all_columns, selected = NULL)
      updateSelectizeInput(session, "multiple_element_B", choices = all_columns, selected = NULL)
      updateSelectizeInput(session, "multiple_element_C", choices = all_columns, selected = NULL)
      updateSelectizeInput(session, "multiple_optional_param1", choices = all_columns, selected = NULL)
      updateSelectizeInput(session, "multiple_optional_param2", choices = all_columns, selected = NULL)
      
      # Store the dataframe for later use
      rv$multiple_df <- df
      
      # Log successful column update
      log_operation("SUCCESS", "Updated column choices for multiple ternary plots", 
                   paste("File:", basename(first_file), "Columns:", length(all_columns)))
      
    }, error = function(e) {
      show_message(paste("Error reading file for column selection:", e$message), "error")
      log_operation("ERROR", "Failed to read file for column selection", e$message)
    })
  })
  
  # Process multiple ternary plots
  process_multiple_ternary <- function() {
    req(input$multiple_xlsx_files)
    
    tryCatch({
      # Get all selected files
      files <- input$multiple_xlsx_files$datapath
      file_names <- input$multiple_xlsx_files$name
      
      # Validate inputs
      if (length(input$multiple_element_A) == 0 || 
          length(input$multiple_element_B) == 0 || 
          length(input$multiple_element_C) == 0) {
        show_message("Please select elements for A, B, and C axes", "warning")
        return(NULL)
      }
      
      # Initialize results
      results <- list()
      processed_count <- 0
      
      # Process each file
      for (i in seq_along(files)) {
        file_path <- files[i]
        file_name <- file_names[i]
        
        tryCatch({
          # Read the file
          df <- openxlsx::read.xlsx(file_path, sheet = 1)
          
          # Extract selected columns
          element_A_cols <- input$multiple_element_A
          element_B_cols <- input$multiple_element_B
          element_C_cols <- input$multiple_element_C
          
          # Create ternary plot data
          plot_data <- data.frame(
            A = rowSums(df[, element_A_cols, drop = FALSE], na.rm = TRUE),
            B = rowSums(df[, element_B_cols, drop = FALSE], na.rm = TRUE),
            C = rowSums(df[, element_C_cols, drop = FALSE], na.rm = TRUE)
          )
          
          # Normalize to percentages
          total <- plot_data$A + plot_data$B + plot_data$C
          plot_data$A_pct <- plot_data$A / total * 100
          plot_data$B_pct <- plot_data$B / total * 100
          plot_data$C_pct <- plot_data$C / total * 100
          
          # Add file identifier for color coding
          plot_data$file_source <- file_name
          
          # Store results
          results[[file_name]] <- list(
            data = plot_data,
            file_path = file_path,
            element_cols = list(A = element_A_cols, B = element_B_cols, C = element_C_cols)
          )
          
          processed_count <- processed_count + 1
          
        }, error = function(e) {
          show_message(paste("Error processing file", file_name, ":", e$message), "error")
          log_operation("ERROR", paste("Failed to process file:", file_name), e$message)
        })
      }
      
      # Update reactive values
      multiple_ternary_results$plots <- results
      multiple_ternary_results$status <- paste("Successfully processed", processed_count, "of", length(files), "files")
      multiple_ternary_results$file_names <- file_names
      
      # Show success message
      show_message(paste("Successfully processed", processed_count, "files"), "success")
      log_operation("SUCCESS", "Multiple ternary processing completed", 
                   paste("Processed", processed_count, "files"))
      
      return(results)
      
    }, error = function(e) {
      show_message(paste("Error in multiple ternary processing:", e$message), "error")
      log_operation("ERROR", "Multiple ternary processing failed", e$message)
      return(NULL)
    })
  }
  
  # Export selected multiple ternary results
  export_selected_multiple_ternary <- function(selected_files = NULL) {
    if (is.null(multiple_ternary_results$plots)) {
      show_message("No multiple ternary results to export", "warning")
      return(NULL)
    }
    
    tryCatch({
      # If no specific files selected, export all
      if (is.null(selected_files)) {
        selected_files <- names(multiple_ternary_results$plots)
      }
      
      # Filter results to selected files
      export_data <- multiple_ternary_results$plots[selected_files]
      
      if (length(export_data) == 0) {
        show_message("No valid files selected for export", "warning")
        return(NULL)
      }
      
      # Create export filename
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      export_filename <- paste0("multiple_ternary_results_", timestamp, ".xlsx")
      export_path <- file.path(rv$output_dir, export_filename)
      
      # Create workbook
      wb <- openxlsx::createWorkbook()
      
      # Add summary sheet
      summary_data <- data.frame(
        File = names(export_data),
        Rows = sapply(export_data, function(x) nrow(x$data)),
        A_Columns = sapply(export_data, function(x) paste(x$element_cols$A, collapse = ", ")),
        B_Columns = sapply(export_data, function(x) paste(x$element_cols$B, collapse = ", ")),
        C_Columns = sapply(export_data, function(x) paste(x$element_cols$C, collapse = ", "))
      )
      
      openxlsx::addWorksheet(wb, "Summary")
      openxlsx::writeData(wb, "Summary", summary_data)
      
      # Add data sheets for each file
      for (file_name in names(export_data)) {
        sheet_name <- substr(file_name, 1, 31) # Excel sheet name limit
        openxlsx::addWorksheet(wb, sheet_name)
        openxlsx::writeData(wb, sheet_name, export_data[[file_name]]$data)
      }
      
      # Save workbook
      openxlsx::saveWorkbook(wb, export_path, overwrite = TRUE)
      
      # Show success message
      show_message(paste("Exported", length(export_data), "files to", export_filename), "success")
      log_operation("SUCCESS", "Multiple ternary results exported", 
                   paste("Exported", length(export_data), "files to", export_filename))
      
      return(export_path)
      
    }, error = function(e) {
      show_message(paste("Error exporting multiple ternary results:", e$message), "error")
      log_operation("ERROR", "Failed to export multiple ternary results", e$message)
      return(NULL)
    })
  }
  
  # Get multiple ternary results
  get_multiple_ternary_results <- function() {
    return(multiple_ternary_results)
  }
  
  # Clear multiple ternary results
  clear_multiple_ternary_results <- function() {
    multiple_ternary_results$plots <- NULL
    multiple_ternary_results$status <- NULL
    multiple_ternary_results$file_names <- NULL
    rv$multiple_df <- NULL
  }
  
  # Button handlers
  observeEvent(input$create_multiple_ternary, {
    tryCatch({
      results <- process_multiple_ternary()
      if (!is.null(results)) {
        show_message("Multiple ternary plots created successfully!", "success")
      }
    }, error = function(e) {
      show_message(paste("Error creating multiple ternary plots:", e$message), "error")
    })
  })
  
  observeEvent(input$save_multiple_ternary, {
    tryCatch({
      if (is.null(multiple_ternary_results$plots)) {
        show_message("No plots to save. Please create plots first.", "warning")
        return()
      }
      
      # Get output directory
      output_dir <- if (!is.null(directory_management) && !is.null(directory_management$output_dir)) {
        directory_management$output_dir()
      } else {
        file.path(getwd(), "output")
      }
      
      # Create subfolder
      folder_name <- if (!is.null(input$multiple_ternary_folder_name) && nchar(input$multiple_ternary_folder_name) > 0) {
        input$multiple_ternary_folder_name
      } else {
        paste0("multiple_ternary_plots_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      }
      
      subfolder_path <- file.path(output_dir, folder_name)
      if (!dir.exists(subfolder_path)) {
        dir.create(subfolder_path, recursive = TRUE)
      }
      
      # Create combined plot with all files
      if (length(multiple_ternary_results$plots) > 1) {
        # Combine all data for comparison plot
        all_data <- do.call(rbind, lapply(names(multiple_ternary_results$plots), function(file_name) {
          plot_data <- multiple_ternary_results$plots[[file_name]]
          plot_data$file_source <- file_name
          return(plot_data)
        }))
        
        # Create comparison plot filename
        comparison_filename <- "multiple_ternary_comparison.png"
        comparison_path <- file.path(subfolder_path, comparison_filename)
        
        # Create comparison ternary plot with different colors
        png(comparison_path, width = 1000, height = 800, res = 150)
        
        # Define colors for different files
        file_names <- unique(all_data$file_source)
        colors <- rainbow(length(file_names))
        names(colors) <- file_names
        
        # Create base plot
        plot(all_data$A_pct, all_data$B_pct, 
             xlab = "Element A (%)", ylab = "Element B (%)",
             main = "Multiple Ternary Plots Comparison",
             pch = 16, cex = 0.6, col = colors[all_data$file_source])
        
        # Add legend
        legend("topright", legend = file_names, col = colors, pch = 16, cex = 0.8)
        
        dev.off()
        saved_count <- saved_count + 1
      }
      
      # Save individual plots
      for (file_name in names(multiple_ternary_results$plots)) {
        tryCatch({
          plot_data <- multiple_ternary_results$plots[[file_name]]
          
          # Create plot filename
          safe_filename <- gsub("[^A-Za-z0-9_-]", "_", file_name)
          plot_filename <- paste0(gsub("\\.(xlsx|xls)$", "", safe_filename, ignore.case = TRUE), "_ternary.png")
          plot_path <- file.path(subfolder_path, plot_filename)
          
          # Create ternary plot
          png(plot_path, width = 800, height = 600, res = 150)
          
          # Simple ternary plot
          plot(plot_data$A_pct, plot_data$B_pct, 
               xlab = "Element A (%)", ylab = "Element B (%)",
               main = paste("Ternary Plot:", file_name),
               pch = 16, col = "blue", cex = 0.8)
          
          dev.off()
          saved_count <- saved_count + 1
          
        }, error = function(e) {
          show_message(paste("Error saving plot for", file_name, ":", e$message), "error")
        })
      }
      
      if (saved_count > 0) {
        show_message(paste("Successfully saved", saved_count, "plots to:", subfolder_path), "success")
        log_operation("SUCCESS", "Multiple ternary plots saved", paste("Saved", saved_count, "plots to", subfolder_path))
      } else {
        show_message("No plots were saved successfully", "error")
      }
      
    }, error = function(e) {
      show_message(paste("Error saving multiple ternary plots:", e$message), "error")
      log_operation("ERROR", "Failed to save multiple ternary plots", e$message)
    })
  })
  
  # Status output
  output$multiple_ternary_status <- renderText({
    if (!is.null(multiple_ternary_results$status)) {
      multiple_ternary_results$status
    } else {
      "No multiple ternary plots processed yet."
    }
  })
  
  # Output display
  output$multiple_ternary_output <- renderUI({
    if (!is.null(multiple_ternary_results$plots)) {
      file_names <- names(multiple_ternary_results$plots)
      
      tagList(
        h4("Processed Files:"),
        lapply(file_names, function(file_name) {
          div(
            p(paste("✓", file_name)),
            style = "margin: 5px 0; color: green;"
          )
        })
      )
    } else {
      p("No files processed yet.")
    }
  })
  
  # Return the module functions for external use
  return(list(
    # Core functionality
    process_multiple_ternary = process_multiple_ternary,
    export_selected_multiple_ternary = export_selected_multiple_ternary,
    get_multiple_ternary_results = get_multiple_ternary_results,
    clear_multiple_ternary_results = clear_multiple_ternary_results,
    
    # Reactive values (for external access)
    multiple_ternary_results = multiple_ternary_results
  ))
}
