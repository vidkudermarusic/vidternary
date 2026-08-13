# ---- Server Ternary Plots Module: Batch ("Multiple Ternary Creator") ----
# Split out of server_ternary_plots.R: the multi-file batch preview/save
# handlers (as opposed to the single-file preview/save logic in
# server_ternary_plots.R, or the group-selection UI in
# server_ternary_plots_groups.R).

register_ternary_plots_batch_handlers <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {

  # Create multiple ternary plots (preview mode)
  observeEvent(input$create_multiple_ternary, {
    req(input$multiple_xlsx_files)
    req(input$multiple_element_A, input$multiple_element_B, input$multiple_element_C)

    if (length(input$multiple_element_A) == 0 || length(input$multiple_element_B) == 0 || length(input$multiple_element_C) == 0) {
      output$multiple_ternary_status <- renderText("Please select elements A, B, and C for all files")
      return()
    }

    tryCatch({
      output$multiple_ternary_status <- renderText("Creating ternary plots in preview mode...")

      file_paths <- input$multiple_xlsx_files$datapath
      file_names <- input$multiple_xlsx_files$name
      plots_created <- 0
      errors <- c()

      # Filter collection now handled by extract_ternary_params with multiple_mode = TRUE

      for (i in seq_along(file_paths)) {
        file_path <- file_paths[i]
        file_name <- file_names[i]

        tryCatch({
          # Use unified parameter extraction for multiple ternary preview
          temp_rv <- list(xlsx_file1 = file_path)
          params <- extract_ternary_params(input, temp_rv, 1, TRUE, directory_management, multiple_mode = TRUE)
          params$xlsx_file <- file_path  # Override for multiple files
          params$output_dir <- tempdir()  # Use temp directory for preview
          params$xlsx_display_name <- file_name  # Use the original file name for proper plot titles

          # Call the main ternary plot function
          result <- do.call(general_ternary_plot, params)

          if (!is.null(result)) {
            plots_created <- plots_created + 1
          }

        }, error = function(e) {
          errors <- c(errors, paste(file_name, "-", e$message))
        })
      }

      # Update results
      rv$multiple_ternary_results$plots <- plots_created

      if (plots_created > 0) {
        output$multiple_ternary_status <- renderText(paste("Successfully created", plots_created, "ternary plots in preview mode"))
        log_operation("SUCCESS", "Multiple ternary preview completed", paste("Created:", plots_created, "plots"))
      } else {
        output$multiple_ternary_status <- renderText("No plots were created successfully")
      }

      if (length(errors) > 0) {
        error_msg <- paste("Errors encountered:", paste(errors, collapse = "; "))
        output$multiple_ternary_status <- renderText(paste("Error creating multiple ternary plots:", error_msg))
        log_operation("ERROR", "Failed to create multiple ternary plots", error_msg)
      }

    }, error = function(e) {
      output$multiple_ternary_status <- renderText(paste("Error creating multiple ternary plots:", e$message))
      log_operation("ERROR", "Failed to create multiple ternary plots", e$message)
    })
  })

  # Save multiple ternary plots to subfolder
  observeEvent(input$save_multiple_ternary, {
    req(input$multiple_xlsx_files)
    req(input$multiple_element_A, input$multiple_element_B, input$multiple_element_C)

    if (length(input$multiple_element_A) == 0 || length(input$multiple_element_B) == 0 || length(input$multiple_element_C) == 0) {
      output$multiple_ternary_status <- renderText("Please select elements A, B, and C for all files")
      return()
    }

    tryCatch({
      # Create output directory using user-selected directory and folder name
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      user_output_dir <- if (!is.null(directory_management) && !is.null(directory_management$output_dir)) {
        directory_management$output_dir()
      } else {
        file.path(getwd(), "output")
      }

      # Use user-provided folder name or default
      folder_name <- if (!is.null(input$multiple_output_folder) && nchar(trimws(input$multiple_output_folder)) > 0) {
        trimws(input$multiple_output_folder)
      } else {
        "multiple_ternary_plots"
      }

      output_dir <- file.path(user_output_dir, paste0(folder_name, "_", timestamp))
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

      output$multiple_ternary_status <- renderText(paste("Saving ternary plots to:", output_dir, "..."))

      file_paths <- input$multiple_xlsx_files$datapath
      file_names <- input$multiple_xlsx_files$name
      plots_saved <- 0
      errors <- c()

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Processing", length(file_paths), "files for multiple ternary plots\n")
        cat("DEBUG: Files:", paste(file_names, collapse = ", "), "\n")
      }

      # Filter collection now handled by extract_ternary_params with multiple_mode = TRUE

      for (i in seq_along(file_paths)) {
        file_path <- file_paths[i]
        file_name <- file_names[i]

        if (getOption("ternary.debug", FALSE)) {
          cat("DEBUG: Processing file", i, "of", length(file_paths), ":", file_name, "\n")
        }

        tryCatch({
          # Use unified parameter extraction for multiple ternary
          temp_rv <- list(xlsx_file1 = file_path)
          params <- extract_ternary_params(input, temp_rv, 1, FALSE, directory_management, multiple_mode = TRUE)
          params$xlsx_file <- file_path  # Override for multiple files
          params$output_dir <- output_dir  # Use specified output directory
          params$xlsx_display_name <- file_name  # Use the original file name for unique filenames

          # Call the main ternary plot function
          result <- do.call(general_ternary_plot, params)

          if (!is.null(result)) {
            plots_saved <- plots_saved + 1
            if (getOption("ternary.debug", FALSE)) {
              cat("DEBUG: Successfully processed file", file_name, "\n")
            }
          } else {
            if (getOption("ternary.debug", FALSE)) {
              cat("DEBUG: general_ternary_plot returned NULL for file", file_name, "\n")
            }
          }

        }, error = function(e) {
          error_msg <- paste(file_name, "-", e$message)
          errors <- c(errors, error_msg)
          if (getOption("ternary.debug", FALSE)) {
            cat("DEBUG: Error processing file", file_name, ":", e$message, "\n")
          }
        })
      }

      # Update results
      rv$multiple_ternary_results$plots <- plots_saved

      if (plots_saved > 0) {
        output$multiple_ternary_status <- renderText(paste("Successfully saved", plots_saved, "ternary plots to:", output_dir))
        log_operation("SUCCESS", "Multiple ternary plots saved", paste("Saved:", plots_saved, "plots to", output_dir))
      } else {
        output$multiple_ternary_status <- renderText("No plots were saved successfully")
      }

      if (length(errors) > 0) {
        error_msg <- paste("Errors encountered:", paste(errors, collapse = "; "))
        output$multiple_ternary_status <- renderText(paste("Error saving multiple ternary plots:", error_msg))
        log_operation("ERROR", "Failed to save multiple ternary plots", error_msg)
      }

    }, error = function(e) {
      output$multiple_ternary_status <- renderText(paste("Error saving multiple ternary plots:", e$message))
      log_operation("ERROR", "Failed to save multiple ternary plots", e$message)
    })
  })
}
