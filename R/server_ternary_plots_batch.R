# ---- Server Ternary Plots Module: Batch ("Multiple Ternary Creator") ----
# Split out of server_ternary_plots.R: the multi-file batch handlers (as
# opposed to the single-file preview/save logic in server_ternary_plots.R,
# or the group-selection UI in server_ternary_plots_groups.R).

register_ternary_plots_batch_handlers <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {

  # Populate Element A/B/C and Optional Parameter 1/2 choices from the first
  # uploaded file's columns. This used to only happen as a side effect of
  # uploading a file to the *main* Ternary Plots tab's Dataset 1 input
  # (see server_file_handlers.R), so a user going straight to this tab and
  # uploading files via multiple_xlsx_files found every dropdown empty -
  # req(input$multiple_element_A, ...) then silently blocked both buttons
  # below, with no created/saved plots and no error shown.
  observeEvent(input$multiple_xlsx_files, {
    req(input$multiple_xlsx_files)
    tryCatch({
      first_file <- input$multiple_xlsx_files$datapath[1]
      df <- openxlsx::read.xlsx(first_file, sheet = 1)
      all_columns <- colnames(df)

      updateSelectizeInput(session, "multiple_element_A", choices = all_columns)
      updateSelectizeInput(session, "multiple_element_B", choices = all_columns)
      updateSelectizeInput(session, "multiple_element_C", choices = all_columns)
      updateSelectizeInput(session, "multiple_optional_param1", choices = c("", all_columns))
      updateSelectizeInput(session, "multiple_optional_param2", choices = c("", all_columns))

      log_operation("SUCCESS", "Updated column choices for Multiple Ternary Creator",
                    paste("File:", basename(first_file), "Columns:", length(all_columns)))
    }, error = function(e) {
      show_message(paste("Error reading file for column selection:", e$message), "error")
      log_operation("ERROR", "Failed to read file for column selection", e$message)
    })
  })

  # Create AND save one ternary plot per uploaded file, straight to the
  # output subfolder. Previously this was two separate buttons - "Create All
  # Ternary Plots" rendered to whatever graphics device happened to be
  # active (not any Shiny output, since there's no plotOutput/renderPlot for
  # it), which produced no visible result and stray Rplots.pdf files instead
  # of a real preview - so it's been folded into the one button that always
  # saves, matching what "Save All Plots to Subfolder" already did correctly.
  observeEvent(input$create_save_multiple_ternary, {
    req(input$multiple_xlsx_files)
    req(input$multiple_element_A, input$multiple_element_B, input$multiple_element_C)

    if (length(input$multiple_element_A) == 0 || length(input$multiple_element_B) == 0 || length(input$multiple_element_C) == 0) {
      output$multiple_ternary_status <- renderText("Please select elements A, B, and C for all files")
      return()
    }

    tryCatch({
      timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      user_output_dir <- if (!is.null(directory_management) && !is.null(directory_management$output_dir)) {
        directory_management$output_dir()
      } else {
        file.path(getwd(), "output")
      }

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

      for (i in seq_along(file_paths)) {
        file_path <- file_paths[i]
        file_name <- file_names[i]

        if (getOption("ternary.debug", FALSE)) {
          cat("DEBUG: Processing file", i, "of", length(file_paths), ":", file_name, "\n")
        }

        tryCatch({
          temp_rv <- list(xlsx_file1 = file_path)
          params <- extract_ternary_params(input, temp_rv, 1, FALSE, directory_management, multiple_mode = TRUE)
          params$xlsx_file <- file_path
          params$output_dir <- output_dir
          params$xlsx_display_name <- file_name

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
          # <<- (not <-) is required: `errors` inside this closure would
          # otherwise be a new local variable in the closure's own
          # environment, never reaching the `errors` in the enclosing
          # observeEvent - so every per-file error was silently discarded
          # and "Errors encountered" never fired, even when every file failed.
          error_msg <- paste(file_name, "-", e$message)
          errors <<- c(errors, error_msg)
          if (getOption("ternary.debug", FALSE)) {
            cat("DEBUG: Error processing file", file_name, ":", e$message, "\n")
          }
        })
      }

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
