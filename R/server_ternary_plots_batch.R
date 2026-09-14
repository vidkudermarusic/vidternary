# ---- Server Ternary Plots Module: Batch ("Multiple Ternary Creator") ----
# The multi-file batch handlers for the "Multiple Ternary Creator" tab (as
# opposed to the single-file preview/save logic in server_ternary_plots.R,
# or the group-selection UI in server_ternary_plots_groups.R).
#
# Its own independent moduleServer()/NS() tab, called directly from
# server_logic.R, with its own namespace: extract_ternary_params(...,
# multiple_mode = TRUE) below reads input$use_mahalanobis/use_iqr_filter/
# lambda/omega/etc. from this tab's own inputs, which resolve to NULL and
# fall back to their documented disabled/default values, matching this
# tab's own UI claim of "Statistical filtering: Disabled to maintain
# simplicity".

#' Wire up the "Multiple Ternary Creator" tab's batch server logic
#'
#' Registers the multi-file batch handlers: uploading several `.xlsx`
#' files, the per-element dynamic filter UI (`multiple_filters_A`/`B`/`C`),
#' and "Create & Save all ternary plots (zip)" - generating one ternary
#' plot per uploaded file from one shared set of element/filter parameters
#' (via `extract_ternary_params(..., multiple_mode = TRUE)`), zipped into
#' one browser download.
#' Called directly from `server_logic.R`'s own
#' `moduleServer("multiple_ternary", ...)`, independent of the "Ternary
#' Plots" tab's own module/namespace.
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @param rv The app's shared `reactiveValues` object.
#' @param show_message Function to show a user-facing status message.
#' @param log_operation Function to record a structured log entry.
#' @return Not meaningful (whatever its last statement happens to
#'   evaluate to, with no explicit `return()`) - called for its side
#'   effect of registering observers/outputs.
#' @export
register_ternary_plots_batch_handlers <- function(input, output, session, rv, show_message, log_operation) {

  # Batch status (baseline; the create/save handler below overwrites this
  # with progress/result messages once the button is clicked)
  output$multiple_ternary_status <- renderText({
    "No multiple ternary plots created yet. Click 'Create & Save all ternary plots (zip)' to start."
  })

  # ---- Dynamic Filter UI Generation (per-element) ----
  output$multiple_filters_A <- renderUI({
    req(input$multiple_element_A)
    lapply(input$multiple_element_A, function(element) {
      div(
        style = "margin: 5px 0; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
        h6(paste("Filter for", element)),
        textInput(session$ns(paste0("multiple_filter_A_", gsub("[^A-Za-z0-9]", "_", element))),
                 paste("Threshold for", element),
                 placeholder = paste("e.g., > 10"))
      )
    })
  })

  output$multiple_filters_B <- renderUI({
    req(input$multiple_element_B)
    lapply(input$multiple_element_B, function(element) {
      div(
        style = "margin: 5px 0; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
        h6(paste("Filter for", element)),
        textInput(session$ns(paste0("multiple_filter_B_", gsub("[^A-Za-z0-9]", "_", element))),
                 paste("Threshold for", element),
                 placeholder = paste("e.g., > 10"))
      )
    })
  })

  output$multiple_filters_C <- renderUI({
    req(input$multiple_element_C)
    lapply(input$multiple_element_C, function(element) {
      div(
        style = "margin: 5px 0; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
        h6(paste("Filter for", element)),
        textInput(session$ns(paste0("multiple_filter_C_", gsub("[^A-Za-z0-9]", "_", element))),
                 paste("Threshold for", element),
                 placeholder = paste("e.g., > 10"))
      )
    })
  })

  output$multiple_optional_param1_filter <- renderUI({
    req(input$multiple_optional_param1)
    lapply(input$multiple_optional_param1, function(element) {
      div(
        style = "margin: 5px 0; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
        h6(paste("Filter for", element)),
        textInput(session$ns(paste0("multiple_filter_op1_", gsub("[^A-Za-z0-9]", "_", element))),
                 paste("Threshold for", element),
                 placeholder = paste("e.g., > 10"))
      )
    })
  })

  output$multiple_optional_param2_filter <- renderUI({
    req(input$multiple_optional_param2)
    lapply(input$multiple_optional_param2, function(element) {
      div(
        style = "margin: 5px 0; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
        h6(paste("Filter for", element)),
        textInput(session$ns(paste0("multiple_filter_op2_", gsub("[^A-Za-z0-9]", "_", element))),
                 paste("Threshold for", element),
                 placeholder = paste("e.g., > 10"))
      )
    })
  })

  # Populate Element A/B/C and Optional Parameter 1/2 choices from the first
  # uploaded file's columns, independently of the main Ternary Plots tab.
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

  # Create AND save one ternary plot per uploaded file, all zipped into one
  # browser download. Writes into a fresh temp directory, then zips
  # whatever succeeded.
  output$create_save_multiple_ternary <- downloadHandler(
    filename = function() {
      folder_name <- if (!is.null(input$multiple_output_folder) && nchar(trimws(input$multiple_output_folder)) > 0) {
        trimws(input$multiple_output_folder)
      } else {
        "multiple_ternary_plots"
      }
      paste0(folder_name, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".zip")
    },
    content = function(file) {
      if (is.null(input$multiple_xlsx_files)) stop("Please select at least one Excel file first.")
      if (length(input$multiple_element_A) == 0 || length(input$multiple_element_B) == 0 || length(input$multiple_element_C) == 0) {
        stop("Please select elements A, B, and C for all files.")
      }

      output_dir <- tempfile("multiple_ternary_save_")
      dir.create(output_dir, recursive = TRUE)
      # Unlike the single-plot Save handlers (server_ternary_plots.R,
      # server_hex_ternary.R), which deliberately leave their one-file temp
      # directory for the OS's normal temp-file cleanup (see those files'
      # own comments), a batch run writes N plots per click - on a
      # long-running Shiny process this accumulates real, unbounded disk
      # usage rather than the negligible single-file case those comments
      # describe. on.exit() (not a call at the end of content()) so this
      # still runs if a later step - the errors.txt write, zip::zip()
      # itself, or the stop() below on a total failure - raises first.
      on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

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
          params <- extract_ternary_params(input, temp_rv, 1, FALSE, multiple_mode = TRUE)
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
          # content() function.
          error_msg <- paste(file_name, "-", e$message)
          errors <<- c(errors, error_msg)
          if (getOption("ternary.debug", FALSE)) {
            cat("DEBUG: Error processing file", file_name, ":", e$message, "\n")
          }
        })
      }

      # Exactly one status message per outcome - full success, partial
      # success, or total failure - always including both the save count
      # and any errors that occurred.
      if (length(errors) == 0 && plots_saved > 0) {
        output$multiple_ternary_status <- renderText(paste("Successfully saved", plots_saved, "ternary plots"))
        log_operation("SUCCESS", "Multiple ternary plots saved", paste("Saved:", plots_saved, "plots"))
      } else if (plots_saved > 0) {
        error_msg <- paste("Errors encountered:", paste(errors, collapse = "; "))
        output$multiple_ternary_status <- renderText(paste0("Saved ", plots_saved, " ternary plot", if (plots_saved != 1) "s" else "",
                                                              "\n", error_msg))
        log_operation("WARNING", "Some multiple ternary plots failed", paste("Saved:", plots_saved, "| Errors:", error_msg))
      } else {
        error_msg <- if (length(errors) > 0) paste("Errors encountered:", paste(errors, collapse = "; ")) else "No plots were saved successfully"
        output$multiple_ternary_status <- renderText(paste("Error saving multiple ternary plots:", error_msg))
        log_operation("ERROR", "Failed to save multiple ternary plots", error_msg)
      }

      if (plots_saved == 0) stop(paste("Failed to save any ternary plots:", paste(errors, collapse = "; ")))
      if (length(errors) > 0) writeLines(errors, file.path(output_dir, "errors.txt"))

      zip::zip(file, files = list.files(output_dir), root = output_dir)
    }
  )
}
