# ---- Server Ternary Plots Module: Batch ("Multiple Ternary Creator") ----
# Split out of server_ternary_plots.R: the multi-file batch handlers (as
# opposed to the single-file preview/save logic in server_ternary_plots.R,
# or the group-selection UI in server_ternary_plots_groups.R).
#
# Now its own independent moduleServer()/NS() tab, called directly from
# server_logic.R rather than nested inside Ternary Plots' server function -
# see server_ternary_plots.R's header comment for why. Its per-element
# dynamic filter UI (multiple_filters_A/B/C etc.) used to live in a shared
# server_filter_management.R that also built Ternary Plots' filter UI in
# the same call; moved directly into this file below as this tab's half of
# that split.
#
# BEHAVIOR CHANGE from the module split (confirmed intentional - see the
# vidternary Structural Audit): extract_ternary_params(..., multiple_mode =
# TRUE) below reads input$use_mahalanobis/use_iqr_filter/lambda/omega/etc. -
# before the split those silently resolved to whatever was set on the (then
# shared-namespace) Ternary Plots tab, even though this tab's own UI always
# claimed "Statistical filtering: Disabled to maintain simplicity". Now that
# this is a genuinely separate module, those all correctly resolve to NULL
# here and fall back to their documented disabled/default values - this
# tab's actual behavior now matches what it already claimed.

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

  # Create AND save one ternary plot per uploaded file, all zipped into one
  # browser download. Previously this was two separate buttons - "Create
  # All Ternary Plots" rendered to whatever graphics device happened to be
  # active (not any Shiny output, since there's no plotOutput/renderPlot for
  # it), which produced no visible result and stray Rplots.pdf files instead
  # of a real preview - so it's been folded into the one button that always
  # saves. Writes into a fresh temp directory rather than a subfolder under
  # a pre-chosen server-side Output Directory, then zips whatever succeeded
  # - see the vidternary Structural Audit's §03 for why the previous global
  # directory picker was removed.
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
          # content() function - so every per-file error was silently
          # discarded and "Errors encountered" never fired, even when
          # every file failed.
          error_msg <- paste(file_name, "-", e$message)
          errors <<- c(errors, error_msg)
          if (getOption("ternary.debug", FALSE)) {
            cat("DEBUG: Error processing file", file_name, ":", e$message, "\n")
          }
        })
      }

      # One status message reflecting the actual combined outcome - the
      # errors-block used to run afterward unconditionally, always
      # overwriting whatever the success branch above had just set - so a
      # batch of 10 files where 9 succeeded and 1 failed showed only the
      # error text, with zero indication that 9 real plots had actually
      # been saved. Now there's exactly one message per outcome - full
      # success, partial success, or total failure - always including any
      # errors that actually occurred, never silently dropping the save
      # count that happened alongside them.
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
