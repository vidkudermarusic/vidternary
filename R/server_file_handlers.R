# ---- Server File Handlers Module ----
# This module contains file upload/download logic and parameter copying
# functionality - entirely for the Ternary Plots tab (Dataset 1/2), called
# from within its moduleServer() wrapper in server_logic.R.
#
# This used to also push column choices into Multiple Ternary Creator's
# selectors (multiple_element_A/B/C etc.) and a since-removed "Multiple
# Plot Types" tab's (scatter_columns/histogram_columns/boxplot_columns -
# those IDs don't exist in the current UI at all, a harmless no-op).
# Removed as part of converting Multiple Ternary Creator to its own
# moduleServer(): it already has its own independent column-population
# logic (see server_ternary_plots_batch.R's observeEvent(input$
# multiple_xlsx_files, ...)) from an earlier fix, so this was always a
# redundant shortcut, not the only path - and once namespaced, this
# tab's session can no longer reach into a different module's inputs
# anyway.

#' Wire up Dataset 1/2 file upload and "Copy Settings" for the "Ternary Plots" tab
#'
#' Registers the Dataset 1/2 `.xlsx` upload handlers (loading the file,
#' populating the element/optional-parameter/multivariate column choices)
#' and the "Copy Settings from Dataset 1" handler, which copies element
#' selections, optional parameters and their per-element filters, the
#' color palette, and multivariate-analysis settings from Dataset 1 onto
#' Dataset 2. Called from [create_server_ternary_plots()], sharing its
#' `moduleServer("ternary_plots", ...)` namespace and `rv`.
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @param rv The app's shared `reactiveValues` object (writes `df1`/`df2`,
#'   `xlsx_file1`/`xlsx_file2`, and resets the categorical-group fields
#'   `group_counts_1`/`_2`, `group_selections_1`/`_2`,
#'   `is_categorical_group_1`/`_2` on every fresh upload).
#' @param show_message Function to show a user-facing status message.
#' @param log_operation Function to record a structured log entry.
#' @return An empty list - this function's effect is entirely the
#'   observers it registers.
#' @export
create_server_file_handlers <- function(input, output, session, rv, show_message, log_operation) {
  
  # ---- File Upload Handlers ----
  
  # Handle file uploads and populate column choices for Dataset 1
  observeEvent(input$xlsx_file1, {
    req(input$xlsx_file1)
    tryCatch({
      data <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
      rv$df1 <- data
      rv$xlsx_file1 <- input$xlsx_file1$datapath  # Store the file path
      
      # Update column choices
      updateSelectizeInput(session, "element_A1", choices = names(data))
      updateSelectizeInput(session, "element_B1", choices = names(data))
      updateSelectizeInput(session, "element_C1", choices = names(data))
      updateSelectizeInput(session, "optional_param1_1", choices = c("", names(data)))
      # Reset to blank rather than letting Shiny try to preserve the
      # previous selection: server_ternary_plots_groups.R's categorical-
      # group detection only re-runs when optional_param2_1's own VALUE
      # changes, so if the new file happens to share a column name with
      # whatever was already selected (e.g. two datasets both having a
      # "Shape" column), the dropdown's value would stay the same, that
      # detection observer would never re-fire, and rv$group_counts_1/
      # rv$group_selections_1 would silently keep referring to the
      # PREVIOUS file's now-stale group data - reachable in the running
      # app, not a hypothetical, since group names/counts are computed
      # per-file. Resetting to "" forces a real value change (clearing the
      # stale state via that observer's own already-existing else branch)
      # and the rv fields below are cleared directly too, so nothing
      # stale can be displayed even for the instant before the observer
      # reacts.
      updateSelectizeInput(session, "optional_param2_1", choices = c("", names(data)), selected = "")
      rv$group_counts_1 <- NULL
      rv$group_selections_1 <- NULL
      rv$is_categorical_group_1 <- FALSE

      show_message("Dataset 1 loaded successfully!", "success")
      log_operation("INFO", "Dataset 1 loaded", paste("File:", input$xlsx_file1$name, "Rows:", nrow(data), "Columns:", ncol(data)))
    }, error = function(e) {
      show_message(paste("Error loading Dataset 1:", e$message), "error")
      log_operation("ERROR", "Failed to load Dataset 1", e$message)
    })
    
    # Update column choices for multivariate analysis when files are uploaded
    new_M <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
    new_col_names <- colnames(new_M)
    
    # Update multivariate analysis column choices
    numeric_cols <- new_col_names[sapply(new_M, is.numeric)]
    updateSelectizeInput(session, "multivariate_columns", choices = numeric_cols, selected = character(0))
  })
  
  # Handle file uploads and populate column choices for Dataset 2
  observeEvent(input$xlsx_file2, {
    req(input$xlsx_file2)
    tryCatch({
      data <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
      rv$df2 <- data
      rv$xlsx_file2 <- input$xlsx_file2$datapath  # Store the file path
      
      # Update column choices
      updateSelectizeInput(session, "element_A2", choices = names(data))
      updateSelectizeInput(session, "element_B2", choices = names(data))
      updateSelectizeInput(session, "element_C2", choices = names(data))
      updateSelectizeInput(session, "optional_param1_2", choices = c("", names(data)))
      # Same reset as Dataset 1's handler above - see that comment.
      updateSelectizeInput(session, "optional_param2_2", choices = c("", names(data)), selected = "")
      rv$group_counts_2 <- NULL
      rv$group_selections_2 <- NULL
      rv$is_categorical_group_2 <- FALSE

      show_message("Dataset 2 loaded successfully!", "success")
      log_operation("INFO", "Dataset 2 loaded", paste("File:", input$xlsx_file2$name, "Rows:", nrow(data), "Columns:", ncol(data)))
    }, error = function(e) {
      show_message(paste("Error loading Dataset 2:", e$message), "error")
      log_operation("ERROR", "Failed to load Dataset 2", e$message)
    })
    
    # Update multivariate analysis column choices for dataset 2
    new_M <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
    new_col_names <- colnames(new_M)
    numeric_cols <- new_col_names[sapply(new_M, is.numeric)]
    updateSelectizeInput(session, "multivariate_columns", choices = numeric_cols, selected = character(0))
  })
  
  # ---- Copy Settings Functionality ----
  
  # Copy all settings from Dataset 1 to Dataset 2
  observeEvent(input$copy_settings, {
    req(input$xlsx_file1, input$xlsx_file2)

    tryCatch({
      # Copy element selections
      updateSelectInput(session, "element_A2", selected = input$element_A1)
      updateSelectInput(session, "element_B2", selected = input$element_B1)
      updateSelectInput(session, "element_C2", selected = input$element_C1)

      # Copy per-element filters (dynamic_filters_A2/B2/C2, built from the
      # element selections just above). Previously not copied at all -
      # despite the button's own label claiming "all settings" - because
      # doing this correctly needs a real fix, not an oversight: these
      # textInputs are (re)built by output$dynamic_filters_A2/B2/C2's
      # renderUI(), which only reacts to element_A2/B2/C2 on the NEXT
      # reactive flush, so the filter_A2_<element> inputs being copied
      # into here don't exist yet in this observer's own execution -
      # copying to them synchronously would silently do nothing, the same
      # class of timing hazard server_plot_builder.R's preset-load handler
      # already works around with session$onFlushed(). Reads Dataset 1's
      # current filter values now (before any UI changes), and writes them
      # once Dataset 2's matching inputs actually exist.
      filters_A1 <- collect_main_ternary_filters(input$element_A1, "A", 1, input)
      filters_B1 <- collect_main_ternary_filters(input$element_B1, "B", 1, input)
      filters_C1 <- collect_main_ternary_filters(input$element_C1, "C", 1, input)
      session$onFlushed(function() {
        copy_element_filters <- function(filters, element_type) {
          for (element in names(filters)) {
            safe_element <- gsub("[^A-Za-z0-9]", "_", element)
            updateTextInput(session, paste0("filter_", element_type, "2_", safe_element), value = filters[[element]])
          }
        }
        copy_element_filters(filters_A1, "A")
        copy_element_filters(filters_B1, "B")
        copy_element_filters(filters_C1, "C")
      }, once = TRUE)

      # Copy optional parameters
      updateSelectInput(session, "optional_param1_2", selected = input$optional_param1_1)
      updateSelectInput(session, "optional_param2_2", selected = input$optional_param2_1)

      # Copy optional parameter representations
      updateSelectInput(session, "optional_param1_representation2", selected = input$optional_param1_representation1)

      # Copy filters
      updateTextInput(session, "filter_op1_2", value = input$filter_op1_1)
      updateTextInput(session, "filter_op2_2", value = input$filter_op2_1)

      # Copy the color palette (for Optional Param 2) - previously the one
      # other setting the button's "all settings" label promised but
      # didn't deliver.
      updateSelectInput(session, "color_palette2", selected = input$color_palette1)

      # Copy multivariate analysis settings
      updateCheckboxInput(session, "use_mahalanobis", value = input$use_mahalanobis)
      updateCheckboxInput(session, "use_isolation_forest", value = input$use_isolation_forest)
      updateCheckboxInput(session, "use_iqr_filter", value = input$use_iqr_filter)
      updateCheckboxInput(session, "use_zscore_filter", value = input$use_zscore_filter)
      updateCheckboxInput(session, "use_mad_filter", value = input$use_mad_filter)
      
      # Copy advanced parameters
      updateNumericInput(session, "lambda", value = input$lambda)
      updateNumericInput(session, "omega", value = input$omega)
      updateRadioButtons(session, "outlier_mode_mahalanobis", selected = input$outlier_mode_mahalanobis)
      updateRadioButtons(session, "outlier_mode_isolation", selected = input$outlier_mode_isolation)
      updateRadioButtons(session, "outlier_mode_iqr", selected = input$outlier_mode_iqr)
      updateRadioButtons(session, "outlier_mode_zscore", selected = input$outlier_mode_zscore)
      updateRadioButtons(session, "outlier_mode_mad", selected = input$outlier_mode_mad)
      updateRadioButtons(session, "mdthresh_mode", selected = input$mdthresh_mode)
      updateNumericInput(session, "custom_mdthresh", value = input$custom_mdthresh)
      updateRadioButtons(session, "mahalanobis_reference", selected = input$mahalanobis_reference)
      updateRadioButtons(session, "mahalanobis_reference_isolation", selected = input$mahalanobis_reference_isolation)
      updateSelectizeInput(session, "multivariate_columns", selected = input$multivariate_columns)
      
      show_message("Settings copied from Dataset 1 to Dataset 2 successfully!", "success")
      log_operation("INFO", "Settings copied", "All settings copied from Dataset 1 to Dataset 2")
      
    }, error = function(e) {
      show_message(paste("Error copying settings:", e$message), "error")
      log_operation("ERROR", "Failed to copy settings", e$message)
    })
  })
  
  # Return the file handler functions for integration
  return(list(
    # File upload handlers are already set up as observeEvent
    # Parameter copying is already set up as observeEvent
    # This function just sets up the event handlers
  ))
}
