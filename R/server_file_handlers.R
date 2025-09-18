# =============================================================================
# vidternary: Shiny Server Module - File Handling
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Server-side logic for file upload/download operations and
#              parameter copying functionality between datasets.
# 
# Key Functions:
#   - create_server_file_handlers(): Main file handling server logic
#   - [File upload, download, and parameter copying functions]
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - shiny, shinyFiles, openxlsx, writexl
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

# File Handling and Parameter Copying Functions
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
      updateSelectizeInput(session, "optional_param2_1", choices = c("", names(data)))
      
      # Update multiple plot types columns
      updateSelectizeInput(session, "scatter_columns", choices = names(data))
      updateSelectizeInput(session, "scatter_x_col", choices = names(data))
      updateSelectizeInput(session, "scatter_y_col", choices = names(data))
      updateSelectizeInput(session, "histogram_columns", choices = names(data))
      updateSelectizeInput(session, "boxplot_columns", choices = names(data))
      
      # Update multiple ternary creator columns
      updateSelectizeInput(session, "multiple_element_A", choices = names(data))
      updateSelectizeInput(session, "multiple_element_B", choices = names(data))
      updateSelectizeInput(session, "multiple_element_C", choices = names(data))
      updateSelectizeInput(session, "multiple_optional_param1", choices = c("", names(data)))
      updateSelectizeInput(session, "multiple_optional_param2", choices = c("", names(data)))
      
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
      updateSelectizeInput(session, "optional_param2_2", choices = c("", names(data)))
      
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
      
      # Copy optional parameters
      updateSelectInput(session, "optional_param1_2", selected = input$optional_param1_1)
      updateSelectInput(session, "optional_param2_2", selected = input$optional_param2_1)
      
      # Copy optional parameter representations
      updateSelectInput(session, "optional_param1_representation2", selected = input$optional_param1_representation1)
      
      # Copy filters
      updateTextInput(session, "filter_op1_2", value = input$filter_op1_1)
      updateTextInput(session, "filter_op2_2", value = input$filter_op2_1)
      
      # Copy multivariate analysis settings
      updateCheckboxInput(session, "use_mahalanobis", value = input$use_mahalanobis)
      updateCheckboxInput(session, "use_robust_mahalanobis", value = input$use_robust_mahalanobis)
      updateCheckboxInput(session, "use_isolation_forest", value = input$use_isolation_forest)
      updateCheckboxInput(session, "use_iqr_filter", value = input$use_iqr_filter)
      updateCheckboxInput(session, "use_zscore_filter", value = input$use_zscore_filter)
      updateCheckboxInput(session, "use_mad_filter", value = input$use_mad_filter)
      
      # Copy advanced parameters
      updateNumericInput(session, "lambda", value = input$lambda)
      updateNumericInput(session, "omega", value = input$omega)
      updateRadioButtons(session, "outlier_mode_mahalanobis", selected = input$outlier_mode_mahalanobis)
      updateRadioButtons(session, "outlier_mode_robust", selected = input$outlier_mode_robust)
      updateRadioButtons(session, "outlier_mode_isolation", selected = input$outlier_mode_isolation)
      updateRadioButtons(session, "outlier_mode_iqr", selected = input$outlier_mode_iqr)
      updateRadioButtons(session, "outlier_mode_zscore", selected = input$outlier_mode_zscore)
      updateRadioButtons(session, "outlier_mode_mad", selected = input$outlier_mode_mad)
      updateRadioButtons(session, "mdthresh_mode", selected = input$mdthresh_mode)
      updateNumericInput(session, "custom_mdthresh", value = input$custom_mdthresh)
      updateRadioButtons(session, "mahalanobis_reference", selected = input$mahalanobis_reference)
      updateRadioButtons(session, "mahalanobis_reference_robust", selected = input$mahalanobis_reference_robust)
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
