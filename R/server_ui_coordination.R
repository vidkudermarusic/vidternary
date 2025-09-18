# =============================================================================
# vidternary: Shiny Server Module - UI Coordination
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Server-side logic for UI coordination functionality including
#              column updates, input synchronization, and interface management.
# 
# Key Functions:
#   - create_server_ui_coordination(): Main UI coordination server logic
#   - [UI coordination functions and input synchronization]
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - shiny
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

create_server_ui_coordination <- function(input, output, session, rv) {
  
  # ---- UI Coordination Functions ----
  
  # Update column choices for multiple plot types when data is loaded
  observe({
    if (!is.null(input$xlsx_file1)) {
      data <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
      
      # Update scatter plot columns
      updateSelectizeInput(session, "scatter_columns", choices = names(data))
      updateSelectizeInput(session, "scatter_x_col", choices = names(data))
      updateSelectizeInput(session, "scatter_y_col", choices = names(data))
      
      # Update histogram columns
      updateSelectizeInput(session, "histogram_columns", choices = names(data))
      
      # Update boxplot columns
      updateSelectizeInput(session, "boxplot_columns", choices = names(data))
    }
  })
  
  # Return the module functions (if any are needed externally)
  return(list(
    # This module primarily contains observe functions
    # No external functions to return at this time
  ))
}
