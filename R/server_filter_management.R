# =============================================================================
# vidternary: Shiny Server Module - Filter Management
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Server-side logic for filter management including filter
#              collection, UI generation, and dynamic filter handling.
# 
# Key Functions:
#   - create_server_filter_management(): Main filter management server logic
#   - [Filter collection, validation, and UI generation functions]
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - shiny
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

create_server_filter_management <- function(input, output, session, rv, show_message, log_operation) {
  
  # ---- Multiple Ternary Creator Helper Functions ----
  
  # Function to collect individual element filters for main ternary plots tab
  # Now centralized in helpers.R as collect_main_ternary_filters
  
  # ---- Dynamic Filter UI Generation ----
  
  # Generate dynamic filter inputs for individual elements
  output$dynamic_filters_A1 <- renderUI({
    req(input$element_A1)
    lapply(input$element_A1, function(element) {
      div(
        style = "margin: 5px 0; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
        h6(paste("Filter for", element)),
        textInput(paste0("filter_A1_", gsub("[^A-Za-z0-9]", "_", element)), 
                 paste("Threshold for", element), 
                 placeholder = paste("e.g., > 10"))
      )
    })
  })
  
  output$dynamic_filters_B1 <- renderUI({
    req(input$element_B1)
    lapply(input$element_B1, function(element) {
      div(
        style = "margin: 5px 0; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
        h6(paste("Filter for", element)),
        textInput(paste0("filter_B1_", gsub("[^A-Za-z0-9]", "_", element)), 
                 paste("Threshold for", element), 
                 placeholder = paste("e.g., > 10"))
      )
    })
  })
  
  output$dynamic_filters_C1 <- renderUI({
    req(input$element_C1)
    lapply(input$element_C1, function(element) {
      div(
        style = "margin: 5px 0; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
        h6(paste("Filter for", element)),
        textInput(paste0("filter_C1_", gsub("[^A-Za-z0-9]", "_", element)), 
                 paste("Threshold for", element), 
                 placeholder = paste("e.g., > 10"))
      )
    })
  })
  
  output$dynamic_filters_A2 <- renderUI({
    req(input$element_A2)
    lapply(input$element_A2, function(element) {
      div(
        style = "margin: 5px 0; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
        h6(paste("Filter for", element)),
        textInput(paste0("filter_A2_", gsub("[^A-Za-z0-9]", "_", element)), 
                 paste("Threshold for", element), 
                 placeholder = paste("e.g., > 10"))
      )
    })
  })
  
  output$dynamic_filters_B2 <- renderUI({
    req(input$element_B2)
    lapply(input$element_B2, function(element) {
      div(
        style = "margin: 5px 0; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
        h6(paste("Filter for", element)),
        textInput(paste0("filter_B2_", gsub("[^A-Za-z0-9]", "_", element)), 
                 paste("Threshold for", element), 
                 placeholder = paste("e.g., > 10"))
      )
    })
  })
  
  output$dynamic_filters_C2 <- renderUI({
    req(input$element_C2)
    lapply(input$element_C2, function(element) {
      div(
        style = "margin: 5px 0; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
        h6(paste("Filter for", element)),
        textInput(paste0("filter_C2_", gsub("[^A-Za-z0-9]", "_", element)), 
                 paste("Threshold for", element), 
                 placeholder = paste("e.g., > 10"))
      )
    })
  })
  
  # ---- Multiple Ternary Creator Dynamic Filters ----
  
  output$multiple_filters_A <- renderUI({
    req(input$multiple_element_A)
    lapply(input$multiple_element_A, function(element) {
      div(
        style = "margin: 5px 0; padding: 5px; border: 1px solid #ddd; border-radius: 3px;",
        h6(paste("Filter for", element)),
        textInput(paste0("multiple_filter_A_", gsub("[^A-Za-z0-9]", "_", element)), 
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
        textInput(paste0("multiple_filter_B_", gsub("[^A-Za-z0-9]", "_", element)), 
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
        textInput(paste0("multiple_filter_C_", gsub("[^A-Za-z0-9]", "_", element)), 
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
        textInput(paste0("multiple_filter_op1_", gsub("[^A-Za-z0-9]", "_", element)), 
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
        textInput(paste0("multiple_filter_op2_", gsub("[^A-Za-z0-9]", "_", element)), 
                 paste("Threshold for", element), 
                 placeholder = paste("e.g., > 10"))
      )
    })
  })
  
  # ---- Helper function to get individual filters ----
  get_individual_filters <- function(elements, element_type) {
    filters <- list()
    if (length(elements) > 0) {
      for (element in elements) {
        # Look for filter input for this element
        filter_id <- paste0("filter_", element_type, "_", gsub("[^A-Za-z0-9]", "_", element))
        # We'll need to access this from the input object in the calling context
        # For now, return empty list - this will be populated by the calling function
        filters[[element]] <- NULL
      }
    }
    return(filters)
  }
  
  # Return the module functions for external use
  return(list(
    # Filter collection functions
    collect_main_ternary_filters = collect_main_ternary_filters,
    get_individual_filters = get_individual_filters
  ))
}
