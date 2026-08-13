# ---- Server Filter Management Module ----
# This module contains all filter-related functionality including filter collection, UI generation, and dynamic filters

create_server_filter_management <- function(input, output, session, rv, show_message, log_operation) {

  # ---- Enforce one filter per ternary plot ----
  # Mahalanobis, Isolation Forest, IQR, Z-score, and MAD are alternative
  # ways to flag outliers, not stages meant to compound on the same plot.
  # Checking one here unchecks the other four, so the checkbox UI can only
  # ever represent zero or one active filter at a time (general_ternary_plot()
  # also enforces this server-side as a defense-in-depth backstop for any
  # caller that bypasses this UI, e.g. batch/programmatic use).
  filter_method_checkboxes <- c("use_mahalanobis", "use_isolation_forest",
                                 "use_iqr_filter", "use_zscore_filter", "use_mad_filter")

  lapply(filter_method_checkboxes, function(checkbox_id) {
    observeEvent(input[[checkbox_id]], {
      if (isTRUE(input[[checkbox_id]])) {
        other_checkboxes <- setdiff(filter_method_checkboxes, checkbox_id)
        for (other_id in other_checkboxes) {
          if (isTRUE(input[[other_id]])) {
            updateCheckboxInput(session, other_id, value = FALSE)
          }
        }
      }
    }, ignoreInit = TRUE)
  })

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
