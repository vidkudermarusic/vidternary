# ---- Server Status Outputs Module ----
# This module contains status outputs and feedback functionality

create_server_status_outputs <- function(input, output, session, rv) {
  
  # ---- Status Output Functions ----
  
  # Application status
  output$status <- renderText({
    paste("Application Status: Ready\n",
          "Dataset 1:", ifelse(is.null(rv$df1), "Not loaded", paste(nrow(rv$df1), "rows,", ncol(rv$df1), "columns")), "\n",
          "Dataset 2:", ifelse(is.null(rv$df2), "Not loaded", paste(nrow(rv$df2), "rows,", ncol(rv$df2), "columns")))
  })
  
  # Multiple ternary plot status (initial placeholder - replaced at runtime
  # by register_ternary_plots_batch_handlers() once the button is clicked)
  output$multiple_ternary_status <- renderText({
    "No multiple ternary plots created yet. Click 'Create & Save all ternary plots to subfolder' to start."
  })
  
  # Cache statistics
  output$cache_stats <- renderText({
    get_cache_stats()
  })
  
  # Project status
  output$project_status <- renderText({
    "Project status: No project loaded"
  })
  
  # Return the module functions (if any are needed externally)
  return(list(
    # This module primarily contains output rendering functions
    # No external functions to return at this time
  ))
}
