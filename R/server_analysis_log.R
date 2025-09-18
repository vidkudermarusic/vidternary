# =============================================================================
# vidternary: Shiny Server Module - Analysis Log
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Server-side logic for analysis log functionality including
#              log controls, rendering, and log management.
# 
# Key Functions:
#   - create_server_analysis_log(): Main analysis log server logic
#   - [Analysis log functions and controls]
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - shiny
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

create_server_analysis_log <- function(input, output, session, rv, show_message, log_operation) {
  
  # ---- Analysis Log Controls ----
  
  # Clear analysis log
  observeEvent(input$clear_log, {
    rv$analysis_log <- list()
    log_operation("INFO", "Analysis log cleared by user")
    show_message("Analysis log cleared.", "info")
  })
  
  # Export analysis log
  observeEvent(input$export_log, {
    log_operation("INFO", "User exported analysis log")
    # Implementation for log export
    show_message("Log export functionality coming soon!", "info")
  })
  
  # Save analysis log
  observeEvent(input$save_log, {
    log_operation("INFO", "User saved analysis log to file")
    # Implementation for log save
    show_message("Log save functionality coming soon!", "info")
  })
  
  # Search analysis log
  observeEvent(input$search_log, {
    log_operation("INFO", "User searched analysis log", input$log_search)
    # Implementation for log search
  })
  
  # ---- Analysis Log Rendering ----
  
  # Main analysis log display
  output$analysis_log <- renderText({
    if (length(rv$analysis_log) == 0) {
      "No activities logged yet. Start using the app to see activity history."
    } else {
      # Filter by log level if specified
      filtered_log <- rv$analysis_log
      if (input$log_level != "all") {
        filtered_log <- rv$analysis_log[sapply(rv$analysis_log, function(entry) entry$level == input$log_level)]
      }
      
      # Search in log if search term provided
      if (!is.null(input$log_search) && nzchar(input$log_search)) {
        search_term <- tolower(input$log_search)
        filtered_log <- filtered_log[sapply(filtered_log, function(entry) {
          grepl(search_term, tolower(entry$message)) || 
            grepl(search_term, tolower(entry$details))
        })]
      }
      
      # Format log entries
      log_text <- ""
      for (entry in filtered_log) {
        log_text <- paste0(log_text, 
                           "[", entry$timestamp, "] ", entry$level, ": ", entry$message)
        if (!is.null(entry$details)) {
          log_text <- paste0(log_text, " (", entry$details, ")")
        }
        log_text <- paste0(log_text, "\n")
      }
      
      if (nzchar(log_text)) {
        log_text
      } else {
        "No log entries match the current filter/search criteria."
      }
    }
  })
  
  # Log statistics display
  output$log_stats <- renderText({
    if (length(rv$analysis_log) == 0) {
      "No log entries"
    } else {
      total_entries <- length(rv$analysis_log)
      info_count <- sum(sapply(rv$analysis_log, function(entry) entry$level == "INFO"))
      warning_count <- sum(sapply(rv$analysis_log, function(entry) entry$level == "WARNING"))
      error_count <- sum(sapply(rv$analysis_log, function(entry) entry$level == "ERROR"))
      
      paste("Total Entries:", total_entries,
            "\nINFO:", info_count,
            "\nWARNING:", warning_count,
            "\nERROR:", error_count)
    }
  })
  
  # Return the module functions (if any are needed externally)
  return(list(
    # This module primarily contains observeEvent and output rendering functions
    # No external functions to return at this time
  ))
}
