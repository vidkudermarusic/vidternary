# ---- Server Export Module ----
# This module contains comprehensive export functionality. The handlers
# themselves are registered by sibling modules, split out for size:
#   server_export_data.R    - filtered-data / comprehensive-export buttons
#   server_export_reports.R - status/history renderers + comprehensive report button

# Export Functions
create_server_export <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {

  register_export_data_handlers(input, output, session, rv, show_message, log_operation, directory_management)
  register_export_report_handlers(input, output, session, rv, show_message, log_operation, directory_management)

  # Return the export functions for integration
  return(list(
    # All functions are already set up as observeEvent and renderPrint/renderUI
    # This function just sets up the event handlers
  ))
}
