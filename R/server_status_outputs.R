# ---- Server Status Outputs Module ----
# This module contains status outputs and feedback functionality. NOT
# namespaced as a moduleServer(): project_status is an app-shell output
# (declared in ui_components.R's sidebar, outside any tab's tabPanel), so
# this stays wired to the top-level session like the other app-shell
# modules (directory management, help system).
#
# The Ternary Plots tab's own `status` baseline and the Multiple Ternary
# Creator tab's `multiple_ternary_status` placeholder used to live here too,
# but both are now-namespaced tab-owned outputs (see server_ternary_plots.R
# and server_ternary_plots_batch.R) - moved there as part of converting
# those two tabs to moduleServer()/NS().

create_server_status_outputs <- function(input, output, session, rv) {

  # ---- Status Output Functions ----

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
