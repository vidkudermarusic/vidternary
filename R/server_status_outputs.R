# ---- Server Status Outputs Module ----
# This module contains status outputs and feedback functionality. NOT
# namespaced as a moduleServer(): project_status is an app-shell output
# (declared in ui_components.R's sidebar, outside any tab's tabPanel), so
# this stays wired to the top-level session like the other app-shell
# modules (directory management, help system).

create_server_status_outputs <- function(input, output, session, rv) {

  # ---- Status Output Functions ----

  # Project status
  output$project_status <- renderText({
    "Project status: No project loaded"
  })
  
  return(list())
}
