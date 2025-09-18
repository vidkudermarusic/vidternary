# =============================================================================
# vidternary: Main Shiny Application
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Main Shiny application entry point for the interactive ternary
#              plot generator with advanced filtering and analysis capabilities.
# 
# Key Functions:
#   - run_app(): Launch the complete Shiny application
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - shiny, and all package dependencies
# 
# Last Modified: 2025-09-07
# 
# =============================================================================
run_app <- function(port = 3838, host = "127.0.0.1") {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("shiny package is required to run this application")
  }
  # Initialize packages to ensure shiny and other dependencies are loaded
  initialize_packages()
  
  # Source required modules - not needed when using devtools::load_all()
  # source("R/multivariate.R")
  
  # Create the app object
  app <- create_app()
  # Create the Shiny app object
  shiny_app <- shiny::shinyApp(ui = app$ui, server = app$server)
  # Run the app
  shiny::runApp(shiny_app, port = port, host = host)
}
# Function to create the app object without running it
create_app <- function() {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("shiny package is required to run this application")
  }
  # Initialize packages to ensure shiny and other dependencies are loaded
  initialize_packages()
  # Create UI and server
  ui <- create_main_ui()
  server <- function(input, output, session) {
    rv <- create_server_logic(input, output, session)
    return(rv)
  }
  # Return the app object
  return(list(ui = ui, server = server))
}
# Function to get the UI component
get_main_ui <- function() {
  create_main_ui()
}
# Function to get the server logic
get_server_logic <- function() {
  create_server_logic
}
