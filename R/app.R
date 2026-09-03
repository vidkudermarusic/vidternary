# ---- Main Shiny Application ----

#' Run the vidternary Shiny app
#'
#' Builds the app (via [create_app()]) and launches it with
#' `shiny::runApp()`. This is the entry point named in the README's Quick
#' Start (`library(vidternary); run_app()`) - previously undocumented and
#' unexported, so that exact sequence failed with "could not find function
#' 'run_app'" on any normal install (only `devtools::load_all()`'d from
#' source, as this package's own dev/test workflow does, happened to make
#' it callable, masking the gap).
#'
#' @param port Port to listen on. Default 3838.
#' @param host Host address to bind to. Default `"127.0.0.1"` (localhost only).
#' @return Never returns normally - `shiny::runApp()` blocks until the app
#'   is stopped (e.g. by closing the browser tab or interrupting the R session).
#' @export
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
#' Build the vidternary Shiny app object without running it
#'
#' Assembles the app's UI (`create_main_ui()`) and server function
#' (wrapping `create_server_logic()`) and returns them as a
#' `list(ui, server)`, suitable for `shiny::shinyApp()` - useful for
#' embedding the app (e.g. in a `shinytest2` test, or a custom launcher)
#' without going through [run_app()]'s own `shiny::runApp()` call.
#'
#' @return A list with `ui` and `server` elements.
#' @export
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
