# ---- Shiny Server Logic Module ----
# This module contains all the server-side logic for the Shiny application

#' Build the full Shiny server function
#'
#' The app's top-level server orchestrator: creates the shared `rv`
#' reactive-values object, then calls each tab's `create_server_*()`
#' factory function in turn, wiring every tab into one shared reactive graph.
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @return The shared `rv` `reactiveValues` object.
#' @export
create_server_logic <- function(input, output, session) {
  
  # Default directories
  default_working_dir <- getwd()
  default_output_dir <- file.path(getwd(), "output")
  
  # Initialize cache cleanup on startup
  clear_expired_cache()
  
  # Periodic cache cleanup (every 5 minutes)
  cache_cleanup_timer <- reactiveTimer(300000)  # 5 minutes in milliseconds
  observe({
    cache_cleanup_timer()
    clear_expired_cache()
  })
  
  # Directory management moved to server_directory_management.R module
  
  # Reactive values for data and results
  rv <- reactiveValues(
    working_dir = character(0),
    output_dir = character(0),
    stats1 = NULL,
    stats2 = NULL,
    validation1 = NULL,
    validation2 = NULL,
    correlation1 = NULL,
    correlation2 = NULL,
    df1 = NULL,
    df2 = NULL,
    comparison_data = list(),  # Data Comparison tab's own N-file data source (independent of df1/df2)
    mahalanobis_result = NULL,
    # Analysis log
    analysis_log = list(),
    xlsx_file1 = NULL,  # Added this line
    xlsx_file2 = NULL,  # Added this line
    # Enhanced features storage
    advanced_plot_data = NULL,
    # Group selection management
    group_selections_1 = NULL,  # Persistent selections for dataset 1
    group_selections_2 = NULL,  # Persistent selections for dataset 2
    group_counts_1 = NULL,      # Group counts for dataset 1
    group_counts_2 = NULL,      # Group counts for dataset 2
    is_categorical_group_1 = FALSE,  # Whether dataset 1 optional param 2 is categorical
    is_categorical_group_2 = FALSE,  # Whether dataset 2 optional param 2 is categorical
    # Plot Builder presets (loaded from plot_builder_presets.json)
    plot_presets = load_builder_presets()
  )
  
  # Add error handling and user feedback
  show_message <- function(message, type = "info") {
    session$sendCustomMessage("showMessage", list(
      message = message,
      type = type
    ))
  }
  
  # Import directory management functions first (needed by other modules)
  directory_management <- create_server_directory_management(input, output, session, default_working_dir, default_output_dir)
  
  # ---- File Upload Handlers ----
  # File handling functionality moved to server_file_handlers.R module
  
  # Import file handler functions
  file_handlers <- create_server_file_handlers(input, output, session, rv, show_message, log_operation)
  
  # Import filter management functions (needed by ternary_plots)
  filter_management <- create_server_filter_management(input, output, session, rv, show_message, log_operation)
  
  # Import ternary plot functions
  ternary_plots <- create_server_ternary_plots(input, output, session, rv, show_message, log_operation, filter_management, directory_management)
  
  # Import hexagonal ternary diagram functions
  # moduleServer()-wrapped: confirmed via a full cross-tab dependency map
  # that this tab has no reads of another tab's inputs/outputs and nothing
  # else reads its own - safe to namespace independently. See
  # ui_hex_ternary_tab.R for the matching NS(id)/ns() wrapping.
  hex_ternary <- moduleServer("hex_ternary", function(input, output, session) {
    create_server_hex_ternary(input, output, session, rv, show_message, log_operation, directory_management)
  })

  # Import plot builder functions (moduleServer()-wrapped, see hex_ternary's note above)
  plot_builder <- moduleServer("plot_builder", function(input, output, session) {
    create_server_plot_builder(input, output, session, rv, show_message, log_operation, directory_management)
  })

  # Import extreme value analysis functions (moduleServer()-wrapped, see hex_ternary's note above)
  evs <- moduleServer("evs", function(input, output, session) {
    create_server_evs(input, output, session, rv, show_message, log_operation, directory_management)
  })

  # Import spatial clustering analysis functions (moduleServer()-wrapped, see hex_ternary's note above)
  spatial <- moduleServer("spatial", function(input, output, session) {
    create_server_spatial(input, output, session, rv, show_message, log_operation, directory_management)
  })

  # Import compositional data analysis functions (moduleServer()-wrapped, see hex_ternary's note above)
  coda <- moduleServer("coda", function(input, output, session) {
    create_server_coda(input, output, session, rv, show_message, log_operation, directory_management)
  })

  # Ternary core functionality now integrated into server_ternary_plots

  # Import UI coordination functions - NOT namespaced: reads input$xlsx_file1,
  # which belongs to the still-flat Ternary Plots tab.
  ui_coordination <- create_server_ui_coordination(input, output, session, rv)

  # Import status output functions - NOT namespaced: writes output$status and
  # output$multiple_ternary_status, both owned by the still-flat Ternary
  # Plots / Multiple Ternary Creator cluster.
  status_outputs <- create_server_status_outputs(input, output, session, rv)

  # Import analysis log functions (moduleServer()-wrapped, see hex_ternary's note above)
  analysis_log <- moduleServer("analysis_log", function(input, output, session) {
    create_server_analysis_log(input, output, session, rv, show_message, log_operation, directory_management)
  })

  # Import help system functions - NOT namespaced: operates on the app-shell
  # help button in ui_components.R, outside any tab's tabPanel.
  help_system <- create_server_help_system(input, output, session)

  # Import data comparison functions (moduleServer()-wrapped, see hex_ternary's note above)
  data_comparison <- moduleServer("data_comparison", function(input, output, session) {
    create_server_data_comparison(input, output, session, rv, show_message, log_operation)
  })
  
  # Advanced plot functionality moved to server_plot_types.R module
  # Analysis log functionality moved to server_analysis_log.R module
  
  # ---- Multiple Ternary Creator Helper Functions ----
  # Filter management functionality moved to server_filter_management.R module
  
  # ---- Dynamic Filter UI Generation ----
  # Dynamic filter UI generation functionality moved to server_filter_management.R module
  
  # Dynamic color inputs moved to server_plot_types.R module
  
  # ---- Ternary Plot Previews ----
  # Ternary plot functionality moved to server_ternary_plots.R module
  
  # Ternary preview 1 moved to server_ternary_plots.R module
  
  # Ternary preview 2 moved to server_ternary_plots.R module
  
  # ---- Helper function to get individual filters ----
  # Helper function functionality moved to server_filter_management.R module
  
    # ---- Plot Save Buttons ----
  # Core ternary plot functionality moved to server_ternary_core.R module
  
  # Multiple plot types functionality moved to server_plot_types.R module
  
  # ---- Data Comparison Tab Functionality ----
  # Data comparison functionality moved to server_data_comparison.R module
  
  # ---- Enhanced Analysis Outputs ----
  # Enhanced analysis outputs functionality moved to server_data_comparison.R module
  
  # ---- Multiple Ternary Creator Functionality ----
  # Multiple ternary creator functionality moved to server_ternary_plots_batch.R module

    # ---- Cache Management ----
  # Cache functionality moved to server_cache_management.R module
  
  # Import cache management functions
  cache_management <- create_server_cache_management(input, output, session, rv, show_message, log_operation)  
  # ---- Help Button ----
  # Help system functionality moved to server_help_system.R module
  
  # ---- Status Outputs ----
  # Status output functionality moved to server_status_outputs.R module
  
  # Analysis log rendering functionality moved to server_analysis_log.R module
  
  # ---- Enhanced Multiple Plot Types Functionality ----
  # UI coordination functionality moved to server_ui_coordination.R module
  
  # Dynamic color inputs moved to server_plot_types.R module
  # Plot functionality moved to server_plot_types.R module

  # ---- Enhanced Directory Settings ----
  # Directory management functionality moved to server_directory_management.R module
  
  # ---- Multivariate Analysis Functions ----
  # Compute_mahalanobis_distance function is now in multivariate.R to avoid duplication
  # Robust Mahalanobis functions removed
  # Compute_isolation_forest_outliers function is now in multivariate.R to avoid duplication

  # ---- Return reactive values ----
  return(rv)
}
