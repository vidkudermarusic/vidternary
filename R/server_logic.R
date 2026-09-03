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

  # Reactive values for data and results
  rv <- reactiveValues(
    df1 = NULL,
    df2 = NULL,
    comparison_data = list(),  # Data Comparison tab's own N-file data source (independent of df1/df2)
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
  
  # Import ternary plot functions - moduleServer()-wrapped. Confirmed via a
  # full cross-tab dependency map (see the vidternary Structural Audit) that
  # Ternary Plots and Multiple Ternary Creator were, despite being separate
  # tabs, one entangled server unit sharing state in several places -
  # Multiple Ternary Creator's handlers were registered from inside this
  # same function, server_filter_management.R built both tabs' filter UI in
  # one call, and this tab's Dataset 1 upload pushed choices into Multiple
  # Ternary Creator's selectors directly. All of that was split apart (see
  # server_ternary_plots.R/server_ternary_plots_batch.R/server_file_handlers.R
  # for the specifics) so each tab could get its own real, independent
  # namespace instead of continuing to share one.
  moduleServer("ternary_plots", function(input, output, session) {
    create_server_file_handlers(input, output, session, rv, show_message, log_operation)
    create_server_ternary_plots(input, output, session, rv, show_message, log_operation)
  })

  # Import Multiple Ternary Creator's batch handlers - moduleServer()-wrapped
  # and now a direct sibling call (previously nested inside
  # create_server_ternary_plots(), see that note above).
  moduleServer("multiple_ternary", function(input, output, session) {
    register_ternary_plots_batch_handlers(input, output, session, rv, show_message, log_operation)
  })

  # Import hexagonal ternary diagram functions
  # moduleServer()-wrapped: confirmed via a full cross-tab dependency map
  # that this tab has no reads of another tab's inputs/outputs and nothing
  # else reads its own - safe to namespace independently. See
  # ui_hex_ternary_tab.R for the matching NS(id)/ns() wrapping.
  moduleServer("hex_ternary", function(input, output, session) {
    create_server_hex_ternary(input, output, session, rv, show_message, log_operation)
  })

  # Import plot builder functions (moduleServer()-wrapped, see hex_ternary's note above)
  moduleServer("plot_builder", function(input, output, session) {
    create_server_plot_builder(input, output, session, rv, show_message, log_operation)
  })

  # Import extreme value analysis functions (moduleServer()-wrapped, see hex_ternary's note above)
  moduleServer("evs", function(input, output, session) {
    create_server_evs(input, output, session, rv, show_message, log_operation)
  })

  # Import spatial clustering analysis functions (moduleServer()-wrapped, see hex_ternary's note above)
  moduleServer("spatial", function(input, output, session) {
    create_server_spatial(input, output, session, rv, show_message, log_operation)
  })

  # Import compositional data analysis functions (moduleServer()-wrapped, see hex_ternary's note above)
  moduleServer("coda", function(input, output, session) {
    create_server_coda(input, output, session, rv, show_message, log_operation)
  })

  # Ternary core functionality now integrated into server_ternary_plots

  # Import status output functions - NOT namespaced: project_status is a
  # genuinely app-shell output (ui_components.R's sidebar, outside any
  # tab). status/multiple_ternary_status used to live here too but are now
  # namespaced, tab-owned outputs (see server_ternary_plots.R/
  # server_ternary_plots_batch.R).
  create_server_status_outputs(input, output, session, rv)

  # Import analysis log functions (moduleServer()-wrapped, see hex_ternary's note above)
  moduleServer("analysis_log", function(input, output, session) {
    create_server_analysis_log(input, output, session, rv, show_message, log_operation)
  })

  # Import data comparison functions (moduleServer()-wrapped, see hex_ternary's note above)
  moduleServer("data_comparison", function(input, output, session) {
    create_server_data_comparison(input, output, session, rv, show_message, log_operation)
  })

  # ---- Multivariate Analysis Functions ----
  # Compute_mahalanobis_distance function is now in multivariate.R to avoid duplication
  # Robust Mahalanobis functions removed
  # Compute_isolation_forest_outliers function is now in multivariate.R to avoid duplication

  # ---- Return reactive values ----
  return(rv)
}
