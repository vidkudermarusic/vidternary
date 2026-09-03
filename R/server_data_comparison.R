# ---- Server Data Comparison Module ----
# This module contains data comparison functionality: an independent
# multi-file data source (not tied to the main Ternary Plots tab's
# rv$df1/rv$df2, and not limited to two files), descriptive statistics,
# correlations, and multivariate analysis. The handlers themselves are
# registered by sibling modules, split out for size:
#   server_data_comparison_upload.R       - file upload + dataset/target/reference selectors
#   server_data_comparison_stats.R        - descriptive stats + correlation
#   server_data_comparison_multivariate.R - Mahalanobis / Isolation Forest
#   server_data_comparison_preview.R      - missing/outlier summaries + Excel preview
#
# The upload module must be registered first: it owns rv$comparison_data
# and the selector inputs the other three modules read from.

#' Wire up the "Data Comparison" tab's server logic
#'
#' Registers this tab's four sibling handler modules (none individually
#' documented - internal, package-private helpers), in the order they
#' must run: `register_data_comparison_upload_handlers()` first (it owns
#' `rv$comparison_data` and the dataset/target/reference selector inputs
#' the other three read from), then
#' `register_data_comparison_stats_handlers()`,
#' `register_data_comparison_multivariate_handlers()`, and
#' `register_data_comparison_preview_handlers()`.
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @param rv The app's shared `reactiveValues` object.
#' @param show_message Function to show a user-facing status message.
#' @param log_operation Function to record a structured log entry.
#' @return An empty list - this function's effect is entirely the four
#'   handler modules it registers.
#' @export
create_server_data_comparison <- function(input, output, session, rv, show_message, log_operation) {

  register_data_comparison_upload_handlers(input, output, session, rv, show_message, log_operation)
  register_data_comparison_stats_handlers(input, output, session, rv, show_message, log_operation)
  register_data_comparison_multivariate_handlers(input, output, session, rv, show_message, log_operation)
  register_data_comparison_preview_handlers(input, output, session, rv, show_message, log_operation)

  # Return the module functions (if any are needed externally)
  return(list(
    # This module primarily contains observeEvent and output rendering functions
    # No external functions to return at this time
  ))
}
