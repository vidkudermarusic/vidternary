# ---- Server Data Comparison Module ----
# This module contains data comparison functionality including statistics,
# correlations, and multivariate analysis. The handlers themselves are
# registered by sibling modules, split out for size:
#   server_data_comparison_stats.R        - descriptive stats + correlation
#   server_data_comparison_multivariate.R - Mahalanobis / Isolation Forest
#   server_data_comparison_preview.R      - validation summaries + Excel preview
#
# Call order matters: output$data_readiness_status is assigned in both the
# stats and preview modules (a pre-existing duplicate binding), and the
# preview module's version must win, so it's registered second here - see
# the notes in those two files.

create_server_data_comparison <- function(input, output, session, rv, show_message, log_operation) {

  register_data_comparison_stats_handlers(input, output, session, rv, show_message, log_operation)
  register_data_comparison_multivariate_handlers(input, output, session, rv, show_message, log_operation)
  register_data_comparison_preview_handlers(input, output, session, rv, show_message, log_operation)

  # Return the module functions (if any are needed externally)
  return(list(
    # This module primarily contains observeEvent and output rendering functions
    # No external functions to return at this time
  ))
}
