# ---- Helper Functions Module: Reporting & File I/O ----
# Split out of helpers.R: report/dashboard generation, correlation/statistics
# summaries, and centralized file-reading utilities.

#' Write a simple HTML report of statistics and a correlation matrix
#'
#' @param stats An object (e.g. from `generate_stats()`) to `print()` into
#'   the report's Statistics section.
#' @param correlation An object (e.g. from `compute_correlation()`) to
#'   `print()` into the report's Correlation Matrix section.
#' @param plot_files Currently unused; accepted for interface compatibility.
#' @param output_path Path to write the HTML file to. Defaults to
#'   `Ternary_Analysis_Report_<timestamp>.html` in the working directory.
#' @return The path the report was written to.
#' @export
generate_report <- function(stats, correlation, plot_files, output_path = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  if (is.null(output_path)) {
    output_path <- paste0("Ternary_Analysis_Report_", timestamp, ".html")
  }

  # Create a simple HTML report since we don't have the Rmd template
  html_content <- paste0(
    "<!DOCTYPE html>",
    "<html><head><title>Ternary Analysis Report</title></head>",
    "<body><h1>Ternary Analysis Report</h1>",
    "<p>Generated on: ", timestamp, "</p>",
    "<h2>Statistics</h2>",
    "<pre>", capture.output(print(stats)), "</pre>",
    "<h2>Correlation Matrix</h2>",
    "<pre>", capture.output(print(correlation)), "</pre>",
    "</body></html>"
  )

  writeLines(html_content, output_path)
  return(output_path)
}

# Centralized success message helper
create_success_message <- function(operation, filename = NULL, details = NULL) {
  base_message <- paste(operation, "completed successfully!")
  if (!is.null(filename)) {
    base_message <- paste(operation, "exported successfully to", filename)
  }
  if (!is.null(details)) {
    base_message <- paste(base_message, details)
  }
  return(base_message)
}

