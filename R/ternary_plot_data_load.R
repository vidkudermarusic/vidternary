# ---- Ternary Plot: Source Data Loading (split out of ternary_plot_data_prep.R) ----
# The very first pipeline stage prepare_ternary_plot_data() runs: reads Sheet 1
# of the uploaded Excel file and validates the element/output-format inputs
# before any filtering or coordinate computation is attempted.

#' Load and validate the source Excel data for a ternary plot
#'
#' The very first thing [prepare_ternary_plot_data()] does: reads Sheet 1
#' of `xlsx_file`, then validates the element/output-format inputs before
#' any filtering or coordinate computation is attempted. Its only output
#' is the loaded data frame.
#'
#' @param xlsx_file Path to the uploaded `.xlsx` file (temp upload path).
#' @param element_A,element_B,element_C Ternary-axis element specs, each a
#'   `list(col = <one or more column names>)`.
#' @param output_format File format for a real save (e.g. `"png"`); only
#'   validated here, not used to load anything.
#' @param preview If `TRUE`, suppress the `print(colnames(M))` console
#'   dump this does outside preview mode.
#' @return A data frame: Sheet 1 of `xlsx_file`, unfiltered.
#' @export
load_and_validate_ternary_source_data <- function(xlsx_file, element_A, element_B, element_C, output_format, preview) {
  log_operation("INFO", "Loading source data", paste("File:", xlsx_file))
  M <- openxlsx::read.xlsx(xlsx_file, sheet = 1)

  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: Initial data loaded, dimensions:", dim(M), "\n")
    cat("DEBUG: Available columns:", paste(colnames(M), collapse=", "), "\n")
  }
  if (!preview) print(colnames(M))

  log_operation("INFO", "Initial data dimensions", paste(dim(M)[1], "rows x", dim(M)[2], "columns"))

  # Input validation
  if (is.null(element_A) || is.null(element_B) || is.null(element_C)) {
    stop("Missing required elements: element_A, element_B, and element_C must be provided")
  }

  if (length(element_A$col) == 0 || length(element_B$col) == 0 || length(element_C$col) == 0) {
    stop("Empty element columns: all elements must have at least one column selected")
  }

  if (!output_format %in% c("png", "jpeg", "pdf", "tiff")) {
    stop("Invalid output_format: must be one of 'png', 'jpeg', 'pdf', 'tiff'")
  }

  log_operation("INFO", "All inputs validated successfully")

  M
}
