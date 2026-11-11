# ---- Plot-note lines for the outlier model's reference and columns ----
# The plot notes for Mahalanobis / Isolation Forest listed lambda, omega,
# thresholds, trees and seed, but not what the model was fitted on, so a
# saved plot could not be reproduced from its notes.

#' Two plot-note lines: reference dataset and columns of an outlier model
#'
#' @param mahalanobis_reference `"self"`, `"dataset1"` or `"dataset2"`.
#' @param reference_data The reference data frame used (`NULL` for self).
#' @param selected_columns Columns the model used.
#' @param max_columns Columns listed before "+ n more".
#' @return A character vector of two lines.
#' @keywords internal
mv_reference_note_lines <- function(mahalanobis_reference, reference_data, selected_columns, max_columns = 6) {
  ref <- switch(as.character(mahalanobis_reference),
    self = "self (the plotted data)",
    dataset1 = "Dataset 1",
    dataset2 = "Dataset 2",
    as.character(mahalanobis_reference))
  if (!identical(as.character(mahalanobis_reference), "self") && !is.null(reference_data)) {
    ref <- sprintf("%s (%d rows)", ref, nrow(reference_data))
  }
  cols <- sub("[. ]?[(]Wt%[)]$", "", selected_columns)
  shown <- if (length(cols) > max_columns) {
    paste0(paste(cols[seq_len(max_columns)], collapse = ", "), " + ", length(cols) - max_columns, " more")
  } else paste(cols, collapse = ", ")
  c(paste("  Reference:", ref), paste("  Columns:", shown))
}
