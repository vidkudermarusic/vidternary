# ---- Ternary Plot: Coordinate Computation (split out of ternary_plot_data_prep.R) ----
# Builds the numeric working data frame ("matrika"), sums each element's
# selected column(s), and normalizes into the final A/B/C ternary coordinates.

#' Compute and validate a ternary plot's normalized A/B/C coordinates
#'
#' Builds `matrika` (the numeric-plus-optional-parameter working data frame:
#' selects `needed_columns`, splits `element_columns` vs. `optional_columns`,
#' converts the former to numeric), sums each element's selected column(s)
#' into `A_values`/`B_values`/`C_values`, and normalizes by their row total
#' into `ternary_points1` - erroring if a needed column is missing from the
#' data, or if nothing survives validation.
#'
#' @section One shared `valid_rows` mask:
#' A row is valid when its element total is finite and positive, AND no
#' needed column (any element sub-column, or either optional parameter's
#' column) is `NA` for that row - a genuinely missing measurement is not
#' silently treated as zero and plotted as if the composition were fully
#' known. That single mask is computed once and applied identically to
#' `matrika`, `M`, and `ternary_points1`, so all three end up with the
#' exact same rows in the exact same order. This matters because
#' [compute_point_styling()] later reads Optional Param 1/2 values out of
#' `matrika` and applies them *positionally* against `ternary_points1` -
#' row validity must be decided in exactly one place so the two objects
#' never drift out of alignment.
#'
#' @param M The data frame to compute coordinates from (already
#'   loaded/filtered by the earlier pipeline stages).
#' @param all_selected_elements Character vector of every column selected
#'   across `element_A`/`B`/`C` (as built earlier in
#'   [prepare_ternary_plot_data()], before this extraction's own call).
#' @param element_A,element_B,element_C Ternary-axis element specs, each a
#'   `list(col = <one or more column names>)`. A *partial* overlap between
#'   two elements' column sets is fine and common in real compositional
#'   data (e.g. A = Fe+O, B = Al+O, C = Ti - O legitimately contributes to
#'   more than one vertex) - but no two of the three may select the exact
#'   same *complete* set (e.g. A = O, B = O), which collapses the ternary
#'   diagram onto a single edge/point and is rejected with a `stop()`.
#' @param optional_param1,optional_param2 Optional `list(col = <column
#'   name(s)>, ...)` specs, included in `needed_columns`/`optional_columns`
#'   when supplied.
#' @param use_mahalanobis,reference_data Used only by this function's own
#'   `getOption("ternary.debug", FALSE)` diagnostic `cat()` calls (printing
#'   a data sample when Mahalanobis filtering against a reference dataset
#'   was active) - not read for any control-flow decision here.
#' @return This function's entire local environment as a list
#'   (`as.list(environment())`) - `matrika` and `ternary_points1` are the
#'   fields [prepare_ternary_plot_data()] and the preview/save renderers
#'   actually read back, and now always share the same row set/order as
#'   `M`. The rest (`needed_columns`, `element_columns`,
#'   `optional_columns`, `A_values`/`B_values`/`C_values`, `total_values`,
#'   `valid_rows`) are echoed back for diagnostic/debug use.
#' @export
compute_ternary_coordinates <- function(M, all_selected_elements, element_A, element_B, element_C,
                                         optional_param1, optional_param2, use_mahalanobis, reference_data) {
  # Two of A/B/C selecting the exact same complete column set (order-
  # independent, hence setequal() rather than identical()) collapses the
  # ternary diagram onto a single edge (two axes identical) or a single
  # point (all three identical) - never a meaningful plot, unlike a
  # PARTIAL overlap (e.g. A: Fe+O, B: Al+O, C: Ti - a real, intentional
  # pattern in oxide chemistry where O legitimately contributes to more
  # than one vertex), which stays fully supported below. Nothing in the UI
  # (three independent selectInputs, ui_ternary_plots_tab.R) prevents this,
  # so it's checked explicitly here.
  if (setequal(element_A$col, element_B$col) || setequal(element_A$col, element_C$col) ||
      setequal(element_B$col, element_C$col)) {
    stop("Elements A, B, and C must each use a different set of columns - two of them currently select the exact same column(s). Sharing SOME columns between elements is fine (e.g. A: Fe+O, B: Al+O, C: Ti), but using the identical complete set for two axes is not, since every point would then collapse onto a single line or point instead of forming a real ternary diagram.")
  }

  needed_columns <- unique(c(all_selected_elements,
                             if (!is.null(optional_param1)) optional_param1$col,
                             if (!is.null(optional_param2)) optional_param2$col))

  log_operation("INFO", "Checking required columns", paste(needed_columns, collapse = ", "))

  if (!all(needed_columns %in% colnames(M))) {
    missing_cols <- setdiff(needed_columns, colnames(M))
    log_operation("ERROR", "Missing required columns", paste(missing_cols, collapse = ", "))
    stop("Error: One or more selected elements/parameters are missing in the dataset.\nAvailable columns: ",
         paste(colnames(M), collapse = ", "))
  }

  log_operation("INFO", "All required columns found")

  # drop = FALSE: needed_columns can no longer collapse to length 1 via
  # A/B/C alone now that the setequal() check above blocks any two of them
  # being fully identical (if no two of three non-empty sets are equal,
  # their union can't be a single element) - kept anyway, matching this
  # codebase's convention of never relying on that kind of proof alone for
  # a data-frame column selection. Without it, R silently drops matrika to
  # a plain vector instead of a data frame, and the very next line crashes
  # with "incorrect number of dimensions".
  matrika <- M[, needed_columns, drop = FALSE]
  cat("DEBUG: Matrika dimensions after column selection:", dim(matrika), "\n")
  if (getOption("ternary.debug", FALSE) && use_mahalanobis && !is.null(reference_data)) {
    cat("DEBUG: Matrika created from filtered M. Sample data (first 5 rows):\n")
    print(head(matrika, 5))
  }
  log_operation("INFO", "Selected columns", paste("Matrix dimensions:", dim(matrika)[1], "rows x", dim(matrika)[2], "columns"))

  # unique(): all_selected_elements is c(element_A$col, element_B$col,
  # element_C$col) as built by the caller, NOT deduplicated - unlike
  # needed_columns above. A partial overlap (explicitly supported - see
  # this function's own @param doc) then leaves a repeated column name in
  # it (e.g. c("Fe","O","Al","O")), which plain *extraction*
  # (matrika[, all_selected_elements, drop=FALSE]) tolerates fine, but
  # *assignment* into matrika by name does not - it throws "duplicate
  # subscripts for columns" instead of converting anything. Deduplicating
  # once here, and reusing element_columns everywhere below instead of the
  # raw all_selected_elements, fixes the assignment on the next line and
  # its counterpart further down without changing which columns end up
  # selected (a repeated name selects the same column either way).
  #
  # This numeric conversion also makes reading A/B/C sums FROM matrika
  # (below) strictly safer than the old approach of reading them from M
  # directly: a numeric-looking Excel column openxlsx read in as character
  # now coerces here (as.numeric()) instead of making rowSums() error, or
  # - for the A/B/C sums specifically - instead of silently disagreeing
  # with what matrika itself considers "this column's numeric value".
  element_columns <- unique(all_selected_elements)
  matrika[, element_columns] <- lapply(matrika[, element_columns, drop = FALSE], as.numeric)
  log_operation("INFO", "Converted element columns to numeric", paste(element_columns, collapse = ", "))

  # Optional parameter columns are kept as character/factor (not coerced to
  # numeric) so categorical grouping still works downstream in
  # compute_point_styling().
  optional_columns <- c()
  # all() wraps the %in% check because optional_param*$col can be a
  # multi-column selection (a vector), and %in% then returns a vector -
  # a bare && on that is a hard error on R >= 4.3 ("'length = 2' in
  # coercion to 'logical(1)'"), not just a warning as on older R. This
  # check is guaranteed TRUE anyway by this point (needed_columns above
  # already includes these columns, and the stop() a few lines up
  # already verified every needed_columns entry exists in M) - all()
  # just makes that safe to evaluate regardless of column count.
  if (!is.null(optional_param1) && all(optional_param1$col %in% names(matrika))) {
    optional_columns <- c(optional_columns, optional_param1$col)
  }
  if (!is.null(optional_param2) && all(optional_param2$col %in% names(matrika))) {
    optional_columns <- c(optional_columns, optional_param2$col)
  }

  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: Element columns converted to numeric:", paste(element_columns, collapse = ", "), "\n")
    cat("DEBUG: Optional columns preserved as character:", paste(optional_columns, collapse = ", "), "\n")
    cat("DEBUG: Final matrika column classes:", paste(sapply(matrika, class), collapse = ", "), "\n")
  }

  log_operation("INFO", "Generating ternary coordinates")

  # Sum the selected columns for each element - per-element sums, NOT a
  # pooled/deduplicated total. A column shared between two elements (e.g.
  # A: Fe+O, B: Al+O) legitimately contributes to both sums; that overlap
  # is intentional (see this function's @param doc) and must be preserved
  # for correct ternary math, so this reads from matrika (element_A$col
  # etc. are always a subset of element_columns) rather than deduplicating.
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: About to calculate ternary coordinates\n")
    cat("DEBUG: Element A columns:", paste(element_A$col, collapse=", "), "\n")
    cat("DEBUG: Element B columns:", paste(element_B$col, collapse=", "), "\n")
    cat("DEBUG: Element C columns:", paste(element_C$col, collapse=", "), "\n")
  }
  A_values <- rowSums(matrika[, element_A$col, drop = FALSE], na.rm = TRUE)
  B_values <- rowSums(matrika[, element_B$col, drop = FALSE], na.rm = TRUE)
  C_values <- rowSums(matrika[, element_C$col, drop = FALSE], na.rm = TRUE)
  total_values <- A_values + B_values + C_values

  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: A_values range:", range(A_values, na.rm=TRUE), "\n")
    cat("DEBUG: B_values range:", range(B_values, na.rm=TRUE), "\n")
    cat("DEBUG: C_values range:", range(C_values, na.rm=TRUE), "\n")
    cat("DEBUG: Total values range:", range(total_values, na.rm=TRUE), "\n")
  }

  # ---- One shared valid-rows mask (see this function's own "One shared
  # valid_rows mask" doc section above for why) ----
  # A row is valid when its element total is finite and positive (a
  # genuine ternary point needs a real, positive A+B+C to normalize by),
  # AND no needed column - any element sub-column, or either optional
  # parameter's column - is NA for that row. rowSums(..., na.rm = TRUE)
  # above would otherwise silently treat a missing element reading as
  # zero; checking matrika directly (before any row is dropped) catches
  # that instead of letting it through as a partially-known composition.
  na_in_needed <- rowSums(is.na(matrika)) > 0
  valid_rows <- is.finite(total_values) & total_values > 0 & !na_in_needed

  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: Valid rows:", sum(valid_rows), "out of", length(valid_rows), "\n")
  }

  # Apply the SAME mask to matrika, M, and the computed coordinates - the
  # three end up with identical rows in identical order, by construction.
  matrika <- matrika[valid_rows, , drop = FALSE]
  M <- M[valid_rows, , drop = FALSE]
  ternary_points1 <- data.frame(
    A = A_values[valid_rows] / total_values[valid_rows],
    B = B_values[valid_rows] / total_values[valid_rows],
    C = C_values[valid_rows] / total_values[valid_rows]
  )

  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: After validation - M dimensions:", dim(M), "\n")
    cat("DEBUG: After validation - matrika dimensions:", dim(matrika), "\n")
    cat("DEBUG: After validation - ternary_points1 dimensions:", dim(ternary_points1), "\n")
    cat("DEBUG: Sample ternary coordinates (first 3 rows):\n")
    print(head(ternary_points1, 3))
  }

  if (nrow(ternary_points1) == 0) stop("Error: No valid data left after filtering.")

  log_operation("SUCCESS", "Generated ternary coordinates", paste(nrow(ternary_points1), "points"))

  as.list(environment())
}
