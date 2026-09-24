# ---- Ternary Plot: Title/Axis-Label Building (split out of ternary_plot_data_prep.R) ----
# Assembles the plot title (element summary, optional-parameter and
# filter/method annotations, source-file "charge" line) and the ternary
# diagram's corner/axis labels. Called once by prepare_ternary_plot_data(),
# right after ternary coordinates are computed.

#' Build a ternary plot's title and axis labels
#'
#' Assembles the plot title (element summary, optional-parameter and
#' filter/method annotations, source-file "charge" line) and the ternary
#' diagram's corner/axis labels. Called once by
#' [prepare_ternary_plot_data()], right after ternary coordinates are
#' computed. Builds display text only, and touches none of the actual
#' filtered data.
#'
#' @param element_A,element_B,element_C Ternary-axis element specs, each a
#'   `list(col = <one or more column names>)`.
#' @param optional_param1 Optional `list(col = ..., filter = ...)` for
#'   point size/type representation.
#' @param optional_param1_representation `"point_size"` or `"point_type"`.
#' @param optional_param2 Optional `list(col = ..., filter = ...)` for
#'   point color / categorical grouping.
#' @param use_mahalanobis,use_isolation_forest,use_iqr_filter,use_zscore_filter,use_mad_filter
#'   Which outlier method(s), if any, are active.
#' @param keep_outliers_mahalanobis,keep_outliers_isolation,keep_outliers_iqr,keep_outliers_zscore,keep_outliers_mad
#'   Whether each active method keeps outliers instead of removing them.
#' @param file_base Base filename (no extension) used for the title's
#'   "charge <name>" line, or `NULL`/empty to fall back to
#'   `xlsx_display_name`/`xlsx_file`.
#' @param xlsx_display_name,xlsx_file Used for the title's "charge" line
#'   only when `file_base` isn't usable.
#' @param title_layout_fn Function used to turn `title_parts` into the
#'   final `plot_title` string - [prepare_ternary_plot_data()] passes its
#'   own local `preview_title_layout()` closure, so that stays the single
#'   source of truth for that formatting rather than being duplicated here.
#' @param mv_status Outcome of [apply_multivariate_filtering()]
#'   (`"applied"`, `"skipped_no_reference"`, `"failed"`, ...). When the
#'   requested Mahalanobis/Isolation Forest filter was skipped or failed, the
#'   title marks it "(NOT APPLIED)" instead of "(filtered)". Default
#'   `"applied"`.
#' @return This function's entire local environment as a list
#'   (`as.list(environment())`), mirroring
#'   [prepare_ternary_plot_data()]'s own return convention - includes
#'   `clean_labels_A`/`B`/`C`, `axis_labels_A`/`B`/`C`, `title_parts`, and
#'   `plot_title`.
#' @export
build_ternary_plot_title <- function(element_A, element_B, element_C,
                                      optional_param1, optional_param1_representation,
                                      optional_param2,
                                      use_mahalanobis, keep_outliers_mahalanobis,
                                      use_isolation_forest, keep_outliers_isolation,
                                      use_iqr_filter, keep_outliers_iqr,
                                      use_zscore_filter, keep_outliers_zscore,
                                      use_mad_filter, keep_outliers_mad,
                                      file_base, xlsx_display_name, xlsx_file,
                                      title_layout_fn, mv_status = "applied") {
  # Clean labels for ternary plot corners (remove Wt% suffix)
  clean_labels_A <- gsub("\\.\\(Wt%\\)", "", paste(element_A$col, collapse = "+"))
  clean_labels_B <- gsub("\\.\\(Wt%\\)", "", paste(element_B$col, collapse = "+"))
  clean_labels_C <- gsub("\\.\\(Wt%\\)", "", paste(element_C$col, collapse = "+"))

  # Labels for plot axes (keep Wt% suffix)
  axis_labels_A <- paste(element_A$col, collapse = "+")
  axis_labels_B <- paste(element_B$col, collapse = "+")
  axis_labels_C <- paste(element_C$col, collapse = "+")

  # Create comprehensive plot title
  title_parts <- c(paste0("Ternary Plot of ", clean_labels_A, ", ", clean_labels_B, ", ", clean_labels_C))

  # Add optional parameter 1 information
  if (!is.null(optional_param1) && length(optional_param1$col) > 0) {
    opt1_label <- paste0("Param1 (", optional_param1_representation, "): ", paste(optional_param1$col, collapse = "+"))
    if (!is.null(optional_param1$filter) && nzchar(optional_param1$filter)) {
      opt1_label <- paste0(opt1_label, " [", optional_param1$filter, "]")
    }
    title_parts <- c(title_parts, opt1_label)
  }

  # Add optional parameter 2 information
  if (!is.null(optional_param2) && length(optional_param2$col) > 0) {
    opt2_label <- paste0("Param2 (color): ", paste(optional_param2$col, collapse = "+"))
    if (!is.null(optional_param2$filter) && nzchar(optional_param2$filter)) {
      opt2_label <- paste0(opt2_label, " [", optional_param2$filter, "]")
    }
    title_parts <- c(title_parts, opt2_label)
  }

  # Add outlier-detection information with enhanced outlier indicators.
  # Mahalanobis distance is a multivariate statistical method; Isolation
  # Forest is a machine-learning algorithm - grouped under "Outlier
  # Detection" (not "Multivariate") since only one of the two actually is.
  # mv_status comes from apply_multivariate_filtering(): when the requested
  # method was skipped or failed, the data is unfiltered and the title must
  # say so rather than "(filtered)".
  mv_not_applied <- mv_status %in% c("skipped_no_reference", "failed")
  mv_methods <- c()
  if (use_mahalanobis) {
    indicator <- if (mv_not_applied) " (NOT APPLIED)" else if (keep_outliers_mahalanobis) "(outliers only)" else "(filtered)"
    mv_methods <- c(mv_methods, paste0("Mahalanobis", indicator))
  }
  if (use_isolation_forest) {
    indicator <- if (mv_not_applied) " (NOT APPLIED)" else if (keep_outliers_isolation) "(outliers only)" else "(filtered)"
    mv_methods <- c(mv_methods, paste0("Isolation Forest", indicator))
  }

  if (length(mv_methods) > 0) {
    title_parts <- c(title_parts, paste("Outlier Detection:", paste(mv_methods, collapse = "+")))
  }

  # Add statistical filtering information with enhanced outlier indicators
  stat_methods <- c()
  if (use_iqr_filter) {
    indicator <- if (keep_outliers_iqr) "(outliers only)" else "(filtered)"
    stat_methods <- c(stat_methods, paste0("IQR", indicator))
  }
  if (use_zscore_filter) {
    indicator <- if (keep_outliers_zscore) "(outliers only)" else "(filtered)"
    stat_methods <- c(stat_methods, paste0("Z-Score", indicator))
  }
  if (use_mad_filter) {
    indicator <- if (keep_outliers_mad) "(outliers only)" else "(filtered)"
    stat_methods <- c(stat_methods, paste0("MAD", indicator))
  }

  if (length(stat_methods) > 0) {
    title_parts <- c(title_parts, paste("Statistical Filtering:", paste(stat_methods, collapse = "+")))
  }

  # Add charge information to title. file_base is a required parameter of
  # this function, so the caller always supplies *something* (possibly
  # NULL), making the plain !is.null(file_base) check below sufficient.
  if (!is.null(file_base) && nzchar(file_base)) {
    title_parts <- c(title_parts, paste("charge", file_base))
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Using file_base for title:", file_base, "\n")
    }
  } else {
    # Fallback to xlsx_file name
    fallback_name <- if (!is.null(xlsx_display_name)) tools::file_path_sans_ext(xlsx_display_name) else if (!is.null(xlsx_file)) tools::file_path_sans_ext(basename(xlsx_file)) else "ternary_plot"
    title_parts <- c(title_parts, paste("charge", fallback_name))
  }

  # Intelligent title splitting for better readability
  plot_title <- title_layout_fn(title_parts)

  log_operation("INFO", "Generated plot title", paste(length(title_parts), "parts"))
  log_operation("INFO", "Final plot title", paste0(substr(plot_title, 1, 100), "..."))

  as.list(environment())
}
