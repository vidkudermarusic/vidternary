# ---- Main Ternary Plot Function Module ----
# general_ternary_plot() is now a thin orchestrator: validate inputs,
# normalize the single-filter-per-plot selection, then delegate to sibling
# modules split out for size:
#   ternary_plot_data_prep.R - loads the file, applies all filters, computes
#                               ternary coordinates/title/point styling
#   ternary_plot_preview.R   - draws the plot to the active graphics device
#                               (always runs, regardless of preview vs. save -
#                               that's the original behavior)
#   ternary_plot_save.R      - opens a file device, redraws, and saves, when
#                               !preview && !is.null(output_dir)

#' Create (and optionally save) a ternary diagram from an Excel file
#'
#' The package's main entry point: loads `xlsx_file`, sums the columns for
#' each of elements A/B/C, applies any per-element filters plus at most one
#' statistical/multivariate outlier method (if more than one `use_*` flag
#' is `TRUE`, only the highest-priority one is kept - see this function's
#' inline comment for the priority order), computes ternary coordinates,
#' draws the plot to the active graphics device, and - unless `preview =
#' TRUE` or `output_dir` is `NULL` - saves it to a file.
#'
#' @param xlsx_file Path to the input `.xlsx` file (Sheet 1 is read).
#' @param working_dir Working directory to `setwd()` into for the duration
#'   of the call (restored on exit). Default `getwd()`.
#' @param output_dir Directory to save the plot into. If `NULL` or
#'   `preview = TRUE`, no file is saved.
#' @param element_A A list `list(col = <column name(s)>, filter = <optional filter string>)` for vertex A.
#' @param element_B Same shape as `element_A`, for vertex B.
#' @param element_C Same shape as `element_A`, for vertex C.
#' @param optional_param1 Optional list `list(col = ..., filter = ...)`
#'   controlling point size or shape (see `optional_param1_representation`).
#' @param optional_param2 Optional list `list(col = ..., filter = ...)` controlling point color.
#' @param color_palette Color palette name for `optional_param2`. Default `"blue"`.
#' @param xlsx_display_name Optional original filename, used for plot
#'   titles/saved filenames instead of `xlsx_file`'s (often temp) basename.
#' @param preview If `TRUE`, draw only (no file saved), regardless of `output_dir`. Default `FALSE`.
#' @param use_mahalanobis Apply a Mahalanobis-distance outlier filter. Default `FALSE`.
#' @param reference_data Reference dataset for Mahalanobis/Isolation
#'   Forest when `mahalanobis_reference` is `"dataset1"`/`"dataset2"`; must
#'   be supplied by the caller in that case.
#' @param optional_param1_representation How to render `optional_param1`:
#'   `"point_size"` or a point-shape mode. Default `"point_size"`.
#' @param output_format Saved file format: `"png"`, `"jpeg"`, `"pdf"`, or `"tiff"`. Default `"png"`.
#' @param use_isolation_forest Apply an Isolation Forest outlier filter. Default `FALSE`.
#' @param use_iqr_filter Apply an IQR outlier filter. Default `FALSE`.
#' @param use_zscore_filter Apply a Z-score outlier filter. Default `FALSE`.
#' @param use_mad_filter Apply a MAD outlier filter. Default `FALSE`.
#' @param lambda Sensitivity parameter for the automatic Mahalanobis threshold. Default 1.
#' @param omega Leniency parameter for the automatic Mahalanobis threshold. Default 0.
#' @param keep_outliers_mahalanobis If `TRUE`, keep only Mahalanobis
#'   outliers instead of removing them. Default `FALSE`.
#' @param keep_outliers_isolation If `TRUE`, keep only Isolation Forest
#'   outliers instead of removing them. Default `FALSE`.
#' @param keep_outliers_iqr If `TRUE`, keep only IQR outliers instead of removing them. Default `FALSE`.
#' @param keep_outliers_zscore If `TRUE`, keep only Z-score outliers instead of removing them. Default `FALSE`.
#' @param keep_outliers_mad If `TRUE`, keep only MAD outliers instead of removing them. Default `FALSE`.
#' @param individual_filters_A Named list of per-column filter strings for element A's column(s).
#' @param individual_filters_B Named list of per-column filter strings for element B's column(s).
#' @param individual_filters_C Named list of per-column filter strings for element C's column(s).
#' @param custom_mdthresh Manual Mahalanobis distance threshold, used when `mdthresh_mode == "manual"`.
#' @param mdthresh_mode Either `"auto"` (default) or `"manual"`.
#' @param mahalanobis_reference Reference dataset for Mahalanobis/Isolation
#'   Forest: `"self"`, `"dataset1"`, or `"dataset2"`. Default `"dataset2"`.
#' @param selected_columns Character vector of numeric columns used by the
#'   statistical/multivariate filter methods.
#' @param include_plot_notes Whether to include filter/parameter notes on
#'   the plot. Default `TRUE`.
#' @param use_manual_point_size Override the automatic point size. Default `FALSE`.
#' @param manual_point_size Point size used when `use_manual_point_size = TRUE`. Default 1.0.
#' @param selected_groups Character vector restricting which categories of
#'   a categorical `optional_param2` are plotted.
#' @param is_categorical_group Whether `optional_param2` should be treated
#'   as categorical (drives group coloring/legend). Default `FALSE`.
#' @return The path to the saved plot file (invisibly, from
#'   `save_ternary_plot_to_file()`), or `NULL` in preview mode / when no
#'   `output_dir` is set.
#' @export
general_ternary_plot <- function(
    xlsx_file,
    working_dir = getwd(),
    output_dir = NULL,
    element_A,
    element_B,
    element_C,
    optional_param1 = NULL,
    optional_param2 = NULL,
    color_palette = "blue",
    xlsx_display_name = NULL,
    preview = FALSE,
    use_mahalanobis = FALSE,
    reference_data = NULL,  # Must be provided by caller for dataset1/dataset2 reference modes
    optional_param1_representation = "point_size",
    output_format = "png",
    use_isolation_forest = FALSE,
    use_iqr_filter = FALSE,
    use_zscore_filter = FALSE,
    use_mad_filter = FALSE,
    lambda = 1,
    omega = 0,
    keep_outliers_mahalanobis = FALSE,
    keep_outliers_isolation = FALSE,
    keep_outliers_iqr = FALSE,
    keep_outliers_zscore = FALSE,
    keep_outliers_mad = FALSE,
    individual_filters_A = NULL,
    individual_filters_B = NULL,
    individual_filters_C = NULL,
    custom_mdthresh = NULL,
    mdthresh_mode = "auto",
    mahalanobis_reference = "dataset2",
    selected_columns = NULL,
    include_plot_notes = TRUE,
    use_manual_point_size = FALSE,
    manual_point_size = 1.0,
    selected_groups = NULL,
    is_categorical_group = FALSE
) {

  # Input validation
  log_operation("INFO", "Starting validation of function parameters")

  # COMPREHENSIVE DEBUGGING: Log all input parameters
  if (getOption("ternary.debug", FALSE)) {
    cat("\n=== COMPREHENSIVE DEBUGGING START ===\n")
    cat("DEBUG: Function called with parameters:\n")
    cat("DEBUG: element_A =", if(is.null(element_A)) "NULL" else paste(names(element_A), collapse=", "), "\n")
    cat("DEBUG: element_A$col =", if(is.null(element_A$col)) "NULL" else paste(element_A$col, collapse=", "), "\n")
    cat("DEBUG: element_B =", if(is.null(element_B)) "NULL" else paste(names(element_B), collapse=", "), "\n")
    cat("DEBUG: element_B$col =", if(is.null(element_B$col)) "NULL" else paste(element_B$col, collapse=", "), "\n")
    cat("DEBUG: element_C =", if(is.null(element_C)) "NULL" else paste(names(element_C), collapse=", "), "\n")
    cat("DEBUG: element_C$col =", if(is.null(element_C$col)) "NULL" else paste(element_C$col, collapse=", "), "\n")
    cat("DEBUG: optional_param1 =", if(is.null(optional_param1)) "NULL" else paste(names(optional_param1), collapse=", "), "\n")
    cat("DEBUG: optional_param1$col =", if(is.null(optional_param1$col)) "NULL" else paste(optional_param1$col, collapse=", "), "\n")
    cat("DEBUG: optional_param1$filter =", if(is.null(optional_param1$filter)) "NULL" else optional_param1$filter, "\n")
    cat("DEBUG: optional_param2 =", if(is.null(optional_param2)) "NULL" else paste(names(optional_param2), collapse=", "), "\n")
    cat("DEBUG: optional_param2$col =", if(is.null(optional_param2$col)) "NULL" else paste(optional_param2$col, collapse=", "), "\n")
    cat("DEBUG: optional_param2$filter =", if(is.null(optional_param2$filter)) "NULL" else optional_param2$filter, "\n")
    cat("DEBUG: use_mahalanobis =", use_mahalanobis, "\n")
    cat("DEBUG: use_isolation_forest =", use_isolation_forest, "\n")
    cat("DEBUG: use_iqr_filter =", use_iqr_filter, "\n")
    cat("DEBUG: use_zscore_filter =", use_zscore_filter, "\n")
    cat("DEBUG: use_mad_filter =", use_mad_filter, "\n")
    cat("DEBUG: selected_columns =", if(is.null(selected_columns)) "NULL" else paste(selected_columns, collapse=", "), "\n")

    cat("=== COMPREHENSIVE DEBUGGING END ===\n\n")
  }

  # The app is designed to apply exactly one statistical/multivariate filter
  # per ternary plot, not a stack of them. The UI enforces this by making the
  # five method checkboxes mutually exclusive (see the observer in
  # server_filter_management.R), but general_ternary_plot() can also be
  # called directly (e.g. batch mode), so the same single-filter rule is
  # enforced here too: if more than one method flag is TRUE, only the
  # highest-priority one is kept and the rest are dropped for this call.
  active_filter_flags <- c(
    mahalanobis = isTRUE(use_mahalanobis),
    isolation_forest = isTRUE(use_isolation_forest),
    iqr = isTRUE(use_iqr_filter),
    zscore = isTRUE(use_zscore_filter),
    mad = isTRUE(use_mad_filter)
  )
  if (sum(active_filter_flags) > 1) {
    chosen_filter <- names(active_filter_flags)[active_filter_flags][1]
    warning_msg <- sprintf(
      "Multiple filters selected (%s) but only one filter is applied per ternary plot - using '%s' and ignoring the rest.",
      paste(names(active_filter_flags)[active_filter_flags], collapse = ", "), chosen_filter
    )
    warning(warning_msg)
    log_operation("WARNING", "Multiple filters selected for one ternary plot", warning_msg)
    use_mahalanobis <- chosen_filter == "mahalanobis"
    use_isolation_forest <- chosen_filter == "isolation_forest"
    use_iqr_filter <- chosen_filter == "iqr"
    use_zscore_filter <- chosen_filter == "zscore"
    use_mad_filter <- chosen_filter == "mad"
  }

  # Check if we have xlsx_file and load data (matching legacy code pattern)
  if (is.null(xlsx_file) || !file.exists(xlsx_file)) {
    stop("Invalid xlsx_file: file does not exist or is NULL")
  }

  # Store original working directory and restore on exit
  # This prevents setwd() side-effects from affecting Shiny's working directory.
  # Kept here (rather than in prepare_ternary_plot_data()) because it must
  # stay in effect across data prep AND the preview/save render calls below,
  # not just for the duration of data loading.
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(working_dir)

  pd <- prepare_ternary_plot_data(
    xlsx_file = xlsx_file,
    working_dir = working_dir,
    output_dir = output_dir,
    element_A = element_A,
    element_B = element_B,
    element_C = element_C,
    optional_param1 = optional_param1,
    optional_param2 = optional_param2,
    color_palette = color_palette,
    xlsx_display_name = xlsx_display_name,
    preview = preview,
    use_mahalanobis = use_mahalanobis,
    reference_data = reference_data,
    optional_param1_representation = optional_param1_representation,
    output_format = output_format,
    use_isolation_forest = use_isolation_forest,
    use_iqr_filter = use_iqr_filter,
    use_zscore_filter = use_zscore_filter,
    use_mad_filter = use_mad_filter,
    lambda = lambda,
    omega = omega,
    keep_outliers_mahalanobis = keep_outliers_mahalanobis,
    keep_outliers_isolation = keep_outliers_isolation,
    keep_outliers_iqr = keep_outliers_iqr,
    keep_outliers_zscore = keep_outliers_zscore,
    keep_outliers_mad = keep_outliers_mad,
    individual_filters_A = individual_filters_A,
    individual_filters_B = individual_filters_B,
    individual_filters_C = individual_filters_C,
    custom_mdthresh = custom_mdthresh,
    mdthresh_mode = mdthresh_mode,
    mahalanobis_reference = mahalanobis_reference,
    selected_columns = selected_columns,
    include_plot_notes = include_plot_notes,
    use_manual_point_size = use_manual_point_size,
    manual_point_size = manual_point_size,
    selected_groups = selected_groups,
    is_categorical_group = is_categorical_group
  )

  # Always draw to whatever device is currently active - this matches the
  # original code, which drew once unconditionally before separately opening
  # a file device for save mode.
  render_ternary_plot_preview(pd)

  if (!preview && !is.null(output_dir)) {
    return(save_ternary_plot_to_file(pd))
  } else {
    # For preview mode, just return NULL since no file was saved
    return(NULL)
  }
}

# Note: Statistical filtering functions are now in statistical_filters.R to avoid duplication
