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

# Main ternary plot function
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
  log_operation("Input validation", "Starting validation of function parameters")

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
