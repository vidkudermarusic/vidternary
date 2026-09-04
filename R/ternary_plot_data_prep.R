# ---- Ternary Plot: Shared Data Preparation (split out of ternary_plot.R) ----
# Everything general_ternary_plot() needs to do exactly once, regardless of
# preview vs. save: load the Excel file, apply individual/optional-parameter/
# statistical/multivariate filters, compute ternary coordinates, build the
# plot title, and work out point size/color/shape. Nothing here touches a
# graphics device - that happens in ternary_plot_preview.R and
# ternary_plot_save.R, both of which take the list this function returns.
#
# The caller (general_ternary_plot() in ternary_plot.R) is responsible for
# setwd(working_dir) + on.exit(restore) *before* calling this, since that
# needs to stay in effect across prep AND the preview/save render calls that
# follow - not just for the duration of this function.
#
# Returns the function's entire local environment as a list (as.list(environment())).
# This is deliberate: dozens of locals computed here (clean_labels_A/B/C,
# ternary_points1, pointSize/pointCol/pointType, plot_title, title_parts,
# param1_values, param1_bins, unique_groups, group_colors, group_counts,
# col1_text/col2_text/col3_text, mahal_result, iso_result, and more) are all
# read by the preview and/or save renderers, and hand-picking a parameter
# list for each risked silently dropping one. Renderers access fields via
# `pd$name` or `with(pd, {...})`.

#' Load and validate the source Excel data for a ternary plot
#'
#' The very first thing [prepare_ternary_plot_data()] does: reads Sheet 1
#' of `xlsx_file`, then validates the element/output-format inputs before
#' any filtering or coordinate computation is attempted. Extracted as its
#' own function because it carries no shared mutable state with the rest
#' of `prepare_ternary_plot_data()` (per the vidternary Structural Audit's
#' §04 responsibility table) - its only output is the loaded data frame.
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

#' Build a ternary plot's title and axis labels
#'
#' Assembles the plot title (element summary, optional-parameter and
#' filter/method annotations, source-file "charge" line) and the ternary
#' diagram's corner/axis labels. Called once by
#' [prepare_ternary_plot_data()], right after ternary coordinates are
#' computed - extracted as its own function because it's purely cosmetic
#' (per the vidternary Structural Audit's §04 responsibility table):
#' builds display text only, and touches none of the actual filtered data.
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
#' @return This function's entire local environment as a list
#'   (`as.list(environment())`), mirroring
#'   [prepare_ternary_plot_data()]'s own return convention - includes
#'   `clean_labels_A`/`B`/`C`, `axis_labels_A`/`B`/`C`, `title_parts`, and
#'   `plot_title`, which is exactly what the original inline code (before
#'   this was split out) left behind in `prepare_ternary_plot_data()`'s
#'   own environment for `as.list(environment())` to capture there.
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
                                      title_layout_fn) {
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

  # Add multivariate analysis information with enhanced outlier indicators
  mv_methods <- c()
  if (use_mahalanobis) {
    indicator <- if (keep_outliers_mahalanobis) "(outliers only)" else "(filtered)"
    mv_methods <- c(mv_methods, paste0("Mahalanobis", indicator))
  }
  if (use_isolation_forest) {
    indicator <- if (keep_outliers_isolation) "(outliers only)" else "(filtered)"
    mv_methods <- c(mv_methods, paste0("Isolation Forest", indicator))
  }

  if (length(mv_methods) > 0) {
    title_parts <- c(title_parts, paste("Multivariate:", paste(mv_methods, collapse = "+")))
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

  # Add charge information to title. The original code guarded this with
  # exists("file_base") (defensive against a code path where it was never
  # assigned) - always TRUE in practice by the time this ran inside
  # prepare_ternary_plot_data() (both of its branches set file_base before
  # reaching here), and moot now regardless: file_base is a required
  # parameter of this function, so the caller always supplies *something*
  # (possibly NULL), making the plain !is.null(file_base) check below
  # exactly equivalent to the original's, without needing exists().
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

#' Apply Mahalanobis-distance or Isolation-Forest outlier filtering
#'
#' Resolves the reference dataset (self / Dataset 1 / Dataset 2), runs
#' whichever of [compute_mahalanobis_distance()]/[compute_isolation_forest()]
#' is active, and re-filters `M` to the kept rows. Skips silently (leaving
#' `M` untouched) if neither `use_mahalanobis` nor `use_isolation_forest` is
#' `TRUE`, or if the requested reference dataset isn't available; a genuine
#' computation error (e.g. too few usable rows/columns) is also caught and
#' left as an unfiltered `M`, with a console message - only the two
#' mandatory-column-selection checks below raise a real, propagating error.
#' Extracted from [prepare_ternary_plot_data()] as its own function because
#' it's the most self-contained of that function's remaining
#' responsibilities (per the vidternary Structural Audit's §04
#' responsibility table) - its only real outputs are `M`, `mahal_result`,
#' and `iso_result`.
#'
#' @param M The data frame to filter (already loaded/individually filtered).
#' @param use_mahalanobis,use_isolation_forest Which method, if either, is active.
#' @param selected_columns Character vector of at least 2 numeric column
#'   names to use; mandatory whenever either method is active.
#' @param mahalanobis_reference Which dataset the reference distribution is
#'   fit to: `"self"`, `"dataset1"`, or `"dataset2"`.
#' @param reference_data Reference dataset for the `"dataset1"`/`"dataset2"`
#'   modes; must be supplied by the caller in that case.
#' @param preview If `TRUE`, suppress the console progress/status messages.
#' @param keep_outliers_isolation,keep_outliers_mahalanobis If `TRUE` for
#'   the active method, keep only the flagged outliers instead of removing them.
#' @param lambda,omega Sensitivity/leniency parameters for the automatic
#'   Mahalanobis threshold formula (see [compute_mahalanobis_distance()]).
#' @param custom_mdthresh Manual Mahalanobis distance threshold, used when
#'   `mdthresh_mode == "manual"`.
#' @param mdthresh_mode `"auto"` or `"manual"` Mahalanobis threshold mode.
#' @return This function's entire local environment as a list
#'   (`as.list(environment())`) - `M`, `mahal_result`, and `iso_result` are
#'   the fields [prepare_ternary_plot_data()] actually reads back; the rest
#'   are this block's own internal working variables, echoed back unchanged
#'   from how they already existed in `prepare_ternary_plot_data()`'s own
#'   environment before this extraction.
#' @export
apply_multivariate_filtering <- function(M, use_mahalanobis, use_isolation_forest, selected_columns,
                                          mahalanobis_reference, reference_data, preview,
                                          keep_outliers_isolation, keep_outliers_mahalanobis,
                                          lambda, omega, custom_mdthresh, mdthresh_mode) {
  mahal_result <- NULL
  iso_result <- NULL

  # Apply multivariate analysis filtering if requested
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: Multivariate analysis check:\n")
    cat("DEBUG: use_mahalanobis =", use_mahalanobis, "\n")
    cat("DEBUG: use_isolation_forest =", use_isolation_forest, "\n")

    cat("DEBUG: reference_data is.null =", is.null(reference_data), "\n")
    if (!is.null(reference_data)) {
      cat("DEBUG: reference_data dimensions:", dim(reference_data), "\n")
    }
    cat("DEBUG: Will enter multivariate section =", use_mahalanobis || use_isolation_forest, "\n")
    cat("DEBUG: Any multivariate method enabled =", use_mahalanobis || use_isolation_forest, "\n")
  }

  if (use_mahalanobis || use_isolation_forest) {
    # MANDATORY COLUMN SELECTION: User must select columns for multivariate analysis
    if (is.null(selected_columns) || length(selected_columns) == 0) {
      stop("Column selection is MANDATORY for multivariate analysis. Please select at least 2 numeric columns before proceeding.")
    }

    # Validate minimum number of columns
    if (length(selected_columns) < 2) {
      stop("At least 2 numeric columns must be selected for multivariate analysis. Currently selected: ", length(selected_columns))
    }

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Using selected columns for multivariate analysis:", paste(selected_columns, collapse = ", "), "\n")
    }

    # Determine reference dataset based on user selection
    # IMPORTANT: reference_data parameter must be provided by caller for dataset1/dataset2 modes
    # For self-reference mode, the function uses its own data (M)
    actual_reference_data <- NULL

    # Use mahalanobis_reference for all multivariate methods
    reference_mode <- mahalanobis_reference

    if (reference_mode == "self") {
      actual_reference_data <- M  # Self-reference
      if (!preview) debug_log("Using self-reference for multivariate analysis")
    } else if (reference_mode == "dataset1") {
      # Use the reference_data parameter provided by caller
      actual_reference_data <- reference_data
      if (!preview) debug_log("Using Dataset 1 as reference for multivariate analysis (reference_data: %d rows)",
                              if (!is.null(reference_data)) nrow(reference_data) else 0)
    } else if (reference_mode == "dataset2") {
      actual_reference_data <- reference_data  # Dataset 2 reference
      if (!preview) debug_log("Using Dataset 2 as reference for multivariate analysis (reference_data: %d rows)",
                              if (!is.null(reference_data)) nrow(reference_data) else 0)
    }

    # Skip if reference dataset is not available
    if (is.null(actual_reference_data)) {
      if (!preview) {
        debug_log("Skipping multivariate analysis: No reference dataset provided")
        debug_log("Reference data status: mahalanobis_reference=%s, reference_data=%s",
                  mahalanobis_reference, if (is.null(reference_data)) "NULL" else paste("dataframe with", nrow(reference_data), "rows"))
        cat("WARNING: Multivariate analysis skipped - no reference dataset available\n")
        cat("Reference mode:", mahalanobis_reference, "\n")
        cat("This means multivariate filtering was NOT applied to the plot.\n")
      }
    } else {
      tryCatch({
        if (use_isolation_forest) {
          iso_result <- compute_isolation_forest(M, actual_reference_data, keep_outliers = keep_outliers_isolation, selected_columns = selected_columns)
          keep_indices <- if (keep_outliers_isolation) {
            iso_result$outlier_indices
          } else {
            !iso_result$outlier_indices
          }
        } else {
          if (getOption("ternary.debug", FALSE)) {
            cat("DEBUG: About to call compute_mahalanobis_distance:\n")
            cat("DEBUG: M dimensions:", dim(M), "\n")
            cat("DEBUG: actual_reference_data dimensions:", dim(actual_reference_data), "\n")
            cat("DEBUG: lambda =", lambda, ", omega =", omega, "\n")
            cat("DEBUG: keep_outliers_mahalanobis =", keep_outliers_mahalanobis, "\n")
            cat("DEBUG: custom_mdthresh =", if (is.null(custom_mdthresh)) "NULL" else custom_mdthresh, "\n")
            cat("DEBUG: selected_columns =", if (is.null(selected_columns)) "NULL" else paste(selected_columns, collapse = ", "), "\n")
            cat("DEBUG: mdthresh_mode =", mdthresh_mode, "\n")

          }

          mahal_result <- compute_mahalanobis_distance(M, actual_reference_data, lambda, omega, keep_outliers = keep_outliers_mahalanobis, custom_mdthresh = custom_mdthresh, selected_columns = selected_columns, mdthresh_mode = mdthresh_mode)

          if (getOption("ternary.debug", FALSE)) {
            cat("DEBUG: compute_mahalanobis_distance returned:\n")
            cat("DEBUG: MDthresh =", mahal_result$MDthresh, "\n")
            cat("DEBUG: outlier_indices length =", length(mahal_result$outlier_indices), "\n")
            cat("DEBUG: outlier_indices sum =", sum(mahal_result$outlier_indices), "\n")
            cat("DEBUG: threshold_method =", mahal_result$threshold_method, "\n")
          }

          # Use the new threshold formula
          threshold_to_use <- mahal_result$MDthresh
          keep_indices <- if (keep_outliers_mahalanobis) {
            mahal_result$outlier_indices
          } else {
            !mahal_result$outlier_indices
          }

          if (getOption("ternary.debug", FALSE)) {
            cat("DEBUG: keep_indices calculation:\n")
            cat("DEBUG: keep_outliers_mahalanobis =", keep_outliers_mahalanobis, "\n")
            cat("DEBUG: keep_indices length =", length(keep_indices), "\n")
            cat("DEBUG: keep_indices sum =", sum(keep_indices), "\n")
          }
        }

        # Apply the filtering
        common_cols <- if (use_isolation_forest) iso_result$common_cols else mahal_result$common_cols
        M_numeric <- as.matrix(M[, common_cols, drop=FALSE])
        original_indices <- which(complete.cases(M_numeric))[keep_indices]

        if (getOption("ternary.debug", FALSE)) {
          cat("DEBUG: Multivariate filtering details:\n")
          cat("DEBUG: common_cols:", paste(common_cols, collapse = ", "), "\n")
          cat("DEBUG: keep_indices length:", length(keep_indices), "\n")
          cat("DEBUG: keep_indices sum:", sum(keep_indices), "\n")
          cat("DEBUG: original_indices length:", length(original_indices), "\n")
          cat("DEBUG: M before filtering:", nrow(M), "rows\n")
        }

        M <- M[original_indices, , drop = FALSE]

        if (getOption("ternary.debug", FALSE)) {
          cat("DEBUG: M after filtering:", nrow(M), "rows\n")
        }

        if (!preview) {
          method_name <- if (use_isolation_forest) "Isolation Forest" else "Mahalanobis"
          ref_name <- if (mahalanobis_reference == "self") "self-reference" else
            if (mahalanobis_reference == "dataset1") "Dataset 1 reference" else "Dataset 2 reference"
          debug_log("After %s filtering (%s):", method_name, ref_name)
          debug_log("Outlier points remaining: %d", sum(keep_indices))
          debug_log("Columns used: %s", paste(common_cols, collapse = ", "))

          # Success confirmation for user
          cat("SUCCESS: Multivariate analysis applied successfully!\n")
          cat("Method:", method_name, "\n")
          cat("Reference:", ref_name, "\n")
          cat("Points filtered:", nrow(M) - sum(keep_indices), "outliers removed\n")
          cat("Columns used:", paste(common_cols, collapse = ", "), "\n")
        }
      }, error = function(e) {
        if (getOption("ternary.debug", FALSE)) {
          cat("DEBUG: Multivariate filtering ERROR:\n")
          cat("DEBUG: Error message:", e$message, "\n")
          cat("DEBUG: Error call:", toString(e$call), "\n")
          cat("DEBUG: Error occurred in:", if (!is.null(e$call) && length(e$call) > 0) toString(e$call[[1]]) else "unknown", "\n")
        }
        if (!preview) {
          debug_log("Multivariate filtering failed: %s", e$message)
          # Show error to user in status output
          cat("ERROR: Multivariate analysis failed:", e$message, "\n")
          cat("This means multivariate filtering was NOT applied to the plot.\n")
        }
      })
    }
  }

  as.list(environment())
}

#' Work out each point's plotted size, color, and shape
#'
#' Turns Optional Parameter 1 (point size or point type) and Optional
#' Parameter 2 (point color / categorical grouping) into per-point
#' `pointSize`/`pointType`/`pointCol` vectors aligned with `ternary_points1`,
#' plus whatever legend metadata the preview/save renderers need to draw a
#' matching legend (`param1_values`/`param1_bins` for the size/type legend,
#' `unique_groups`/`group_colors`/`group_counts` for a categorical color
#' legend). When Optional Parameter 2 is a categorical grouping, this is
#' also where rows outside the selected groups get dropped from
#' `ternary_points1` itself (with the point size/type vectors kept in sync) -
#' the reason this function takes and can return a modified
#' `ternary_points1` and `selected_groups`, unlike a pure "compute some
#' columns" helper. Extracted from [prepare_ternary_plot_data()] as its own
#' function because it's the largest of that function's remaining inline
#' responsibilities (per the vidternary Structural Audit's §04
#' responsibility table) - everything here is genuinely self-contained once
#' `ternary_points1`/`matrika` exist, with no interaction with the
#' filtering or multivariate-analysis steps that ran earlier.
#'
#' @param ternary_points1 Data frame of computed ternary coordinates (`A`,
#'   `B`, `C` columns), one row per plotted point so far.
#' @param matrika The prepared data frame `optional_param1`/`optional_param2`
#'   values are read from (element columns numeric, optional-parameter
#'   columns preserved as-is for categorical grouping).
#' @param optional_param1 Optional `list(col = <column name(s)>, filter =
#'   ...)` driving point size/type; `NULL` for the plain default styling.
#' @param optional_param1_representation `"point_size"` or `"point_type"` -
#'   how `optional_param1` maps onto the plotted points.
#' @param optional_param2 Optional `list(col = <column name>, filter = ...)`
#'   driving point color / categorical grouping; `NULL` for plain black
#'   points.
#' @param color_palette Palette name (`"blue"`, `"red"`, `"viridis"`, or
#'   `"rainbow"`) used when `optional_param2` is numeric rather than
#'   categorical.
#' @param use_manual_point_size If `TRUE`, use `manual_point_size` for every
#'   point instead of `optional_param1_representation`'s mapping.
#' @param manual_point_size Fixed point size used when
#'   `use_manual_point_size` is `TRUE`.
#' @param is_categorical_group Whether `optional_param2`'s column has been
#'   detected as categorical (drives group-based filtering/coloring instead
#'   of a continuous color scale).
#' @param selected_groups Character vector of categorical group values (from
#'   `optional_param2`) to include, when `is_categorical_group` is `TRUE`;
#'   other rows are excluded. Reassigned to "every group present" if none of
#'   the originally-requested groups actually match any data.
#' @return This function's entire local environment as a list
#'   (`as.list(environment())`) - `pointSize`, `pointType`, `pointCol`,
#'   `ternary_points1` (possibly row-filtered), `selected_groups` (possibly
#'   reassigned), `param1_values`, `param1_bins`, `unique_groups`,
#'   `group_colors`, `group_counts`, `MIN_POINT_SIZE`, and `MAX_POINT_SIZE`
#'   are the fields [prepare_ternary_plot_data()] and the preview/save
#'   renderers actually read back; the rest are this block's own internal
#'   working variables, echoed back unchanged from how they already existed
#'   in `prepare_ternary_plot_data()`'s own environment before this
#'   extraction.
#' @export
compute_point_styling <- function(ternary_points1, matrika, optional_param1, optional_param1_representation,
                                   optional_param2, color_palette, use_manual_point_size, manual_point_size,
                                   is_categorical_group, selected_groups) {
  # Prepare optional parameters for plotting
  MIN_POINT_SIZE <- 0.1
  MAX_POINT_SIZE <- 2.5
  pointSize <- rep(MIN_POINT_SIZE, nrow(ternary_points1))
  pointType <- rep(16, nrow(ternary_points1))
  pointCol <- rep("black", nrow(ternary_points1))

  # Optional param 1: point size or point type (enhanced from legacy code)
  if (use_manual_point_size) {
    # Use manual point size for all points
    pointSize <- rep(manual_point_size, nrow(ternary_points1))
    pointType <- rep(16, nrow(ternary_points1))  # Default circle
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Using manual point size:", manual_point_size, "\n")
    }
  } else if (!is.null(optional_param1)) {
    # Enhanced debugging for optional param1 data extraction
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Processing optional_param1\n")
      cat("DEBUG: optional_param1$col:", paste(optional_param1$col, collapse = ", "), "\n")
      cat("DEBUG: matrika dimensions:", dim(matrika), "\n")
      cat("DEBUG: matrika column names:", paste(names(matrika), collapse = ", "), "\n")
      cat("DEBUG: optional_param1$col in matrika:", optional_param1$col %in% names(matrika), "\n")
    }

    # Safety check: ensure the column exists in matrika
    if (!all(optional_param1$col %in% names(matrika))) {
      cat("ERROR: Optional param1 column(s) not found in matrika:",
          paste(setdiff(optional_param1$col, names(matrika)), collapse = ", "), "\n")
      cat("Available columns:", paste(names(matrika), collapse = ", "), "\n")
      stop("Optional param1 column not found in processed data")
    }

    param1_values <- matrika[, optional_param1$col, drop = FALSE]
    if (ncol(param1_values) > 1) {
      param1_values <- rowSums(param1_values, na.rm = TRUE)
    } else {
      param1_values <- param1_values[, 1]
    }

    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: param1_values extracted, length:", length(param1_values), "\n")
      cat("DEBUG: param1_values class:", class(param1_values), "\n")
      cat("DEBUG: param1_values range:", range(param1_values, na.rm = TRUE), "\n")
    }

    if (optional_param1_representation == "point_size") {
      # Point size representation
      minPointSize <- MIN_POINT_SIZE
      maxSize <- MAX_POINT_SIZE
      pointSize <- param1_values * (maxSize - minPointSize) / max(param1_values, na.rm = TRUE) + minPointSize
      # This formula assumes 0 -> minPointSize and max(param1_values) ->
      # maxSize, which only holds when param1_values is non-negative -
      # Optional Param 1 accepts any numeric column, not just ones
      # guaranteed non-negative (e.g. a signed measurement). A single
      # negative value maps below minPointSize (including zero or negative,
      # which points()/plot() silently draws as invisible - no warning, no
      # error, just a point missing from the plot with no indication why);
      # if every value is negative, max(param1_values) itself goes negative
      # and inverts the whole scale, so a row close to that (least
      # negative) maps to maxSize while more-negative rows can map far past
      # it. Clipped to the intended [minPointSize, maxSize] range either
      # way, with a warning naming how many points were affected instead of
      # letting this pass unnoticed.
      out_of_range <- pointSize < minPointSize | pointSize > maxSize
      out_of_range[is.na(out_of_range)] <- FALSE
      if (any(out_of_range)) {
        warning(sprintf(
          "%d point(s) have a negative or out-of-range Optional Param 1 value under Point Size representation; clipped to the visible size range [%.2g, %.2g] instead of rendering invisibly or oversized. Point Type representation may suit signed data better.",
          sum(out_of_range), minPointSize, maxSize
        ))
      }
      pointSize <- pmin(pmax(pointSize, minPointSize), maxSize)
      pointType <- 16  # Default circle
    } else if (optional_param1_representation == "point_type") {
      # Point type representation
      pointSize <- 0.7  # Fixed size
      # Create bins for point types
      param1_breaks <- quantile(param1_values, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
      param1_breaks <- unique(param1_breaks)

      if (length(param1_breaks) < 2) {
        param1_bins <- factor(rep(1, length(param1_values)), labels = "All")
        pointType <- rep(16, length(param1_values))  # All circles
      } else {
        param1_bins <- cut(param1_values, breaks = param1_breaks, include.lowest = TRUE)
        # Assign different point types based on bins
        point_types <- c(16, 17, 15, 18, 19)  # circle, triangle, square, diamond, filled diamond
        pointType <- point_types[as.numeric(param1_bins)]
      }
    }
  }
  # No else branch needed here: pointSize/pointType are already correctly
  # set to the full-length rep(MIN_POINT_SIZE/16, nrow(ternary_points1))
  # defaults a few lines up, for exactly this "neither manual size nor
  # optional_param1" case. A previous version of this branch reset them to
  # bare scalars (pointSize <- MIN_POINT_SIZE; pointType <- 16, length 1,
  # not length nrow(ternary_points1)) - which did nothing useful (the
  # values were identical, just wrongly shaped) and unconditionally tripped
  # the "Final safety check" further down into reinitializing both vectors
  # back to the very same values it had just overwritten - on every single
  # render/save that doesn't use Optional Param 1 (confirmed the single
  # most common case in practice), printing "Point size/type vector has
  # issues. Reinitializing." to the console/log every time even though
  # nothing was ever actually wrong.

  # Optional param 2: color (enhanced to handle categorical groups)
  if (!is.null(optional_param2)) {
    # Enhanced debugging for optional param2 data extraction
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Processing optional_param2\n")
      cat("DEBUG: optional_param2$col:", paste(optional_param2$col, collapse = ", "), "\n")
      cat("DEBUG: matrika dimensions:", dim(matrika), "\n")
      cat("DEBUG: matrika column names:", paste(names(matrika), collapse = ", "), "\n")
      cat("DEBUG: optional_param2$col in matrika:", optional_param2$col %in% names(matrika), "\n")
    }

    # Safety check: ensure the column exists in matrika
    if (!all(optional_param2$col %in% names(matrika))) {
      cat("ERROR: Optional param2 column(s) not found in matrika:",
          paste(setdiff(optional_param2$col, names(matrika)), collapse = ", "), "\n")
      cat("Available columns:", paste(names(matrika), collapse = ", "), "\n")
      stop("Optional param2 column not found in processed data")
    }

    param2_values <- matrika[, optional_param2$col, drop = FALSE]
    if (ncol(param2_values) > 1) {
      # For multiple columns, combine them (you might want to adjust this logic)
      param2_values <- param2_values[, 1]  # Take first column for now
    } else {
      param2_values <- param2_values[, 1]
    }

    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: optional_param2$col:", optional_param2$col, "\n")
      cat("DEBUG: matrika dimensions:", dim(matrika), "\n")
      cat("DEBUG: matrika column names:", paste(names(matrika), collapse = ", "), "\n")
      cat("DEBUG: param2_values class:", class(param2_values), "\n")
      cat("DEBUG: param2_values length:", length(param2_values), "\n")
      cat("DEBUG: param2_values first 10 values:", paste(head(param2_values, 10), collapse = ", "), "\n")
      cat("DEBUG: param2_values unique values:", paste(unique(param2_values), collapse = ", "), "\n")
    }

    # Check if this is categorical data
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: is_categorical_group:", is_categorical_group, "\n")
      cat("DEBUG: selected_groups:", if (is.null(selected_groups)) "NULL" else paste(selected_groups, collapse = ", "), "\n")
      cat("DEBUG: selected_groups length:", if (is.null(selected_groups)) 0 else length(selected_groups), "\n")
      cat("DEBUG: param2_values unique values:", paste(unique(param2_values), collapse = ", "), "\n")
    }

    # is_categorical_group alone, not also requiring a non-empty
    # selected_groups: the moment a user picks a categorical column for
    # Optional Param 2, rv$is_categorical_group_1/_2 flips to TRUE
    # immediately (server_ternary_plots_groups.R's detection observer) -
    # but rv$group_selections_1/_2 (and so selected_groups here) stays
    # NULL/empty until the user actually checks a box in the group
    # checklist that appears below it. Generating a plot/preview in that
    # gap (upload -> pick A/B/C -> pick a categorical color column -> hit
    # Save without first checking any group) used to fall through to the
    # ELSE branch below - the NUMERIC color-legend path - which calls
    # quantile() on param2_values; for a character/factor column that's an
    # immediate, uncaught "non-numeric argument to binary operator" crash,
    # confirmed via direct reproduction through the real reactive server
    # (not just this function in isolation). Requiring only
    # is_categorical_group here routes that state into the categorical
    # branch instead, where an empty selected_groups already resolves
    # correctly with no further changes needed: gsub() on a NULL
    # selected_groups returns character(0), matching nothing in
    # param2_values, which lands on the "no groups matched" fallback
    # immediately below and shows every group - exactly the graceful
    # "nothing chosen yet" behavior this state should have had all along.
    if (is_categorical_group) {
      # Handle categorical groups
      # Extract group names from selected_groups (remove sample counts in parentheses)
      group_names <- gsub("\\s*\\([^)]*\\)$", "", selected_groups)

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Extracted group names:", paste(group_names, collapse = ", "), "\n")
        cat("DEBUG: Original selected_groups:", paste(selected_groups, collapse = ", "), "\n")
      }

      # Filter data to selected groups
      group_mask <- param2_values %in% group_names

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Group mask sum:", sum(group_mask), "out of", length(group_mask), "\n")
        cat("DEBUG: Matching groups found:", sum(group_mask) > 0, "\n")
      }

      # Safety check: if no groups match (including the now-routed-here
      # case of no groups selected yet at all), show all groups instead.
      if (sum(group_mask) == 0) {
        if (is.null(group_names) || length(group_names) == 0) {
          cat("No groups selected yet - showing all groups.\n")
        } else {
          cat("Warning: No data matches selected groups. Showing all groups instead.\n")
        }
        group_mask <- rep(TRUE, length(param2_values))
        selected_groups <- unique(param2_values)
        group_names <- selected_groups
      }

      ternary_points1 <- ternary_points1[group_mask, ]
      param2_values <- param2_values[group_mask]
      pointSize <- pointSize[group_mask]
      pointType <- pointType[group_mask]

      # Generate distinct colors for groups
      unique_groups <- unique(param2_values)
      n_groups <- length(unique_groups)
      group_colors <- generate_distinct_colors(n_groups)

      # Assign colors to groups
      group_color_map <- setNames(group_colors, unique_groups)
      pointCol <- group_color_map[as.character(param2_values)]

      # Safety check: ensure pointCol doesn't contain NA values
      if (any(is.na(pointCol))) {
        cat("Warning: Some groups don't have colors assigned. Using default colors.\n")
        pointCol[is.na(pointCol)] <- "black"
      }

      # Safety check: ensure pointSize and pointType don't contain NA values
      if (any(is.na(pointSize))) {
        cat("Warning: Some point sizes are NA. Using default size.\n")
        pointSize[is.na(pointSize)] <- MIN_POINT_SIZE
      }
      if (any(is.na(pointType))) {
        cat("Warning: Some point types are NA. Using default type.\n")
        pointType[is.na(pointType)] <- 16
      }

      # Store group information for legend
      group_counts <- table(param2_values)

    } else {
      # Handle numeric data (existing logic)
      #
      # Guard: this branch is only reachable for a column the caller has
      # already decided ISN'T categorical (is_categorical_group FALSE, or
      # TRUE with no matching groups after the earlier fallback) - but
      # nothing downstream of that decision re-checks it's actually
      # numeric before quantile()ing it a few lines down. In the live app
      # this can't happen via the UI as of today's cap fix (a text column
      # with too many distinct values to be treated as categorical is
      # exactly the scenario that fix targets - see
      # server_ternary_plots_groups.R's own comment), but
      # prepare_ternary_plot_data()/general_ternary_plot() are exported,
      # directly callable functions, not gated behind that UI-side
      # detection - a direct call passing a genuinely non-numeric
      # optional_param2 column with is_categorical_group left FALSE (or a
      # categorical column whose real cardinality exceeds what the caller
      # checked) would otherwise still reach quantile() on text data and
      # crash with a raw "non-numeric argument to binary operator" -
      # exactly the class of bug fixed for the "no groups selected yet"
      # case earlier in this file. Failing clearly here, at the actual
      # point of the mismatch, costs nothing for the common numeric case
      # and replaces that raw crash with an actionable message for the
      # rest.
      if (!is.numeric(param2_values)) {
        # Deliberately not claiming a specific cause (e.g. "too many
        # distinct values") - is_categorical_group can land FALSE here for
        # two different, real reasons: the column's own cardinality
        # exceeded the 50-unique-values cap (server_ternary_plots_groups.R),
        # or - in the Multiple Ternary Creator's batch path specifically -
        # categorical detection is never wired up at all regardless of the
        # column's cardinality (extract_ternary_params()'s own safety-check
        # re-detection short-circuits there before it can run at all,
        # confirmed by reading server_ternary_plots_batch.R's caller - a
        # documented, pre-existing limitation of that tab, not something
        # this fix changes). A message asserting the wrong one of those two
        # causes would send a batch-tab user chasing a column-cardinality
        # fix that was never the actual problem.
        stop("Optional Param 2 (", paste(optional_param2$col, collapse = "+"),
             ") has non-numeric values but isn't being treated as a categorical color grouping here ",
             "(either it has more than 50 distinct values, or this tab doesn't support categorical grouping for Optional Param 2). ",
             "Choose a column with 50 or fewer distinct values in a tab that supports categorical grouping, or use a numeric column instead.")
      }
      # Check if the selected column is Aspect.Ratio for special handling
      if (length(optional_param2$col) == 1 && optional_param2$col == "Aspect.Ratio") {
        # Use hardcoded breaks for Aspect.Ratio
        param2_breaks <- c(1, 1.5, 3, 5, 10, 100000)
        param2_labels <- c("1-1.5", "1.5-3", "3-5", "5-10", "10+")
        param2_bins <- cut(param2_values, breaks = param2_breaks, labels = param2_labels, include.lowest = TRUE)
        n_colors <- length(levels(param2_bins))
      } else {
        # Use quantile-based binning for other columns
        param2_breaks <- quantile(param2_values, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
        param2_breaks <- unique(param2_breaks) # Make breaks unique

        if (length(param2_breaks) < 2) {
          # All values identical or not enough to make bins; fallback coloring
          param2_bins <- factor(rep(1, length(param2_values)), labels = "All")
          n_colors <- 1
        } else {
          param2_bins <- cut(param2_values, breaks = param2_breaks, include.lowest = TRUE)
          n_colors <- length(levels(param2_bins))
        }
      }

      if (color_palette == "blue") {
        param2_colors <- colorRampPalette(c("#357ABD", "#002147"))(n_colors)
      } else if (color_palette == "red") {
        param2_colors <- colorRampPalette(c("#FF6666", "#990000"))(n_colors)
      } else if (color_palette == "viridis") {
        if (!requireNamespace("viridisLite", quietly = TRUE)) install.packages("viridisLite")
        param2_colors <- viridisLite::viridis(n_colors)
      } else if (color_palette == "rainbow") {
        param2_colors <- rainbow(n_colors)
      } else {
        param2_colors <- rep("grey", n_colors)
      }
      pointCol <- param2_colors[as.numeric(param2_bins)]
    }

  }
  # No else branch needed here either - same reasoning as the matching
  # pointSize/pointType case above: pointCol is already correctly
  # rep("black", nrow(ternary_points1)) from its initial declaration. This
  # branch used to reset it to the bare scalar "black" (length 1), which
  # the "Final safety check" below then silently reinitialized back to the
  # exact same value, correctly shaped - on every render/save with no
  # Optional Param 2 set, printing its own "Point color vector has issues.
  # Reinitializing." for no real reason.

  # Final safety check: ensure all vectors are properly initialized
  n_points <- nrow(ternary_points1)
  if (length(pointSize) != n_points || any(is.na(pointSize))) {
    cat("Warning: Point size vector has issues. Reinitializing.\n")
    pointSize <- rep(MIN_POINT_SIZE, n_points)
  }
  if (length(pointType) != n_points || any(is.na(pointType))) {
    cat("Warning: Point type vector has issues. Reinitializing.\n")
    pointType <- rep(16, n_points)
  }
  if (length(pointCol) != n_points || any(is.na(pointCol))) {
    cat("Warning: Point color vector has issues. Reinitializing.\n")
    pointCol <- rep("black", n_points)
  }

  if (getOption("ternary.debug", FALSE)) {
    if (length(pointSize) > 0) {
      cat("DEBUG: Point size range:", range(pointSize), "\n")
    } else {
      cat("DEBUG: Point size range: empty vector\n")
    }
    if (length(pointType) > 0) {
      cat("DEBUG: Point type range:", range(pointType), "\n")
    } else {
      cat("DEBUG: Point type range: empty vector\n")
    }
    if (length(pointCol) > 0) {
      cat("DEBUG: Point color unique values:", unique(pointCol), "\n")
    } else {
      cat("DEBUG: Point color unique values: empty vector\n")
    }
  }

  as.list(environment())
}

#' Apply IQR/Z-score/MAD statistical outlier filtering
#'
#' Dispatches to whichever of [apply_iqr_filter()]/[apply_zscore_filter()]/
#' [apply_mad_filter()] (`statistical_filters.R`) is active, applying each
#' in turn with this app's fixed multiplier/threshold constants (IQR:
#' `1.5`, Z-score/MAD: `3`). Only one method is meant to be active per plot
#' - enforced upstream in `general_ternary_plot()`, not here - but this
#' function has no mutual-exclusion guard of its own and will run all
#' three in sequence if somehow more than one flag is set, matching the
#' original inline code's own behavior exactly (a pure lift, not a
#' rewrite). Extracted from [prepare_ternary_plot_data()] as its own
#' function because it's the most self-contained of that function's
#' remaining responsibilities (per the vidternary Structural Audit's §04
#' responsibility table) - it only reads `M` and the filter flags/
#' parameters below, and its only real output is `M`.
#'
#' @param M The data frame to filter (already loaded/individually filtered).
#' @param use_iqr_filter,use_zscore_filter,use_mad_filter Which method(s),
#'   if any, are active.
#' @param selected_columns Character vector of at least 2 numeric column
#'   names to use; mandatory whenever any method is active.
#' @param keep_outliers_iqr,keep_outliers_zscore,keep_outliers_mad If
#'   `TRUE` for the active method, keep only the flagged outliers instead
#'   of removing them.
#' @return This function's entire local environment as a list
#'   (`as.list(environment())`) - `M` is the only field
#'   [prepare_ternary_plot_data()] actually reads back.
#' @export
apply_statistical_filtering <- function(M, use_iqr_filter, use_zscore_filter, use_mad_filter,
                                         selected_columns, keep_outliers_iqr, keep_outliers_zscore,
                                         keep_outliers_mad) {
  # Apply statistical filtering BEFORE multivariate analysis (as in legacy code)
  if (use_iqr_filter || use_zscore_filter || use_mad_filter) {
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Applying statistical filtering methods\n")
    }

    # MANDATORY COLUMN SELECTION: User must select columns for statistical filtering
    if (is.null(selected_columns) || length(selected_columns) == 0) {
      stop("Column selection is MANDATORY for statistical filtering. Please select at least 2 numeric columns before proceeding.")
    }

    # Validate minimum number of columns
    if (length(selected_columns) < 2) {
      stop("At least 2 numeric columns must be selected for statistical filtering. Currently selected: ", length(selected_columns))
    }

    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Using selected columns for statistical filtering:", paste(selected_columns, collapse = ", "), "\n")
    }

    if (use_iqr_filter) {
      if (getOption("ternary.debug", FALSE)) cat("DEBUG: Applying IQR filter\n")
      M <- apply_iqr_filter(M, selected_columns, 1.5, keep_outliers_iqr)
    }

    if (use_zscore_filter) {
      if (getOption("ternary.debug", FALSE)) cat("DEBUG: Applying Z-score filter\n")
      M <- apply_zscore_filter(M, selected_columns, 3, keep_outliers_zscore)
    }

    if (use_mad_filter) {
      if (getOption("ternary.debug", FALSE)) cat("DEBUG: Applying MAD filter\n")
      M <- apply_mad_filter(M, selected_columns, 3, keep_outliers_mad)
    }

    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: After statistical filtering, data dimensions:", dim(M), "\n")
    }
  }

  as.list(environment())
}

#' Resolve a ternary plot's output folder and file-base name
#'
#' Two distinct code paths, both producing `custom_folder`, `file_base`,
#' and `plot_folder_name`: when `output_dir` is supplied for a real
#' (non-preview) save, uses `output_dir` directly (the Multiple Ternary
#' Creator's "one shared folder for every plot" behavior), routing the
#' display name through [extract_file_base()] (`file_management.R`) so a
#' crafted upload filename containing path separators can't steer the save
#' outside `output_dir`; otherwise delegates to
#' [create_ternary_output_dir()] (`file_management.R`), which creates a
#' per-charge subfolder (with a timestamp suffix if one already exists) or
#' returns `custom_folder = NULL` in preview mode. Extracted from
#' [prepare_ternary_plot_data()] as its own function because it's fully
#' self-contained (per the vidternary Structural Audit's §04 responsibility
#' table) - of its three outputs, only `file_base` is read again by this
#' function's caller (passed to [build_ternary_plot_title()] for the
#' title's "charge" line); `custom_folder`/`plot_folder_name` are computed
#' but not read by any downstream consumer today, exactly as before this
#' extraction - a pure reorganization, not a cleanup of that.
#'
#' @param xlsx_file Path to the uploaded `.xlsx` file (temp upload path).
#' @param xlsx_display_name Optional original filename, preferred over
#'   `xlsx_file`'s temp-upload basename.
#' @param output_dir Base output directory for a real (non-preview) save,
#'   or `NULL` to use the original per-charge-subfolder logic.
#' @param preview If `TRUE`, this call is only feeding a live preview
#'   render, not a save - no directory is created.
#' @param working_dir Directory to resolve relative paths against, passed
#'   through to [create_ternary_output_dir()].
#' @return This function's entire local environment as a list
#'   (`as.list(environment())`) - `custom_folder`, `file_base`, and
#'   `plot_folder_name` are the fields that existed in
#'   [prepare_ternary_plot_data()]'s own environment before this
#'   extraction; `dir_info` is this function's own internal working
#'   variable in the `create_ternary_output_dir()` branch, echoed back
#'   unchanged.
#' @export
resolve_ternary_output_directory <- function(xlsx_file, xlsx_display_name, output_dir, preview, working_dir) {
  # Create output directory structure using file management module
  # For multiple ternary plots, use the output_dir directly instead of creating subfolders

  if (!is.null(output_dir) && !preview) {
    # Use the output_dir directly for multiple ternary plots. Routed
    # through extract_file_base() (file_management.R), the same safe
    # basename()-then-strip-extension helper the preview branch below
    # already uses via create_ternary_output_dir() - a raw
    # tools::file_path_sans_ext(xlsx_display_name) here (the client-
    # supplied upload filename) would let a crafted name containing path
    # separators steer this save outside output_dir.
    custom_folder <- output_dir
    file_base <- extract_file_base(xlsx_file, xlsx_display_name)
    plot_folder_name <- paste0("charge", file_base)
  } else {
    # Use the original directory creation logic for single plots
  dir_info <- create_ternary_output_dir(
    xlsx_file = xlsx_file,
    xlsx_display_name = xlsx_display_name,
    output_dir = output_dir,
    preview = preview,
    working_dir = working_dir
  )

  custom_folder <- dir_info$custom_folder
  file_base <- dir_info$file_base
  plot_folder_name <- dir_info$plot_folder_name
  }

  as.list(environment())
}

#' Apply per-element and optional-parameter filters to a ternary plot's data
#'
#' Applies element A/B/C's per-column filters (via `apply_individual_filters()`
#' below - either one shared filter string applied to every selected column,
#' or genuinely independent per-column filters, depending on whether
#' `individual_filters_A`/`B`/`C` is supplied), then `optional_param1`'s and
#' `optional_param2`'s own single filter string, if either has one. Defines
#' `parse_filter_condition()`, `apply_filter()`, and `apply_individual_filters()`
#' as its own *local* closures, exactly as in the original single-function
#' version of this code - they are NOT the same functions as the
#' same-named ones in `helpers.R`/`helpers_filters.R` (different,
#' ternary-plot-specific behavior), so they must stay local rather than
#' becoming top-level functions, to avoid silently shadowing those
#' unrelated global functions package-wide (a real instance of exactly
#' this bug class, unrelated to these, was found and fixed three times
#' elsewhere in this package - see the vidternary Structural Audit's
#' §03/§08). [prepare_ternary_plot_data()] keeps its own two sibling local
#' closures (`preview_title_layout()`, `calculate_plot_dimensions()`)
#' untouched in its own body - neither is used by filtering, and both are
#' still needed there (as `build_ternary_plot_title()`'s `title_layout_fn`
#' callback, and echoed into `pd` for `ternary_plot_save.R`'s own use).
#' Extracted from [prepare_ternary_plot_data()] as its own function per the
#' vidternary Structural Audit's §04 responsibility table.
#'
#' @param M The data frame to filter (already loaded/validated).
#' @param element_A,element_B,element_C Ternary-axis element specs, each a
#'   `list(col = <one or more column names>, filter = <filter string or
#'   NULL/list>)`.
#' @param individual_filters_A,individual_filters_B,individual_filters_C
#'   Named lists (by column) of per-element filter strings, as built by
#'   `collect_main_ternary_filters()`; `NULL`/empty falls back to each
#'   element's own single `filter` value applied to every selected column.
#' @param optional_param1,optional_param2 Optional `list(col = <column
#'   name(s)>, filter = <filter string or NULL>)` for point size/type and
#'   point color / categorical grouping, respectively.
#' @param preview If `TRUE`, suppress the console progress messages
#'   `apply_individual_filters()` and this function's own filter loops print.
#' @return This function's entire local environment as a list
#'   (`as.list(environment())`) - `M` (filtered) and `all_selected_elements`
#'   are the fields [prepare_ternary_plot_data()] actually reads back; the
#'   rest are this block's own internal working variables (including its
#'   three local closures), echoed back unchanged from how they already
#'   existed in `prepare_ternary_plot_data()`'s own environment before this
#'   extraction.
#' @export
apply_element_and_parameter_filters <- function(M, element_A, element_B, element_C,
                                                 individual_filters_A, individual_filters_B, individual_filters_C,
                                                 optional_param1, optional_param2, preview) {
  # ---- CRITICAL HELPER FUNCTIONS ----
  # These are local/nested on purpose, exactly as in the original single-file
  # version - they are NOT the same functions as the same-named ones in
  # helpers.R/helpers_filters.R (different, ternary-plot-specific behavior),
  # so they must stay local rather than becoming top-level functions, to
  # avoid silently shadowing those unrelated global functions package-wide.

  # Parses a filter string like "> 10" or "<= 5.2" into a logical vector
  # over col_values. This one implementation backs every filter code path
  # in this function - apply_filter() below, and the two per-column loops
  # inside apply_individual_filters() further down (one for "same filter
  # applied to every selected column", one for genuinely independent
  # per-column filters) - which used to each parse this string with their
  # own copy-pasted logic. Only one of the three stripped stray non-numeric
  # characters from a malformed value (e.g. a trailing unit) before giving
  # up, so the exact same malformed filter string was a hard error via one
  # path and silently cleaned up via another; all three now agree, using
  # the more forgiving behavior.
  parse_filter_condition <- function(col_values, filter) {
    if (!grepl("^[><=!]+", filter)) {
      stop("Invalid filter format. Use operators: >, <, >=, <=, ==, !=")
    }
    operator <- gsub("^([><=!]+).*", "\\1", filter)
    value_str <- gsub("^([><=!]+)\\s*", "", filter)
    value <- as.numeric(value_str)
    if (is.na(value)) {
      value <- as.numeric(gsub("[^0-9.-]", "", value_str))
      if (is.na(value)) stop("Invalid filter value: ", value_str, ". Must be a numeric value.")
    }
    switch(operator,
      ">"  = col_values > value,
      "<"  = col_values < value,
      ">=" = col_values >= value,
      "<=" = col_values <= value,
      "==" = col_values == value,
      "!=" = col_values != value,
      stop("Invalid filter format. Use operators: >, <, >=, <=, ==, !=")
    )
  }

  # Safe filtering function - prevents security issues by avoiding eval()
  apply_filter <- function(df, col, filter) {
    if (is.null(filter)) return(df)
    df[parse_filter_condition(df[[col]], filter), , drop = FALSE]
  }

  # Individual element filtering function - handles both single and individual column filters
  apply_individual_filters <- function(data, element, individual_filters, element_name, preview = FALSE) {
    if (is.null(element) || is.null(element$col) || length(element$col) == 0) {
      if (getOption("ternary.debug", FALSE)) cat("DEBUG: No", element_name, "elements selected\n")
      return(data)
    }



    # If no individual filters provided, use the old single filter method
    if (is.null(individual_filters) || length(individual_filters) == 0) {
      if (!is.null(element$filter) && !is.na(element$filter) && nzchar(as.character(element$filter))) {
        if (length(element$col) > 1) {
          # For multiple columns, apply same filter to each column individually
          data[, element$col] <- lapply(data[, element$col, drop = FALSE], as.numeric)
          keep_rows <- rep(TRUE, nrow(data))
          for (col in element$col) {
            keep_rows <- keep_rows & parse_filter_condition(data[[col]], element$filter)
          }
          data <- data[keep_rows, , drop = FALSE]
        } else {
          data[, element$col] <- as.numeric(data[, element$col])
          # Handle single column with list filter structure
          if (is.list(element$filter) && length(element$filter) > 0) {
            # Extract the actual filter value from the list
            filter_value <- element$filter[[1]]
            if (!is.null(filter_value) && !is.na(filter_value) && nchar(trimws(as.character(filter_value))) > 0) {
              data <- apply_filter(data, element$col, filter_value)
            }
          } else {
            # Direct filter value
            data <- apply_filter(data, element$col, element$filter)
          }
        }
        if (!preview) {
          cat("After filtering", paste(element$col, collapse = "+"), "with filter", paste(element$filter, collapse = ", "), ":\n")
          print(dim(data))
        }
      }
    } else {
      # Apply individual filters to each element
      data[, element$col] <- lapply(data[, element$col, drop = FALSE], as.numeric)
      keep_rows <- rep(TRUE, nrow(data))

      for (col in element$col) {
        if (col %in% names(individual_filters) && !is.null(individual_filters[[col]]) && !is.na(individual_filters[[col]]) && nzchar(as.character(individual_filters[[col]]))) {
          keep_rows <- keep_rows & parse_filter_condition(data[[col]], individual_filters[[col]])

          if (!preview) {
            cat("Applied filter to", col, ":", individual_filters[[col]], "\n")
          }
        }
      }

      data <- data[keep_rows, , drop = FALSE]
      if (!preview) {
        cat("After filtering", element_name, "elements with individual filters:\n")
        print(dim(data))
      }
    }

    if (getOption("ternary.debug", FALSE)) cat("DEBUG: After", element_name, "filtering, data dimensions:", dim(data), "\n")
    return(data)
  }

  # Define all_selected_elements at function level for use throughout
  all_selected_elements <- c(element_A$col, element_B$col, element_C$col)

  # Apply individual element filtering (A, B, C) with individual filters.
  # apply_individual_filters() is the *local* function defined just above,
  # not a shared one from helpers.R/helpers_filters.R - an earlier version
  # of this comment claimed otherwise, but a same-named top-level function
  # in helpers_filters.R had already drifted into a completely different
  # (and, since local scoping always wins here, entirely unused)
  # implementation; that orphaned copy has been removed rather than the
  # claim corrected, since this local one is - and was always meant to be -
  # the one real implementation.

  # Apply individual element filters
  M <- apply_individual_filters(M, element_A, individual_filters_A, "A", preview)
  M <- apply_individual_filters(M, element_B, individual_filters_B, "B", preview)
  M <- apply_individual_filters(M, element_C, individual_filters_C, "C", preview)

  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: After individual element filtering, data dimensions:", dim(M), "\n")
  }

  # Apply optional parameter 1 filtering. Used to parse optional_param1$filter
  # with its own copy-pasted operator/value logic instead of going through
  # parse_filter_condition()/apply_filter() above - the two inline copies
  # missed by pass 4's filter-parser consolidation (which unified
  # apply_filter() and both apply_individual_filters() loops, but not
  # these). Two concrete inconsistencies that fell out of that: (1) this
  # copy had no fallback to strip stray non-numeric characters from a
  # malformed value (e.g. a trailing unit) before giving up, so the exact
  # same malformed filter string was a hard error via one path and
  # silently cleaned up via another; (2) an unrecognized operator (or a
  # filter string not starting with one at all) was silently ignored here
  # - no filter applied, no error - where parse_filter_condition() already
  # raises a clear "Invalid filter format" message for the same input via
  # every other filter path in this function. Now consolidated onto
  # apply_filter() like every other filter path here, so a malformed or
  # unrecognized filter behaves identically no matter which of this
  # function's filter inputs it came from.
  if (!is.null(optional_param1) && !is.null(optional_param1$filter) && nzchar(optional_param1$filter)) {
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Applying optional parameter 1 filter:", optional_param1$filter, "\n")
      cat("DEBUG: Filtering columns:", paste(optional_param1$col, collapse=", "), "\n")
    }

    for (col in optional_param1$col) {
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Applying filter", optional_param1$filter, "to column", col, "\n")
        cat("DEBUG: Column values range:", range(M[[col]], na.rm=TRUE), "\n")
      }

      M <- apply_filter(M, col, optional_param1$filter)

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: After filtering column", col, ", data dimensions:", dim(M), "\n")
      }
    }
  } else {
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: No optional parameter 1 filter applied\n")
    }
  }

  # Apply optional parameter 2 filtering - same consolidation as optional
  # parameter 1 above, for the same reasons.
  if (!is.null(optional_param2) && !is.null(optional_param2$filter) && nzchar(optional_param2$filter)) {
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Applying optional parameter 2 filter:", optional_param2$filter, "\n")
      cat("DEBUG: Filtering columns:", paste(optional_param2$col, collapse=", "), "\n")
    }

    for (col in optional_param2$col) {
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Applying filter", optional_param2$filter, "to column", col, "\n")
        cat("DEBUG: Column values range:", range(M[[col]], na.rm=TRUE), "\n")
      }

      M <- apply_filter(M, col, optional_param2$filter)

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: After filtering column", col, ", data dimensions:", dim(M), "\n")
      }
    }
  } else {
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: No optional parameter 2 filter applied\n")
    }
  }

  as.list(environment())
}

#' Compute and validate a ternary plot's normalized A/B/C coordinates
#'
#' Builds `matrika` (the numeric-plus-optional-parameter working data frame:
#' selects `needed_columns`, drops zero-sum rows, `na.omit()`s, then splits
#' `element_columns` vs. `optional_columns`), sums each element's selected
#' column(s) into `A_values`/`B_values`/`C_values`, normalizes by their row
#' total into `ternary_points1`, and drops any row whose total is zero or
#' whose resulting coordinate is `NA`/infinite from both `M` and
#' `ternary_points1` - erroring if a needed column is missing from the data,
#' or if nothing survives validation. Extracted from
#' [prepare_ternary_plot_data()] as its own function because it's the last
#' and most central of that function's identified responsibilities (per
#' the vidternary Structural Audit's §04 responsibility table) - tackled
#' last of the seven extractions on this function, once every other piece
#' it interacts with (filtering, multivariate dispatch, point styling,
#' title assembly) had already been extracted and verified.
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
#'   actually read back; the rest (`needed_columns`, `element_columns`,
#'   `optional_columns`, `A_values`/`B_values`/`C_values`, `total_values`,
#'   `valid_rows`, and the possibly-further-filtered `M`) are echoed back
#'   unchanged from how they already existed in
#'   `prepare_ternary_plot_data()`'s own environment before this
#'   extraction.
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
  # (three independent selectInputs, ui_ternary_plots_tab.R) prevented this
  # before - reachable simply by picking the same element twice - and it
  # used to reach the raw, uncaught indexing crashes fixed just below
  # instead of a clear message.
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
  element_columns <- unique(all_selected_elements)
  matrika[, element_columns] <- lapply(matrika[, element_columns, drop = FALSE], as.numeric)
  row_sums <- rowSums(matrika[, element_columns, drop = FALSE], na.rm = TRUE)
  matrika <- matrika[row_sums > 0, , drop = FALSE]
  cat("DEBUG: Matrika dimensions after removing zero-sum rows:", dim(matrika), "\n")
  matrika <- na.omit(matrika)
  cat("DEBUG: Matrika dimensions after na.omit:", dim(matrika), "\n")
  log_operation("INFO", "Removed NA values", paste("Matrix dimensions:", dim(matrika)[1], "rows x", dim(matrika)[2], "columns"))

  # Only convert element columns to numeric, preserve optional parameter columns as character/factor
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

  # Element columns were already converted to numeric above (right after
  # matrika was built) - this used to redundantly repeat the identical
  # conversion here a second time; harmless (as.numeric() on already-numeric
  # data is a no-op) but pointless, so it's gone rather than left as
  # confusing duplicate work for the next reader.

  # Keep optional parameter columns as character/factor for categorical data
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: Element columns converted to numeric:", paste(element_columns, collapse = ", "), "\n")
    cat("DEBUG: Optional columns preserved as character:", paste(optional_columns, collapse = ", "), "\n")
    cat("DEBUG: Final matrika column classes:", paste(sapply(matrika, class), collapse = ", "), "\n")
  }

  # Keep as data frame to preserve different column types
  # matrika <- as.matrix(matrika)  # Don't convert to matrix to preserve column types

  log_operation("INFO", "Generating ternary coordinates")

  # Calculate ternary coordinates from the filtered data
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: About to calculate ternary coordinates\n")
    cat("DEBUG: Final filtered data dimensions:", dim(M), "\n")
    cat("DEBUG: Element A columns:", paste(element_A$col, collapse=", "), "\n")
    cat("DEBUG: Element B columns:", paste(element_B$col, collapse=", "), "\n")
    cat("DEBUG: Element C columns:", paste(element_C$col, collapse=", "), "\n")
  }

  # Sum the selected columns for each element
  A_values <- rowSums(M[, element_A$col, drop = FALSE], na.rm = TRUE)
  B_values <- rowSums(M[, element_B$col, drop = FALSE], na.rm = TRUE)
  C_values <- rowSums(M[, element_C$col, drop = FALSE], na.rm = TRUE)

  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: A_values range:", range(A_values, na.rm=TRUE), "\n")
    cat("DEBUG: B_values range:", range(B_values, na.rm=TRUE), "\n")
    cat("DEBUG: C_values range:", range(C_values, na.rm=TRUE), "\n")
  }

  # Calculate ternary coordinates
  total_values <- A_values + B_values + C_values
  ternary_points1 <- data.frame(
    A = A_values / total_values,
    B = B_values / total_values,
    C = C_values / total_values
  )

  # Validate ternary coordinates to prevent Ternary package errors
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: Validating ternary coordinates\n")
    cat("DEBUG: Total values range:", range(total_values, na.rm=TRUE), "\n")
    cat("DEBUG: Any zero totals:", any(total_values == 0, na.rm=TRUE), "\n")
    cat("DEBUG: Any NA in coordinates:", any(is.na(ternary_points1)), "\n")
    cat("DEBUG: Any infinite values:", any(is.infinite(as.matrix(ternary_points1))), "\n")
  }

  # Remove rows with invalid coordinates (zero totals, NA, or infinite values)
  valid_rows <- total_values > 0 & !is.na(total_values) &
                !is.na(ternary_points1$A) & !is.na(ternary_points1$B) & !is.na(ternary_points1$C) &
                !is.infinite(ternary_points1$A) & !is.infinite(ternary_points1$B) & !is.infinite(ternary_points1$C)

  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: Valid rows:", sum(valid_rows), "out of", length(valid_rows), "\n")
  }

  # Filter data and coordinates
  M <- M[valid_rows, , drop = FALSE]
  ternary_points1 <- ternary_points1[valid_rows, , drop = FALSE]

  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: After validation - M dimensions:", dim(M), "\n")
    cat("DEBUG: After validation - ternary_points1 dimensions:", dim(ternary_points1), "\n")
  }

  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: Ternary coordinates calculated\n")
    cat("DEBUG: ternary_points1 dimensions:", dim(ternary_points1), "\n")
    cat("DEBUG: Sample ternary coordinates (first 3 rows):\n")
    print(head(ternary_points1, 3))
  }

  if (nrow(ternary_points1) == 0) stop("Error: No valid data left after filtering.")

  log_operation("SUCCESS", "Generated ternary coordinates", paste(nrow(ternary_points1), "points"))

  as.list(environment())
}

#' Load, filter, and prepare data for a single ternary plot
#'
#' Does everything [general_ternary_plot()] needs exactly once, regardless
#' of preview vs. save: loads the Excel file, applies individual-element,
#' optional-parameter, statistical, and multivariate filters, computes
#' ternary coordinates, builds the plot title, and works out point
#' size/color/shape. Touches no graphics device - see
#' `ternary_plot_preview.R`/`ternary_plot_save.R`'s renderers for that.
#' The caller is responsible
#' for `setwd(working_dir)` (with `on.exit()` restore) before calling this,
#' since that needs to stay in effect across this call and the render call
#' that follows it.
#'
#' @param xlsx_file Path to the uploaded `.xlsx` file (temp upload path).
#' @param working_dir Directory to resolve relative paths against; the
#'   caller must already have `setwd()`'d into it.
#' @param output_dir Base output directory for a real (non-preview) save.
#' @param element_A,element_B,element_C Ternary-axis element specs, each a
#'   `list(col = <one or more column names>)`.
#' @param optional_param1 Optional `list(col = <column name(s)>, filter =
#'   <filter string or NULL>)` for point size/type representation.
#' @param optional_param2 Optional `list(col = <column name(s)>, filter =
#'   <filter string or NULL>)` for point color / categorical grouping.
#' @param color_palette Palette name (`"blue"`, `"red"`, `"viridis"`, or
#'   `"rainbow"`) used when `optional_param2` drives point color.
#' @param xlsx_display_name Optional original filename, preferred over
#'   `xlsx_file`'s temp-upload basename for titles/output filenames.
#' @param preview If `TRUE`, skip directory/file creation - this call is
#'   only feeding a live preview render, not a save.
#' @param use_mahalanobis Apply Mahalanobis-distance outlier filtering.
#' @param reference_data Optional reference dataset for multivariate
#'   filtering, when `mahalanobis_reference`/an isolation-forest reference
#'   mode needs one other than the file being processed.
#' @param optional_param1_representation `"point_size"` or `"point_type"` -
#'   how `optional_param1` maps onto the plotted points.
#' @param output_format File format for a real save (e.g. `"png"`).
#' @param use_isolation_forest Apply isolation-forest outlier filtering.
#' @param use_iqr_filter,use_zscore_filter,use_mad_filter Apply IQR /
#'   Z-score / MAD statistical outlier filtering. Only one
#'   statistical/multivariate filter is meant to be active per plot -
#'   enforced upstream in [general_ternary_plot()].
#' @param lambda,omega Sensitivity/leniency parameters for the automatic
#'   Mahalanobis threshold formula (see [compute_mahalanobis_distance()]).
#' @param keep_outliers_mahalanobis,keep_outliers_isolation,keep_outliers_iqr,keep_outliers_zscore,keep_outliers_mad
#'   If `TRUE` for the active filter method, keep only the flagged
#'   outliers instead of removing them.
#' @param individual_filters_A,individual_filters_B,individual_filters_C
#'   Named lists (by column) of per-element filter strings, as built by
#'   `collect_main_ternary_filters()`.
#' @param custom_mdthresh Manual Mahalanobis distance threshold, used when
#'   `mdthresh_mode == "manual"`.
#' @param mdthresh_mode `"auto"` or `"manual"` Mahalanobis threshold mode.
#' @param mahalanobis_reference Which dataset the Mahalanobis/isolation-
#'   forest reference distribution is fit to (`"self"`, `"dataset1"`, or
#'   `"dataset2"`).
#' @param selected_columns Character vector of numeric columns used for
#'   multivariate/statistical filtering (independent of the ternary axes).
#' @param include_plot_notes If `TRUE`, include the filter/method summary
#'   notes text alongside the plot.
#' @param use_manual_point_size If `TRUE`, use `manual_point_size` for
#'   every point instead of `optional_param1_representation`'s mapping.
#' @param manual_point_size Fixed point size used when
#'   `use_manual_point_size` is `TRUE`.
#' @param selected_groups Character vector of categorical group values
#'   (from `optional_param2`) to include, when `is_categorical_group` is
#'   `TRUE`; other rows are excluded.
#' @param is_categorical_group Whether `optional_param2`'s column has been
#'   detected as categorical (drives group-based filtering/coloring
#'   instead of a continuous color scale).
#' @return This function's entire local environment as a list
#'   (`as.list(environment())`), deliberately untyped: dozens of locals
#'   computed here (`clean_labels_A`/`B`/`C`, `ternary_points1`,
#'   `pointSize`/`pointCol`/`pointType`, `plot_title`, `title_parts`,
#'   `param1_values`, `param1_bins`, `unique_groups`, `group_colors`,
#'   `group_counts`, `col1_text`/`col2_text`/`col3_text`, `mahal_result`,
#'   `iso_result`, and more) are all read by the preview and/or save
#'   renderers via `pd$name` or `with(pd, {...})`, and hand-picking a
#'   narrower return list risked silently dropping one.
#' @export
#'
#' @section Restructuring (see the vidternary Structural Audit's §04/§08):
#' All seven of this function's identified responsibilities have now been
#' extracted into their own top-level, independently testable/documented
#' functions: [load_and_validate_ternary_source_data()] (the very first
#' thing this function does), [apply_element_and_parameter_filters()]
#' (per-element and optional-parameter filtering - moves three of this
#' function's five local closures into its own scope, leaving
#' `preview_title_layout()`/`calculate_plot_dimensions()` as the two that
#' still live here), [apply_statistical_filtering()] (IQR/Z-score/MAD
#' dispatch), [apply_multivariate_filtering()] (the Mahalanobis/Isolation
#' Forest outlier dispatch - the most tangled of the seven),
#' [resolve_ternary_output_directory()] (output-folder + file-base-name
#' resolution - the first to do real filesystem work rather than pure
#' in-memory transforms), [compute_ternary_coordinates()] (building
#' `matrika`/`ternary_points1` and validating them - the most central of
#' the seven, tackled last, once every other piece it interacts with had
#' already been extracted and verified), [build_ternary_plot_title()]
#' (called once, right after ternary coordinates are computed), and
#' [compute_point_styling()] (point size/type/color plus categorical-group
#' filtering and legend metadata - the largest of the seven). All seven are
#' called here, in this order, and their results merged back into this
#' function's own local environment via `list2env()`, so the final
#' `as.list(environment())` this function returns is unchanged in every
#' field and value across the whole restructuring - a pure internal
#' reorganization, not a behavior change, verified via golden-output
#' byte-diffing at every step.
prepare_ternary_plot_data <- function(
    xlsx_file,
    working_dir,
    output_dir,
    element_A,
    element_B,
    element_C,
    optional_param1,
    optional_param2,
    color_palette,
    xlsx_display_name,
    preview,
    use_mahalanobis,
    reference_data,
    optional_param1_representation,
    output_format,
    use_isolation_forest,
    use_iqr_filter,
    use_zscore_filter,
    use_mad_filter,
    lambda,
    omega,
    keep_outliers_mahalanobis,
    keep_outliers_isolation,
    keep_outliers_iqr,
    keep_outliers_zscore,
    keep_outliers_mad,
    individual_filters_A,
    individual_filters_B,
    individual_filters_C,
    custom_mdthresh,
    mdthresh_mode,
    mahalanobis_reference,
    selected_columns,
    include_plot_notes,
    use_manual_point_size,
    manual_point_size,
    selected_groups,
    is_categorical_group
) {

  # Variables that are only conditionally assigned below (depending on which
  # filter method or point-styling branch runs). Pre-declaring them as NULL
  # lets the render functions check `!is.null(pd$x)` instead of relying on
  # R's exists() the way the original single-function version did (exists()
  # only works within the same function's scope, which no longer holds once
  # this logic is split across three functions).
  mahal_result <- NULL
  iso_result <- NULL
  param1_bins <- NULL
  param1_values <- NULL
  unique_groups <- NULL
  group_colors <- NULL
  group_counts <- NULL

  # Load + validate: extracted into load_and_validate_ternary_source_data()
  # (see this function's own "Restructuring" doc section above) - identical
  # behavior, its only output is M.
  M <- load_and_validate_ternary_source_data(xlsx_file, element_A, element_B, element_C, output_format, preview)

  # ---- CRITICAL HELPER FUNCTIONS (title/dimension formatting) ----
  # preview_title_layout()/calculate_plot_dimensions() are local/nested on
  # purpose, exactly as in the original single-file version - they are NOT
  # the same functions as similarly-named ones elsewhere in the package
  # (different, ternary-plot-specific behavior), so they must stay local
  # rather than becoming top-level functions, to avoid silently shadowing
  # unrelated global utilities package-wide (a real instance of exactly
  # this bug class, unrelated to these, was found and fixed three times
  # elsewhere in this package - see the vidternary Structural Audit's
  # §03/§08). This function's other three local closures -
  # parse_filter_condition()/apply_filter()/apply_individual_filters() -
  # moved into apply_element_and_parameter_filters() (see this function's
  # own "Restructuring" doc section above) along with the filtering logic
  # that uses them; unrelated to title/dimension formatting, so they don't
  # need to stay in this function's own scope.

  # Function to preview title layout for debugging
  preview_title_layout <- function(title_parts) {
    final_title <- paste(title_parts, collapse = "\n")
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Title preview:\n")
      cat("Original parts:", length(title_parts), "\n")
      cat("Final title:\n", final_title, "\n")
      cat("Line count:", length(strsplit(final_title, "\n")[[1]]), "\n")
    }
    return(final_title)
  }

  # Function to calculate optimal plot dimensions based on title length.
  # Used by ternary_plot_save.R (via pd$calculate_plot_dimensions()) since
  # that's the only place that needs pixel dimensions for a file device.
  calculate_plot_dimensions <- function(title_parts) {
    final_title <- paste(title_parts, collapse = "\n")
    line_count <- length(strsplit(final_title, "\n")[[1]])

    # Base dimensions (matching legacy file)
    base_width <- 1200
    base_height <- 1400

    # Adjust height based on title lines
    if (line_count == 1) {
      # Single line: standard height
      height <- base_height
    } else if (line_count == 2) {
      # Two lines: increase height slightly
      height <- base_height + 100
    } else {
      # Three or more lines: increase height more
      height <- base_height + 200
    }

    return(list(width = base_width, height = height))
  }

  # Per-element and optional-parameter filtering: extracted into
  # apply_element_and_parameter_filters() (see this function's own
  # "Restructuring" doc section above) - identical behavior; the only
  # fields read again by this function itself are M (filtered) and
  # all_selected_elements (used by the coordinate-computation code below).
  filter_result <- apply_element_and_parameter_filters(
    M = M,
    element_A = element_A, element_B = element_B, element_C = element_C,
    individual_filters_A = individual_filters_A,
    individual_filters_B = individual_filters_B,
    individual_filters_C = individual_filters_C,
    optional_param1 = optional_param1, optional_param2 = optional_param2,
    preview = preview
  )
  list2env(filter_result, environment())

  # Statistical-outlier dispatch: extracted into apply_statistical_filtering()
  # (see this function's own "Restructuring" doc section above) - identical
  # behavior; the only field read back afterward is M (possibly re-filtered).
  stat_result <- apply_statistical_filtering(
    M = M,
    use_iqr_filter = use_iqr_filter,
    use_zscore_filter = use_zscore_filter,
    use_mad_filter = use_mad_filter,
    selected_columns = selected_columns,
    keep_outliers_iqr = keep_outliers_iqr,
    keep_outliers_zscore = keep_outliers_zscore,
    keep_outliers_mad = keep_outliers_mad
  )
  list2env(stat_result, environment())

  # Multivariate outlier dispatch: extracted into apply_multivariate_filtering()
  # (see this function's own "Restructuring" doc section above) - identical
  # behavior; the only fields read back afterward are M (possibly
  # re-filtered), mahal_result, and iso_result.
  mv_result <- apply_multivariate_filtering(
    M = M,
    use_mahalanobis = use_mahalanobis,
    use_isolation_forest = use_isolation_forest,
    selected_columns = selected_columns,
    mahalanobis_reference = mahalanobis_reference,
    reference_data = reference_data,
    preview = preview,
    keep_outliers_isolation = keep_outliers_isolation,
    keep_outliers_mahalanobis = keep_outliers_mahalanobis,
    lambda = lambda,
    omega = omega,
    custom_mdthresh = custom_mdthresh,
    mdthresh_mode = mdthresh_mode
  )
  list2env(mv_result, environment())

  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: After multivariate filtering, data dimensions:", dim(M), "\n")
    if (use_mahalanobis && !is.null(reference_data)) {
      cat("DEBUG: Multivariate analysis was applied. Original data should be filtered.\n")
      cat("DEBUG: Sample of filtered data (first 5 rows):\n")
      print(head(M, 5))
    }
  }

  # Output-directory resolution: extracted into
  # resolve_ternary_output_directory() (see this function's own
  # "Restructuring" doc section above) - identical behavior; the only
  # field read again by this function itself is file_base (passed to
  # build_ternary_plot_title() below).
  outdir_result <- resolve_ternary_output_directory(
    xlsx_file = xlsx_file,
    xlsx_display_name = xlsx_display_name,
    output_dir = output_dir,
    preview = preview,
    working_dir = working_dir
  )
  list2env(outdir_result, environment())

  # Coordinate computation + validation: extracted into
  # compute_ternary_coordinates() (see this function's own "Restructuring"
  # doc section above) - identical behavior; matrika and ternary_points1
  # are the fields read again by this function itself (and, via pd, by the
  # preview/save renderers).
  coord_result <- compute_ternary_coordinates(
    M = M,
    all_selected_elements = all_selected_elements,
    element_A = element_A, element_B = element_B, element_C = element_C,
    optional_param1 = optional_param1, optional_param2 = optional_param2,
    use_mahalanobis = use_mahalanobis, reference_data = reference_data
  )
  list2env(coord_result, environment())

  # Title + axis-label assembly: extracted into build_ternary_plot_title()
  # (see this function's own "Restructuring" doc section above) - identical
  # behavior; list2env() merges every local that function's own
  # as.list(environment()) return produced (clean_labels_A/B/C,
  # axis_labels_A/B/C, title_parts, plot_title, and the transient
  # opt1_label/opt2_label/mv_methods/stat_methods/indicator/fallback_name
  # locals along the way) back into this function's own environment, so
  # this function's final as.list(environment()) return is byte-for-byte
  # what it would have been with the inline version.
  list2env(
    build_ternary_plot_title(
      element_A = element_A, element_B = element_B, element_C = element_C,
      optional_param1 = optional_param1, optional_param1_representation = optional_param1_representation,
      optional_param2 = optional_param2,
      use_mahalanobis = use_mahalanobis, keep_outliers_mahalanobis = keep_outliers_mahalanobis,
      use_isolation_forest = use_isolation_forest, keep_outliers_isolation = keep_outliers_isolation,
      use_iqr_filter = use_iqr_filter, keep_outliers_iqr = keep_outliers_iqr,
      use_zscore_filter = use_zscore_filter, keep_outliers_zscore = keep_outliers_zscore,
      use_mad_filter = use_mad_filter, keep_outliers_mad = keep_outliers_mad,
      file_base = file_base, xlsx_display_name = xlsx_display_name, xlsx_file = xlsx_file,
      title_layout_fn = preview_title_layout
    ),
    environment()
  )

  # Point styling: extracted into compute_point_styling() (see this
  # function's own "Restructuring" doc section above) - identical behavior;
  # list2env() merges pointSize/pointType/pointCol, the possibly-filtered
  # ternary_points1, the possibly-reassigned selected_groups, and every
  # legend-metadata field (param1_values/param1_bins,
  # unique_groups/group_colors/group_counts, MIN_POINT_SIZE/MAX_POINT_SIZE)
  # back into this function's own environment.
  ps_result <- compute_point_styling(
    ternary_points1 = ternary_points1,
    matrika = matrika,
    optional_param1 = optional_param1,
    optional_param1_representation = optional_param1_representation,
    optional_param2 = optional_param2,
    color_palette = color_palette,
    use_manual_point_size = use_manual_point_size,
    manual_point_size = manual_point_size,
    is_categorical_group = is_categorical_group,
    selected_groups = selected_groups
  )
  list2env(ps_result, environment())

  # ---- Plot notes text (computed once here; drawn via mtext() by both
  # ternary_plot_preview.R and ternary_plot_save.R, which only differ in
  # which graphics device is active when they call mtext()) ----
  if (include_plot_notes) {
    # Generate comprehensive plot summary organized into 3 columns
    # Column 1: Elements and their filters
    elements_summary <- c()
    elements_summary <- c(elements_summary, paste("Data points:", nrow(ternary_points1)))
    elements_summary <- c(elements_summary, paste("Elements A:", paste(element_A$col, collapse = "+")))
    elements_summary <- c(elements_summary, paste("Elements B:", paste(element_B$col, collapse = "+")))
    elements_summary <- c(elements_summary, paste("Elements C:", paste(element_C$col, collapse = "+")))



    if (!is.null(individual_filters_A) && length(individual_filters_A) > 0) {
      filter_text <- paste("Element A filters:", paste(sapply(names(individual_filters_A), function(x) paste0(x, ":", individual_filters_A[[x]])), collapse = ", "))
      elements_summary <- c(elements_summary, filter_text)
    }

    if (!is.null(individual_filters_B) && length(individual_filters_B) > 0) {
      filter_text <- paste("Element B filters:", paste(sapply(names(individual_filters_B), function(x) paste0(x, ":", individual_filters_B[[x]])), collapse = ", "))
      elements_summary <- c(elements_summary, filter_text)
    }

    if (!is.null(individual_filters_C) && length(individual_filters_C) > 0) {
      filter_text <- paste("Element C filters:", paste(sapply(names(individual_filters_C), function(x) paste0(x, ":", individual_filters_C[[x]])), collapse = ", "))
      elements_summary <- c(elements_summary, filter_text)
    }

    # Column 2: Optional parameters and their filters
    optional_summary <- c()
    optional_summary <- c(optional_summary, "Optional Parameters:")

    if (!is.null(optional_param1)) {
      optional_summary <- c(optional_summary, paste("Parameter 1:", paste(optional_param1$col, collapse = "+")))

      if (!is.null(optional_param1$filter) && nzchar(optional_param1$filter)) {
        optional_summary <- c(optional_summary, paste("  Filter:", optional_param1$filter))

      } else {
        if (getOption("ternary.debug", FALSE)) {
          cat("DEBUG: Optional param1 filter is NULL or empty\n")
        }
      }
    }

    if (!is.null(optional_param2)) {
      optional_summary <- c(optional_summary, paste("Parameter 2:", paste(optional_param2$col, collapse = "+")))

      if (!is.null(optional_param2$filter) && nzchar(optional_param2$filter)) {
        optional_summary <- c(optional_summary, paste("  Filter:", optional_param2$filter))

      } else {
        if (getOption("ternary.debug", FALSE)) {
          cat("DEBUG: Optional param2 filter is NULL or empty\n")
        }
      }
      optional_summary <- c(optional_summary, paste("  Color palette:", color_palette))
    }

    # Column 3: Statistical filtering and multivariate analysis
    analysis_summary <- c()
    analysis_summary <- c(analysis_summary, "Analysis Methods:")

    # Multivariate analysis
    if (use_mahalanobis || use_isolation_forest) {
      mv_info <- c()
      if (use_mahalanobis) {
        outlier_status <- if (keep_outliers_mahalanobis) "(keep only outliers)" else "(remove outliers)"
        mv_info <- c(mv_info, paste("Mahalanobis (λ=", lambda, ", ω=", omega, ")", outlier_status))

        # Add detailed Mahalanobis distance information if available
        if (!is.null(mahal_result)) {
          mv_info <- c(mv_info, paste("  MDmean:", round(mahal_result$MDmean, 3)))
          mv_info <- c(mv_info, paste("  MDthresh:", round(mahal_result$MDthresh, 3)))
          mv_info <- c(mv_info, paste("  stdMD:", round(mahal_result$stdMD, 3)))
          if (!is.null(mahal_result$threshold_method)) {
            mv_info <- c(mv_info, paste("  Method:", mahal_result$threshold_method))
          }
        }
      }
      if (use_isolation_forest) {
        outlier_status <- if (keep_outliers_isolation) "(keep only outliers)" else "(remove outliers)"
        mv_info <- c(mv_info, paste("Isolation Forest", outlier_status))
      }
      analysis_summary <- c(analysis_summary, paste("Multivariate:", paste(mv_info, collapse = ", ")))
    }

    # Statistical filtering
    if (use_iqr_filter || use_zscore_filter || use_mad_filter) {
      stat_info <- c()
      if (use_iqr_filter) {
        outlier_status <- if (keep_outliers_iqr) "(keep only outliers)" else "(remove outliers)"
        stat_info <- c(stat_info, paste("IQR", outlier_status))
      }
      if (use_zscore_filter) {
        outlier_status <- if (keep_outliers_zscore) "(keep only outliers)" else "(remove outliers)"
        stat_info <- c(stat_info, paste("Z-Score", outlier_status))
      }
      if (use_mad_filter) {
        outlier_status <- if (keep_outliers_mad) "(keep only outliers)" else "(remove outliers)"
        stat_info <- c(stat_info, paste("MAD", outlier_status))
      }
      analysis_summary <- c(analysis_summary, paste("Statistical:", paste(stat_info, collapse = ", ")))
    }

    # Create three-column layout for plot notes with intelligent positioning
    # Calculate positions for better visibility based on content length
    col1_text <- paste(elements_summary, collapse = "\n")
    col2_text <- paste(optional_summary, collapse = "\n")
    col3_text <- paste(analysis_summary, collapse = "\n")

    # Calculate optimal positioning based on text length and content
    col1_lines <- length(strsplit(col1_text, "\n")[[1]])
    col2_lines <- length(strsplit(col2_text, "\n")[[1]])
    col3_lines <- length(strsplit(col3_text, "\n")[[1]])

    # Determine optimal line positioning based on content length
    if (max(col1_lines, col2_lines, col3_lines) <= 6) {
      # Short content: use standard positioning
      line_pos <- 2
      text_cex <- 0.6
    } else if (max(col1_lines, col2_lines, col3_lines) <= 12) {
      # Medium content: adjust positioning
      line_pos <- 3
      text_cex <- 0.55
    } else {
      # Long content: use extended positioning
      line_pos <- 4
      text_cex <- 0.5
    }
  } else {
    # If plot notes are not included, create empty variables to avoid errors
    col1_text <- ""
    col2_text <- ""
    col3_text <- ""
    line_pos <- 2
    text_cex <- 0.6
  }

  return(as.list(environment()))
}
