# ---- Ternary Plot: Filtering Stage (split out of ternary_plot_data_prep.R) ----
# The three filtering functions prepare_ternary_plot_data() calls in sequence:
# per-element/optional-parameter filters, then statistical (IQR/Z-score/MAD)
# outlier filtering, then multivariate (Mahalanobis/Isolation Forest) outlier
# filtering. Kept together since they're one documented family (this file's
# functions are called back-to-back by the orchestrator) rather than three
# separate one-function files.

#' Apply per-element and optional-parameter filters to a ternary plot's data
#'
#' Applies element A/B/C's per-column filters (via `apply_individual_filters()`
#' below - either one shared filter string applied to every selected column,
#' or genuinely independent per-column filters, depending on whether
#' `individual_filters_A`/`B`/`C` is supplied), then `optional_param1`'s and
#' `optional_param2`'s own single filter string, if either has one. Defines
#' `parse_filter_condition()`, `apply_filter()`, and `apply_individual_filters()`
#' as its own *local* closures - they are NOT the same functions as the
#' same-named ones in `helpers.R`/`helpers_filters.R` (different,
#' ternary-plot-specific behavior), so they must stay local rather than
#' becoming top-level functions, to avoid silently shadowing those
#' unrelated global functions package-wide.
#' [prepare_ternary_plot_data()] keeps its own two sibling local
#' closures (`preview_title_layout()`, `calculate_plot_dimensions()`)
#' untouched in its own body - neither is used by filtering, and both are
#' still needed there (as `build_ternary_plot_title()`'s `title_layout_fn`
#' callback, and echoed into `pd` for `ternary_plot_save.R`'s own use).
#' Extracted from [prepare_ternary_plot_data()] as its own function per the
#' vidternary Structural Audit's Sec.04 responsibility table.
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
  # Optional Parameter 1/2 are a styling dimension (point size/type, or
  # color) rather than a composition axis - unlike Elements A/B/C, they
  # support exactly one column each; every UI control that offers them is
  # single-select. This function is exported and directly callable outside
  # the UI, though, so a length > 1 here needs an explicit, actionable
  # error message rather than being silently misapplied.
  if (!is.null(optional_param1) && length(optional_param1$col) > 1) {
    stop("Optional Param 1 (", paste(optional_param1$col, collapse = ", "),
         ") has more than one column selected, but it supports exactly one ",
         "(it is a styling dimension, not a summed composition axis like Elements A/B/C). ",
         "Choose a single column.")
  }
  if (!is.null(optional_param2) && length(optional_param2$col) > 1) {
    stop("Optional Param 2 (", paste(optional_param2$col, collapse = ", "),
         ") has more than one column selected, but it supports exactly one ",
         "(it is a styling dimension, not a summed composition axis like Elements A/B/C). ",
         "Choose a single column.")
  }

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
  # per-column filters). All three now agree on the same forgiving
  # behavior: stray non-numeric characters (e.g. a trailing unit) are
  # stripped from a malformed value before giving up.
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
  # not a shared one from helpers.R/helpers_filters.R.

  # Apply individual element filters
  M <- apply_individual_filters(M, element_A, individual_filters_A, "A", preview)
  M <- apply_individual_filters(M, element_B, individual_filters_B, "B", preview)
  M <- apply_individual_filters(M, element_C, individual_filters_C, "C", preview)

  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: After individual element filtering, data dimensions:", dim(M), "\n")
  }

  # Apply optional parameter 1 filtering, consolidated onto apply_filter()
  # like every other filter path in this function, so a malformed or
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

#' Apply IQR/Z-score/MAD statistical outlier filtering
#'
#' Dispatches to whichever of [apply_iqr_filter()]/[apply_zscore_filter()]/
#' [apply_mad_filter()] (`statistical_filters.R`) is active, applying each
#' in turn with this app's fixed multiplier/threshold constants (IQR:
#' `1.5`, Z-score/MAD: `3`). Only one method is meant to be active per plot
#' - enforced upstream in `general_ternary_plot()`, not here - but this
#' function has no mutual-exclusion guard of its own and will run all
#' three in sequence if somehow more than one flag is set. It only reads
#' `M` and the filter flags/parameters below, and its only real output is
#' `M`.
#'
#' Every active method is run over *all* of `selected_columns` and a row is
#' dropped (or kept, if `keep_outliers_*`) when it crosses the fence in
#' *any one* of them - so the effective per-row false-positive rate rises
#' with the number of selected columns. See the scientific caveats in
#' `statistical_filters.R`'s source header for that and for the
#' skew/small-sample limitations of the individual fences.
#'
#' @param M The data frame to filter (already loaded/individually filtered).
#' @param use_iqr_filter,use_zscore_filter,use_mad_filter Which method(s),
#'   if any, are active.
#' @param selected_columns Character vector of at least 2 numeric column
#'   names to use; mandatory whenever any method is active.
#' @param keep_outliers_iqr,keep_outliers_zscore,keep_outliers_mad If
#'   `TRUE` for the active method, keep only the flagged outliers instead
#'   of removing them.
#' @param stat_filter_log10 If `TRUE`, every active fence is fitted and
#'   applied on `log10(value)` rather than the raw value - the appropriate
#'   choice for the right-skewed inclusion measurements this app filters.
#'   Default `FALSE` (raw-scale fences, the original behaviour).
#' @return This function's entire local environment as a list
#'   (`as.list(environment())`) - `M` is the only field
#'   [prepare_ternary_plot_data()] actually reads back.
#' @export
apply_statistical_filtering <- function(M, use_iqr_filter, use_zscore_filter, use_mad_filter,
                                         selected_columns, keep_outliers_iqr, keep_outliers_zscore,
                                         keep_outliers_mad, stat_filter_log10 = FALSE) {
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
      M <- apply_iqr_filter(M, selected_columns, 1.5, keep_outliers_iqr, log_transform = stat_filter_log10)
    }

    if (use_zscore_filter) {
      if (getOption("ternary.debug", FALSE)) cat("DEBUG: Applying Z-score filter\n")
      M <- apply_zscore_filter(M, selected_columns, 3, keep_outliers_zscore, log_transform = stat_filter_log10)
    }

    if (use_mad_filter) {
      if (getOption("ternary.debug", FALSE)) cat("DEBUG: Applying MAD filter\n")
      M <- apply_mad_filter(M, selected_columns, 3, keep_outliers_mad, log_transform = stat_filter_log10)
    }

    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: After statistical filtering, data dimensions:", dim(M), "\n")
    }
  }

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
#' Its only real outputs are `M`, `mahal_result`, and `iso_result`.
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
#' @param isolation_ntrees,isolation_contamination Number of trees and
#'   contamination fraction for [compute_isolation_forest()], when
#'   `use_isolation_forest = TRUE`. User-adjustable in the UI.
#' @param isolation_sample_size Rows each isolation tree trains on - `NULL`
#'   for every complete reference row (default), or a whole number `>= 2`
#'   to sub-sample (see [compute_isolation_forest()]).
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
                                          lambda, omega, custom_mdthresh, mdthresh_mode,
                                          isolation_ntrees = 200, isolation_contamination = 0.10,
                                          isolation_sample_size = NULL) {
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
          iso_result <- compute_isolation_forest(M, actual_reference_data, keep_outliers = keep_outliers_isolation, selected_columns = selected_columns,
                                                  ntrees = isolation_ntrees, contamination = isolation_contamination,
                                                  sample_size = isolation_sample_size)
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

        # Apply the filtering.
        #
        # common_cols: compute_isolation_forest() returns its selected
        # columns as `columns_used` (see its own roxygen @return), not
        # `common_cols` - only compute_mahalanobis_distance() uses that
        # name.
        common_cols <- if (use_isolation_forest) iso_result$columns_used else mahal_result$common_cols

        # Row selection: Isolation Forest's keep_indices (from
        # compute_isolation_forest()'s outlier_indices, multivariate.R) is
        # already a full-length, original-row-order logical mask - built
        # via `scores1 <- rep(NA_real_, nrow(X1)); scores1[cc1] <-
        # scores1_c`, which maps every target row (including incomplete
        # ones, marked FALSE/not-outlier) back onto its own original
        # position - so `M[keep_indices, ]` is the correct, sufficient
        # reconstruction on its own. Mahalanobis's own keep_indices is a
        # DIFFERENT shape: mahal_distances is computed directly over
        # data1_clean (only the complete rows, in complete-row order), so
        # its keep_indices has to be re-expanded back to M's own original
        # row numbers via which(complete.cases(...)) first.
        if (use_isolation_forest) {
          original_indices <- which(keep_indices)
        } else {
          M_numeric <- as.matrix(M[, common_cols, drop = FALSE])
          original_indices <- which(complete.cases(M_numeric))[keep_indices]
        }

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
