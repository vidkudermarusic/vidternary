# ---- Helper Functions Module: Filter Collection & Application ----
# Split out of helpers.R: functions that collect filter values from Shiny
# `input`, and apply them to a data frame.

#' Collect per-element filter values from Shiny `input`
#'
#' For each name in `elements`, looks up the matching dynamically-named
#' filter text input (built from `prefix`/`filter_type`/`dataset_num`/the
#' element name) and includes it in the result if non-empty.
#'
#' @param elements Character vector of element/column names to look up filters for.
#' @param filter_type Filter-type label used in the input ID (e.g. `"A"`, `"op1"`).
#' @param input The Shiny `input` object.
#' @param prefix Input ID prefix, e.g. `"filter"` or `"multiple_filter"`. Default `"filter"`.
#' @param dataset_num Dataset number suffix (1 or 2), used only when `prefix == "filter"`.
#' @return A named list of non-empty filter values, keyed by element name.
#' @export
collect_filters <- function(elements, filter_type, input, prefix = "filter", dataset_num = NULL) {
  if (is.null(elements) || length(elements) == 0) return(list())

  filters <- list()
  for (element in elements) {
    # Create safe element name
    safe_element <- gsub("[^A-Za-z0-9]", "_", element)

    # Build input ID based on prefix and parameters
    if (prefix == "multiple_filter") {
      input_id <- paste0(prefix, "_", filter_type, "_", safe_element)
    } else {
      # For main ternary filters, match the UI element naming convention
      input_id <- paste0(prefix, "_", filter_type, dataset_num, "_", safe_element)
    }

    # Get filter value and validate
    filter_value <- input[[input_id]]
    if (!is.null(filter_value) && !is.na(filter_value) && nchar(trimws(as.character(filter_value))) > 0) {
      filters[[element]] <- filter_value
    }


  }
  return(filters)
}

#' `collect_filters()` for the Multiple Ternary Creator's element filters
#'
#' @param elements Character vector of element/column names.
#' @param element_type Element-type label used in the input ID (e.g. `"A"`).
#' @param input The Shiny `input` object.
#' @return A named list of non-empty filter values.
#' @export
collect_individual_filters <- function(elements, element_type, input) {
  collect_filters(elements, element_type, input, prefix = "multiple_filter")
}

#' `collect_filters()` for the main Ternary Plots tab's element filters
#'
#' @param elements Character vector of element/column names.
#' @param element_type Element-type label used in the input ID (e.g. `"A"`).
#' @param dataset_num Dataset number (1 or 2), used in the input ID.
#' @param input The Shiny `input` object.
#' @return A named list of non-empty filter values.
#' @export
collect_main_ternary_filters <- function(elements, element_type, dataset_num, input) {
  collect_filters(elements, element_type, input, prefix = "filter", dataset_num = dataset_num)
}

#' Apply a single comparison-operator filter string to a data frame column
#'
#' Parses a filter string like `"> 10"`, `"<= 5.2"`, or `"!= 3"` and returns
#' the matching rows, without using `eval()`.
#'
#' @param df A data frame.
#' @param col Name of the numeric column to filter on.
#' @param filter Filter string starting with one of `>`, `<`, `>=`, `<=`,
#'   `==`, `!=`, followed by a numeric value. `NULL` returns `df` unchanged.
#' @return The filtered data frame.
#' @export
apply_filter <- function(df, col, filter) {
  if (is.null(filter)) return(df)

  # Safe filtering using base R functions instead of dangerous eval()
  if (grepl("^[><=!]+", filter)) {
    # Handle comparison operators safely
    operator <- gsub("^([><=!]+).*", "\\1", filter)
    value_str <- gsub("^[><=!]+\\s*", "", filter)
    if (grepl(",", value_str, fixed = TRUE)) {
      stop("Invalid filter value: ", value_str, ". Use a dot as the decimal separator (e.g. \"> 1.5\"); commas are not allowed.")
    }
    value <- suppressWarnings(as.numeric(value_str))

    if (is.na(value)) {
      stop("Invalid filter value: ", value_str, ". Must be a numeric value.")
    }

    # which() drops rows whose value is NA: indexing a data frame with an NA
    # logical would insert a phantom all-NA row instead.
    x <- df[[col]]
    if (operator == ">") return(df[which(x > value), , drop = FALSE])
    if (operator == "<") return(df[which(x < value), , drop = FALSE])
    if (operator == ">=") return(df[which(x >= value), , drop = FALSE])
    if (operator == "<=") return(df[which(x <= value), , drop = FALSE])
    if (operator == "==") return(df[which(x == value), , drop = FALSE])
    if (operator == "!=") return(df[which(x != value), , drop = FALSE])
  }

  stop("Invalid filter format. Use operators: >, <, >=, <=, ==, !=")
}

# The real apply_individual_filters() implementation is a local function
# inside prepare_ternary_plot_data() (ternary_plot_data_prep.R) - see that
# function's own comment.

#' Build the full parameter list for `general_ternary_plot()` from Shiny inputs
#'
#' Reads elements A/B/C, optional parameters 1/2, all element/optional-param
#' filters, the active statistical/multivariate filter method and its
#' settings, output options, and group-selection state from `input`/`rv`,
#' assembling them into the single list `general_ternary_plot()` expects.
#' Used by both the main Ternary Plots tab (`multiple_mode = FALSE`) and
#' the Multiple Ternary Creator (`multiple_mode = TRUE`).
#'
#' @param input The Shiny `input` object.
#' @param rv The app's shared `reactiveValues` object.
#' @param dataset_num Dataset number (1 or 2); ignored when `multiple_mode = TRUE`.
#' @param preview Whether this is for a live preview (`TRUE`) or an actual
#'   save (`FALSE`). Default `FALSE`.
#' @param multiple_mode Whether to read the Multiple Ternary Creator's
#'   inputs instead of the main Ternary Plots tab's. Default `FALSE`.
#' @return A named list of parameters ready to pass to `general_ternary_plot()`,
#'   or `NULL` if element A/B/C aren't all set yet. `output_dir` defaults to
#'   `NULL` (no save) - every real save path (a `downloadHandler`'s own
#'   `content()` function) overrides it explicitly with a fresh temp
#'   directory before calling `general_ternary_plot()`, the same way the
#'   Multiple Ternary Creator's batch handler already does.
#' @export
extract_ternary_params <- function(input, rv, dataset_num, preview = FALSE, multiple_mode = FALSE) {
  # Essential parameters
  xlsx_file <- rv[[paste0("xlsx_file", dataset_num)]]

  # Required elements (A, B, C) - handle both main and multiple modes
  if (multiple_mode) {
    element_A <- list(col = input$multiple_element_A)
    element_B <- list(col = input$multiple_element_B)
    element_C <- list(col = input$multiple_element_C)
  } else {
    element_A <- list(col = input[[paste0("element_A", dataset_num)]])
    element_B <- list(col = input[[paste0("element_B", dataset_num)]])
    element_C <- list(col = input[[paste0("element_C", dataset_num)]])
  }

  # Validate required elements
  if (is.null(element_A$col) || length(element_A$col) == 0 ||
      is.null(element_B$col) || length(element_B$col) == 0 ||
      is.null(element_C$col) || length(element_C$col) == 0) {
    return(NULL)  # Return NULL if required elements are missing
  }

  # Optional parameters
  optional_param1 <- NULL
  optional_param1_representation <- "point_size"
  if (multiple_mode) {
    # multiple_optional_param1 comes from a selectizeInput(multiple = TRUE)
    # (ui_multiple_ternary_tab.R) and so can be a length-&gt;1 character
    # vector when 2+ columns are picked - a plain `!= ""` then returns a
    # vector, and && on that is a hard error on R &gt;= 4.3.0. !any(... == "")
    # is the vector-safe equivalent of "none of the selected values is an
    # empty string".
    if (!is.null(input$multiple_optional_param1) &&
        length(input$multiple_optional_param1) > 0 &&
        !any(input$multiple_optional_param1 == "")) {
      # Get optional parameter 1 filters for multiple mode
      optional_param1_filters <- collect_filters(input$multiple_optional_param1, "op1", input, prefix = "multiple_filter")
      # Extract the first filter value from the list
      optional_param1_filter <- if (length(optional_param1_filters) > 0) {
        first_filter_name <- names(optional_param1_filters)[1]
        optional_param1_filters[[first_filter_name]]
      } else NULL

      optional_param1 <- list(
        col = input$multiple_optional_param1,
        filter = optional_param1_filter
      )

      # Debug output for optional parameter 1 filters
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Optional param1 filters collected:", if (is.null(optional_param1_filter)) "NULL" else optional_param1_filter, "\n")
        cat("DEBUG: Optional param1 filters list length:", length(optional_param1_filters), "\n")
        cat("DEBUG: Optional param1 filters list names:", paste(names(optional_param1_filters), collapse = ", "), "\n")
      }
      if (!is.null(input$multiple_optional_param1_representation)) {
        optional_param1_representation <- input$multiple_optional_param1_representation
      }
    }
  } else {
    # Enhanced debugging for main ternary plots
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Checking optional_param1 for dataset", dataset_num, "\n")
      cat("DEBUG: input$optional_param1_", dataset_num, " = ",
          if (is.null(input[[paste0("optional_param1_", dataset_num)]])) "NULL" else paste(input[[paste0("optional_param1_", dataset_num)]], collapse = ", "), "\n")
      cat("DEBUG: input$filter_op1_", dataset_num, " = ",
          if (is.null(input[[paste0("filter_op1_", dataset_num)]])) "NULL" else input[[paste0("filter_op1_", dataset_num)]], "\n")
    }

    if (!is.null(input[[paste0("optional_param1_", dataset_num)]]) &&
        input[[paste0("optional_param1_", dataset_num)]] != "" &&
        length(input[[paste0("optional_param1_", dataset_num)]]) > 0) {
      optional_param1 <- list(
        col = input[[paste0("optional_param1_", dataset_num)]],
        filter = if (!is.null(input[[paste0("filter_op1_", dataset_num)]]) && nzchar(input[[paste0("filter_op1_", dataset_num)]])) input[[paste0("filter_op1_", dataset_num)]] else NULL
      )
      if (!is.null(input[[paste0("optional_param1_representation", dataset_num)]])) {
        optional_param1_representation <- input[[paste0("optional_param1_representation", dataset_num)]]
      }

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Optional param1 created:", paste(optional_param1$col, collapse = ", "), "\n")
        cat("DEBUG: Optional param1 filter:", if (is.null(optional_param1$filter)) "NULL" else optional_param1$filter, "\n")
        cat("DEBUG: Optional param1 representation:", optional_param1_representation, "\n")
      }
    }
  }

  optional_param2 <- NULL
  color_palette <- "blue"
  if (multiple_mode) {
    # Same vector-safety fix as multiple_optional_param1 above - see that
    # comment for why the check is written this way.
    if (!is.null(input$multiple_optional_param2) &&
        length(input$multiple_optional_param2) > 0 &&
        !any(input$multiple_optional_param2 == "")) {
      # Get optional parameter 2 filters for multiple mode
      optional_param2_filters <- collect_filters(input$multiple_optional_param2, "op2", input, prefix = "multiple_filter")
      # Extract the first filter value from the list
      optional_param2_filter <- if (length(optional_param2_filters) > 0) {
        first_filter_name <- names(optional_param2_filters)[1]
        optional_param2_filters[[first_filter_name]]
      } else NULL

      optional_param2 <- list(
        col = input$multiple_optional_param2,
        filter = optional_param2_filter
      )

      # Debug output for optional parameter 2 filters
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Optional param2 filters collected:", if (is.null(optional_param2_filter)) "NULL" else optional_param2_filter, "\n")
        cat("DEBUG: Optional param2 filters list length:", length(optional_param2_filters), "\n")
        cat("DEBUG: Optional param2 filters list names:", paste(names(optional_param2_filters), collapse = ", "), "\n")
      }
      if (!is.null(input$multiple_color_palette)) {
        color_palette <- input$multiple_color_palette
      }
    }
  } else {
    # Enhanced debugging for optional_param2
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Checking optional_param2 for dataset", dataset_num, "\n")
      cat("DEBUG: input$optional_param2_", dataset_num, " = ",
          if (is.null(input[[paste0("optional_param2_", dataset_num)]])) "NULL" else paste(input[[paste0("optional_param2_", dataset_num)]], collapse = ", "), "\n")
      cat("DEBUG: input$filter_op2_", dataset_num, " = ",
          if (is.null(input[[paste0("filter_op2_", dataset_num)]])) "NULL" else input[[paste0("filter_op2_", dataset_num)]], "\n")
      cat("DEBUG: input$color_palette", dataset_num, " = ",
          if (is.null(input[[paste0("color_palette", dataset_num)]])) "NULL" else input[[paste0("color_palette", dataset_num)]], "\n")
    }

    if (!is.null(input[[paste0("optional_param2_", dataset_num)]]) &&
        input[[paste0("optional_param2_", dataset_num)]] != "" &&
        length(input[[paste0("optional_param2_", dataset_num)]]) > 0) {
      optional_param2 <- list(
        col = input[[paste0("optional_param2_", dataset_num)]],
        filter = if (!is.null(input[[paste0("filter_op2_", dataset_num)]]) && nzchar(input[[paste0("filter_op2_", dataset_num)]])) input[[paste0("filter_op2_", dataset_num)]] else NULL
      )
      if (!is.null(input[[paste0("color_palette", dataset_num)]])) {
        color_palette <- input[[paste0("color_palette", dataset_num)]]
      }

      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Optional param2 created:", paste(optional_param2$col, collapse = ", "), "\n")
        cat("DEBUG: Optional param2 filter:", if (is.null(optional_param2$filter)) "NULL" else optional_param2$filter, "\n")
        cat("DEBUG: Color palette:", color_palette, "\n")
      }
    }
  }

  # Individual filters (A, B, C)
  if (multiple_mode) {
    individual_filters_A <- collect_individual_filters(element_A$col, "A", input)
    individual_filters_B <- collect_individual_filters(element_B$col, "B", input)
    individual_filters_C <- collect_individual_filters(element_C$col, "C", input)
  } else {
    individual_filters_A <- collect_main_ternary_filters(element_A$col, "A", dataset_num, input)
    individual_filters_B <- collect_main_ternary_filters(element_B$col, "B", dataset_num, input)
    individual_filters_C <- collect_main_ternary_filters(element_C$col, "C", dataset_num, input)
  }



  # Global parameters (same for all datasets)
  use_mahalanobis <- if (!is.null(input$use_mahalanobis)) input$use_mahalanobis else FALSE
  use_isolation_forest <- if (!is.null(input$use_isolation_forest)) input$use_isolation_forest else FALSE

  use_iqr_filter <- if (!is.null(input$use_iqr_filter)) input$use_iqr_filter else FALSE
  use_zscore_filter <- if (!is.null(input$use_zscore_filter)) input$use_zscore_filter else FALSE
  use_mad_filter <- if (!is.null(input$use_mad_filter)) input$use_mad_filter else FALSE
  # When set, the active IQR/Z-score/MAD fence is fitted on log10(value) -
  # better for the right-skewed inclusion measurements this app filters.
  stat_filter_log10 <- isTRUE(input$stat_filter_log10)

  lambda <- if (!is.null(input$lambda)) input$lambda else 1
  omega <- if (!is.null(input$omega)) input$omega else 0

  # Isolation Forest parameters - user-adjustable in the UI (previously
  # fixed internal defaults with no way to see or change what was used).
  # NA-safety matches every other numericInput read here (input$lambda/
  # omega above included): a cleared field reports NA_real_, not NULL, so
  # !is.null() alone isn't enough - is.na() must be checked too.
  isolation_ntrees <- if (!is.null(input$isolation_ntrees) && !is.na(input$isolation_ntrees)) input$isolation_ntrees else 200
  isolation_contamination <- if (!is.null(input$isolation_contamination) && !is.na(input$isolation_contamination)) input$isolation_contamination else 0.10
  # sample_size: the "Use all reference rows" checkbox (default checked, and
  # the pre-checkbox behaviour) maps to NULL; unchecking it activates the
  # numeric sub-sample size. A cleared numeric field (NA_real_) also falls
  # back to NULL rather than erroring downstream.
  isolation_sample_size <- if (isTRUE(input$isolation_use_all_rows) || is.null(input$isolation_use_all_rows)) {
    NULL
  } else if (!is.null(input$isolation_sample_size) && !is.na(input$isolation_sample_size)) {
    input$isolation_sample_size
  } else {
    NULL
  }
  isolation_seed <- if (!is.null(input$isolation_seed) && !is.na(input$isolation_seed)) input$isolation_seed else 42

  # Reference data handling
  reference_data <- NULL
  mahalanobis_reference <- "self"
  if (use_mahalanobis || use_isolation_forest) {
    mahalanobis_reference <- if (!is.null(input$mahalanobis_reference)) input$mahalanobis_reference else "self"

    if (mahalanobis_reference == "dataset1" && !is.null(rv$df1)) {
      reference_data <- rv$df1
    } else if (mahalanobis_reference == "dataset2" && !is.null(rv$df2)) {
      reference_data <- rv$df2
    }
  }

  # Other parameters
  custom_mdthresh <- if (!is.null(input$custom_mdthresh)) input$custom_mdthresh else NULL
  mdthresh_mode <- if (!is.null(input$mdthresh_mode)) input$mdthresh_mode else "auto"
  selected_columns <- if (!is.null(input$multivariate_columns)) input$multivariate_columns else NULL

  # File format - the Multiple Ternary Creator tab has its own dropdown
  # (multiple_output_format), separate from the main tab's (output_format).
  output_format <- if (multiple_mode) {
    if (!is.null(input$multiple_output_format)) input$multiple_output_format else "png"
  } else {
    if (!is.null(input$output_format)) input$output_format else "png"
  }

  # Manual point size control
  use_manual_point_size <- if (!is.null(input$use_manual_point_size)) input$use_manual_point_size else FALSE
  manual_point_size <- if (!is.null(input$manual_point_size)) input$manual_point_size else 1.0

  # Group selection for categorical data
  selected_groups <- if (!is.null(input[[paste0("selected_groups_", dataset_num)]])) input[[paste0("selected_groups_", dataset_num)]] else NULL
  is_categorical_group <- if (!is.null(rv[[paste0("is_categorical_group_", dataset_num)]])) rv[[paste0("is_categorical_group_", dataset_num)]] else FALSE

  # Debug output for group selection
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: selected_groups:", if (is.null(selected_groups)) "NULL" else paste(selected_groups, collapse = ", "), "\n")
    cat("DEBUG: is_categorical_group:", is_categorical_group, "\n")
    cat("DEBUG: selected_groups length:", if (is.null(selected_groups)) 0 else length(selected_groups), "\n")
  }

  # Additional safety check for categorical detection
  #
  # length(optional_param2$col) == 1 is required here, not optional: in
  # multiple_mode, optional_param2$col is input$multiple_optional_param2, a
  # selectizeInput(multiple = TRUE) (ui_multiple_ternary_tab.R), so it can
  # be a length > 1 character vector whenever 2+ columns are picked.
  # Without this guard, a multi-column selection would crash two ways
  # below: `optional_param2$col %in% names(data)` returns a same-length
  # vector, and `&&` on a vector is a hard error on R >= 4.3.0; even past
  # that, `data[[optional_param2$col]]` requires a length-1 index and
  # errors on a vector regardless of R version. A multi-column Optional
  # Parameter 2 was never meant to drive categorical detection anyway
  # (ternary_plot_preview.R/ternary_plot_save.R's own "Aspect.Ratio"
  # numeric-legend check uses the identical length-1 guard for the same
  # reason), so skipping this safety check entirely for a multi-column
  # selection is intentional, not just crash-avoidant.
  if (!is_categorical_group && !is.null(optional_param2) &&
      length(optional_param2$col) == 1 && !is.null(xlsx_file)) {
    # Check if the data is actually categorical even if not detected as such
    data <- rv[[paste0("df", dataset_num)]]
    if (!is.null(data) && optional_param2$col %in% names(data)) {
      column_data <- data[[optional_param2$col]]
      # !is.numeric() alone already covers character AND factor (neither
      # is ever is.numeric() in R) - matches the identical fix in
      # server_ternary_plots_groups.R's two detection observers (see that
      # file's own comment): applies the same 50-unique-values cap
      # uniformly to every non-numeric type instead of granting character/
      # factor columns unconditional, uncapped categorical status.
      is_categorical_group <- !is.numeric(column_data) && length(unique(column_data)) <= 50
    }
  }

  keep_outliers_mahalanobis <- if (!is.null(input$outlier_mode_mahalanobis)) input$outlier_mode_mahalanobis else FALSE
  keep_outliers_isolation <- if (!is.null(input$outlier_mode_isolation)) input$outlier_mode_isolation else FALSE
  keep_outliers_iqr <- if (!is.null(input$outlier_mode_iqr)) input$outlier_mode_iqr else FALSE
  keep_outliers_zscore <- if (!is.null(input$outlier_mode_zscore)) input$outlier_mode_zscore else FALSE
  keep_outliers_mad <- if (!is.null(input$outlier_mode_mad)) input$outlier_mode_mad else FALSE

  # Return parameters list
  list(
    xlsx_file = xlsx_file,
    # Not a real UI input anywhere - every real caller (server_ternary_plots.R,
    # server_ternary_plots_batch.R) overwrites this with the actual uploaded
    # file's name (input$xlsx_file1$name etc.) right after calling this
    # function, so this always started out NULL in practice anyway.
    xlsx_display_name = NULL,
    # working_dir just needs *a* real directory to setwd() into/back from
    # for relative-path resolution - getwd() always works and nothing
    # meaningful depends on it being user-chosen. output_dir stays NULL
    # (no save) until a real save path overrides it with a fresh temp
    # directory right before calling general_ternary_plot() - see this
    # function's own @return doc.
    working_dir = getwd(),
    output_dir = NULL,
    element_A = element_A,
    element_B = element_B,
    element_C = element_C,
    optional_param1 = optional_param1,
    optional_param2 = optional_param2,
    color_palette = color_palette,
    optional_param1_representation = optional_param1_representation,
    preview = preview,
    use_mahalanobis = use_mahalanobis,
    use_isolation_forest = use_isolation_forest,
    use_iqr_filter = use_iqr_filter,
    use_zscore_filter = use_zscore_filter,
    use_mad_filter = use_mad_filter,
    stat_filter_log10 = stat_filter_log10,
    lambda = lambda,
    omega = omega,
    isolation_ntrees = isolation_ntrees,
    isolation_contamination = isolation_contamination,
    isolation_sample_size = isolation_sample_size,
    isolation_seed = isolation_seed,
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
    reference_data = reference_data,
    include_plot_notes = if (!is.null(input$include_plot_notes)) input$include_plot_notes else TRUE,
    output_format = output_format,
    use_manual_point_size = use_manual_point_size,
    manual_point_size = manual_point_size,
    selected_groups = selected_groups,
    is_categorical_group = is_categorical_group
  )
}
