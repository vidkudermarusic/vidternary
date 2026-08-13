# ---- Helper Functions Module: Filter Collection & Application ----
# Split out of helpers.R: functions that collect filter values from Shiny
# `input`, and apply them to a data frame.

# Unified filter collection function
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

# Simplified wrapper functions for backward compatibility
collect_individual_filters <- function(elements, element_type, input) {
  collect_filters(elements, element_type, input, prefix = "multiple_filter")
}

collect_optional_param_filters <- function(elements, param_type, input) {
  collect_filters(elements, param_type, input, prefix = "multiple_filter")
}

collect_main_ternary_filters <- function(elements, element_type, dataset_num, input) {
  collect_filters(elements, element_type, input, prefix = "filter", dataset_num = dataset_num)
}

# Function to collect all filters for multiple ternary creator
collect_all_multiple_filters <- function(input) {
  list(
    individual_filters_A = collect_filters(input$multiple_element_A, "A", input, prefix = "multiple_filter"),
    individual_filters_B = collect_filters(input$multiple_element_B, "B", input, prefix = "multiple_filter"),
    individual_filters_C = collect_filters(input$multiple_element_C, "C", input, prefix = "multiple_filter"),
    optional_param1_filters = collect_filters(input$multiple_optional_param1, "op1", input, prefix = "multiple_filter"),
    optional_param2_filters = collect_filters(input$multiple_optional_param2, "op2", input, prefix = "multiple_filter")
  )
}

# Function to apply filter safely
apply_filter <- function(df, col, filter) {
  if (is.null(filter)) return(df)

  # Safe filtering using base R functions instead of dangerous eval()
  if (grepl("^[><=!]+", filter)) {
    # Handle comparison operators safely
    operator <- gsub("^([><=!]+).*", "\\1", filter)
    value_str <- gsub("^[><=!]+\\s*", "", filter)
    value <- as.numeric(value_str)

    if (is.na(value)) {
      stop("Invalid filter value: ", value_str, ". Must be a numeric value.")
    }

    if (operator == ">") return(df[df[[col]] > value, , drop = FALSE])
    if (operator == "<") return(df[df[[col]] < value, , drop = FALSE])
    if (operator == ">=") return(df[df[[col]] >= value, , drop = FALSE])
    if (operator == "<=") return(df[df[[col]] <= value, , drop = FALSE])
    if (operator == "==") return(df[df[[col]] == value, , drop = FALSE])
    if (operator == "!=") return(df[df[[col]] != value, , drop = FALSE])
  }

  stop("Invalid filter format. Use operators: >, <, >=, <=, ==, !=")
}

# Function to get individual filters for elements
get_individual_filters <- function(elements, dataset_suffix) {
  if (is.null(elements) || length(elements) == 0) {
    return(list())
  }

  # For global scope usage, return empty list (will be populated by UI)
  # This function is primarily used in the Shiny server context where 'input' is available
  filters <- list()
  for (element in elements) {
    filters[[element]] <- NULL  # Will be filled by UI
  }
  return(filters)
}

# Apply individual filters
apply_individual_filters <- function(data, element, individual_filters, element_name, preview = FALSE) {
  if (is.null(individual_filters) || length(individual_filters) == 0) {
    return(data)
  }

  filtered_data <- data

  for (filter_name in names(individual_filters)) {
    filter_value <- individual_filters[[filter_name]]
    if (!is.null(filter_value) && length(filter_value) > 0) {
      filtered_data <- apply_filter(filtered_data, filter_name, filter_value)
    }
  }

  if (preview) {
    message(paste("Applied filters for", element_name, ":", nrow(filtered_data), "rows remaining"))
  }

  return(filtered_data)
}

# Unified parameter extraction for ternary plots
extract_ternary_params <- function(input, rv, dataset_num, preview = FALSE, directory_management = NULL, multiple_mode = FALSE) {
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
    if (!is.null(input$multiple_optional_param1) &&
        input$multiple_optional_param1 != "" &&
        length(input$multiple_optional_param1) > 0) {
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
    if (!is.null(input$multiple_optional_param2) &&
        input$multiple_optional_param2 != "" &&
        length(input$multiple_optional_param2) > 0) {
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

  lambda <- if (!is.null(input$lambda)) input$lambda else 1
  omega <- if (!is.null(input$omega)) input$omega else 0

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

  # File format
  output_format <- if (!is.null(input$output_format)) input$output_format else "png"

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
  if (!is_categorical_group && !is.null(optional_param2) && !is.null(xlsx_file)) {
    # Check if the data is actually categorical even if not detected as such
    data <- rv[[paste0("df", dataset_num)]]
    if (!is.null(data) && optional_param2$col %in% names(data)) {
      column_data <- data[[optional_param2$col]]
      is_categorical_group <- is.character(column_data) || is.factor(column_data) ||
                             (!is.numeric(column_data) && length(unique(column_data)) <= 50)
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
    xlsx_display_name = if (!is.null(input$xlsx_display_name)) input$xlsx_display_name else NULL,
    working_dir = getwd(),
    output_dir = if (!is.null(directory_management)) directory_management$output_dir() else file.path(getwd(), "output"),
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
    reference_data = reference_data,
    include_plot_notes = if (!is.null(input$include_plot_notes)) input$include_plot_notes else TRUE,
    output_format = output_format,
    use_manual_point_size = use_manual_point_size,
    manual_point_size = manual_point_size,
    selected_groups = selected_groups,
    is_categorical_group = is_categorical_group
  )
}
