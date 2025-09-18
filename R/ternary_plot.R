# =============================================================================
# vidternary: Main Ternary Plot Function Module
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Core ternary plot generation functionality with advanced filtering
#              and multivariate analysis capabilities. This module contains the
#              main general_ternary_plot() function and supporting utilities.
# 
# Key Functions:
#   - general_ternary_plot(): Main ternary plot generation function
#   - calculate_plot_dimensions(): Dynamic plot sizing based on content
#   - split_long_text(): Text formatting for plot notes
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - openxlsx, Ternary, PlotTools, viridisLite
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

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
    use_robust_mahalanobis = FALSE,
    use_isolation_forest = FALSE,
    use_iqr_filter = FALSE,
    use_zscore_filter = FALSE,
    use_mad_filter = FALSE,
    lambda = 1,
    omega = 0,
    keep_outliers_mahalanobis = FALSE,
    keep_outliers_robust = FALSE,
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
    cat("DEBUG: use_robust_mahalanobis =", use_robust_mahalanobis, "\n")
    cat("DEBUG: use_isolation_forest =", use_isolation_forest, "\n")
    cat("DEBUG: use_iqr_filter =", use_iqr_filter, "\n")
    cat("DEBUG: use_zscore_filter =", use_zscore_filter, "\n")
    cat("DEBUG: use_mad_filter =", use_mad_filter, "\n")
    cat("DEBUG: selected_columns =", if(is.null(selected_columns)) "NULL" else paste(selected_columns, collapse=", "), "\n")

    cat("=== COMPREHENSIVE DEBUGGING END ===\n\n")
  }
  
  # Check if we have xlsx_file and load data (matching legacy code pattern)
  if (is.null(xlsx_file) || !file.exists(xlsx_file)) {
    stop("Invalid xlsx_file: file does not exist or is NULL")
  }
  
  # Store original working directory and restore on exit
  # This prevents setwd() side-effects from affecting Shiny's working directory
  original_wd <- getwd()
  on.exit(setwd(original_wd), add = TRUE)
  setwd(working_dir)
  
  log_operation("Data loading", paste("Loading data from:", xlsx_file))
  M <- openxlsx::read.xlsx(xlsx_file, sheet = 1)
  
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: Initial data loaded, dimensions:", dim(M), "\n")
    cat("DEBUG: Available columns:", paste(colnames(M), collapse=", "), "\n")
  }
  if (!preview) print(colnames(M))
  
  log_operation("Data dimensions", paste("Initial data dimensions:", dim(M)[1], "rows x", dim(M)[2], "columns"))
  
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
  
  log_operation("Input validation", "All inputs validated successfully")
  
  # ---- CRITICAL HELPER FUNCTIONS ----
  
  # Safe filtering function - prevents security issues by avoiding eval()
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
  
  # Function to calculate optimal plot dimensions based on title length
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
  
  # Function to split long text into multiple lines (max 45 characters per line)
  split_long_text <- function(text, max_chars = 45) {
    if (is.null(text) || nchar(text) == 0) return(text)
    
    # Split by newlines first to handle existing line breaks
    lines <- strsplit(text, "\n")[[1]]
    result_lines <- c()
    
    for (line in lines) {
      if (nchar(line) <= max_chars) {
        # Line is short enough, keep as is
        result_lines <- c(result_lines, line)
      } else {
        # Line is too long, split it
        words <- strsplit(line, " ")[[1]]
        current_line <- ""
        
        for (word in words) {
          # Check if adding this word would exceed the limit
          if (nchar(current_line) + nchar(word) + 1 <= max_chars) {
            # Add word to current line
            if (current_line == "") {
              current_line <- word
            } else {
              current_line <- paste(current_line, word)
            }
          } else {
            # Current line is full, start a new line
            if (current_line != "") {
              result_lines <- c(result_lines, current_line)
            }
            current_line <- word
          }
        }
        
        # Add the last line if it's not empty
        if (current_line != "") {
          result_lines <- c(result_lines, current_line)
        }
      }
    }
    
    return(paste(result_lines, collapse = "\n"))
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
            # Safe filtering instead of eval()
            if (grepl("^[><=!]+", element$filter)) {
              operator <- gsub("^([><=!]+).*", "\\1", element$filter)
              value_str <- gsub("^([><=!]+)\\s*", "", element$filter)
              value <- as.numeric(value_str)
              
              if (is.na(value)) {
                stop("Invalid filter value: ", value_str, ". Must be a numeric value.")
              }
              
              if (operator == ">") col_filter <- data[[col]] > value
              else if (operator == "<") col_filter <- data[[col]] < value
              else if (operator == ">=") col_filter <- data[[col]] >= value
              else if (operator == "<=") col_filter <- data[[col]] <= value
              else if (operator == "==") col_filter <- data[[col]] == value
              else if (operator == "!=") col_filter <- data[[col]] != value
              else stop("Invalid filter format. Use operators: >, <, >=, <=, ==, !=")
              
              keep_rows <- keep_rows & col_filter
            } else {
              stop("Invalid filter format. Use operators: >, <, >=, <=, ==, !=")
            }
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
          # Safe filtering instead of eval()
          filter_expr <- individual_filters[[col]]
          if (grepl("^[><=!]+", filter_expr)) {
            operator <- gsub("^([><=!]+).*", "\\1", filter_expr)
            value_str <- gsub("^([><=!]+)\\s*", "", filter_expr)
            value <- as.numeric(value_str)
            
            if (is.na(value)) {
              # Try to clean the value string by removing invalid characters
              clean_value_str <- gsub("[^0-9.-]", "", value_str)
              value <- as.numeric(clean_value_str)
              if (is.na(value)) {
                stop("Invalid filter value: ", value_str, ". Must be a numeric value.")
              }
            }
            
            if (operator == ">") col_filter <- data[[col]] > value
            else if (operator == "<") col_filter <- data[[col]] < value
            else if (operator == ">=") col_filter <- data[[col]] >= value
            else if (operator == "<=") col_filter <- data[[col]] <= value
            else if (operator == "==") col_filter <- data[[col]] == value
            else if (operator == "!=") col_filter <- data[[col]] != value
            else stop("Invalid filter format. Use operators: >, <, >=, <=, ==, !=")
            
            keep_rows <- keep_rows & col_filter
          } else {
            stop("Invalid filter format. Use operators: >, <, >=, <=, ==, !=")
          }
          
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
  
  # Apply individual element filtering (A, B, C) with individual filters
  # Note: apply_individual_filters function is defined in helpers.R to avoid duplication
  
  # Apply individual element filters
  M <- apply_individual_filters(M, element_A, individual_filters_A, "A", preview)
  M <- apply_individual_filters(M, element_B, individual_filters_B, "B", preview)
  M <- apply_individual_filters(M, element_C, individual_filters_C, "C", preview)
  
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: After individual element filtering, data dimensions:", dim(M), "\n")
  }
  
  # Apply optional parameter 1 filtering
  if (!is.null(optional_param1) && !is.null(optional_param1$filter) && nzchar(optional_param1$filter)) {
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Applying optional parameter 1 filter:", optional_param1$filter, "\n")
      cat("DEBUG: Filtering columns:", paste(optional_param1$col, collapse=", "), "\n")
    }
    
    if (grepl("^[><=!]+", optional_param1$filter)) {
      operator <- gsub("^([><=!]+).*", "\\1", optional_param1$filter)
      value_str <- gsub("^[><=!]+\\s*", "", optional_param1$filter)
      value <- as.numeric(value_str)
      
      if (is.na(value)) {
        stop("Invalid optional_param1 filter value: ", value_str, ". Must be a numeric value.")
      }
      
      for (col in optional_param1$col) {
        if (getOption("ternary.debug", FALSE)) {
          cat("DEBUG: Applying filter", optional_param1$filter, "to column", col, "\n")
          cat("DEBUG: Column values range:", range(M[[col]], na.rm=TRUE), "\n")
        }
        
        if (operator == ">") M <- M[M[[col]] > value, , drop = FALSE]
        if (operator == "<") M <- M[M[[col]] < value, , drop = FALSE]
        if (operator == ">=") M <- M[M[[col]] >= value, , drop = FALSE]
        if (operator == "<=") M <- M[M[[col]] <= value, , drop = FALSE]
        if (operator == "==") M <- M[M[[col]] == value, , drop = FALSE]
        if (operator == "!=") M <- M[M[[col]] != value, , drop = FALSE]
        
        if (getOption("ternary.debug", FALSE)) {
          cat("DEBUG: After filtering column", col, ", data dimensions:", dim(M), "\n")
        }
      }
    }
  } else {
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: No optional parameter 1 filter applied\n")
    }
  }
  
  # Apply optional parameter 2 filtering
  if (!is.null(optional_param2) && !is.null(optional_param2$filter) && nzchar(optional_param2$filter)) {
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Applying optional parameter 2 filter:", optional_param2$filter, "\n")
      cat("DEBUG: Filtering columns:", paste(optional_param2$col, collapse=", "), "\n")
    }
    
    if (grepl("^[><=!]+", optional_param2$filter)) {
      operator <- gsub("^([><=!]+).*", "\\1", optional_param2$filter)
      value_str <- gsub("^[><=!]+\\s*", "", optional_param2$filter)
      value <- as.numeric(value_str)
      
      if (is.na(value)) {
        stop("Invalid optional_param2 filter value: ", value_str, ". Must be a numeric value.")
      }
      
      for (col in optional_param2$col) {
        if (getOption("ternary.debug", FALSE)) {
          cat("DEBUG: Applying filter", optional_param2$filter, "to column", col, "\n")
          cat("DEBUG: Column values range:", range(M[[col]], na.rm=TRUE), "\n")
        }
        
        if (operator == ">") M <- M[M[[col]] > value, , drop = FALSE]
        if (operator == "<") M <- M[M[[col]] < value, , drop = FALSE]
        if (operator == ">=") M <- M[M[[col]] >= value, , drop = FALSE]
        if (operator == "<=") M <- M[M[[col]] <= value, , drop = FALSE]
        if (operator == "==") M <- M[M[[col]] == value, , drop = FALSE]
        if (operator == "!=") M <- M[M[[col]] != value, , drop = FALSE]
        
        if (getOption("ternary.debug", FALSE)) {
          cat("DEBUG: After filtering column", col, ", data dimensions:", dim(M), "\n")
        }
      }
    }
  } else {
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: No optional parameter 2 filter applied\n")
    }
  }
  
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
  
  # Apply multivariate analysis filtering if requested
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: Multivariate analysis check:\n")
    cat("DEBUG: use_mahalanobis =", use_mahalanobis, "\n")
    cat("DEBUG: use_robust_mahalanobis =", use_robust_mahalanobis, "\n")
    cat("DEBUG: use_isolation_forest =", use_isolation_forest, "\n")

    cat("DEBUG: reference_data is.null =", is.null(reference_data), "\n")
    if (!is.null(reference_data)) {
      cat("DEBUG: reference_data dimensions:", dim(reference_data), "\n")
    }
    cat("DEBUG: Will enter multivariate section =", use_mahalanobis || use_robust_mahalanobis || use_isolation_forest, "\n")
    cat("DEBUG: Any multivariate method enabled =", use_mahalanobis || use_robust_mahalanobis || use_isolation_forest, "\n")
  }
  
  if (use_mahalanobis || use_robust_mahalanobis || use_isolation_forest) {
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
        if (use_robust_mahalanobis) {
          mahal_result <- compute_robust_mahalanobis(M, actual_reference_data, keep_outliers = keep_outliers_robust, selected_columns = selected_columns)
          threshold_to_use <- mahal_result$threshold_95
          keep_indices <- if (keep_outliers_robust) {
            mahal_result$outlier_indices
          } else {
            which(mahal_result$distances <= threshold_to_use)
          }
        } else if (use_isolation_forest) {
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
        common_cols <- if (use_robust_mahalanobis) mahal_result$common_cols else 
          if (use_isolation_forest) iso_result$common_cols else mahal_result$common_cols
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
          method_name <- if (use_robust_mahalanobis) "Robust Mahalanobis" else
            if (use_isolation_forest) "Isolation Forest" else "Mahalanobis"
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
  
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: After multivariate filtering, data dimensions:", dim(M), "\n")
    if (use_mahalanobis && !is.null(reference_data)) {
      cat("DEBUG: Multivariate analysis was applied. Original data should be filtered.\n")
      cat("DEBUG: Sample of filtered data (first 5 rows):\n")
      print(head(M, 5))
    }
  }
  
  # Create output directory structure using file management module
  # For multiple ternary plots, use the output_dir directly instead of creating subfolders

  
  if (!is.null(output_dir) && !preview) {
    # Use the output_dir directly for multiple ternary plots
    custom_folder <- output_dir
    file_base <- if (!is.null(xlsx_display_name)) tools::file_path_sans_ext(xlsx_display_name) else if (!is.null(xlsx_file)) tools::file_path_sans_ext(basename(xlsx_file)) else "ternary_plot"
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
  

  
  needed_columns <- unique(c(all_selected_elements, 
                             if (!is.null(optional_param1)) optional_param1$col,
                             if (!is.null(optional_param2)) optional_param2$col))
  
  log_operation("Column validation", paste("Checking required columns:", paste(needed_columns, collapse = ", ")))
  
  if (!all(needed_columns %in% colnames(M))) {
    missing_cols <- setdiff(needed_columns, colnames(M))
    log_operation("Column validation", paste("ERROR: Missing columns:", paste(missing_cols, collapse = ", ")))
    stop("Error: One or more selected elements/parameters are missing in the dataset.\nAvailable columns: ", 
         paste(colnames(M), collapse = ", "))
  }
  
  log_operation("Column validation", "All required columns found")
  
  matrika <- M[, needed_columns]
  cat("DEBUG: Matrika dimensions after column selection:", dim(matrika), "\n")
  if (getOption("ternary.debug", FALSE) && use_mahalanobis && !is.null(reference_data)) {
    cat("DEBUG: Matrika created from filtered M. Sample data (first 5 rows):\n")
    print(head(matrika, 5))
  }
  log_operation("Data processing", paste("Selected columns. Matrix dimensions:", dim(matrika)[1], "rows x", dim(matrika)[2], "columns"))
  
  matrika[, all_selected_elements] <- lapply(matrika[, all_selected_elements, drop = FALSE], as.numeric)
  row_sums <- rowSums(matrika[, all_selected_elements, drop = FALSE], na.rm = TRUE)
  matrika <- matrika[row_sums > 0, , drop = FALSE]
  cat("DEBUG: Matrika dimensions after removing zero-sum rows:", dim(matrika), "\n")
  matrika <- na.omit(matrika)
  cat("DEBUG: Matrika dimensions after na.omit:", dim(matrika), "\n")
  log_operation("Data processing", paste("After removing NA values. Matrix dimensions:", dim(matrika)[1], "rows x", dim(matrika)[2], "columns"))
  
  # Only convert element columns to numeric, preserve optional parameter columns as character/factor
  element_columns <- all_selected_elements
  optional_columns <- c()
  if (!is.null(optional_param1) && optional_param1$col %in% names(matrika)) {
    optional_columns <- c(optional_columns, optional_param1$col)
  }
  if (!is.null(optional_param2) && optional_param2$col %in% names(matrika)) {
    optional_columns <- c(optional_columns, optional_param2$col)
  }
  
  # Convert only element columns to numeric
  matrika[, element_columns] <- lapply(matrika[, element_columns, drop = FALSE], as.numeric)
  
  # Keep optional parameter columns as character/factor for categorical data
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: Element columns converted to numeric:", paste(element_columns, collapse = ", "), "\n")
    cat("DEBUG: Optional columns preserved as character:", paste(optional_columns, collapse = ", "), "\n")
    cat("DEBUG: Final matrika column classes:", paste(sapply(matrika, class), collapse = ", "), "\n")
  }
  
  # Keep as data frame to preserve different column types
  # matrika <- as.matrix(matrika)  # Don't convert to matrix to preserve column types
  
  log_operation("Ternary coordinates", "Generating ternary coordinates")
  
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
  
  log_operation("Ternary coordinates", paste("Generated ternary coordinates for", nrow(ternary_points1), "points"))
  
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
  if (use_robust_mahalanobis) {
    indicator <- if (keep_outliers_robust) "(outliers only)" else "(filtered)"
    mv_methods <- c(mv_methods, paste0("Robust MCD", indicator))
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
  
  # Add charge information to title
  if (exists("file_base") && !is.null(file_base) && nzchar(file_base)) {
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
  plot_title <- preview_title_layout(title_parts)
  
  log_operation("Plot title", paste("Generated title with", length(title_parts), "parts"))
  log_operation("Plot title", paste("Final title:", substr(plot_title, 1, 100), "..."))
  
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
  } else {
    pointSize <- MIN_POINT_SIZE
    pointType <- 16
  }
  
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
    
    if (is_categorical_group && !is.null(selected_groups) && length(selected_groups) > 0) {
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
      
      # Safety check: if no groups match, show all groups instead
      if (sum(group_mask) == 0) {
        cat("Warning: No data matches selected groups. Showing all groups instead.\n")
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
    
  } else {
    pointCol <- "black"
  }
  
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
  
  # Create the ternary plot
  log_operation("Plotting", paste("Starting to plot", nrow(ternary_points1), "points"))
  
  # Set outer margins to prevent clipping of multi-line titles and notes
  # Top margin for titles, bottom margin for plot notes
  op <- par(oma = c(4, 0, 3, 0))
  on.exit(par(op))
  
  # Check if Ternary package is available
  if (!requireNamespace("Ternary", quietly = TRUE)) {
    stop("Ternary package is required for ternary plotting. Please install it first.")
  }
  
  # Create the ternary plot using the Ternary package
  Ternary::TernaryPlot(
    atip = clean_labels_A, btip = clean_labels_B, ctip = clean_labels_C,
    alab = paste(axis_labels_A, "→"), blab = paste(axis_labels_B, "→"), clab = paste("←", axis_labels_C),
    col = "white", 
    grid.lines = 5, 
    grid.lty = "dotted", 
    grid.minor.lines = 1, 
    grid.minor.lty = "dotted"
  )
  
  # Plot all points
  if (getOption("ternary.debug", FALSE)) cat("DEBUG: Plotting all points with TernaryPoints\n")
  
  # Safety check: ensure we have data to plot
  if (nrow(ternary_points1) == 0) {
    if (getOption("ternary.debug", FALSE)) cat("DEBUG: No data to plot - skipping TernaryPoints\n")
    # Add a message to the plot
    text(0.5, 0.5, "No data to display\n(No groups selected or all data filtered out)", 
         cex = 1.2, col = "red", adj = 0.5)
  } else {
    # Ensure all vectors have the same length as ternary_points1
    n_points <- nrow(ternary_points1)
    if (length(pointSize) != n_points) {
      pointSize <- rep(pointSize[1], n_points)
    }
    if (length(pointCol) != n_points) {
      pointCol <- rep(pointCol[1], n_points)
    }
    if (length(pointType) != n_points) {
      pointType <- rep(pointType[1], n_points)
    }
    
    Ternary::TernaryPoints(ternary_points1, cex = pointSize, col = pointCol, pch = pointType)
    if (getOption("ternary.debug", FALSE)) cat("DEBUG: All points plotted successfully\n")
  }
  
  # Enhanced title plotting for multi-line titles
  if (grepl("\n", plot_title)) {
    # Multi-line title: adjust line position and size
    title(main = plot_title, cex.main = 0.7, line = 0.8)
  } else {
    # Single-line title: standard formatting
    title(main = plot_title, cex.main = 0.8, line = 0.5)
  }
  
  # Add legends for optional parameters if selected
  if (!is.null(optional_param1) && !use_manual_point_size) {
    # Legend for optional parameter 1 (point size) - top right
    if (length(optional_param1$col) == 1 && optional_param1_representation == "point_size") {
      if (requireNamespace("PlotTools", quietly = TRUE)) {
        PlotTools::SizeLegend(
          "topright",
          width = c(MIN_POINT_SIZE, MAX_POINT_SIZE),
          lend = "round",
          legend = paste(
            signif(seq(max(param1_values, na.rm = TRUE), min(param1_values, na.rm = TRUE), length.out = 5), digits = 3)
          ),
          title = paste(optional_param1$col, collapse = "+"),
          bty = "n",
          cex = 0.7
        )
      } else {
        # Fallback to regular legend
        size_range <- seq(min(param1_values, na.rm = TRUE), max(param1_values, na.rm = TRUE), length.out = 5)
        legend_sizes <- size_range * (MAX_POINT_SIZE - MIN_POINT_SIZE) / max(param1_values, na.rm = TRUE) + MIN_POINT_SIZE
        
        legend("topright", 
               title = paste(optional_param1$col, collapse = "+"),
               legend = paste("Size:", signif(size_range, 3)),
               pch = 16,
               pt.cex = legend_sizes,
               cex = 0.7)
      }
    } else if (length(optional_param1$col) > 1) {
      # Multiple columns - show combined legend
      legend("topright", 
             title = paste(optional_param1$col, collapse = "+"),
             legend = "Combined",
             pt.cex = 1.5,
             pch = 16,
             cex = 0.7)
    }
  }
  
  if (!is.null(optional_param2)) {
    # Legend for optional parameter 2 (color) - handle both categorical and numeric
    if (is_categorical_group && !is.null(selected_groups) && length(selected_groups) > 0) {
      # Categorical group legend
      create_group_legend(unique_groups, group_colors, group_counts)
    } else if (length(optional_param2$col) == 1) {
      # Numeric data legend - show color legend with exactly 5 bins
      # Generate 5 colors for the legend using the selected palette
      if (color_palette == "blue") {
        legend_colors <- colorRampPalette(c("#357ABD", "#002147"))(5)
      } else if (color_palette == "red") {
        legend_colors <- colorRampPalette(c("#FF6666", "#990000"))(5)
      } else if (color_palette == "viridis") {
        if (!requireNamespace("viridisLite", quietly = TRUE)) install.packages("viridisLite")
        legend_colors <- viridisLite::viridis(5)
      } else if (color_palette == "rainbow") {
        legend_colors <- rainbow(5)
      } else {
        legend_colors <- rep("grey", 5)
      }
      
      # Create legend labels based on whether it's Aspect.Ratio or not
      if (optional_param2$col == "Aspect.Ratio") {
        # Use hardcoded labels for Aspect.Ratio
        legend_labels <- c("1-1.5", "1.5-3", "3-5", "5-10", "10+")
      } else {
        # Use dynamic range labels for other columns
        # Check if data is actually numeric
        if (is.numeric(param2_values) && all(is.finite(param2_values), na.rm = TRUE)) {
          param2_range <- range(param2_values, na.rm = TRUE)
          param2_breaks_legend <- seq(param2_range[1], param2_range[2], length.out = 6)
          legend_labels <- paste0(round(param2_breaks_legend[1:5], 3), " - ", round(param2_breaks_legend[2:6], 3))
        } else {
          # Fallback for non-numeric data
          legend_labels <- "All"
        }
      }
      
      legend("topleft", 
             legend = legend_labels, 
             col = legend_colors, 
             pch = 16, 
             title = paste(optional_param2$col, collapse = "+"),
             cex = 0.7,
             y.intersp = 1.2)
    } else {
      # Multiple columns - show combined legend
      legend("topleft", 
             title = paste(optional_param2$col, collapse = "+"),
             legend = "Combined",
             fill = if (color_palette == "blue") "#357ABD" else if (color_palette == "red") "#FF6666" else if (color_palette == "viridis") "#440154" else if (color_palette == "rainbow") "#FF0000" else "grey",
             cex = 0.7)
    }
  }
  
  # Add comprehensive plot notes if requested
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
    if (use_mahalanobis || use_robust_mahalanobis || use_isolation_forest) {
      mv_info <- c()
      if (use_mahalanobis) {
        outlier_status <- if (keep_outliers_mahalanobis) "(keep only outliers)" else "(remove outliers)"
        mv_info <- c(mv_info, paste("Mahalanobis (λ=", lambda, ", ω=", omega, ")", outlier_status))
        
        # Add detailed Mahalanobis distance information if available
        if (exists("mahal_result") && !is.null(mahal_result)) {
          mv_info <- c(mv_info, paste("  MDmean:", round(mahal_result$MDmean, 3)))
          mv_info <- c(mv_info, paste("  MDthresh:", round(mahal_result$MDthresh, 3)))
          mv_info <- c(mv_info, paste("  stdMD:", round(mahal_result$stdMD, 3)))
          if (!is.null(mahal_result$threshold_method)) {
            mv_info <- c(mv_info, paste("  Method:", mahal_result$threshold_method))
          }
        }
      }
      if (use_robust_mahalanobis) {
        outlier_status <- if (keep_outliers_robust) "(keep only outliers)" else "(remove outliers)"
        mv_info <- c(mv_info, paste("Robust MCD", outlier_status))
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
    
    # Apply text splitting to ensure no line exceeds 45 characters
    col1_text <- split_long_text(col1_text, 45)
    col2_text <- split_long_text(col2_text, 45)
    col3_text <- split_long_text(col3_text, 45)
    
    # Calculate optimal positioning based on text length and content
    col1_lines <- length(strsplit(col1_text, "\n")[[1]])
    col2_lines <- length(strsplit(col2_text, "\n")[[1]])
    col3_lines <- length(strsplit(col3_text, "\n")[[1]])
    
    # Determine optimal line positioning based on content length
    if (max(col1_lines, col2_lines, col3_lines) <= 6) {
      # Short content: use standard positioning
      line_pos <- 2
      text_cex <- 0.6 * 0.75  # Apply 0.75 multiplier for smaller font
    } else if (max(col1_lines, col2_lines, col3_lines) <= 12) {
      # Medium content: adjust positioning
      line_pos <- 3
      text_cex <- 0.55 * 0.75  # Apply 0.75 multiplier for smaller font
    } else {
      # Long content: use extended positioning
      line_pos <- 4
      text_cex <- 0.5 * 0.75  # Apply 0.75 multiplier for smaller font
    }
    
    # Add plot notes in three columns below the plot with intelligent positioning
    # Column 1 (left) - Elements and filters
    mtext(col1_text, side = 1, line = line_pos, cex = text_cex, col = "darkblue", outer = TRUE, adj = 0)
    
    # Column 2 (center) - Optional parameters
    mtext(col2_text, side = 1, line = line_pos, cex = text_cex, col = "darkgreen", outer = TRUE, adj = 0.5)
    
    # Column 3 (right) - Analysis methods
    mtext(col3_text, side = 1, line = line_pos, cex = text_cex, col = "darkred", outer = TRUE, adj = 1)
    
    # Add a debug message to confirm plot notes are being added
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Plot notes added - Elements:", length(elements_summary), "Optional:", length(optional_summary), "Analysis:", length(analysis_summary), "\n")
        cat("DEBUG: Plot notes text lengths - col1:", nchar(col1_text), "col2:", nchar(col2_text), "col3:", nchar(col3_text), "\n")
        cat("DEBUG: Plot notes positioning - line_pos:", line_pos, "text_cex:", text_cex, "\n")
      }
  } else {
    # If plot notes are not included, create empty variables to avoid errors
    col1_text <- ""
    col2_text <- ""
    col3_text <- ""
    line_pos <- 2
    text_cex <- 0.6 * 0.75  # Apply 0.75 multiplier for smaller font
  }
  
  # Save plot if not in preview mode
  if (!preview && !is.null(output_dir)) {
    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Generate simple filename
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file_base <- if (!is.null(xlsx_display_name)) tools::file_path_sans_ext(xlsx_display_name) else if (!is.null(xlsx_file)) tools::file_path_sans_ext(basename(xlsx_file)) else "ternary_plot"
    filename <- file.path(output_dir, paste0("charge_", file_base, "_", timestamp, ".", output_format))
    
    # Calculate plot dimensions based on title length
    plot_dims <- calculate_plot_dimensions(title_parts)
    
    # Save plot with calculated dimensions
    if (output_format == "png") {
      png(filename, width = plot_dims$width, height = plot_dims$height, res = 200)
    } else if (output_format == "jpeg") {
      jpeg(filename, width = plot_dims$width, height = plot_dims$height, res = 200, quality = 95)
    } else if (output_format == "pdf") {
      pdf(filename, width = plot_dims$width/100, height = plot_dims$height/100)
    } else if (output_format == "tiff") {
      tiff(filename, width = plot_dims$width, height = plot_dims$height, res = 200, compression = "lzw")
    }
    
    # Set outer margins to prevent clipping of multi-line titles and notes
    op <- par(oma = c(4, 0, 3, 0))
    
    # Recreate the entire plot on the file device
    # Check if Ternary package is available
    if (!requireNamespace("Ternary", quietly = TRUE)) {
      stop("Ternary package is required for ternary plotting. Please install it first.")
    }
    
    # Create the ternary plot using the Ternary package
    Ternary::TernaryPlot(
      atip = clean_labels_A, btip = clean_labels_B, ctip = clean_labels_C,
      alab = paste(axis_labels_A, "→"), blab = paste(axis_labels_B, "→"), clab = paste("←", axis_labels_C),
      col = "white", 
      grid.lines = 5, 
      grid.lty = "dotted", 
      grid.minor.lines = 1, 
      grid.minor.lty = "dotted"
    )
    
    # Plot all points
    if (getOption("ternary.debug", FALSE)) cat("DEBUG: Plotting all points with TernaryPoints for file save\n")
    
    # Safety check: ensure we have data to plot
    if (nrow(ternary_points1) == 0) {
      if (getOption("ternary.debug", FALSE)) cat("DEBUG: No data to plot for file save - skipping TernaryPoints\n")
      # Add a message to the plot
      text(0.5, 0.5, "No data to display\n(No groups selected or all data filtered out)", 
           cex = 1.2, col = "red", adj = 0.5)
    } else {
      # Ensure all vectors have the same length as ternary_points1
      n_points <- nrow(ternary_points1)
      if (length(pointSize) != n_points) {
        pointSize <- rep(pointSize[1], n_points)
      }
      if (length(pointCol) != n_points) {
        pointCol <- rep(pointCol[1], n_points)
      }
      if (length(pointType) != n_points) {
        pointType <- rep(pointType[1], n_points)
      }
      
      Ternary::TernaryPoints(ternary_points1, cex = pointSize, col = pointCol, pch = pointType)
      if (getOption("ternary.debug", FALSE)) cat("DEBUG: All points plotted successfully for file save\n")
    }
    
    # Enhanced title plotting for multi-line titles
    if (grepl("\n", plot_title)) {
      # Multi-line title: adjust line position and size
      title(main = plot_title, cex.main = 0.7, line = 0.8)
    } else {
      # Single-line title: standard formatting
      title(main = plot_title, cex.main = 0.8, line = 0.5)
    }
    
    # Add legends for optional parameters if selected
    if (!is.null(optional_param1) && !use_manual_point_size) {
      # Legend for optional parameter 1 (point size) - top right
      if (length(optional_param1$col) == 1 && optional_param1_representation == "point_size") {
        if (requireNamespace("PlotTools", quietly = TRUE)) {
          PlotTools::SizeLegend(
            "topright",
            width = c(MIN_POINT_SIZE, MAX_POINT_SIZE),
            lend = "round",
            legend = paste(
              signif(seq(max(param1_values, na.rm = TRUE), min(param1_values, na.rm = TRUE), length.out = 5), digits = 3)
            ),
            title = paste(optional_param1$col, collapse = "+"),
            bty = "n",
            cex = 0.7
          )
        } else {
          # Fallback to regular legend
          size_range <- seq(min(param1_values, na.rm = TRUE), max(param1_values, na.rm = TRUE), length.out = 5)
          legend_sizes <- size_range * (MAX_POINT_SIZE - MIN_POINT_SIZE) / max(param1_values, na.rm = TRUE) + MIN_POINT_SIZE
          
          legend("topright", 
                 title = paste(optional_param1$col, collapse = "+"),
                 legend = paste("Size:", signif(size_range, 3)),
                 pch = 16,
                 pt.cex = legend_sizes,
                 cex = 0.7)
        }
      } else if (length(optional_param1$col) == 1 && optional_param1_representation == "point_type") {
        # Point type representation - show different point types
        if (exists("param1_bins") && length(levels(param1_bins)) > 1) {
          point_types <- c(16, 17, 15, 18, 19)  # circle, triangle, square, diamond, filled diamond
          legend("topright", 
                 title = paste(optional_param1$col, collapse = "+"),
                 legend = levels(param1_bins),
                 pch = point_types[seq_len(length(levels(param1_bins)))],
                 pt.cex = 1.0,
                 cex = 0.7)
        } else {
          legend("topright", 
                 title = paste(optional_param1$col, collapse = "+"),
                 legend = "All",
                 pch = 16,
                 pt.cex = 1.0,
                 cex = 0.7)
        }
      } else if (length(optional_param1$col) > 1) {
        # Multiple columns - show combined legend
        legend("topright", 
               title = paste(optional_param1$col, collapse = "+"),
               legend = "Combined",
               pt.cex = 1.5,
               pch = 16,
               cex = 0.7)
      }
    }
    
    if (!is.null(optional_param2)) {
      # Legend for optional parameter 2 (color) - handle both categorical and numeric for file save
      if (is_categorical_group && !is.null(selected_groups) && length(selected_groups) > 0) {
        # Categorical group legend for file save
        create_group_legend(unique_groups, group_colors, group_counts)
      } else if (length(optional_param2$col) == 1) {
        # Numeric data legend - show color legend with exactly 5 bins
        # Generate 5 colors for the legend using the selected palette
        if (color_palette == "blue") {
          legend_colors <- colorRampPalette(c("#357ABD", "#002147"))(5)
        } else if (color_palette == "red") {
          legend_colors <- colorRampPalette(c("#FF6666", "#990000"))(5)
        } else if (color_palette == "viridis") {
          if (!requireNamespace("viridisLite", quietly = TRUE)) install.packages("viridisLite")
          legend_colors <- viridisLite::viridis(5)
        } else if (color_palette == "rainbow") {
          legend_colors <- rainbow(5)
        } else {
          legend_colors <- rep("grey", 5)
        }
        
        # Create legend labels based on whether it's Aspect.Ratio or not
        if (optional_param2$col == "Aspect.Ratio") {
          # Use hardcoded labels for Aspect.Ratio
          legend_labels <- c("1-1.5", "1.5-3", "3-5", "5-10", "10+")
        } else {
          # Use dynamic range labels for other columns
          # Check if data is actually numeric
          if (is.numeric(param2_values) && all(is.finite(param2_values), na.rm = TRUE)) {
            param2_range <- range(param2_values, na.rm = TRUE)
            param2_breaks_legend <- seq(param2_range[1], param2_range[2], length.out = 6)
            legend_labels <- paste0(round(param2_breaks_legend[1:5], 3), " - ", round(param2_breaks_legend[2:6], 3))
          } else {
            # Fallback for non-numeric data
            legend_labels <- "All"
          }
        }
        
        legend("topleft", 
               legend = legend_labels, 
               col = legend_colors, 
               pch = 16, 
               title = paste(optional_param2$col, collapse = "+"),
               cex = 0.7,
               y.intersp = 1.2)
      } else {
        # Multiple columns - show combined legend
        legend("topleft", 
               title = paste(optional_param2$col, collapse = "+"),
               legend = "Combined",
               fill = if (color_palette == "blue") "#357ABD" else if (color_palette == "red") "#FF6666" else if (color_palette == "viridis") "#440154" else if (color_palette == "rainbow") "#FF0000" else "grey",
               cex = 0.7)
      }
    }
    
    # Add comprehensive plot notes if requested
    if (include_plot_notes && nchar(col1_text) > 0) {
      # Add plot notes in three columns below the plot with intelligent positioning
      # Column 1 (left) - Elements and filters
      mtext(col1_text, side = 1, line = line_pos, cex = text_cex, col = "darkblue", outer = TRUE, adj = 0)
      
      # Column 2 (center) - Optional parameters
      mtext(col2_text, side = 1, line = line_pos, cex = text_cex, col = "darkgreen", outer = TRUE, adj = 0.5)
      
      # Column 3 (right) - Analysis methods
      mtext(col3_text, side = 1, line = line_pos, cex = text_cex, col = "darkred", outer = TRUE, adj = 1)
      
      # Add a debug message to confirm plot notes are being added
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Plot notes added for file save - Elements:", length(elements_summary), "Optional:", length(optional_summary), "Analysis:", length(analysis_summary), "\n")
        cat("DEBUG: Plot notes text lengths for file save - col1:", nchar(col1_text), "col2:", nchar(col2_text), "col3:", nchar(col3_text), "\n")
        cat("DEBUG: Plot notes positioning for file save - line_pos:", line_pos, "text_cex:", text_cex, "\n")
      }
    }
    
    # Restore original par settings
    par(op)
    
    # Close the device
    dev.off()
    
    log_operation("Plot saving", paste("Plot saved to:", filename))
    
    # Return file path for non-preview mode
    return(filename)
  } else {
    # For preview mode, just return NULL since no file was saved
    return(NULL)
  }
  
  log_operation("Function completion", "general_ternary_plot function completed successfully")
}

# Note: Statistical filtering functions are now in statistical_filters.R to avoid duplication
# Note: Multivariate analysis functions are now in multivariate.R to avoid duplication
