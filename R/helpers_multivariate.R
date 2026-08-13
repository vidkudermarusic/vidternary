# ---- Helper Functions Module: Multivariate Analysis Orchestration ----
# Split out of helpers.R: wrappers around multivariate.R's core Mahalanobis
# distance / Isolation Forest functions, and combining their results.

# Wrapper functions for multivariate analysis methods
perform_mahalanobis_analysis <- function(data, columns, lambda) {
  if (length(columns) < 2) return(NULL)
  numeric_cols <- sapply(data, is.numeric)
  available_cols <- colnames(data)[numeric_cols]
  selected_cols <- intersect(columns, available_cols)

  if (length(selected_cols) < 2) return(NULL)

  result <- compute_mahalanobis_distance(
    data[, selected_cols, drop = FALSE],
    data[, selected_cols, drop = FALSE],
    lambda = lambda,
    omega = 0,
    keep_outliers = FALSE,
    custom_mdthresh = NULL,
    selected_columns = selected_cols,
    mdthresh_mode = "auto"
  )

  if (!is.null(result)) {
    # compute_mahalanobis_distance() drops NA rows before computing distances,
    # so result$outlier_indices is only as long as the number of complete
    # cases - not nrow(data). Map it back to full length (NA rows are never
    # flagged) so it's directly comparable/combinable with other methods'
    # outlier vectors, which are always nrow(data)-aligned (see
    # compute_isolation_forest()'s equivalent cc1 mapping in multivariate.R).
    complete_rows <- complete.cases(data[, result$common_cols, drop = FALSE])
    full_length_flags <- logical(nrow(data))
    full_length_flags[complete_rows] <- result$outlier_indices
    result$outlier_indices <- full_length_flags
  }

  return(result)
}

# perform_robust_mahalanobis_analysis function removed

perform_isolation_forest_analysis <- function(data, columns, omega) {
  if (length(columns) < 2) return(NULL)
  numeric_cols <- sapply(data, is.numeric)
  available_cols <- colnames(data)[numeric_cols]
  selected_cols <- intersect(columns, available_cols)

  if (length(selected_cols) < 2) return(NULL)

  result <- compute_isolation_forest(
    data[, selected_cols, drop = FALSE],
    data[, selected_cols, drop = FALSE],
    selected_columns = selected_cols,
    keep_outliers = FALSE
  )
  return(result)
}

# Function to combine outlier results from multiple methods
combine_outlier_results <- function(results) {
  # Initialize combined outliers list
  combined <- list(
    outlier_indices = logical(0),
    outlier_count = 0,
    methods_agreement = NULL,
    summary = NULL
  )

  # Extract outlier indices from each method
  outlier_methods <- list()

  if (!is.null(results$mahalanobis_results) && !is.null(results$mahalanobis_results$outlier_indices)) {
    outlier_methods$mahalanobis <- results$mahalanobis_results$outlier_indices
  }

  if (!is.null(results$isolation_forest_results) && !is.null(results$isolation_forest_results$outlier_indices)) {
    outlier_methods$isolation_forest <- results$isolation_forest_results$outlier_indices
  }

  if (!is.null(results$iqr_filter_results) && !is.null(results$iqr_filter_results$outlier_indices)) {
    outlier_methods$iqr_filter <- results$iqr_filter_results$outlier_indices
  }

  if (!is.null(results$zscore_filter_results) && !is.null(results$zscore_filter_results$outlier_indices)) {
    outlier_methods$zscore_filter <- results$zscore_filter_results$outlier_indices
  }

  if (!is.null(results$mad_filter_results) && !is.null(results$mad_filter_results$outlier_indices)) {
    outlier_methods$mad_filter <- results$mad_filter_results$outlier_indices
  }

  # If no outlier methods available, return empty result
  if (length(outlier_methods) == 0) {
    return(combined)
  }

  # Every method's outlier_indices must be aligned 1:1 with the original
  # data's rows (position i means row i of df1) before they can be combined
  # with `|`. Truncating a mismatched vector to x[1:min_length] would keep
  # the first N *positions* rather than the same *rows* across methods,
  # silently unioning outlier flags for the wrong observations - so a length
  # mismatch here indicates a bug in one of the upstream *_results producers
  # and should fail loudly rather than be papered over.
  lengths <- sapply(outlier_methods, length)
  if (length(unique(lengths)) > 1) {
    stop("combine_outlier_results: outlier methods have mismatched lengths (",
         paste(names(lengths), lengths, sep = "=", collapse = ", "),
         ") - each method must return outlier_indices aligned to nrow(data).")
  }

  # Combine using union (any method flags as outlier)
  if (length(outlier_methods) > 0) {
    if (length(outlier_methods) == 1) {
      combined$outlier_indices <- outlier_methods[[1]]
    } else {
      combined$outlier_indices <- Reduce(`|`, outlier_methods)
    }
    combined$outlier_count <- sum(combined$outlier_indices)

    # Calculate agreement between methods
    if (length(outlier_methods) > 1) {
      agreement_matrix <- matrix(0, nrow = length(outlier_methods), ncol = length(outlier_methods))
      method_names <- names(outlier_methods)
      rownames(agreement_matrix) <- method_names
      colnames(agreement_matrix) <- method_names

      for (i in seq_along(outlier_methods)) {
        for (j in seq_along(outlier_methods)) {
          if (i != j) {
            agreement_matrix[i, j] <- sum(outlier_methods[[i]] & outlier_methods[[j]]) /
                                     sum(outlier_methods[[i]] | outlier_methods[[j]]) * 100
          } else {
            agreement_matrix[i, j] <- 100
          }
        }
      }
      combined$methods_agreement <- agreement_matrix
    }

    # Create summary
    combined$summary <- list(
      total_methods = length(outlier_methods),
      total_outliers = combined$outlier_count,
      outlier_percentage = round(combined$outlier_count / length(combined$outlier_indices) * 100, 2),
      methods_used = names(outlier_methods)
    )
  }

  return(combined)
}

# Multivariate analysis calculation with multiple methods
multivariate_analysis <- function(use_mahalanobis = FALSE,
                                 use_isolation_forest = FALSE, use_iqr_filter = FALSE,
                                 use_zscore_filter = FALSE, use_mad_filter = FALSE,
                                 lambda = 2, omega = 0.1, outlier_mode = "both",
                                 mdthresh_mode = "auto", custom_mdthresh = NULL,
                                 mahalanobis_reference = "dataset1", multivariate_columns = NULL,
                                 selected_columns = NULL, xlsx_file1 = NULL, xlsx_file2 = NULL,
                                 universal_reference = "dataset1",
                                 iqr_multiplier = 1.5, zscore_threshold = 3, mad_threshold = 3) {

  # Check if at least one method is selected
  if (!(use_mahalanobis || use_isolation_forest)) {
    log_operation("WARNING", "No multivariate analysis method selected")
    return(NULL)
  }

  # Check if files are uploaded based on universal reference mode
  if (universal_reference == "dataset2") {
    # Need both files for dataset2 reference
    if (is.null(xlsx_file1) || is.null(xlsx_file2)) {
      log_operation("ERROR", "Both datasets required for dataset2 reference mode")
      return(NULL)
    }
  } else {
    # Self-reference only needs one file
    if (is.null(xlsx_file1)) {
      log_operation("ERROR", "Dataset 1 required for analysis")
      return(NULL)
    }
  }

  tryCatch({
    # Load data
    df1 <- if (!is.null(xlsx_file1)) openxlsx::read.xlsx(xlsx_file1$datapath, sheet = 1) else NULL
    df2 <- if (!is.null(xlsx_file2)) openxlsx::read.xlsx(xlsx_file2$datapath, sheet = 1) else NULL

    results <- list(
      methods_used = c(),
      mahalanobis_results = NULL,
      isolation_forest_results = NULL,
      iqr_filter_results = NULL,
      zscore_filter_results = NULL,
      mad_filter_results = NULL,
      combined_outliers = NULL,
      timestamp = Sys.time()
    )

    # Mahalanobis distance analysis
    if (use_mahalanobis && !is.null(multivariate_columns)) {
      if (mahalanobis_reference == "dataset1" && !is.null(df1)) {
        results$mahalanobis_results <- perform_mahalanobis_analysis(df1, multivariate_columns, lambda)
        results$methods_used <- c(results$methods_used, "Mahalanobis")
      } else if (mahalanobis_reference == "dataset2" && !is.null(df2)) {
        results$mahalanobis_results <- perform_mahalanobis_analysis(df2, multivariate_columns, lambda)
        results$methods_used <- c(results$methods_used, "Mahalanobis (Dataset 2)")
      }
    }

    # Robust Mahalanobis analysis removed

    # Isolation Forest analysis
    if (use_isolation_forest && !is.null(multivariate_columns)) {
      if (mahalanobis_reference == "dataset1" && !is.null(df1)) {
        results$isolation_forest_results <- perform_isolation_forest_analysis(df1, multivariate_columns, omega)
        results$methods_used <- c(results$methods_used, "Isolation Forest")
      } else if (mahalanobis_reference == "dataset2" && !is.null(df2)) {
        results$isolation_forest_results <- perform_isolation_forest_analysis(df2, multivariate_columns, omega)
        results$methods_used <- c(results$methods_used, "Isolation Forest (Dataset 2)")
      }
    }

    # Statistical filters. combine_outlier_results() expects each entry to be
    # a list with an $outlier_indices vector aligned to nrow(df1) - not the
    # filtered data.frame that apply_*_filter() returns for plotting use - so
    # the flag-only helpers from statistical_filters.R are used here instead.
    # `lambda`/`omega` are Mahalanobis-specific sensitivity knobs (no fixed
    # range) and must not be reused as the Z-score/MAD threshold (~2-3 by
    # convention); each filter gets its own dedicated threshold argument.
    if (use_iqr_filter) {
      results$iqr_filter_results <- list(
        outlier_indices = get_iqr_outlier_flags(df1, multivariate_columns, iqr_multiplier)
      )
      results$methods_used <- c(results$methods_used, "IQR Filter")
    }

    if (use_zscore_filter) {
      results$zscore_filter_results <- list(
        outlier_indices = get_zscore_outlier_flags(df1, multivariate_columns, zscore_threshold)
      )
      results$methods_used <- c(results$methods_used, "Z-Score Filter")
    }

    if (use_mad_filter) {
      results$mad_filter_results <- list(
        outlier_indices = get_mad_outlier_flags(df1, multivariate_columns, mad_threshold)
      )
      results$methods_used <- c(results$methods_used, "MAD Filter")
    }

    # Combine outlier results
    results$combined_outliers <- combine_outlier_results(results)

    log_operation("INFO", "Multivariate analysis completed successfully",
                  paste("Methods used:", paste(results$methods_used, collapse = ", ")))

    return(results)

  }, error = function(e) {
    log_operation("ERROR", "Multivariate analysis failed", e$message)
    return(NULL)
  })
}

# Data analysis function for Dataset 1
analyze_data1 <- function(element_A1, element_B1, element_C1, xlsx_file1,
                          optional_param1_1 = "", optional_param2_1 = "") {
  if (is.null(xlsx_file1)) {
    return(NULL)
  }

  tryCatch({
    df <- openxlsx::read.xlsx(xlsx_file1$datapath, sheet = 1)

    # Collect all columns for analysis - ensure they are character strings
    cols <- character(0)
    if (!is.null(element_A1) && element_A1 != "") cols <- c(cols, as.character(element_A1))
    if (!is.null(element_B1) && element_B1 != "") cols <- c(cols, as.character(element_B1))
    if (!is.null(element_C1) && element_C1 != "") cols <- c(cols, as.character(element_C1))
    if (!is.null(optional_param1_1) && optional_param1_1 != "") cols <- c(cols, as.character(optional_param1_1))
    if (!is.null(optional_param2_1) && optional_param2_1 != "") cols <- c(cols, as.character(optional_param2_1))

    # Remove duplicates and ensure columns exist
    cols <- unique(cols)
    available_cols <- colnames(df)
    cols <- cols[cols %in% available_cols]

    if (length(cols) == 0) {
      log_operation("WARNING", "No valid columns found for Dataset 1 analysis")
      return(NULL)
    }

    # Validate data
    validation <- validate_data_enhanced(df, cols)
    if (!validation$valid) {
      log_operation("WARNING", "Data validation failed for Dataset 1", validation$issues)
      return(NULL)
    }

    # Generate statistics
    stats <- generate_stats(df, cols)

    # Compute correlation
    correlation <- compute_correlation(df, cols)

    # Data quality check
    quality <- check_data_quality(df, cols)

    result <- list(
      df = df,
      stats = stats,
      validation = validation,
      correlation = correlation,
      quality = quality,
      timestamp = Sys.time()
    )

    log_operation("INFO", "Dataset 1 analysis completed successfully",
                  paste("Columns analyzed:", length(cols), "Rows:", nrow(df)))

    return(result)

  }, error = function(e) {
    log_operation("ERROR", "Failed to analyze Dataset 1", e$message)
    return(NULL)
  })
}

# Data analysis function for Dataset 2
analyze_data2 <- function(element_A2, element_B2, element_C2, xlsx_file2,
                          optional_param1_2 = "", optional_param2_2 = "") {
  if (is.null(xlsx_file2)) {
    return(NULL)
  }

  tryCatch({
    df <- openxlsx::read.xlsx(xlsx_file2$datapath, sheet = 1)

    # Collect all columns for analysis - ensure they are character strings
    cols <- character(0)
    if (!is.null(element_A2) && element_A2 != "") cols <- c(cols, as.character(element_A2))
    if (!is.null(element_B2) && element_B2 != "") cols <- c(cols, as.character(element_B2))
    if (!is.null(element_C2) && element_C2 != "") cols <- c(cols, as.character(element_C2))
    if (!is.null(optional_param1_2) && optional_param1_2 != "") cols <- c(cols, as.character(optional_param1_2))
    if (!is.null(optional_param2_2) && optional_param2_2 != "") cols <- c(cols, as.character(optional_param2_2))

    # Remove duplicates and ensure columns exist
    cols <- unique(cols)
    available_cols <- colnames(df)
    cols <- cols[cols %in% available_cols]

    if (length(cols) == 0) {
      log_operation("WARNING", "No valid columns found for Dataset 2 analysis")
      return(NULL)
    }

    # Validate data
    validation <- validate_data_enhanced(df, cols)
    if (!validation$valid) {
      log_operation("WARNING", "Data validation failed for Dataset 2", validation$issues)
      return(NULL)
    }

    # Generate statistics
    stats <- generate_stats(df, cols)

    # Compute correlation
    correlation <- compute_correlation(df, cols)

    # Data quality check
    quality <- check_data_quality(df, cols)

    result <- list(
      df = df,
      stats = stats,
      validation = validation,
      correlation = correlation,
      quality = quality,
      timestamp = Sys.time()
    )

    log_operation("INFO", "Dataset 2 analysis completed successfully",
                  paste("Columns analyzed:", length(cols), "Rows:", nrow(df)))

    return(result)

  }, error = function(e) {
    log_operation("ERROR", "Failed to analyze Dataset 2", e$message)
    return(NULL)
  })
}
