# ---- Helper Functions Module: Data Quality & Validation ----
# Split out of helpers.R: functions that validate/inspect data quality.

#' Check missing values, zero-variance columns, and IQR outliers
#'
#' @param data A data frame.
#' @param cols Columns to check. Defaults to all columns of `data`.
#' @return A list: `missing_counts` (named integer vector), `zero_var_cols`
#'   (character vector of column names), `outliers_iqr` (named integer
#'   vector of IQR-outlier counts).
#' @export
validate_data_quality <- function(data, cols = NULL) {
  if (is.null(cols)) {
    cols <- colnames(data)
  }

  # Check for missing values
  missing_counts <- sapply(data[, cols, drop = FALSE], function(x) sum(is.na(x)))

  # Check for infinite values
  infinite_counts <- sapply(data[, cols, drop = FALSE], function(x) sum(is.infinite(x)))

  # Check for zero variance columns
  zero_var_cols <- sapply(data[, cols, drop = FALSE], function(x) {
    if (is.numeric(x)) var(x, na.rm = TRUE) == 0 else FALSE
  })

  # Check for outliers using IQR method
  outliers_iqr <- sapply(data[, cols, drop = FALSE], function(x) {
    if (is.numeric(x)) {
      q1 <- quantile(x, 0.25, na.rm = TRUE)
      q3 <- quantile(x, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower <- q1 - 1.5 * iqr
      upper <- q3 + 1.5 * iqr
      sum(x < lower | x > upper, na.rm = TRUE)
    } else 0
  })

  return(list(
    missing_counts = missing_counts,
    zero_var_cols = names(zero_var_cols[zero_var_cols]),
    outliers_iqr = outliers_iqr
  ))
}

#' Validate a data frame and selected columns, with detailed error/warning reporting
#'
#' Checks that `df` is non-null with rows/columns, that `cols` all exist,
#' and reports (as warnings, not errors) high missing-value rates,
#' infinite values, and zero-variance columns. Logs the outcome via
#' `log_operation()`.
#'
#' @param df A data frame to validate.
#' @param cols Character vector of column names expected to be present.
#' @param operation_name Label used in log messages. Default `"Data validation"`.
#' @return A list: `valid` (logical), `errors`, `warnings` (character
#'   vectors), plus (when `df`/`cols` are usable) `missing_counts`,
#'   `infinite_counts`, `zero_var_cols`, `numeric_cols`.
#' @export
validate_data_enhanced <- function(df, cols, operation_name = "Data validation") {
  errors <- character(0)
  warnings <- character(0)

  # Check if dataframe exists
  if (is.null(df)) {
    errors <- c(errors, "Dataframe is NULL")
    return(list(valid = FALSE, errors = errors, warnings = warnings))
  }

  # Check if dataframe has rows
  if (nrow(df) == 0) {
    errors <- c(errors, "Dataframe has no rows")
    return(list(valid = FALSE, errors = errors, warnings = warnings))
  }

  # Check if dataframe has columns
  if (ncol(df) == 0) {
    errors <- c(errors, "Dataframe has no columns")
    return(list(valid = FALSE, errors = errors, warnings = warnings))
  }

  # Check if required columns exist
  missing_cols <- setdiff(cols, colnames(df))
  if (length(missing_cols) > 0) {
    errors <- c(errors, paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }

  # Check for numeric columns if needed
  numeric_cols <- cols[sapply(df[, cols, drop = FALSE], is.numeric)]
  if (length(numeric_cols) == 0) {
    warnings <- c(warnings, "No numeric columns found in selected columns")
  }

  # Check for missing values
  missing_counts <- sapply(df[, cols, drop = FALSE], function(x) sum(is.na(x)))
  high_missing <- names(missing_counts[missing_counts > nrow(df) * 0.5])
  if (length(high_missing) > 0) {
    warnings <- c(warnings, paste("High missing values (>50%) in columns:", paste(high_missing, collapse = ", ")))
  }

  # Check for infinite values
  infinite_counts <- sapply(df[, cols, drop = FALSE], function(x) sum(is.infinite(x)))
  infinite_cols <- names(infinite_counts[infinite_counts > 0])
  if (length(infinite_cols) > 0) {
    warnings <- c(warnings, paste("Infinite values found in columns:", paste(infinite_cols, collapse = ", ")))
  }

  # Check for zero variance columns
  zero_var_cols <- sapply(df[, cols, drop = FALSE], function(x) {
    if (is.numeric(x)) var(x, na.rm = TRUE) == 0 else FALSE
  })
  zero_var_names <- names(zero_var_cols[zero_var_cols])
  if (length(zero_var_names) > 0) {
    warnings <- c(warnings, paste("Zero variance columns:", paste(zero_var_names, collapse = ", ")))
  }

  # Log validation results
  if (length(errors) > 0) {
    log_operation("ERROR", paste(operation_name, "- Validation failed"), paste(errors, collapse = "; "))
  } else if (length(warnings) > 0) {
    log_operation("WARNING", paste(operation_name, "- Validation passed with warnings"), paste(warnings, collapse = "; "))
  } else {
    log_operation("INFO", paste(operation_name, "- Validation passed"), "All checks passed")
  }

  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = warnings,
    missing_counts = missing_counts,
    infinite_counts = infinite_counts,
    zero_var_cols = zero_var_names,
    numeric_cols = numeric_cols
  ))
}

#' Assert that element A/B/C inputs are all set
#'
#' @param inputs A list (or Shiny `input`-like object) expected to have
#'   non-empty `element_A`/`element_B`/`element_C` entries.
#' @return `NULL`, invisibly, if valid; otherwise raises an error via `stop()`.
#' @export
validate_inputs <- function(inputs) {
  required_fields <- c("element_A", "element_B", "element_C")
  missing_fields <- required_fields[!sapply(required_fields, function(x) !is.null(inputs[[x]]) && length(inputs[[x]]) > 0)]

  if (length(missing_fields) > 0) {
    stop("Missing required fields: ", paste(missing_fields, collapse = ", "))
  }
}

#' Full data-quality assessment comparing two datasets
#'
#' Reports missing/infinite values, zero-variance and constant columns,
#' and IQR outliers for each dataset's common numeric columns, plus an
#' overall 0-100 quality score and letter grade per dataset (via
#' `calculate_quality_score()`). Used by the Data Comparison tab's
#' Missing/Outlier Summary and by `run_comprehensive_analysis()`.
#'
#' @param data1 First dataset, as a data frame.
#' @param data2 Second dataset, as a data frame.
#' @return A list including `missing_values`, `infinite_values`,
#'   `zero_variance`, `outliers_iqr`, `quality_score` (each with `data1`/
#'   `data2` sub-lists), and `processing_time`.
#' @export
check_data_quality <- function(data1, data2) {
  start_time <- Sys.time()
  quality_report <- list()

  # Find common numeric columns
  numeric_cols1 <- sapply(data1, is.numeric)
  numeric_cols2 <- sapply(data2, is.numeric)
  common_cols <- intersect(colnames(data1)[numeric_cols1], colnames(data2)[numeric_cols2])

  quality_report$common_cols <- common_cols
  quality_report$num_common_cols <- length(common_cols)
  quality_report$data1_rows <- nrow(data1)
  quality_report$data2_rows <- nrow(data2)

  # Check for missing values
  quality_report$missing_values <- list(
    data1 = sapply(data1, function(x) sum(is.na(x))),
    data2 = sapply(data2, function(x) sum(is.na(x)))
  )

  # Check for infinite values
  quality_report$infinite_values <- list(
    data1 = sapply(data1, function(x) sum(is.infinite(x))),
    data2 = sapply(data2, function(x) sum(is.infinite(x)))
  )

  # Check for zero variance columns
  quality_report$zero_variance <- list(
    data1 = sapply(data1, function(x) if(is.numeric(x)) var(x, na.rm=TRUE) == 0 else FALSE),
    data2 = sapply(data2, function(x) if(is.numeric(x)) var(x, na.rm=TRUE) == 0 else FALSE)
  )

  # Check data types
  quality_report$data_types <- list(
    data1 = sapply(data1, class),
    data2 = sapply(data2, class)
  )

  # Check for outliers using IQR method
  quality_report$outliers_iqr <- list(
    data1 = sapply(data1, function(x) {
      if(is.numeric(x)) {
        q1 <- quantile(x, 0.25, na.rm=TRUE)
        q3 <- quantile(x, 0.75, na.rm=TRUE)
        iqr <- q3 - q1
        lower <- q1 - 1.5 * iqr
        upper <- q3 + 1.5 * iqr
        sum(x < lower | x > upper, na.rm=TRUE)
      } else 0
    }),
    data2 = sapply(data2, function(x) {
      if(is.numeric(x)) {
        q1 <- quantile(x, 0.25, na.rm=TRUE)
        q3 <- quantile(x, 0.75, na.rm=TRUE)
        iqr <- q3 - q1
        lower <- q1 - 1.5 * iqr
        upper <- q3 + 1.5 * iqr
        sum(x < lower | x > upper, na.rm=TRUE)
      } else 0
    })
  )

  # Enhanced quality metrics
  quality_report$correlation_analysis <- list(
    data1 = if(ncol(data1) > 1) cor(data1[, sapply(data1, is.numeric)], use = "pairwise.complete.obs") else NULL,
    data2 = if(ncol(data2) > 1) cor(data2[, sapply(data2, is.numeric)], use = "pairwise.complete.obs") else NULL
  )

  # Check for high correlation (potential multicollinearity)
  quality_report$high_correlation <- list(
    data1 = if(!is.null(quality_report$correlation_analysis$data1)) {
      cor_matrix <- quality_report$correlation_analysis$data1
      high_cor <- which(abs(cor_matrix) > 0.9 & cor_matrix != 1, arr.ind = TRUE)
      if(length(high_cor) > 0) {
        data.frame(
          var1 = rownames(cor_matrix)[high_cor[, 1]],
          var2 = colnames(cor_matrix)[high_cor[, 2]],
          correlation = cor_matrix[high_cor]
        )
      } else NULL
    } else NULL,
    data2 = if(!is.null(quality_report$correlation_analysis$data2)) {
      cor_matrix <- quality_report$correlation_analysis$data2
      high_cor <- which(abs(cor_matrix) > 0.9 & cor_matrix != 1, arr.ind = TRUE)
      if(length(high_cor) > 0) {
        data.frame(
          var1 = rownames(cor_matrix)[high_cor[, 1]],
          var2 = colnames(cor_matrix)[high_cor[, 2]],
          correlation = cor_matrix[high_cor]
        )
      } else NULL
    } else NULL
  )

  # Data distribution summary
  quality_report$distribution_summary <- list(
    data1 = sapply(data1, function(x) {
      if(is.numeric(x)) {
        c(mean = mean(x, na.rm = TRUE),
          median = median(x, na.rm = TRUE),
          sd = sd(x, na.rm = TRUE),
          skewness = if(length(x) > 2) tryCatch(moments::skewness(x, na.rm = TRUE), error = function(e) NA) else NA,
          kurtosis = if(length(x) > 2) tryCatch(moments::skewness(x, na.rm = TRUE), error = function(e) NA) else NA)
      } else NULL
    }),
    data2 = sapply(data2, function(x) {
      if(is.numeric(x)) {
        c(mean = mean(x, na.rm = TRUE),
          median = median(x, na.rm = TRUE),
          sd = sd(x, na.rm = TRUE),
          skewness = if(length(x) > 2) tryCatch(moments::skewness(x, na.rm = TRUE), error = function(e) NA) else NA,
          kurtosis = if(length(x) > 2) tryCatch(moments::skewness(x, na.rm = TRUE), error = function(e) NA) else NA)
      } else NULL
    })
  )

  # Overall quality score
  quality_report$quality_score <- list(
    data1 = calculate_quality_score(quality_report$missing_values$data1,
                                    quality_report$infinite_values$data1,
                                    quality_report$zero_variance$data1,
                                    quality_report$outliers_iqr$data1,
                                    nrow(data1), ncol(data1)),
    data2 = calculate_quality_score(quality_report$missing_values$data2,
                                    quality_report$infinite_values$data2,
                                    quality_report$zero_variance$data2,
                                    quality_report$outliers_iqr$data2,
                                    nrow(data2), ncol(data2))
  )

  end_time <- Sys.time()
  quality_report$processing_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

  log_operation("Performance", paste("Data quality check completed in", round(quality_report$processing_time, 2), "seconds"))

  return(quality_report)
}

#' Compute a 0-100 data-quality score and letter grade
#'
#' Starts at 100 and subtracts penalties for missing values, infinite
#' values, zero-variance columns, and outliers (outlier penalty capped at 30).
#'
#' @param missing_vals Named vector/list of per-column missing-value counts.
#' @param infinite_vals Named vector/list of per-column infinite-value counts.
#' @param zero_var Logical vector/list of which columns have zero variance.
#' @param outliers Named vector/list of per-column outlier counts.
#' @param n_rows Total row count, for normalizing penalties.
#' @param n_cols Total column count, for normalizing penalties.
#' @return A list: `score` (0-100), `grade` (`"A"`-`"F"`), `details` (list
#'   of the individual penalty components).
#' @export
calculate_quality_score <- function(missing_vals, infinite_vals, zero_var, outliers, n_rows, n_cols) {
  total_cells <- n_rows * n_cols

  # Penalties
  missing_penalty <- sum(missing_vals) / total_cells * 100
  infinite_penalty <- sum(infinite_vals) / total_cells * 100
  zero_var_penalty <- sum(zero_var) / n_cols * 50
  outlier_penalty <- min(sum(outliers) / total_cells * 20, 30) # Cap at 30%

  # Base score starts at 100
  base_score <- 100

  # Subtract penalties
  final_score <- base_score - missing_penalty - infinite_penalty - zero_var_penalty - outlier_penalty

  # Ensure score is between 0 and 100
  final_score <- max(0, min(100, final_score))

  # Assign grade
  grade <- if(final_score >= 90) "A" else
    if(final_score >= 80) "B" else
      if(final_score >= 70) "C" else
        if(final_score >= 60) "D" else "F"

  return(list(
    score = round(final_score, 1),
    grade = grade,
    details = list(
      missing_penalty = round(missing_penalty, 1),
      infinite_penalty = round(infinite_penalty, 1),
      zero_var_penalty = round(zero_var_penalty, 1),
      outlier_penalty = round(outlier_penalty, 1)
    )
  ))
}
