# ---- Libraries ----
required_packages <- c("openxlsx", "Ternary", "PlotTools", "shiny", "shinyFiles", 
                       "shinyjqui", "shinyBS", "ggplot2", "GGally", "rmarkdown", 
                       "corrplot", "knitr", "colourpicker", "DT")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(openxlsx)
library(Ternary)
library(PlotTools)
library(shiny)
library(shinyFiles)
library(shinyjqui)
library(shinyBS)
library(ggplot2)
library(GGally)
library(rmarkdown)
library(corrplot)
library(knitr)
library(colourpicker)
library(DT)

options(shiny.maxRequestSize = 100 * 1024^2)  # 100 MB

# ---- Default Directories ----
default_working_dir <- "C:/mag_naloga_R"
default_output_dir <- "C:/mag_naloga_R/output"

# ---- Helper Analysis Functions ----
validate_data <- function(df, cols) {
  missing_counts <- sapply(df[, cols, drop=FALSE], function(x) sum(is.na(x)))
  outlier_flags <- sapply(cols, function(col) {
    vals <- as.numeric(df[[col]])
    q1 <- quantile(vals, 0.25, na.rm=TRUE)
    q3 <- quantile(vals, 0.75, na.rm=TRUE)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    sum(vals < lower | vals > upper, na.rm=TRUE)
  })
  list(missing=missing_counts, outliers=outlier_flags)
}

generate_stats <- function(df, cols) {
  stats <- lapply(cols, function(col) {
    vals <- as.numeric(df[[col]])
    list(
      mean=mean(vals, na.rm=TRUE),
      median=median(vals, na.rm=TRUE),
      sd=sd(vals, na.rm=TRUE)
    )
  })
  names(stats) <- cols
  return(stats)
}

compute_correlation <- function(df, cols) {
  cor(df[, cols], use="complete.obs")
}

plot_correlation_matrix <- function(df, cols, file) {
  g <- ggcorr(df[, cols], label=TRUE)
  ggsave(file, plot=g, width=6, height=6)
}

# ---- Mahalanobis Distance Function ----
compute_mahalanobis_distance <- function(data1, data2, lambda = 1, omega = 0, keep_outliers = FALSE, custom_mdthresh = NULL, selected_columns = NULL) {
  # Use selected columns or all numeric columns that exist in both datasets
  if (!is.null(selected_columns) && length(selected_columns) > 0) {
    # Use user-selected columns
    available_cols1 <- colnames(data1)[colnames(data1) %in% selected_columns]
    available_cols2 <- colnames(data2)[colnames(data2) %in% selected_columns]
    common_cols <- intersect(available_cols1, available_cols2)
  } else {
    # Use all numeric columns (original behavior)
    numeric_cols1 <- sapply(data1, is.numeric)
    numeric_cols2 <- sapply(data2, is.numeric)
    common_cols <- intersect(colnames(data1)[numeric_cols1], colnames(data2)[numeric_cols2])
  }
  
  if (length(common_cols) < 2) {
    stop("Need at least 2 common numeric columns for Mahalanobis distance calculation")
  }
  
  # Extract numeric data
  data1_numeric <- as.matrix(data1[, common_cols, drop=FALSE])
  data2_numeric <- as.matrix(data2[, common_cols, drop=FALSE])
  
  # Remove rows with NA values
  data1_clean <- data1_numeric[complete.cases(data1_numeric), , drop=FALSE]
  data2_clean <- data2_numeric[complete.cases(data2_numeric), , drop=FALSE]
  
  if (nrow(data1_clean) == 0 || nrow(data2_clean) == 0) {
    stop("No complete cases found in one or both datasets")
  }
  
  # Check for sufficient observations
  if (nrow(data2_clean) <= ncol(data2_clean)) {
    stop(paste("Insufficient observations in reference dataset. Need at least", ncol(data2_clean) + 1, "rows, but have", nrow(data2_clean)))
  }
  
  # Check for zero variance columns
  col_vars <- apply(data2_clean, 2, var, na.rm = TRUE)
  zero_var_cols <- common_cols[col_vars == 0 | is.na(col_vars)]
  if (length(zero_var_cols) > 0) {
    stop(paste("Columns with zero variance in reference dataset:", paste(zero_var_cols, collapse = ", ")))
  }
  
  # Calculate covariance matrix from reference dataset (data2)
  cov_matrix <- cov(data2_clean)
  
  # Check if covariance matrix is singular
  if (det(cov_matrix) == 0 || is.na(det(cov_matrix))) {
    # Try to identify problematic columns
    eigenvals <- eigen(cov_matrix, only.values = TRUE)$values
    min_eigenval <- min(eigenvals)
    condition_number <- max(eigenvals) / min_eigenval
    
    stop(paste("Covariance matrix is singular or near-singular.",
               "Determinant:", det(cov_matrix),
               "Condition number:", condition_number,
               "This usually means columns are perfectly correlated or have identical values."))
  }
  
  # Calculate Mahalanobis distances for data1 relative to data2
  mahal_distances <- mahalanobis(data1_clean, 
                                 center = colMeans(data2_clean), 
                                 cov = cov_matrix)
  
  # Calculate threshold - either custom or using formula
  MDmean <- mean(mahal_distances)
  stdMD <- sd(mahal_distances)
  
  if (!is.null(custom_mdthresh)) {
    MDthresh <- custom_mdthresh
  } else {
    MDthresh <- MDmean + sqrt(100/(100 + lambda - omega)) * stdMD
  }
  
  # Calculate probabilistic thresholds for comparison
  df <- length(common_cols)
  threshold_95 <- qchisq(0.95, df = df)
  threshold_99 <- qchisq(0.99, df = df)
  
  # Calculate p-values for each distance
  p_values <- 1 - pchisq(mahal_distances, df = df)
  
  # Identify outliers based on custom threshold
  outlier_indices <- mahal_distances > MDthresh
  
  return(list(
    distances = mahal_distances,
    p_values = p_values,
    threshold_95 = threshold_95,
    threshold_99 = threshold_99,
    MDthresh = MDthresh,
    MDmean = MDmean,
    stdMD = stdMD,
    lambda = lambda,
    omega = omega,
    outlier_95 = sum(mahal_distances > threshold_95),
    outlier_99 = sum(mahal_distances > threshold_99),
    outlier_custom = sum(outlier_indices),
    total_points = length(mahal_distances),
    df = df,
    common_cols = common_cols,
    outlier_indices = outlier_indices,
    keep_outliers = keep_outliers
  ))
}

# ---- Data Quality Check Function ----
check_data_quality <- function(data1, data2) {
  numeric_cols1 <- sapply(data1, is.numeric)
  numeric_cols2 <- sapply(data2, is.numeric)
  common_cols <- intersect(colnames(data1)[numeric_cols1], colnames(data2)[numeric_cols2])
  
  result <- list()
  result$common_cols <- common_cols
  result$num_common_cols <- length(common_cols)
  
  if (length(common_cols) >= 2) {
    data1_numeric <- as.matrix(data1[, common_cols, drop=FALSE])
    data2_numeric <- as.matrix(data2[, common_cols, drop=FALSE])
    
    data1_clean <- data1_numeric[complete.cases(data1_numeric), , drop=FALSE]
    data2_clean <- data2_numeric[complete.cases(data2_numeric), , drop=FALSE]
    
    result$data1_rows <- nrow(data1_clean)
    result$data2_rows <- nrow(data2_clean)
    
    # Check variances
    col_vars1 <- apply(data1_clean, 2, var, na.rm = TRUE)
    col_vars2 <- apply(data2_clean, 2, var, na.rm = TRUE)
    
    result$zero_var_cols1 <- common_cols[col_vars1 == 0 | is.na(col_vars1)]
    result$zero_var_cols2 <- common_cols[col_vars2 == 0 | is.na(col_vars2)]
    
    # Check correlation matrix condition
    if (nrow(data2_clean) > ncol(data2_clean) && length(result$zero_var_cols2) == 0) {
      cov_matrix <- cov(data2_clean)
      result$cov_det <- det(cov_matrix)
      eigenvals <- eigen(cov_matrix, only.values = TRUE)$values
      result$condition_number <- max(eigenvals) / min(eigenvals)
      result$min_eigenval <- min(eigenvals)
    }
  }
  
  return(result)
}

# ---- Additional Multivariate Analysis Functions ----
compute_robust_mahalanobis <- function(data1, data2, method = "MCD", keep_outliers = FALSE, selected_columns = NULL) {
  # Robust Mahalanobis using Minimum Covariance Determinant (MCD)
  if (!requireNamespace("robustbase", quietly = TRUE)) {
    install.packages("robustbase")
  }
  
  # Use selected columns or all numeric columns that exist in both datasets
  if (!is.null(selected_columns) && length(selected_columns) > 0) {
    # Use user-selected columns
    available_cols1 <- colnames(data1)[colnames(data1) %in% selected_columns]
    available_cols2 <- colnames(data2)[colnames(data2) %in% selected_columns]
    common_cols <- intersect(available_cols1, available_cols2)
  } else {
    # Use all numeric columns (original behavior)
    numeric_cols1 <- sapply(data1, is.numeric)
    numeric_cols2 <- sapply(data2, is.numeric)
    common_cols <- intersect(colnames(data1)[numeric_cols1], colnames(data2)[numeric_cols2])
  }
  
  if (length(common_cols) < 2) {
    stop("Need at least 2 common numeric columns")
  }
  
  data1_numeric <- as.matrix(data1[, common_cols, drop=FALSE])
  data2_numeric <- as.matrix(data2[, common_cols, drop=FALSE])
  
  data1_clean <- data1_numeric[complete.cases(data1_numeric), , drop=FALSE]
  data2_clean <- data2_numeric[complete.cases(data2_numeric), , drop=FALSE]
  
  if (nrow(data1_clean) == 0 || nrow(data2_clean) == 0) {
    stop("No complete cases found")
  }
  
  # Check for sufficient observations for MCD
  if (nrow(data2_clean) < 2 * ncol(data2_clean)) {
    stop(paste("Insufficient observations for robust MCD estimation. Need at least", 2 * ncol(data2_clean), "rows, but have", nrow(data2_clean)))
  }
  
  # Check for zero variance columns
  col_vars <- apply(data2_clean, 2, var, na.rm = TRUE)
  zero_var_cols <- common_cols[col_vars == 0 | is.na(col_vars)]
  if (length(zero_var_cols) > 0) {
    stop(paste("Columns with zero variance in reference dataset:", paste(zero_var_cols, collapse = ", ")))
  }
  
  # Use robust covariance estimation
  robust_cov <- robustbase::covMcd(data2_clean)
  
  # Calculate robust Mahalanobis distances
  robust_distances <- mahalanobis(data1_clean, 
                                  center = robust_cov$center, 
                                  cov = robust_cov$cov)
  
  # Calculate threshold using chi-square distribution
  df <- length(common_cols)
  threshold_95 <- qchisq(0.95, df = df)
  
  # Identify outliers
  outlier_indices <- robust_distances > threshold_95
  
  return(list(
    distances = robust_distances,
    threshold_95 = threshold_95,
    outlier_count = sum(outlier_indices),
    total_points = length(robust_distances),
    method = method,
    outlier_indices = outlier_indices,
    keep_outliers = keep_outliers,
    common_cols = common_cols
  ))
}

compute_isolation_forest <- function(data1, data2, contamination = 0.1, keep_outliers = FALSE, selected_columns = NULL) {
  # Isolation Forest for outlier detection
  if (!requireNamespace("isotree", quietly = TRUE)) {
    install.packages("isotree")
  }
  
  # Use selected columns or all numeric columns that exist in both datasets
  if (!is.null(selected_columns) && length(selected_columns) > 0) {
    # Use user-selected columns
    available_cols1 <- colnames(data1)[colnames(data1) %in% selected_columns]
    available_cols2 <- colnames(data2)[colnames(data2) %in% selected_columns]
    common_cols <- intersect(available_cols1, available_cols2)
  } else {
    # Use all numeric columns (original behavior)
    numeric_cols1 <- sapply(data1, is.numeric)
    numeric_cols2 <- sapply(data2, is.numeric)
    common_cols <- intersect(colnames(data1)[numeric_cols1], colnames(data2)[numeric_cols2])
  }
  
  if (length(common_cols) < 2) {
    stop("Need at least 2 common numeric columns")
  }
  
  data1_numeric <- as.matrix(data1[, common_cols, drop=FALSE])
  data2_numeric <- as.matrix(data2[, common_cols, drop=FALSE])
  
  data1_clean <- data1_numeric[complete.cases(data1_numeric), , drop=FALSE]
  data2_clean <- data2_numeric[complete.cases(data2_numeric), , drop=FALSE]
  
  if (nrow(data1_clean) == 0 || nrow(data2_clean) == 0) {
    stop("No complete cases found")
  }
  
  # Train isolation forest on reference data
  iso_model <- isotree::isolation.forest(data2_clean, contamination = contamination)
  
  # Predict anomaly scores for data1
  anomaly_scores <- predict(iso_model, data1_clean)
  
  # Determine threshold (top contamination% are outliers)
  threshold <- quantile(anomaly_scores, 1 - contamination)
  
  # Identify outliers
  outlier_indices <- anomaly_scores > threshold
  
  return(list(
    scores = anomaly_scores,
    threshold = threshold,
    outlier_count = sum(outlier_indices),
    total_points = length(anomaly_scores),
    method = "Isolation Forest",
    common_cols = common_cols,
    outlier_indices = outlier_indices,
    keep_outliers = keep_outliers
  ))
}

# ---- Statistical Filtering Functions ----
apply_iqr_filter <- function(data, cols, multiplier = 1.5, keep_outliers = FALSE) {
  # IQR-based outlier filtering
  filtered_data <- data
  outlier_indices <- logical(nrow(data))
  
  for (col in cols) {
    if (is.numeric(data[[col]])) {
      q1 <- quantile(data[[col]], 0.25, na.rm = TRUE)
      q3 <- quantile(data[[col]], 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      lower_bound <- q1 - multiplier * iqr
      upper_bound <- q3 + multiplier * iqr
      
      col_outliers <- data[[col]] < lower_bound | data[[col]] > upper_bound
      outlier_indices <- outlier_indices | col_outliers
    }
  }
  
  if (keep_outliers) {
    # Keep only outliers
    filtered_data <- data[outlier_indices, ]
  } else {
    # Remove outliers
    filtered_data <- data[!outlier_indices, ]
  }
  
  return(filtered_data)
}

apply_zscore_filter <- function(data, cols, threshold = 3, keep_outliers = FALSE) {
  # Z-score based outlier filtering
  filtered_data <- data
  outlier_indices <- logical(nrow(data))
  
  for (col in cols) {
    if (is.numeric(data[[col]])) {
      z_scores <- abs(scale(data[[col]]))
      col_outliers <- z_scores > threshold
      outlier_indices <- outlier_indices | col_outliers
    }
  }
  
  if (keep_outliers) {
    # Keep only outliers
    filtered_data <- data[outlier_indices, ]
  } else {
    # Remove outliers
    filtered_data <- data[!outlier_indices, ]
  }
  
  return(filtered_data)
}

apply_mad_filter <- function(data, cols, threshold = 3, keep_outliers = FALSE) {
  # Median Absolute Deviation (MAD) based filtering
  filtered_data <- data
  outlier_indices <- logical(nrow(data))
  
  for (col in cols) {
    if (is.numeric(data[[col]])) {
      median_val <- median(data[[col]], na.rm = TRUE)
      mad_val <- mad(data[[col]], na.rm = TRUE)
      lower_bound <- median_val - threshold * mad_val
      upper_bound <- median_val + threshold * mad_val
      
      col_outliers <- data[[col]] < lower_bound | data[[col]] > upper_bound
      outlier_indices <- outlier_indices | col_outliers
    }
  }
  
  if (keep_outliers) {
    # Keep only outliers
    filtered_data <- data[outlier_indices, ]
  } else {
    # Remove outliers
    filtered_data <- data[!outlier_indices, ]
  }
  
  return(filtered_data)
}

# ---- Report Generation Function ----
generate_report <- function(stats, correlation, plot_files, output_path = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
  if (is.null(output_path)) {
    output_path <- paste0("Ternary_Analysis_Report_", timestamp, ".html")
  }
  
  # Create a simple HTML report since we don't have the Rmd template
  html_content <- paste0(
    "<!DOCTYPE html>",
    "<html><head><title>Ternary Analysis Report</title></head>",
    "<body><h1>Ternary Analysis Report</h1>",
    "<p>Generated on: ", timestamp, "</p>",
    "<h2>Statistics</h2>",
    "<pre>", capture.output(print(stats)), "</pre>",
    "<h2>Correlation Matrix</h2>",
    "<pre>", capture.output(print(correlation)), "</pre>",
    "</body></html>"
  )
  
  writeLines(html_content, output_path)
  return(output_path)
}

# ---- Main Ternary Plot Function ----
# ... (copy unchanged from App3.R) ...

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
    reference_data = NULL,
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
    mahalanobis_reference = "dataset2",
    selected_columns = NULL,
    show_filtered_points = TRUE,
    draw_inclusion_area = FALSE,
    inclusion_shape = "ellipse",
    inclusion_confidence = 95
) {
  
  setwd(working_dir)
  M <- read.xlsx(xlsx_file, sheet = 1)
  cat("DEBUG: Initial data loaded, dimensions:", dim(M), "\n")
  cat("DEBUG: Available columns:", paste(colnames(M), collapse = ", "), "\n")
  if (!preview) print(colnames(M))
  
  apply_filter <- function(df, col, filter) {
    if (is.null(filter)) return(df)
    expr <- paste0("df[['", col, "']] ", filter)
    df[eval(parse(text = expr)), , drop = FALSE]
  }
  
  label_with_element <- function(el) {
    if (is.null(el$col)) return("")
    if (length(el$col) > 1) paste(el$col, collapse = "+") else el$col
  }
  label_with_filter <- function(el) {
    if (is.null(el$col)) return("")
    col_label <- if (length(el$col) > 1) paste(el$col, collapse = "+") else el$col
    if (!is.null(el$filter) && nzchar(el$filter)) {
      paste0(col_label, " (", el$filter, ")")
    } else {
      col_label
    }
  }
  
  # Apply individual element filtering (A, B, C) with individual filters
  apply_individual_filters <- function(data, element, individual_filters, element_name) {
    if (is.null(element) || is.null(element$col) || length(element$col) == 0) {
      cat("DEBUG: No", element_name, "elements selected\n")
      return(data)
    }
    
    cat("DEBUG: Processing", element_name, "elements:", paste(element$col, collapse = ", "), "\n")
    cat("DEBUG: Individual filters for", element_name, ":", if(is.null(individual_filters)) "NULL" else paste(names(individual_filters), collapse = ", "), "\n")
    
    # If no individual filters provided, use the old single filter method
    if (is.null(individual_filters) || length(individual_filters) == 0) {
      if (!is.null(element$filter) && nzchar(element$filter)) {
        if (length(element$col) > 1) {
          # For multiple columns, apply same filter to each column individually
          data[, element$col] <- lapply(data[, element$col, drop = FALSE], as.numeric)
          keep_rows <- rep(TRUE, nrow(data))
          for (col in element$col) {
            col_filter <- eval(parse(text = paste0("data[['", col, "']] ", element$filter)))
            keep_rows <- keep_rows & col_filter
          }
          data <- data[keep_rows, , drop = FALSE]
        } else {
          data[, element$col] <- as.numeric(data[, element$col])
          data <- apply_filter(data, element$col, element$filter)
        }
        if (!preview) {
          cat("After filtering", paste(element$col, collapse = "+"), "with filter", element$filter, ":\n")
          print(dim(data))
        }
      }
    } else {
      # Apply individual filters to each element
      data[, element$col] <- lapply(data[, element$col, drop = FALSE], as.numeric)
      keep_rows <- rep(TRUE, nrow(data))
      
      for (col in element$col) {
        if (col %in% names(individual_filters) && !is.null(individual_filters[[col]]) && nzchar(individual_filters[[col]])) {
          col_filter <- eval(parse(text = paste0("data[['", col, "']] ", individual_filters[[col]])))
          keep_rows <- keep_rows & col_filter
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
    
    cat("DEBUG: After", element_name, "filtering, data dimensions:", dim(data), "\n")
    return(data)
  }
  
  # Apply filtering for elements A, B, C
  M <- apply_individual_filters(M, element_A, individual_filters_A, "A")
  M <- apply_individual_filters(M, element_B, individual_filters_B, "B")
  M <- apply_individual_filters(M, element_C, individual_filters_C, "C")
  
  cat("DEBUG: After all element filtering, data dimensions:", dim(M), "\n")
  
  # Apply optional parameter filtering (sum filtering for these)
  for (el in list(optional_param1, optional_param2)) {
    if (!is.null(el) && !is.null(el$col) && !is.null(el$filter) && nzchar(el$filter)) {
      if (length(el$col) > 1) {
        M[, el$col] <- lapply(M[, el$col, drop = FALSE], as.numeric)
        row_sum <- rowSums(M[, el$col, drop = FALSE], na.rm = TRUE)
        expr <- paste0("row_sum ", el$filter)
        M <- M[eval(parse(text = expr)), , drop = FALSE]
      } else {
        M[, el$col] <- as.numeric(M[, el$col])
        M <- apply_filter(M, el$col, el$filter)
      }
      if (!preview) {
        cat("After filtering", paste(el$col, collapse = "+"), "with filter", el$filter, ":\n")
        print(dim(M))
        print(colnames(M))
      }
    }
  }
  if (nrow(M) == 0) stop("No data left after filtering. Please check your filter conditions and data.")
  
  cat("DEBUG: After optional parameter filtering, data dimensions:", dim(M), "\n")
  
  # Apply statistical filtering methods
  all_selected_elements <- c(element_A$col, element_B$col, element_C$col)
  
  if (use_iqr_filter) {
    M <- apply_iqr_filter(M, all_selected_elements, keep_outliers = keep_outliers_iqr)
    if (!preview) cat("Applied IQR filtering\n")
  }
  
  if (use_zscore_filter) {
    M <- apply_zscore_filter(M, all_selected_elements, keep_outliers = keep_outliers_zscore)
    if (!preview) cat("Applied Z-score filtering\n")
  }
  
  if (use_mad_filter) {
    M <- apply_mad_filter(M, all_selected_elements, keep_outliers = keep_outliers_mad)
    if (!preview) cat("Applied MAD filtering\n")
  }
  
  cat("DEBUG: After statistical filtering, data dimensions:", dim(M), "\n")
  
  # Apply Mahalanobis distance filtering if requested
  if (use_mahalanobis) {
    # Determine reference dataset
    actual_reference_data <- if (mahalanobis_reference == "self") M else reference_data
    
    # Skip if dataset2 reference is selected but no reference data provided
    if (mahalanobis_reference == "dataset2" && is.null(reference_data)) {
      if (!preview) cat("Skipping multivariate analysis: No reference dataset provided\n")
    } else {
      tryCatch({
        if (use_robust_mahalanobis) {
          mahal_result <- compute_robust_mahalanobis(M, actual_reference_data, keep_outliers = keep_outliers_robust, selected_columns = selected_columns)
          threshold_to_use <- mahal_result$threshold_95
          keep_indices <- if (keep_outliers_robust) {
            mahal_result$outlier_indices
          } else {
            !mahal_result$outlier_indices
          }
        } else if (use_isolation_forest) {
          iso_result <- compute_isolation_forest(M, actual_reference_data, keep_outliers = keep_outliers_isolation, selected_columns = selected_columns)
          keep_indices <- if (keep_outliers_isolation) {
            iso_result$outlier_indices
          } else {
            !iso_result$outlier_indices
          }
        } else {
          mahal_result <- compute_mahalanobis_distance(M, actual_reference_data, lambda, omega, keep_outliers = keep_outliers_mahalanobis, custom_mdthresh = custom_mdthresh, selected_columns = selected_columns)
          # Use the new threshold formula
          threshold_to_use <- mahal_result$MDthresh
          keep_indices <- if (keep_outliers_mahalanobis) {
            mahal_result$outlier_indices
          } else {
            !mahal_result$outlier_indices
          }
        }
        
        # Apply the filtering
        common_cols <- if (use_robust_mahalanobis) mahal_result$common_cols else 
          if (use_isolation_forest) iso_result$common_cols else mahal_result$common_cols
        M_numeric <- as.matrix(M[, common_cols, drop=FALSE])
        M_clean <- M_numeric[complete.cases(M_numeric), , drop=FALSE]
        original_indices <- which(complete.cases(M_numeric))[keep_indices]
        M <- M[original_indices, , drop = FALSE]
        
        if (!preview) {
          method_name <- if (use_robust_mahalanobis) "Robust Mahalanobis" else
            if (use_isolation_forest) "Isolation Forest" else "Mahalanobis"
          ref_name <- if (mahalanobis_reference == "self") "self-reference" else "Dataset 2 reference"
          cat("After", method_name, "filtering (", ref_name, "):\n")
          cat("Outlier points remaining:", sum(keep_indices), "\n")
          cat("Columns used:", paste(common_cols, collapse = ", "), "\n")
        }
      }, error = function(e) {
        if (!preview) cat("Multivariate filtering failed:", e$message, "\n")
      })
    }
  }
  
  cat("DEBUG: After multivariate filtering, data dimensions:", dim(M), "\n")
  
  if (is.null(output_dir)) {
    plots_dir <- file.path(getwd(), "plots2")
  } else {
    plots_dir <- output_dir
  }
  if (!dir.exists(plots_dir)) dir.create(plots_dir, recursive = TRUE)
  if (!is.null(xlsx_display_name)) {
    file_base <- gsub("\\.xlsx$", "", basename(xlsx_display_name))
  } else {
    file_base <- gsub("\\.xlsx$", "", basename(xlsx_file))
  }
  plot_folder_name <- paste0("charge", file_base)
  custom_folder <- file.path(plots_dir, plot_folder_name)
  if (dir.exists(custom_folder)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    custom_folder <- file.path(plots_dir, paste0(plot_folder_name, "_", timestamp))
  }
  dir.create(custom_folder, recursive = TRUE, showWarnings = FALSE)
  
  all_selected_elements <- c(element_A$col, element_B$col, element_C$col)
  needed_columns <- unique(c(all_selected_elements, 
                             if (!is.null(optional_param1)) optional_param1$col,
                             if (!is.null(optional_param2)) optional_param2$col))
  if (!all(needed_columns %in% colnames(M))) {
    stop("Error: One or more selected elements/parameters are missing in the dataset.\nAvailable columns: ", 
         paste(colnames(M), collapse = ", "))
  }
  matrika <- M[, needed_columns]
  cat("DEBUG: Matrika dimensions after column selection:", dim(matrika), "\n")
  matrika[, all_selected_elements] <- lapply(matrika[, all_selected_elements, drop = FALSE], as.numeric)
  row_sums <- rowSums(matrika[, all_selected_elements, drop = FALSE], na.rm = TRUE)
  matrika <- matrika[row_sums > 0, , drop = FALSE]
  cat("DEBUG: Matrika dimensions after removing zero-sum rows:", dim(matrika), "\n")
  if (nrow(matrika) == 0) stop("Error: No valid data left after filtering zero-sum ternary rows.")
  matrika <- na.omit(matrika)
  cat("DEBUG: Matrika dimensions after na.omit:", dim(matrika), "\n")
  
  matrika[] <- lapply(matrika, as.numeric)
  matrika <- as.matrix(matrika)
  # First set: normalized
  ternary_data <- data.frame(
    A = if (length(element_A$col) == 1) matrika[, element_A$col] else rowSums(matrika[, element_A$col, drop = FALSE], na.rm = TRUE),
    B = if (length(element_B$col) == 1) matrika[, element_B$col] else rowSums(matrika[, element_B$col, drop = FALSE], na.rm = TRUE),
    C = if (length(element_C$col) == 1) matrika[, element_C$col] else rowSums(matrika[, element_C$col, drop = FALSE], na.rm = TRUE)
  )
  Total <- ternary_data$A + ternary_data$B + ternary_data$C
  fA <- ternary_data$A / Total
  fB <- ternary_data$B / Total
  fC <- 1 - (fA + fB)
  ternary_points1 <- data.frame(A = fA, B = fB, C = fC)
  ternary_data <- ternary_data / rowSums(ternary_data)
  cat("DEBUG: Ternary points dimensions:", dim(ternary_points1), "\n")
  cat("DEBUG: First few ternary points:\n")
  print(head(ternary_points1))
  if (nrow(ternary_data) == 0) stop("Error: No valid data left after filtering.")
  
  labels_A <- label_with_element(element_A)
  labels_B <- label_with_element(element_B)
  labels_C <- label_with_element(element_C)
  clean_labels_A <- gsub("\\.\\(Wt%\\)", "", labels_A)
  clean_labels_B <- gsub("\\.\\(Wt%\\)", "", labels_B)
  clean_labels_C <- gsub("\\.\\(Wt%\\)", "", labels_C)
  opt1_label <- if (!is.null(optional_param1)) label_with_filter(optional_param1) else NULL
  opt2_label <- if (!is.null(optional_param2)) label_with_filter(optional_param2) else NULL
  opt1_legend_title <- if (!is.null(optional_param1)) label_with_filter(optional_param1) else NULL
  opt2_legend_title <- if (!is.null(optional_param2)) label_with_filter(optional_param2) else NULL
  
  # Build comprehensive plot title with all applied methods
  title_parts <- c(paste0("Ternary Plot of ", clean_labels_A, ", ", clean_labels_B, ", ", clean_labels_C))
  
  # Add optional parameters
  if (!is.null(opt1_label)) title_parts <- c(title_parts, opt1_label)
  if (!is.null(opt2_label)) title_parts <- c(title_parts, opt2_label)
  
  # Add statistical filtering methods
  stat_methods <- c()
  if (use_iqr_filter) stat_methods <- c(stat_methods, paste0("IQR", if (keep_outliers_iqr) "(outliers only)" else "(filtered)"))
  if (use_zscore_filter) stat_methods <- c(stat_methods, paste0("Z-score", if (keep_outliers_zscore) "(outliers only)" else "(filtered)"))
  if (use_mad_filter) stat_methods <- c(stat_methods, paste0("MAD", if (keep_outliers_mad) "(outliers only)" else "(filtered)"))
  if (length(stat_methods) > 0) title_parts <- c(title_parts, paste("Stats:", paste(stat_methods, collapse = "+")))
  
  # Add multivariate analysis methods
  mv_methods <- c()
  if (use_mahalanobis && !is.null(reference_data)) {
    if (use_robust_mahalanobis) {
      mv_methods <- c(mv_methods, paste0("RobustMD", if (keep_outliers_robust) "(outliers only)" else "(filtered)"))
    } else if (use_isolation_forest) {
      mv_methods <- c(mv_methods, paste0("IsoForest", if (keep_outliers_isolation) "(outliers only)" else "(filtered)"))
    } else {
      mv_methods <- c(mv_methods, paste0("Mahalanobis", if (keep_outliers_mahalanobis) "(outliers only)" else "(filtered)"))
    }
  }
  if (length(mv_methods) > 0) title_parts <- c(title_parts, paste("Multivariate:", paste(mv_methods, collapse = "+")))
  
  # Add plot options info
  plot_options <- c()
  if (show_filtered_points) plot_options <- c(plot_options, "show_outliers")
  if (draw_inclusion_area) plot_options <- c(plot_options, paste0("inclusion_", inclusion_shape, "_", inclusion_confidence, "%"))
  if (length(plot_options) > 0) title_parts <- c(title_parts, paste("Plot:", paste(plot_options, collapse = "+")))
  
  # Add individual filter info if any applied
  filter_info <- c()
  if (!is.null(individual_filters_A) && length(individual_filters_A) > 0) {
    active_filters_A <- individual_filters_A[!sapply(individual_filters_A, is.null) & nzchar(individual_filters_A)]
    if (length(active_filters_A) > 0) {
      filter_info <- c(filter_info, paste0("A:", length(active_filters_A), "f"))
    }
  }
  if (!is.null(individual_filters_B) && length(individual_filters_B) > 0) {
    active_filters_B <- individual_filters_B[!sapply(individual_filters_B, is.null) & nzchar(individual_filters_B)]
    if (length(active_filters_B) > 0) {
      filter_info <- c(filter_info, paste0("B:", length(active_filters_B), "f"))
    }
  }
  if (!is.null(individual_filters_C) && length(individual_filters_C) > 0) {
    active_filters_C <- individual_filters_C[!sapply(individual_filters_C, is.null) & nzchar(individual_filters_C)]
    if (length(active_filters_C) > 0) {
      filter_info <- c(filter_info, paste0("C:", length(active_filters_C), "f"))
    }
  }
  if (length(filter_info) > 0) title_parts <- c(title_parts, paste("Filters:", paste(filter_info, collapse = ",")))
  
  title_parts <- c(title_parts, paste("charge", file_base))
  plot_title <- paste(title_parts, collapse = ", ")
  
  file_name <- gsub("_+", "_", gsub("[^A-Za-z0-9]+", "_", plot_title))
  file_name <- gsub("^_+|_+$", "", file_name) 
  file_name <- paste0(file_name, ".", output_format)
  file_path <- file.path(custom_folder, file_name)
  file_path <- normalizePath(file_path, winslash = "/", mustWork = FALSE)
  if (!preview) {
    if (output_format == "png") {
      png(file_path, width = 1200, height = 1200, res = 200)
    } else if (output_format == "jpeg") {
      jpeg(file_path, width = 1200, height = 1200, res = 200, quality = 95)
    } else if (output_format == "pdf") {
      pdf(file_path, width = 12, height = 12)
    } else if (output_format == "tiff") {
      tiff(file_path, width = 1200, height = 1200, res = 200, compression = "lzw")
    }
  }
  
  # Optional param 1: point size or point type
  if (!is.null(optional_param1)) {
    param1_values <- matrika[, optional_param1$col]
    
    if (optional_param1_representation == "point_size") {
      # Point size representation
      minPointSize <- 0.2
      maxSize <- 2.5
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
    pointSize <- 0.04  # 10 times smaller than previous default of 0.4
    pointType <- 16
  }
  
  # Optional param 2: color (default: "grey")
  if (!is.null(optional_param2)) {
    param2_values <- matrika[, optional_param2$col]
    param2_breaks <- quantile(param2_values, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
    param2_breaks <- unique(param2_breaks) # <-- make breaks unique
    
    if (length(param2_breaks) < 2) {
      # All values identical or not enough to make bins; fallback coloring
      param2_bins <- factor(rep(1, length(param2_values)), labels = "All")
      n_colors <- 1
    } else {
      param2_bins <- cut(param2_values, breaks = param2_breaks, include.lowest = TRUE)
      n_colors <- length(levels(param2_bins))
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
      param2_colors <- rep("black", n_colors)
    }
    pointCol <- param2_colors[as.numeric(param2_bins)]
    
  } else {
    pointCol <- "black"
  }
  
  cat("DEBUG: Point size range:", range(pointSize), "\n")
  cat("DEBUG: Point type range:", range(pointType), "\n")
  cat("DEBUG: Point color unique values:", unique(pointCol), "\n")
  
  # Simple plotting approach - plot all points
  cat("DEBUG: About to plot all points, count:", nrow(ternary_points1), "\n")
  
  TernaryPlot(
    atip = clean_labels_A, btip = clean_labels_B, ctip = clean_labels_C,
    alab = paste(clean_labels_A, "→"), blab = paste(clean_labels_B, "→"), clab = paste("←", clean_labels_C),
    col = "white", 
    grid.lines = 5, 
    grid.lty = "dotted", 
    grid.minor.lines = 1, 
    grid.minor.lty = "dotted"
  )
  
  # Simple plotting - just plot all points
  cat("DEBUG: Plotting all points with TernaryPoints\n")
  TernaryPoints(ternary_points1, cex = pointSize, col = pointCol, pch = pointType)
  cat("DEBUG: All points plotted successfully\n")
  
  title(main = plot_title, cex.main = 0.8)
  if (!is.null(optional_param1)) {
    if (optional_param1_representation == "point_size") {
      PlotTools::SizeLegend(
        "topright",
        width = c(min(pointSize), max(pointSize)),  
        lend = "round",  
        legend = paste(
          signif(seq(max(param1_values, na.rm = TRUE), min(param1_values, na.rm = TRUE), length.out = 5), digits = 3),
          "\u03bcm"
        ),
        title = opt1_legend_title,
        bty = "n",  
        cex = 0.8
      )
    } else if (optional_param1_representation == "point_type") {
      legend("topright",
             legend = levels(param1_bins),
             pch = point_types[seq_along(levels(param1_bins))],
             title = opt1_legend_title,
             cex = 0.8
      )
    }
  }
  if (!is.null(optional_param2)) {
    legend("topleft",
           legend = levels(param2_bins),
           col = param2_colors,
           pch = 20,
           title = opt2_legend_title,
           cex = 0.8
    )
  }
  
  # Add notes section to bottom right of plot
  notes_text <- c()
  
  # Add elements information
  if (!is.null(element_A$col) && length(element_A$col) > 0) {
    notes_text <- c(notes_text, paste("Element A:", paste(element_A$col, collapse = ", ")))
  }
  if (!is.null(element_B$col) && length(element_B$col) > 0) {
    notes_text <- c(notes_text, paste("Element B:", paste(element_B$col, collapse = ", ")))
  }
  if (!is.null(element_C$col) && length(element_C$col) > 0) {
    notes_text <- c(notes_text, paste("Element C:", paste(element_C$col, collapse = ", ")))
  }
  
  # Add individual filters information
  if (!is.null(individual_filters_A) && length(individual_filters_A) > 0) {
    active_filters_A <- individual_filters_A[!sapply(individual_filters_A, is.null) & nzchar(individual_filters_A)]
    if (length(active_filters_A) > 0) {
      filter_details <- paste(names(active_filters_A), active_filters_A, sep = " ", collapse = ", ")
      notes_text <- c(notes_text, paste("Filters A:", filter_details))
    }
  }
  if (!is.null(individual_filters_B) && length(individual_filters_B) > 0) {
    active_filters_B <- individual_filters_B[!sapply(individual_filters_B, is.null) & nzchar(individual_filters_B)]
    if (length(active_filters_B) > 0) {
      filter_details <- paste(names(active_filters_B), active_filters_B, sep = " ", collapse = ", ")
      notes_text <- c(notes_text, paste("Filters B:", filter_details))
    }
  }
  if (!is.null(individual_filters_C) && length(individual_filters_C) > 0) {
    active_filters_C <- individual_filters_C[!sapply(individual_filters_C, is.null) & nzchar(individual_filters_C)]
    if (length(active_filters_C) > 0) {
      filter_details <- paste(names(active_filters_C), active_filters_C, sep = " ", collapse = ", ")
      notes_text <- c(notes_text, paste("Filters C:", filter_details))
    }
  }
  
  # Add optional parameters information
  if (!is.null(optional_param1)) {
    notes_text <- c(notes_text, paste("Optional Param 1:", paste(optional_param1$col, collapse = ", "), 
                                      if (!is.null(optional_param1$filter) && nzchar(optional_param1$filter)) paste("(filter:", optional_param1$filter, ")") else "",
                                      "representation:", optional_param1_representation))
  }
  if (!is.null(optional_param2)) {
    notes_text <- c(notes_text, paste("Optional Param 2:", paste(optional_param2$col, collapse = ", "), 
                                      if (!is.null(optional_param2$filter) && nzchar(optional_param2$filter)) paste("(filter:", optional_param2$filter, ")") else "",
                                      "color palette:", color_palette))
  }
  
  # Add statistical filtering information
  stat_methods <- c()
  if (use_iqr_filter) stat_methods <- c(stat_methods, paste0("IQR", if (keep_outliers_iqr) "(outliers only)" else "(filtered)"))
  if (use_zscore_filter) stat_methods <- c(stat_methods, paste0("Z-score", if (keep_outliers_zscore) "(outliers only)" else "(filtered)"))
  if (use_mad_filter) stat_methods <- c(stat_methods, paste0("MAD", if (keep_outliers_mad) "(outliers only)" else "(filtered)"))
  if (length(stat_methods) > 0) {
    notes_text <- c(notes_text, paste("Statistical filtering:", paste(stat_methods, collapse = ", ")))
  }
  
  # Add multivariate analysis information
  mv_methods <- c()
  if (use_mahalanobis) {
    if (use_robust_mahalanobis) {
      mv_methods <- c(mv_methods, paste0("Robust Mahalanobis", if (keep_outliers_robust) "(outliers only)" else "(filtered)"))
    } else if (use_isolation_forest) {
      mv_methods <- c(mv_methods, paste0("Isolation Forest", if (keep_outliers_isolation) "(outliers only)" else "(filtered)"))
    } else {
      mv_methods <- c(mv_methods, paste0("Mahalanobis", if (keep_outliers_mahalanobis) "(outliers only)" else "(filtered)"))
    }
    if (!is.null(selected_columns) && length(selected_columns) > 0) {
      mv_methods[length(mv_methods)] <- paste0(mv_methods[length(mv_methods)], " (cols:", paste(selected_columns, collapse = ", "), ")")
    }
    if (mahalanobis_reference == "self") {
      mv_methods[length(mv_methods)] <- paste0(mv_methods[length(mv_methods)], " (self-ref)")
    } else {
      mv_methods[length(mv_methods)] <- paste0(mv_methods[length(mv_methods)], " (dataset2-ref)")
    }
  }
  if (length(mv_methods) > 0) {
    notes_text <- c(notes_text, paste("Multivariate analysis:", paste(mv_methods, collapse = ", ")))
  }
  
  # Add plot options information
  plot_options <- c()
  if (show_filtered_points) plot_options <- c(plot_options, "show filtered points")
  if (draw_inclusion_area) plot_options <- c(plot_options, paste0("inclusion area (", inclusion_shape, ", ", inclusion_confidence, "%)"))
  if (length(plot_options) > 0) {
    notes_text <- c(notes_text, paste("Plot options:", paste(plot_options, collapse = ", ")))
  }
  
  # Add output format information
  notes_text <- c(notes_text, paste("Output format:", output_format))
  
  # Add timestamp
  notes_text <- c(notes_text, paste("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  
  # Add notes section to plot using proper ternary coordinates
  if (length(notes_text) > 0) {
    # Use legend to display notes in bottom right corner
    # Limit the number of notes to avoid overcrowding
    max_notes <- min(length(notes_text), 8)
    notes_display <- notes_text[1:max_notes]
    
    # Create a simple legend with notes
    legend("bottomright", 
           legend = notes_display,
           cex = 0.4,  # Very small font
           bty = "n",  # No box
           text.col = "darkblue",
           title = "Plot Notes:",
           title.col = "darkblue",
           title.adj = 0,
           x.intersp = 0.5,
           y.intersp = 0.3)
  }
  
  if (!preview) {
    dev.off()
    if (file.exists(file_path)) print(paste("Plot successfully saved to:", file_path))
  }
  
  cat("DEBUG: Function completed successfully\n")
}

# ---- Shiny App ----
run_ternary_gui <- function(xlsx_file = NULL, working_dir = default_working_dir, output_dir = default_output_dir) {
  if (!is.null(xlsx_file)) {
    M <- openxlsx::read.xlsx(xlsx_file, sheet = 1)
    col_names <- colnames(M)
  } else {
    M <- NULL
    col_names <- character(0)
  }
  
  ui <- fluidPage(
    titlePanel(
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        "Custom Ternary Builder v6 - Individual Element Filtering",
        div(
          style = "display: flex; gap: 10px; align-items: center;",
          actionButton("plot1", "Save Plot 1", class = "btn-primary"),
          actionButton("plot2", "Save Plot 2", class = "btn-primary"),
          actionButton("plot_both", "Save Both Plots", class = "btn-success"),
          actionButton("help_button", "?", 
                       style = "background-color: #007bff; color: white; border: none; border-radius: 50%; width: 30px; height: 30px; font-weight: bold; font-size: 16px;",
                       title = "Help")
        )
      )
    ),
    fluidRow(
      column(12,
             h3("Directory Settings"),
             fluidRow(
               column(6,
                      shinyDirButton("working_dir", "Select Working Directory", "Please select a working directory"),
                      verbatimTextOutput("working_dir_text")
               ),
               column(6,
                      shinyDirButton("output_dir", "Select Output Directory", "Please select an output directory"),
                      verbatimTextOutput("output_dir_text")
               )
             )
      )
    ),
    hr(),
    tabsetPanel(
      tabPanel("Ternary Plots",
               fluidRow(
                 column(12,
                        fluidRow(
                          column(6, h3("Dataset 1 (Primary)")),
                          column(6, h3("Dataset 2 (Reference)"))
                        ),
                        fluidRow(
                          column(6,
                                 fileInput("xlsx_file1", "Choose Primary XLSX File", accept = c(".xlsx"))
                          ),
                          column(6,
                                 fileInput("xlsx_file2", "Choose Reference XLSX File", accept = c(".xlsx"))
                          )
                        )
                 )
               ),
               # Plot Previews moved to top
               fluidRow(
                 column(12,
                        hr(),
                        h3("Plot Previews"),
                        fluidRow(
                          column(6,
                                 textOutput("preview_info1"),
                                 jqui_resizable(plotOutput("ternary_preview1", height = "500px"))
                          ),
                          column(6,
                                 textOutput("preview_info2"),
                                 jqui_resizable(plotOutput("ternary_preview2", height = "500px"))
                          )
                        )
                 )
               ),
               fluidRow(
                 column(12,
                        fluidRow(
                          column(6,
                                 div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
                                     h4(style = "color: #dc3545; margin-top: 0;", "Element A (Required)"),
                                     selectInput("element_A1", "Element A (multiple allowed):", choices = col_names, multiple = TRUE),
                                     uiOutput("dynamic_filters_A1"),
                                     helpText("Note: Each selected element can have its own filter condition (logical AND between elements)"),
                                     helpText("Example: Fe > 10, Al > 5, Si > 0 (each element gets its own threshold)")
                                 ),
                                 div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
                                     h4(style = "color: #dc3545; margin-top: 0;", "Element B (Required)"),
                                     selectInput("element_B1", "Element B (multiple allowed):", choices = col_names, multiple = TRUE),
                                     uiOutput("dynamic_filters_B1")
                                 ),
                                 div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
                                     h4(style = "color: #dc3545; margin-top: 0;", "Element C (Required)"),
                                     selectInput("element_C1", "Element C (multiple allowed):", choices = col_names, multiple = TRUE),
                                     uiOutput("dynamic_filters_C1")
                                 ),
                                 selectInput("optional_param1_1", "Optional Param 1:", choices = c("", col_names)),
                                 selectInput("optional_param1_representation1", "Optional Param 1 Representation:",
                                             choices = c("Point Size" = "point_size", "Point Type" = "point_type"),
                                             selected = "point_size"),
                                 textInput("filter_op1_1", "Filter for Optional Param 1", ""),
                                 selectInput("optional_param2_1", "Optional Param 2:", choices = c("", col_names)),
                                 textInput("filter_op2_1", "Filter for Optional Param 2", ""),
                                 selectInput("color_palette1", "Color Palette for Optional Param 2:",
                                             choices = c("Blue" = "blue", "Red" = "red", "Viridis" = "viridis", "Rainbow" = "rainbow"),
                                             selected = "blue")
                          ),
                          column(6,
                                 div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
                                     h4(style = "color: #dc3545; margin-top: 0;", "Element A (Required)"),
                                     selectInput("element_A2", "Element A (multiple allowed):", choices = col_names, multiple = TRUE),
                                     uiOutput("dynamic_filters_A2")
                                 ),
                                 div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
                                     h4(style = "color: #dc3545; margin-top: 0;", "Element B (Required)"),
                                     selectInput("element_B2", "Element B (multiple allowed):", choices = col_names, multiple = TRUE),
                                     uiOutput("dynamic_filters_B2")
                                 ),
                                 div(style = "border: 2px solid #dc3545; padding: 10px; border-radius: 5px; margin: 10px 0;",
                                     h4(style = "color: #dc3545; margin-top: 0;", "Element C (Required)"),
                                     selectInput("element_C2", "Element C (multiple allowed):", choices = col_names, multiple = TRUE),
                                     uiOutput("dynamic_filters_C2")
                                 ),
                                 selectInput("optional_param1_2", "Optional Param 1:", choices = c("", col_names)),
                                 selectInput("optional_param1_representation2", "Optional Param 1 Representation:",
                                             choices = c("Point Size" = "point_size", "Point Type" = "point_type"),
                                             selected = "point_size"),
                                 textInput("filter_op1_2", "Filter for Optional Param 1", ""),
                                 selectInput("optional_param2_2", "Optional Param 2:", choices = c("", col_names)),
                                 textInput("filter_op2_2", "Filter for Optional Param 2", ""),
                                 selectInput("color_palette2", "Color Palette for Optional Param 2:",
                                             choices = c("Blue" = "blue", "Red" = "red", "Viridis" = "viridis", "Rainbow" = "rainbow"),
                                             selected = "blue")
                          )
                        )
                 )
               ),
               fluidRow(
                 column(12,
                        hr(),
                        h3("Analysis Methods"),
                        fluidRow(
                          column(4,
                                 div(style = "border: 2px solid #007bff; padding: 15px; border-radius: 8px; margin: 10px 0; background-color: #f8f9fa;",
                                     h4(style = "color: #007bff; margin-top: 0;", "Multivariate Analysis"),
                                     checkboxInput("use_mahalanobis", "Use Mahalanobis Distance Filtering", value = FALSE),
                                     helpText("Standard Mahalanobis distance with custom threshold formula"),
                                     conditionalPanel(
                                       condition = "input.use_mahalanobis || input.use_robust_mahalanobis",
                                       radioButtons("mahalanobis_reference", "Reference dataset:", 
                                                    choices = c("Dataset 2 as reference" = "dataset2", "Self-reference (dataset itself)" = "self"), 
                                                    selected = "dataset2", inline = TRUE),
                                       helpText("Dataset 2: Compare to external reference | Self: Find outliers within same dataset"),
                                       selectInput("mahalanobis_columns", "Columns for Mahalanobis calculation:", 
                                                   choices = col_names, multiple = TRUE,
                                                   selected = NULL),
                                       helpText("Select specific columns for multivariate analysis (leave empty for all common numeric columns)")
                                     ),
                                     div(style = "border: 1px solid #dee2e6; padding: 10px; border-radius: 5px; margin: 10px 0; background-color: #ffffff;",
                                         h5("Threshold calculation:"),
                                         radioButtons("mdthresh_mode", "Threshold calculation:", 
                                                      choices = c("Automatic (λ, ω formula)" = "auto", "Manual value" = "manual"), 
                                                      selected = "auto", inline = TRUE),
                                         conditionalPanel(
                                           condition = "input.mdthresh_mode == 'manual'",
                                           numericInput("custom_mdthresh", "Custom MDthresh value:", value = 10, min = 0, step = 0.1)
                                         ),
                                         conditionalPanel(
                                           condition = "input.mdthresh_mode == 'auto'",
                                           numericInput("lambda", "Lambda parameter (λ):", value = 1, min = 0, step = 0.1),
                                           numericInput("omega", "Omega parameter (ω):", value = 0, min = 0, step = 0.1),
                                           helpText("λ and ω control threshold strictness in Mahalanobis formula"),
                                           helpText("Formula: MDthresh = MDmean + √(100/(100+λ-ω)) × stdMD")
                                         )
                                     ),
                                     radioButtons("outlier_mode_mahalanobis", "Outlier handling:", 
                                                  choices = c("Remove outliers" = FALSE, "Keep only outliers" = TRUE), 
                                                  selected = FALSE, inline = TRUE),
                                     checkboxInput("use_robust_mahalanobis", "Use Robust Mahalanobis (MCD)", value = FALSE),
                                     helpText("Robust covariance estimation using Minimum Covariance Determinant"),
                                     radioButtons("outlier_mode_robust", "Outlier handling:", 
                                                  choices = c("Remove outliers" = FALSE, "Keep only outliers" = TRUE), 
                                                  selected = FALSE, inline = TRUE),
                                     checkboxInput("use_isolation_forest", "Use Isolation Forest", value = FALSE),
                                     helpText("Anomaly detection using isolation forest algorithm"),
                                     radioButtons("outlier_mode_isolation", "Outlier handling:", 
                                                  choices = c("Remove outliers" = FALSE, "Keep only outliers" = TRUE), 
                                                  selected = FALSE, inline = TRUE)
                                 )
                          ),
                          column(4,
                                 div(style = "border: 2px solid #28a745; padding: 15px; border-radius: 8px; margin: 10px 0; background-color: #f8f9fa;",
                                     h4(style = "color: #28a745; margin-top: 0;", "Statistical Filtering"),
                                     checkboxInput("use_iqr_filter", "Use IQR Filtering", value = FALSE),
                                     helpText("Interquartile Range method: removes points beyond Q1-1.5×IQR and Q3+1.5×IQR"),
                                     radioButtons("outlier_mode_iqr", "Outlier handling:", 
                                                  choices = c("Remove outliers" = FALSE, "Keep only outliers" = TRUE), 
                                                  selected = FALSE, inline = TRUE),
                                     checkboxInput("use_zscore_filter", "Use Z-Score Filtering", value = FALSE),
                                     helpText("Z-score method: removes points with |z-score| > 3 (3 standard deviations)"),
                                     radioButtons("outlier_mode_zscore", "Outlier handling:", 
                                                  choices = c("Remove outliers" = FALSE, "Keep only outliers" = TRUE), 
                                                  selected = FALSE, inline = TRUE),
                                     checkboxInput("use_mad_filter", "Use MAD Filtering", value = FALSE),
                                     helpText("Median Absolute Deviation: robust alternative to z-score for outlier detection"),
                                     radioButtons("outlier_mode_mad", "Outlier handling:", 
                                                  choices = c("Remove outliers" = FALSE, "Keep only outliers" = TRUE), 
                                                  selected = FALSE, inline = TRUE)
                                 )
                          ),
                          column(4,
                                 div(style = "border: 2px solid #ffc107; padding: 15px; border-radius: 8px; margin: 10px 0; background-color: #f8f9fa;",
                                     h4(style = "color: #ffc107; margin-top: 0;", "Output & Plot Options"),
                                     selectInput("output_format", "Output Format:",
                                                 choices = c("PNG" = "png", "JPEG" = "jpeg", "PDF" = "pdf", "TIFF" = "tiff"),
                                                 selected = "png"),
                                     checkboxInput("show_filtered_points", "Show filtered points on plot", value = TRUE),
                                     helpText("Display points that were filtered out by analysis methods"),
                                     checkboxInput("draw_inclusion_area", "Draw inclusion area", value = FALSE),
                                     helpText("Draw ellipse/circle around normal data points (excluding outliers)"),
                                     conditionalPanel(
                                       condition = "input.draw_inclusion_area == true",
                                       selectInput("inclusion_shape", "Inclusion shape:",
                                                   choices = c("Ellipse" = "ellipse", "Circle" = "circle", "Convex Hull" = "hull"),
                                                   selected = "ellipse"),
                                       numericInput("inclusion_confidence", "Confidence level (%):", value = 95, min = 50, max = 99, step = 1),
                                       helpText("Confidence level for inclusion area (95% = 2σ for normal distribution)")
                                     ),
                                     div(style = "border: 1px solid #dee2e6; padding: 10px; border-radius: 5px; margin: 10px 0; background-color: #ffffff;",
                                         h5("Data Quality Check"),
                                         actionButton("check_data_quality", "Check Data Quality", class = "btn-info"),
                                         br(), br(),
                                         verbatimTextOutput("data_quality_output"),
                                         verbatimTextOutput("mahalanobis_info")
                                     )
                                 )
                          )
                        )
                 )
               ),
               fluidRow(
                 column(12,
                        verbatimTextOutput("status"),
                        uiOutput("analysis_buttons"),
                        uiOutput("dynamic_output")
                 )
               )
      ),
      tabPanel("Data Comparison",
               fluidRow(
                 column(12,
                        h3("Dataset Comparison"),
                        uiOutput("comparison_buttons"),
                        uiOutput("comparison_output")
                 )
               )
      )
    ),
    tags$hr(),
    tags$footer(
      HTML("© 2025 Vid Kuder Marušič — <a href='mailto:vid.kudermarusic@gmail.com'>vid.kudermarusic@gmail.com</a>"),
      align = "center",
      style = "width: 100%; color: #888; background-color: #f9f9f9; padding: 10px 0; font-size: 0.95em;"
    )
  )
  
  server <- function(input, output, session) {
    # For sharing between observers
    rv <- reactiveValues(
      stats1=NULL, validation1=NULL, correlation1=NULL, df1=NULL,
      stats2=NULL, validation2=NULL, correlation2=NULL, df2=NULL,
      mahalanobis_result=NULL
    )
    
    # Help button functionality
    observeEvent(input$help_button, {
      showModal(modalDialog(
        title = "Help - Custom Ternary Builder v6",
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Close"),
        div(
          style = "max-height: 70vh; overflow-y: auto;",
          h3("Application Overview"),
          p("This application allows you to create ternary plots from Excel data with advanced filtering and multivariate analysis capabilities."),
          
          h4("Required Elements (Red Sections)"),
          div(style = "border-left: 4px solid #dc3545; padding-left: 15px; margin: 10px 0;",
              p(strong("Element A, B, C:"), "These are REQUIRED for the application to work. You must select at least one column for each element."),
              p("• Multiple columns can be selected for each element"),
              p("• Individual filters can be applied to each selected column"),
              p("• Example: Fe > 10, Al > 5, Si > 0 (each element gets its own threshold)")
          ),
          
          h4("Data Input"),
          p(strong("Primary Dataset:"), "Your main dataset for analysis"),
          p(strong("Reference Dataset:"), "Optional reference dataset for comparison and multivariate analysis"),
          p(strong("Working Directory:"), "Directory where your Excel files are located"),
          p(strong("Output Directory:"), "Directory where plots will be saved"),
          
          h4("Analysis Methods"),
          div(style = "border: 1px solid #007bff; padding: 10px; border-radius: 5px; margin: 10px 0;",
              h5("Multivariate Analysis (Blue Section)"),
              p(strong("Mahalanobis Distance:"), "Standard multivariate outlier detection using custom threshold formula"),
              p(strong("Robust Mahalanobis (MCD):"), "Robust covariance estimation using Minimum Covariance Determinant"),
              p(strong("Isolation Forest:"), "Anomaly detection using isolation forest algorithm"),
              p(strong("Threshold Calculation:"), "Automatic using λ, ω formula or manual value"),
              p(strong("Formula:"), "MDthresh = MDmean + √(100/(100+λ-ω)) × stdMD"),
              p(strong("Reference Options:"), "Dataset 2 as reference or self-reference (same dataset)")
          ),
          
          div(style = "border: 1px solid #28a745; padding: 10px; border-radius: 5px; margin: 10px 0;",
              h5("Statistical Filtering (Green Section)"),
              p(strong("IQR Filtering:"), "Interquartile Range method - removes points beyond Q1-1.5×IQR and Q3+1.5×IQR"),
              p(strong("Z-Score Filtering:"), "Z-score method - removes points with |z-score| > 3 (3 standard deviations)"),
              p(strong("MAD Filtering:"), "Median Absolute Deviation - robust alternative to z-score for outlier detection")
          ),
          
          div(style = "border: 1px solid #ffc107; padding: 10px; border-radius: 5px; margin: 10px 0;",
              h5("Output & Plot Options (Yellow Section)"),
              p(strong("Output Formats:"), "PNG, JPEG, PDF, TIFF"),
              p(strong("Show Filtered Points:"), "Display points that were filtered out by analysis methods"),
              p(strong("Draw Inclusion Area:"), "Draw ellipse/circle around normal data points (excluding outliers)"),
              p(strong("Data Quality Check:"), "Diagnose common data issues like singular matrices, zero variance columns")
          ),
          
          h4("Filtering Logic"),
          p("• Individual filters apply to each selected column separately (logical AND)"),
          p("• Example: If you select Fe, Al, Si for Element A and set filters '> 10', '> 5', '> 0', the condition becomes: Fe > 10 AND Al > 5 AND Si > 0"),
          p("• Optional parameters use sum filtering (sum of all selected columns)"),
          
          h4("Outlier Handling"),
          p("For each analysis method, you can choose to:"),
          p("• Remove outliers: Keep only normal points"),
          p("• Keep only outliers: Keep only the points identified as outliers"),
          
          h4("Plot Features"),
          p("• Ternary plots show normalized proportions of elements A, B, C"),
          p("• Optional parameters can be represented as point size or point type"),
          p("• Color palettes available for optional parameter 2"),
          p("• Inclusion areas can be drawn around normal data points"),
          p("• Filtered points can be displayed with distinct markers"),
          
          h4("Troubleshooting"),
          p(strong("No points plotted:"), "Check that you have selected elements A, B, C and that your data contains valid values"),
          p(strong("Multivariate analysis errors:"), "Use 'Check Data Quality' button to diagnose issues like singular matrices or insufficient observations"),
          p(strong("Filter issues:"), "Ensure filter syntax is correct (e.g., '> 10', '< 5', '>= 0.5')"),
          
          h4("Keyboard Shortcuts"),
          p("• Ctrl+S: Save current plot"),
          p("• Ctrl+Q: Close application"),
          
          h4("Contact"),
          p("For technical support or questions, contact: vid.kudermarusic@gmail.com")
        )
      ))
    })
    
    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())
    shinyDirChoose(input, "working_dir", roots = volumes, session = session)
    shinyDirChoose(input, "output_dir", roots = volumes, session = session)
    
    working_dir <- reactive({
      wd <- shinyFiles::parseDirPath(volumes, input$working_dir)
      if (length(wd) == 0) default_working_dir else wd
    })
    
    output_dir <- reactive({
      od <- shinyFiles::parseDirPath(volumes, input$output_dir)
      if (length(od) == 0) default_output_dir else od
    })
    
    output$working_dir_text <- renderText({
      wd <- working_dir()
      if (length(wd) == 0) "" else wd
    })
    
    output$output_dir_text <- renderText({
      od <- output_dir()
      if (length(od) == 0) "" else od
    })
    
    # Update column choices when files are uploaded
    observeEvent(input$xlsx_file1, {
      req(input$xlsx_file1)
      new_M <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
      new_col_names <- colnames(new_M)
      updateSelectInput(session, "element_A1", choices = new_col_names, selected = character(0))
      updateSelectInput(session, "element_B1", choices = new_col_names, selected = character(0))
      updateSelectInput(session, "element_C1", choices = new_col_names, selected = character(0))
      updateSelectInput(session, "optional_param1_1", choices = c("", new_col_names), selected = "")
      updateSelectInput(session, "optional_param2_1", choices = c("", new_col_names), selected = "")
      
      # Update Mahalanobis column choices with numeric columns
      numeric_cols <- names(new_M)[sapply(new_M, is.numeric)]
      updateSelectInput(session, "mahalanobis_columns", choices = numeric_cols, selected = character(0))
    })
    
    observeEvent(input$xlsx_file2, {
      req(input$xlsx_file2)
      new_M <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
      new_col_names <- colnames(new_M)
      updateSelectInput(session, "element_A2", choices = new_col_names, selected = character(0))
      updateSelectInput(session, "element_B2", choices = new_col_names, selected = character(0))
      updateSelectInput(session, "element_C2", choices = new_col_names, selected = character(0))
      updateSelectInput(session, "optional_param1_2", choices = c("", new_col_names), selected = "")
      updateSelectInput(session, "optional_param2_2", choices = c("", new_col_names), selected = "")
      
      # Update Mahalanobis column choices if second dataset is loaded
      if (!is.null(input$xlsx_file1)) {
        df1 <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
        df2 <- new_M
        numeric_cols1 <- names(df1)[sapply(df1, is.numeric)]
        numeric_cols2 <- names(df2)[sapply(df2, is.numeric)]
        common_numeric_cols <- intersect(numeric_cols1, numeric_cols2)
        updateSelectInput(session, "mahalanobis_columns", choices = common_numeric_cols, selected = character(0))
      }
    })
    
    # Dynamic filter UI generation
    create_dynamic_filters <- function(elements, dataset_suffix) {
      if (is.null(elements) || length(elements) == 0) {
        return(NULL)
      }
      
      filter_inputs <- lapply(seq_along(elements), function(i) {
        element_name <- elements[i]
        input_id <- paste0("filter_", gsub("[^A-Za-z0-9]", "_", element_name), "_", dataset_suffix)
        
        div(
          style = "margin-bottom: 5px;",
          textInput(
            inputId = input_id,
            label = paste("Filter for", element_name, ":"),
            value = "",
            placeholder = "e.g. > 0.5"
          )
        )
      })
      
      do.call(tagList, filter_inputs)
    }
    
    # Dynamic filter outputs
    output$dynamic_filters_A1 <- renderUI({
      create_dynamic_filters(input$element_A1, "A1")
    })
    
    output$dynamic_filters_B1 <- renderUI({
      create_dynamic_filters(input$element_B1, "B1")
    })
    
    output$dynamic_filters_C1 <- renderUI({
      create_dynamic_filters(input$element_C1, "C1")
    })
    
    output$dynamic_filters_A2 <- renderUI({
      create_dynamic_filters(input$element_A2, "A2")
    })
    
    output$dynamic_filters_B2 <- renderUI({
      create_dynamic_filters(input$element_B2, "B2")
    })
    
    output$dynamic_filters_C2 <- renderUI({
      create_dynamic_filters(input$element_C2, "C2")
    })
    
    # Helper function to get individual filter values
    get_individual_filters <- function(elements, dataset_suffix) {
      if (is.null(elements) || length(elements) == 0) {
        return(list())
      }
      
      filters <- lapply(elements, function(element_name) {
        input_id <- paste0("filter_", gsub("[^A-Za-z0-9]", "_", element_name), "_", dataset_suffix)
        filter_value <- input[[input_id]]
        if (is.null(filter_value) || !nzchar(filter_value)) {
          return(NULL)
        }
        return(filter_value)
      })
      
      names(filters) <- elements
      # Remove NULL values
      filters[!sapply(filters, is.null)]
    }
    
    # Copy all parameters from dataset 1 to dataset 2
    observeEvent(c(input$element_A1, input$element_B1, input$element_C1, 
                   input$optional_param1_1, input$optional_param2_1,
                   input$filter_op1_1, input$filter_op2_1,
                   input$optional_param1_representation1, input$color_palette1), {
                     if (!is.null(input$xlsx_file2)) {
                       # Get current dataset 2 column names
                       df2 <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
                       col_names2 <- colnames(df2)
                       
                       # Copy elements if they exist in dataset 2
                       if (length(input$element_A1) > 0) {
                         matching_A <- intersect(input$element_A1, col_names2)
                         if (length(matching_A) > 0) {
                           updateSelectInput(session, "element_A2", selected = matching_A)
                           # Copy individual filters for matching elements
                           filters_A1 <- get_individual_filters(input$element_A1, "A1")
                           for (element in matching_A) {
                             if (element %in% names(filters_A1)) {
                               input_id_A2 <- paste0("filter_", gsub("[^A-Za-z0-9]", "_", element), "_A2")
                               updateTextInput(session, input_id_A2, value = filters_A1[[element]])
                             }
                           }
                         }
                       }
                       
                       if (length(input$element_B1) > 0) {
                         matching_B <- intersect(input$element_B1, col_names2)
                         if (length(matching_B) > 0) {
                           updateSelectInput(session, "element_B2", selected = matching_B)
                           # Copy individual filters for matching elements
                           filters_B1 <- get_individual_filters(input$element_B1, "B1")
                           for (element in matching_B) {
                             if (element %in% names(filters_B1)) {
                               input_id_B2 <- paste0("filter_", gsub("[^A-Za-z0-9]", "_", element), "_B2")
                               updateTextInput(session, input_id_B2, value = filters_B1[[element]])
                             }
                           }
                         }
                       }
                       
                       if (length(input$element_C1) > 0) {
                         matching_C <- intersect(input$element_C1, col_names2)
                         if (length(matching_C) > 0) {
                           updateSelectInput(session, "element_C2", selected = matching_C)
                           # Copy individual filters for matching elements
                           filters_C1 <- get_individual_filters(input$element_C1, "C1")
                           for (element in matching_C) {
                             if (element %in% names(filters_C1)) {
                               input_id_C2 <- paste0("filter_", gsub("[^A-Za-z0-9]", "_", element), "_C2")
                               updateTextInput(session, input_id_C2, value = filters_C1[[element]])
                             }
                           }
                         }
                       }
                       
                       # Copy optional parameters if they exist in dataset 2
                       if (input$optional_param1_1 != "" && input$optional_param1_1 %in% col_names2) {
                         updateSelectInput(session, "optional_param1_2", selected = input$optional_param1_1)
                         updateTextInput(session, "filter_op1_2", value = input$filter_op1_1)
                         updateSelectInput(session, "optional_param1_representation2", selected = input$optional_param1_representation1)
                       }
                       
                       if (input$optional_param2_1 != "" && input$optional_param2_1 %in% col_names2) {
                         updateSelectInput(session, "optional_param2_2", selected = input$optional_param2_1)
                         updateTextInput(session, "filter_op2_2", value = input$filter_op2_1)
                         updateSelectInput(session, "color_palette2", selected = input$color_palette1)
                       }
                     }
                   })
    
    # Data analysis functions for both datasets
    analyze_data1 <- reactive({
      req(input$element_A1, input$element_B1, input$element_C1, input$xlsx_file1)
      df <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet=1)
      cols <- c(input$element_A1, input$element_B1, input$element_C1,
                if (input$optional_param1_1 != "") input$optional_param1_1,
                if (input$optional_param2_1 != "") input$optional_param2_1)
      validation <- validate_data(df, cols)
      stats <- generate_stats(df, cols)
      correlation <- compute_correlation(df, cols)
      rv$stats1 <- stats
      rv$validation1 <- validation
      rv$correlation1 <- correlation
      rv$df1 <- df
      list(df=df, stats=stats, validation=validation, correlation=correlation)
    })
    
    analyze_data2 <- reactive({
      req(input$element_A2, input$element_B2, input$element_C2, input$xlsx_file2)
      df <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet=1)
      cols <- c(input$element_A2, input$element_B2, input$element_C2,
                if (input$optional_param1_2 != "") input$optional_param1_2,
                if (input$optional_param2_2 != "") input$optional_param2_2)
      validation <- validate_data(df, cols)
      stats <- generate_stats(df, cols)
      correlation <- compute_correlation(df, cols)
      rv$stats2 <- stats
      rv$validation2 <- validation
      rv$correlation2 <- correlation
      rv$df2 <- df
      list(df=df, stats=stats, validation=validation, correlation=correlation)
    })
    
    # Multivariate analysis calculation
    multivariate_analysis <- reactive({
      # Check if at least one method is selected
      if (!(input$use_mahalanobis || input$use_robust_mahalanobis || input$use_isolation_forest)) {
        return(NULL)
      }
      
      # Check if files are uploaded based on reference mode
      if (input$mahalanobis_reference == "dataset2") {
        # Need both files for dataset2 reference
        if (is.null(input$xlsx_file1) || is.null(input$xlsx_file2)) {
          return(NULL)
        }
      } else {
        # Self-reference only needs one file
        if (is.null(input$xlsx_file1)) {
          return(NULL)
        }
      }
      
      tryCatch({
        df1 <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet=1)
        df2 <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet=1)
        
        # Get custom MDthresh if manual mode is selected
        custom_mdthresh_val <- if (input$mdthresh_mode == "manual") input$custom_mdthresh else NULL
        
        # Get selected columns for Mahalanobis calculation
        selected_cols <- if (length(input$mahalanobis_columns) > 0) input$mahalanobis_columns else NULL
        
        # Determine reference dataset based on user selection
        if (input$mahalanobis_reference == "self") {
          # Self-reference: use df1 as both datasets
          reference_df <- df1
        } else {
          # Dataset 2 reference: use df2 as reference
          reference_df <- df2
        }
        
        if (input$use_robust_mahalanobis) {
          result <- compute_robust_mahalanobis(df1, reference_df, selected_columns = selected_cols)
        } else if (input$use_isolation_forest) {
          result <- compute_isolation_forest(df1, reference_df, selected_columns = selected_cols)
        } else {
          result <- compute_mahalanobis_distance(df1, reference_df, input$lambda, input$omega, custom_mdthresh = custom_mdthresh_val, selected_columns = selected_cols)
        }
        
        rv$mahalanobis_result <- result
        result
      }, error = function(e) {
        # Print error for debugging
        cat("Error in multivariate analysis:", e$message, "\n")
        NULL
      })
    })
    
    # Data quality check
    observeEvent(input$check_data_quality, {
      output$data_quality_output <- renderPrint({
        if (is.null(input$xlsx_file1) || is.null(input$xlsx_file2)) {
          cat("Please upload both datasets first.\n")
          return()
        }
        
        tryCatch({
          df1 <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet=1)
          df2 <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet=1)
          
          quality_check <- check_data_quality(df1, df2)
          
          cat("=== DATA QUALITY REPORT ===\n")
          cat("Common numeric columns:", quality_check$num_common_cols, "\n")
          if (quality_check$num_common_cols >= 2) {
            cat("Column names:", paste(quality_check$common_cols, collapse = ", "), "\n")
            cat("Dataset 1 complete rows:", quality_check$data1_rows, "\n")
            cat("Dataset 2 complete rows:", quality_check$data2_rows, "\n")
            
            # Check for issues
            issues <- character(0)
            
            if (quality_check$data2_rows <= quality_check$num_common_cols) {
              issues <- c(issues, paste("⚠️ Dataset 2 has too few rows for", quality_check$num_common_cols, "columns"))
            }
            
            if (length(quality_check$zero_var_cols1) > 0) {
              issues <- c(issues, paste("⚠️ Dataset 1 zero-variance columns:", paste(quality_check$zero_var_cols1, collapse = ", ")))
            }
            
            if (length(quality_check$zero_var_cols2) > 0) {
              issues <- c(issues, paste("⚠️ Dataset 2 zero-variance columns:", paste(quality_check$zero_var_cols2, collapse = ", ")))
            }
            
            if (!is.null(quality_check$cov_det)) {
              cat("Covariance matrix determinant:", sprintf("%.2e", quality_check$cov_det), "\n")
              cat("Condition number:", sprintf("%.2e", quality_check$condition_number), "\n")
              
              if (quality_check$cov_det < 1e-10) {
                issues <- c(issues, "⚠️ Covariance matrix is near-singular (very small determinant)")
              }
              
              if (quality_check$condition_number > 1e12) {
                issues <- c(issues, "⚠️ Covariance matrix is ill-conditioned (high condition number)")
              }
            }
            
            if (length(issues) == 0) {
              cat("✅ Data quality looks good for multivariate analysis!\n")
            } else {
              cat("\n=== POTENTIAL ISSUES ===\n")
              for (issue in issues) {
                cat(issue, "\n")
              }
              cat("\n💡 SUGGESTIONS:\n")
              cat("- Remove columns with zero variance\n")
              cat("- Check for duplicate or highly correlated columns\n")
              cat("- Ensure sufficient observations in reference dataset\n")
              cat("- Consider using only a subset of most relevant columns\n")
            }
          } else {
            cat("❌ Need at least 2 common numeric columns for multivariate analysis\n")
          }
        }, error = function(e) {
          cat("Error in data quality check:", e$message, "\n")
        })
      })
    })
    
    output$mahalanobis_info <- renderPrint({
      result <- multivariate_analysis()
      if (is.null(result)) {
        cat("Multivariate analysis not available. Please ensure:\n")
        cat("- Both datasets are loaded\n")
        cat("- Both datasets have at least 2 common numeric columns\n")
        cat("- At least one multivariate method is selected\n")
      } else {
        if (input$use_robust_mahalanobis) {
          cat("Robust Mahalanobis Analysis (MCD):\n")
          cat("Method:", result$method, "\n")
          cat("Total points analyzed:", result$total_points, "\n")
          cat("Outlier count:", result$outlier_count, "\n")
          cat("95% threshold value:", round(result$threshold_95, 3), "\n")
        } else if (input$use_isolation_forest) {
          cat("Isolation Forest Analysis:\n")
          cat("Method:", result$method, "\n")
          cat("Total points analyzed:", result$total_points, "\n")
          cat("Outlier count:", result$outlier_count, "\n")
          cat("Threshold value:", round(result$threshold, 3), "\n")
        } else {
          cat("Mahalanobis Distance Analysis:\n")
          cat("Total points analyzed:", result$total_points, "\n")
          cat("Degrees of freedom:", result$df, "\n")
          cat("Columns used:", paste(result$common_cols, collapse = ", "), "\n")
          cat("MDmean:", round(result$MDmean, 3), "\n")
          cat("stdMD:", round(result$stdMD, 3), "\n")
          
          if (input$mdthresh_mode == "manual") {
            cat("Threshold mode: Manual\n")
            cat("Custom MDthresh:", round(result$MDthresh, 3), "\n")
          } else {
            cat("Threshold mode: Automatic (λ, ω formula)\n")
            cat("λ:", input$lambda, "ω:", input$omega, "\n")
            cat("Calculated MDthresh:", round(result$MDthresh, 3), "\n")
          }
          
          cat("Points above 95% threshold:", result$outlier_95, "\n")
          cat("Points above 99% threshold:", result$outlier_99, "\n")
          cat("Points above custom threshold:", result$outlier_custom, "\n")
          cat("P-value range:", round(min(result$p_values), 4), "to", round(max(result$p_values), 4), "\n")
        }
      }
    })
    
    # Preview functions
    output$preview_info1 <- renderText({
      if (length(input$element_A1) == 0 || length(input$element_B1) == 0 || length(input$element_C1) == 0) {
        "Preview 1: Select elements A, B and C for Dataset 1!"
      } else {
        ""
      }
    })
    
    output$preview_info2 <- renderText({
      if (length(input$element_A2) == 0 || length(input$element_B2) == 0 || length(input$element_C2) == 0) {
        "Preview 2: Select elements A, B and C for Dataset 2!"
      } else {
        ""
      }
    })
    
    # Ternary plot previews
    output$ternary_preview1 <- renderPlot({
      req(input$element_A1, input$element_B1, input$element_C1, input$xlsx_file1)
      
      # Get reference data for multivariate analysis if needed
      reference_data <- NULL
      if ((input$use_mahalanobis || input$use_robust_mahalanobis || input$use_isolation_forest) && !is.null(input$xlsx_file2)) {
        reference_data <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet=1)
      }
      
      # Get individual filters
      individual_filters_A1 <- get_individual_filters(input$element_A1, "A1")
      individual_filters_B1 <- get_individual_filters(input$element_B1, "B1")
      individual_filters_C1 <- get_individual_filters(input$element_C1, "C1")
      
      # Get custom MDthresh if manual mode is selected
      custom_mdthresh_val <- if (input$mdthresh_mode == "manual") input$custom_mdthresh else NULL
      
      general_ternary_plot(
        xlsx_file = input$xlsx_file1$datapath,
        working_dir = if (length(working_dir()) > 0) working_dir() else getwd(),
        output_dir = NULL,
        element_A = list(col = input$element_A1),
        element_B = list(col = input$element_B1),
        element_C = list(col = input$element_C1),
        optional_param1 = if (input$optional_param1_1 != "") list(col = input$optional_param1_1, filter = if (nzchar(input$filter_op1_1)) input$filter_op1_1 else NULL) else NULL,
        optional_param2 = if (input$optional_param2_1 != "") list(col = input$optional_param2_1, filter = if (nzchar(input$filter_op2_1)) input$filter_op2_1 else NULL) else NULL,
        color_palette = input$color_palette1,
        xlsx_display_name = input$xlsx_file1$name,
        preview = TRUE,
        use_mahalanobis = input$use_mahalanobis || input$use_robust_mahalanobis || input$use_isolation_forest,
        reference_data = reference_data,
        mahalanobis_reference = input$mahalanobis_reference,
        selected_columns = if (length(input$mahalanobis_columns) > 0) input$mahalanobis_columns else NULL,
        optional_param1_representation = input$optional_param1_representation1,
        output_format = input$output_format,
        use_robust_mahalanobis = input$use_robust_mahalanobis,
        use_isolation_forest = input$use_isolation_forest,
        use_iqr_filter = input$use_iqr_filter,
        use_zscore_filter = input$use_zscore_filter,
        use_mad_filter = input$use_mad_filter,
        lambda = input$lambda,
        omega = input$omega,
        keep_outliers_mahalanobis = input$outlier_mode_mahalanobis,
        keep_outliers_robust = input$outlier_mode_robust,
        keep_outliers_isolation = input$outlier_mode_isolation,
        keep_outliers_iqr = input$outlier_mode_iqr,
        keep_outliers_zscore = input$outlier_mode_zscore,
        keep_outliers_mad = input$outlier_mode_mad,
        individual_filters_A = individual_filters_A1,
        individual_filters_B = individual_filters_B1,
        individual_filters_C = individual_filters_C1,
        custom_mdthresh = custom_mdthresh_val,
        show_filtered_points = input$show_filtered_points,
        draw_inclusion_area = input$draw_inclusion_area,
        inclusion_shape = input$inclusion_shape,
        inclusion_confidence = input$inclusion_confidence
      )
    })
    
    output$ternary_preview2 <- renderPlot({
      req(input$element_A2, input$element_B2, input$element_C2, input$xlsx_file2)
      
      # Get reference data for multivariate analysis if needed
      reference_data <- NULL
      if ((input$use_mahalanobis || input$use_robust_mahalanobis || input$use_isolation_forest) && !is.null(input$xlsx_file1)) {
        reference_data <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet=1)
      }
      
      # Get individual filters
      individual_filters_A2 <- get_individual_filters(input$element_A2, "A2")
      individual_filters_B2 <- get_individual_filters(input$element_B2, "B2")
      individual_filters_C2 <- get_individual_filters(input$element_C2, "C2")
      
      # Get custom MDthresh if manual mode is selected
      custom_mdthresh_val <- if (input$mdthresh_mode == "manual") input$custom_mdthresh else NULL
      
      general_ternary_plot(
        xlsx_file = input$xlsx_file2$datapath,
        working_dir = if (length(working_dir()) > 0) working_dir() else getwd(),
        output_dir = NULL,
        element_A = list(col = input$element_A2),
        element_B = list(col = input$element_B2),
        element_C = list(col = input$element_C2),
        optional_param1 = if (input$optional_param1_2 != "") list(col = input$optional_param1_2, filter = if (nzchar(input$filter_op1_2)) input$filter_op1_2 else NULL) else NULL,
        optional_param2 = if (input$optional_param2_2 != "") list(col = input$optional_param2_2, filter = if (nzchar(input$filter_op2_2)) input$filter_op2_2 else NULL) else NULL,
        color_palette = input$color_palette2,
        xlsx_display_name = input$xlsx_file2$name,
        preview = TRUE,
        use_mahalanobis = input$use_mahalanobis || input$use_robust_mahalanobis || input$use_isolation_forest,
        reference_data = reference_data,
        mahalanobis_reference = input$mahalanobis_reference,
        selected_columns = if (length(input$mahalanobis_columns) > 0) input$mahalanobis_columns else NULL,
        optional_param1_representation = input$optional_param1_representation2,
        output_format = input$output_format,
        use_robust_mahalanobis = input$use_robust_mahalanobis,
        use_isolation_forest = input$use_isolation_forest,
        use_iqr_filter = input$use_iqr_filter,
        use_zscore_filter = input$use_zscore_filter,
        use_mad_filter = input$use_mad_filter,
        lambda = input$lambda,
        omega = input$omega,
        keep_outliers_mahalanobis = input$outlier_mode_mahalanobis,
        keep_outliers_robust = input$outlier_mode_robust,
        keep_outliers_isolation = input$outlier_mode_isolation,
        keep_outliers_iqr = input$outlier_mode_iqr,
        keep_outliers_zscore = input$outlier_mode_zscore,
        keep_outliers_mad = input$outlier_mode_mad,
        individual_filters_A = individual_filters_A2,
        individual_filters_B = individual_filters_B2,
        individual_filters_C = individual_filters_C2,
        custom_mdthresh = custom_mdthresh_val,
        show_filtered_points = input$show_filtered_points,
        draw_inclusion_area = input$draw_inclusion_area,
        inclusion_shape = input$inclusion_shape,
        inclusion_confidence = input$inclusion_confidence
      )
    })
    
    # Plot saving functions
    save_plot <- function(dataset_num) {
      if (dataset_num == 1) {
        req(input$element_A1, input$element_B1, input$element_C1, input$xlsx_file1)
        xlsx_path <- input$xlsx_file1$datapath
        xlsx_name <- input$xlsx_file1$name
        element_A <- list(col = input$element_A1)
        element_B <- list(col = input$element_B1)
        element_C <- list(col = input$element_C1)
        optional_param1 <- if (input$optional_param1_1 != "") list(col = input$optional_param1_1, filter = if (nzchar(input$filter_op1_1)) input$filter_op1_1 else NULL) else NULL
        optional_param2 <- if (input$optional_param2_1 != "") list(col = input$optional_param2_1, filter = if (nzchar(input$filter_op2_1)) input$filter_op2_1 else NULL) else NULL
        color_palette <- input$color_palette1
        individual_filters_A <- get_individual_filters(input$element_A1, "A1")
        individual_filters_B <- get_individual_filters(input$element_B1, "B1")
        individual_filters_C <- get_individual_filters(input$element_C1, "C1")
      } else {
        req(input$element_A2, input$element_B2, input$element_C2, input$xlsx_file2)
        xlsx_path <- input$xlsx_file2$datapath
        xlsx_name <- input$xlsx_file2$name
        element_A <- list(col = input$element_A2)
        element_B <- list(col = input$element_B2)
        element_C <- list(col = input$element_C2)
        optional_param1 <- if (input$optional_param1_2 != "") list(col = input$optional_param1_2, filter = if (nzchar(input$filter_op1_2)) input$filter_op1_2 else NULL) else NULL
        optional_param2 <- if (input$optional_param2_2 != "") list(col = input$optional_param2_2, filter = if (nzchar(input$filter_op2_2)) input$filter_op2_2 else NULL) else NULL
        color_palette <- input$color_palette2
        individual_filters_A <- get_individual_filters(input$element_A2, "A2")
        individual_filters_B <- get_individual_filters(input$element_B2, "B2")
        individual_filters_C <- get_individual_filters(input$element_C2, "C2")
      }
      
      # Get reference data for multivariate analysis if needed
      reference_data <- NULL
      if (input$use_mahalanobis || input$use_robust_mahalanobis || input$use_isolation_forest) {
        if (dataset_num == 1 && !is.null(input$xlsx_file2)) {
          reference_data <- openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet=1)
        } else if (dataset_num == 2 && !is.null(input$xlsx_file1)) {
          reference_data <- openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet=1)
        }
      }
      
      # Get optional parameter 1 representation
      optional_param1_representation <- if (dataset_num == 1) input$optional_param1_representation1 else input$optional_param1_representation2
      
      wd <- if (length(working_dir()) > 0) working_dir() else getwd()
      od <- if (length(output_dir()) > 0) output_dir() else NULL
      
      # Get custom MDthresh if manual mode is selected
      custom_mdthresh_val <- if (input$mdthresh_mode == "manual") input$custom_mdthresh else NULL
      
      general_ternary_plot(
        xlsx_file = xlsx_path,
        working_dir = wd,
        output_dir = od,
        element_A = element_A,
        element_B = element_B,
        element_C = element_C,
        optional_param1 = optional_param1,
        optional_param2 = optional_param2,
        color_palette = color_palette,
        xlsx_display_name = xlsx_name,
        preview = FALSE,
        use_mahalanobis = input$use_mahalanobis || input$use_robust_mahalanobis || input$use_isolation_forest,
        reference_data = reference_data,
        mahalanobis_reference = input$mahalanobis_reference,
        selected_columns = if (length(input$mahalanobis_columns) > 0) input$mahalanobis_columns else NULL,
        optional_param1_representation = optional_param1_representation,
        output_format = input$output_format,
        use_robust_mahalanobis = input$use_robust_mahalanobis,
        use_isolation_forest = input$use_isolation_forest,
        use_iqr_filter = input$use_iqr_filter,
        use_zscore_filter = input$use_zscore_filter,
        use_mad_filter = input$use_mad_filter,
        lambda = input$lambda,
        omega = input$omega,
        keep_outliers_mahalanobis = input$outlier_mode_mahalanobis,
        keep_outliers_robust = input$outlier_mode_robust,
        keep_outliers_isolation = input$outlier_mode_isolation,
        keep_outliers_iqr = input$outlier_mode_iqr,
        keep_outliers_zscore = input$outlier_mode_zscore,
        keep_outliers_mad = input$outlier_mode_mad,
        individual_filters_A = individual_filters_A,
        individual_filters_B = individual_filters_B,
        individual_filters_C = individual_filters_C,
        custom_mdthresh = custom_mdthresh_val,
        show_filtered_points = input$show_filtered_points,
        draw_inclusion_area = input$draw_inclusion_area,
        inclusion_shape = input$inclusion_shape,
        inclusion_confidence = input$inclusion_confidence
      )
    }
    
    observeEvent(input$plot1, {
      save_plot(1)
      output$status <- renderText("Plot 1 saved! Check your output directory.")
    })
    
    observeEvent(input$plot2, {
      save_plot(2)
      output$status <- renderText("Plot 2 saved! Check your output directory.")
    })
    
    observeEvent(input$plot_both, {
      save_plot(1)
      save_plot(2)
      output$status <- renderText("Both plots saved! Check your output directory.")
    })
    
    observeEvent(input$close_app, {
      stopApp()
    })
    
    
    
    # Analysis outputs
    output$analysis_stats1 <- renderPrint({
      ad <- analyze_data1()
      ad$stats
    })
    
    output$analysis_validation1 <- renderPrint({
      ad <- analyze_data1()
      ad$validation
    })
    
    output$excel_preview1 <- DT::renderDataTable({
      req(input$xlsx_file1)
      openxlsx::read.xlsx(input$xlsx_file1$datapath, sheet = 1)
    })
    
    output$analysis_stats2 <- renderPrint({
      ad <- analyze_data2()
      ad$stats
    })
    
    output$analysis_validation2 <- renderPrint({
      ad <- analyze_data2()
      ad$validation
    })
    
    output$excel_preview2 <- DT::renderDataTable({
      req(input$xlsx_file2)
      openxlsx::read.xlsx(input$xlsx_file2$datapath, sheet = 1)
    })
    
    # Dynamic analysis buttons and outputs
    selected_analysis <- reactiveVal(NULL)
    observeEvent(input$show_stats1, { selected_analysis("stats1") })
    observeEvent(input$show_missing1, { selected_analysis("missing1") })
    observeEvent(input$show_excel1, { selected_analysis("excel1") })
    observeEvent(input$show_stats2, { selected_analysis("stats2") })
    observeEvent(input$show_missing2, { selected_analysis("missing2") })
    observeEvent(input$show_excel2, { selected_analysis("excel2") })
    
    output$analysis_buttons <- renderUI({
      req(input$xlsx_file1, input$xlsx_file2)
      tagList(
        h4("Dataset 1 Analysis:"),
        actionButton("show_stats1", "Descriptive Statistics 1"),
        actionButton("show_missing1", "Missing/Outlier Summary 1"),
        actionButton("show_excel1", "Excel File Preview 1"),
        br(), br(),
        h4("Dataset 2 Analysis:"),
        actionButton("show_stats2", "Descriptive Statistics 2"),
        actionButton("show_missing2", "Missing/Outlier Summary 2"),
        actionButton("show_excel2", "Excel File Preview 2")
      )
    })
    
    output$dynamic_output <- renderUI({
      req(input$xlsx_file1, input$xlsx_file2)
      sel <- selected_analysis()
      if (is.null(sel) || length(sel) != 1) return(NULL)
      switch(sel,
             stats1 = verbatimTextOutput("analysis_stats1"),
             missing1 = verbatimTextOutput("analysis_validation1"),
             excel1 = DT::dataTableOutput("excel_preview1"),
             stats2 = verbatimTextOutput("analysis_stats2"),
             missing2 = verbatimTextOutput("analysis_validation2"),
             excel2 = DT::dataTableOutput("excel_preview2"),
             NULL
      )
    })
    
    # Comparison tab
    selected_comparison <- reactiveVal(NULL)
    observeEvent(input$compare_stats, { selected_comparison("stats") })
    observeEvent(input$compare_mahalanobis, { selected_comparison("mahalanobis") })
    
    output$comparison_buttons <- renderUI({
      req(input$xlsx_file1, input$xlsx_file2)
      tagList(
        actionButton("compare_stats", "Compare Statistics"),
        actionButton("compare_mahalanobis", "Mahalanobis Analysis")
      )
    })
    
    output$comparison_output <- renderUI({
      req(input$xlsx_file1, input$xlsx_file2)
      sel <- selected_comparison()
      if (is.null(sel) || length(sel) != 1) return(NULL)
      switch(sel,
             stats = verbatimTextOutput("comparison_stats"),
             mahalanobis = verbatimTextOutput("comparison_mahalanobis"),
             NULL
      )
    })
    
    output$comparison_stats <- renderPrint({
      ad1 <- analyze_data1()
      ad2 <- analyze_data2()
      cat("=== DATASET 1 STATISTICS ===\n")
      print(ad1$stats)
      cat("\n=== DATASET 2 STATISTICS ===\n")
      print(ad2$stats)
    })
    
    output$comparison_mahalanobis <- renderPrint({
      result <- multivariate_analysis()
      if (is.null(result)) {
        cat("Multivariate analysis not available.")
      } else {
        cat("Multivariate Analysis Results:\n")
        print(result)
      }
    })
    
    # Tooltips
    bsTooltip("filter_A1", "Enter a filter, e.g. > 0.5. Leave blank for no filter. Applied to each element individually.", "right", options = list(container = "body"))
    bsTooltip("filter_B1", "Enter a filter, e.g. < 1. Leave blank for no filter. Applied to each element individually.", "right", options = list(container = "body"))
    bsTooltip("filter_C1", "Enter a filter, e.g. == 2. Leave blank for no filter. Applied to each element individually.", "right", options = list(container = "body"))
    bsTooltip("filter_A2", "Enter a filter, e.g. > 0.5. Leave blank for no filter. Applied to each element individually.", "right", options = list(container = "body"))
    bsTooltip("filter_B2", "Enter a filter, e.g. < 1. Leave blank for no filter. Applied to each element individually.", "right", options = list(container = "body"))
    bsTooltip("filter_C2", "Enter a filter, e.g. == 2. Leave blank for no filter. Applied to each element individually.", "right", options = list(container = "body"))
    bsTooltip("optional_param1_representation1", "Choose how to represent Optional Param 1: Point Size (variable size) or Point Type (different shapes).", "right", options = list(container = "body"))
    bsTooltip("optional_param1_representation2", "Choose how to represent Optional Param 1: Point Size (variable size) or Point Type (different shapes).", "right", options = list(container = "body"))
    bsTooltip("use_mahalanobis", "Enable Mahalanobis distance filtering using the new formula with λ and ω parameters.", "right", options = list(container = "body"))
    bsTooltip("use_robust_mahalanobis", "Enable Robust Mahalanobis distance filtering using Minimum Covariance Determinant (MCD).", "right", options = list(container = "body"))
    bsTooltip("use_isolation_forest", "Enable Isolation Forest for outlier detection.", "right", options = list(container = "body"))
    bsTooltip("lambda", "Lambda parameter (λ) in the Mahalanobis threshold formula. Controls threshold strictness.", "right", options = list(container = "body"))
    bsTooltip("omega", "Omega parameter (ω) in the Mahalanobis threshold formula. Controls threshold strictness.", "right", options = list(container = "body"))
    bsTooltip("use_iqr_filter", "Apply Interquartile Range (IQR) filtering to remove statistical outliers.", "right", options = list(container = "body"))
    bsTooltip("use_zscore_filter", "Apply Z-score filtering to remove statistical outliers.", "right", options = list(container = "body"))
    bsTooltip("use_mad_filter", "Apply Median Absolute Deviation (MAD) filtering to remove statistical outliers.", "right", options = list(container = "body"))
    bsTooltip("output_format", "Choose the output format for saved plots.", "right", options = list(container = "body"))
  }
  shinyApp(ui, server)
}

# ---- Launch the App ----
run_ternary_gui() 
