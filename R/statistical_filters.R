# =============================================================================
# vidternary: Statistical Filtering Module
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Statistical methods for outlier detection and data filtering
#              including IQR, Z-score, and MAD-based filtering approaches.
# 
# Key Functions:
#   - apply_iqr_filter(): Interquartile Range-based outlier filtering
#   - apply_zscore_filter(): Z-score-based outlier filtering
#   - apply_mad_filter(): Median Absolute Deviation-based filtering
#   - collect_filters(): Filter collection and validation utilities
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - moments
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

# IQR-based outlier filtering
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

# Z-score based outlier filtering
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

# Median Absolute Deviation (MAD) based filtering
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

# Note: check_data_quality function is now in helpers.R to avoid duplication
# Use the version from helpers.R for comprehensive data quality checking

# Note: calculate_quality_score function is now in helpers.R to avoid duplication
# Use the version from helpers.R for comprehensive quality scoring

# Basic statistical functions
# Note: validate_data function is now in helpers.R as validate_data_enhanced to avoid duplication
# Use the enhanced version from helpers.R for comprehensive data validation

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

# Note: Functions are exported via NAMESPACE file
