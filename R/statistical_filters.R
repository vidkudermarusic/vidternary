# ---- Statistical Filtering Module ----
# This module provides various statistical methods for outlier detection and filtering:
# - IQR (Interquartile Range) filtering - detects positive outliers only (values > Q3 + multiplier*IQR)
# - Z-score filtering - detects positive outliers only (z-scores > threshold)
# - MAD (Median Absolute Deviation) filtering - detects positive outliers only (values > median + threshold*MAD)
#
# Note: All methods now focus on positive outliers (unusually high values) rather than both positive and negative outliers.
# This is useful for detecting high-value anomalies while preserving the main data distribution.
#
# ---- Scientific caveats (read before trusting a result) ----
#
# 1. Union rule across columns. Each method is run over every selected
#    column and a row is flagged if it exceeds the fence in ANY ONE of
#    them (the flags are OR-ed together). If a single column's upper-tail
#    probability under the fence is p, then across k roughly-independent
#    columns the per-row false-positive rate is about 1 - (1 - p)^k, which
#    is larger - often much larger - than the nominal single-column p.
#    Example: MAD's `median + 3*MAD` fence cuts ~1.4% of a normal upper
#    tail; over k = 6 columns that is ~1 - 0.986^6 ~ 8% of rows flagged
#    from noise alone. Select as few columns as the question needs, and
#    read the flagged fraction with k in mind.
#
# 2. Symmetric fences on skewed data. Tukey's `Q3 + 1.5*IQR` fence and the
#    mean/SD z-score both assume a roughly symmetric distribution. Nearly
#    every inclusion measurement this app filters - wt% of a minor
#    element, ECD, area - is strongly right-skewed (approximately
#    log-normal), so a raw upper fence sits only a little above the bulk
#    and systematically flags the legitimate heavy right tail as
#    "outliers". Because these filters are upper-tail-only by design, this
#    is exactly the tail that skew distorts most. MAD about the median is
#    less sensitive but still assumes symmetry. The `log_transform = TRUE`
#    option on every function below computes the fence on `log10(value)`
#    instead, which turns the additive fence into a multiplicative one and
#    is far more appropriate for right-skewed positive data; non-positive
#    and non-finite values are then dropped from the fit and never flagged
#    (they are low values, and these filters only flag high ones).
#
# 3. Z-score small-sample ceiling. In a sample of size n, the largest
#    absolute z-score ANY single point can reach is (n - 1) / sqrt(n)
#    (Shiffler 1988). With the app's default threshold of 3 this means no
#    row can be flagged at all until n >= 11 (at n = 10 the maximum
#    possible z is 2.85, whatever the outlier's magnitude), and the
#    estimate stays badly masked - the outlier inflates the very mean and
#    SD it is measured against - for n well into the hundreds. On small
#    filtered subsets the z-score filter can therefore silently do
#    nothing. `apply_zscore_filter()` emits a `warning()` when n is below
#    the ceiling for the chosen threshold. IQR and MAD do not share this
#    failure mode.

# Largest n at which a single point's |z| can still not exceed `threshold`,
# plus one: the smallest n for which the z-score filter is even capable of
# flagging a row. Solves (n - 1)/sqrt(n) = t for n, i.e. n = s^2 with
# s = (t + sqrt(t^2 + 4)) / 2, then rounds up.
.zscore_sample_ceiling <- function(threshold) {
  s <- (threshold + sqrt(threshold^2 + 4)) / 2
  ceiling(s^2)
}

# The vector a fence is fitted to and compared against. With
# log_transform = FALSE this is the column unchanged. With
# log_transform = TRUE it is log10() of the strictly-positive, finite
# entries, with every other entry set to NA so it is excluded from the
# fence fit (quantile/median/mad/scale all called with na.rm semantics)
# and never satisfies `> upper_bound` (NA comparisons are coerced to FALSE
# by the callers) - correct for these upper-tail-only filters, since a
# zero or negative is a low value, not a high outlier.
.stat_filter_values <- function(x, log_transform) {
  if (!isTRUE(log_transform)) return(x)
  ok <- is.finite(x) & x > 0
  out <- rep(NA_real_, length(x))
  out[ok] <- log10(x[ok])
  out
}

# Outlier-flag helpers (positive outliers only). Each returns a full-length
# logical vector aligned 1:1 with nrow(data), TRUE = flagged as outlier.
# NA values in a filtered column never satisfy "> upper_bound" for real, but
# comparing against NA yields NA (not FALSE) in R, and combining with `|`
# then propagates that NA into outlier_indices. Left unguarded, indexing a
# data frame with an NA logical/position silently inserts a phantom all-NA
# row instead of keeping or dropping the real one - so NA comparisons are
# explicitly coerced to FALSE (never flagged as an outlier) below.
#
# A row is flagged if it exceeds the fence in ANY ONE of `cols` (the
# per-column flags are OR-ed). See caveat 1 in the module header for what
# that does to the effective false-positive rate as the number of columns
# grows.
get_iqr_outlier_flags <- function(data, cols, multiplier = 1.5, log_transform = FALSE) {
  outlier_indices <- logical(nrow(data))
  for (col in cols) {
    if (is.numeric(data[[col]])) {
      v <- .stat_filter_values(data[[col]], log_transform)
      if (all(is.na(v))) next  # e.g. log_transform on an all-non-positive column
      q1 <- quantile(v, 0.25, na.rm = TRUE)
      q3 <- quantile(v, 0.75, na.rm = TRUE)
      iqr <- q3 - q1
      upper_bound <- q3 + multiplier * iqr

      # Only consider positive outliers (values above upper bound)
      col_outliers <- v > upper_bound
      col_outliers[is.na(col_outliers)] <- FALSE
      outlier_indices <- outlier_indices | col_outliers
    }
  }
  outlier_indices
}

get_zscore_outlier_flags <- function(data, cols, threshold = 3, log_transform = FALSE) {
  outlier_indices <- logical(nrow(data))
  for (col in cols) {
    if (is.numeric(data[[col]])) {
      v <- .stat_filter_values(data[[col]], log_transform)
      if (all(is.na(v))) next
      # scale() centres by the NA-removed mean and divides by the
      # NA-removed sample SD, so NA entries stay NA and are handled below.
      z_scores <- as.numeric(scale(v))
      # Only consider positive outliers (positive z-scores above threshold)
      col_outliers <- z_scores > threshold
      col_outliers[is.na(col_outliers)] <- FALSE
      outlier_indices <- outlier_indices | col_outliers
    }
  }
  outlier_indices
}

get_mad_outlier_flags <- function(data, cols, threshold = 3, log_transform = FALSE) {
  outlier_indices <- logical(nrow(data))
  for (col in cols) {
    if (is.numeric(data[[col]])) {
      v <- .stat_filter_values(data[[col]], log_transform)
      if (all(is.na(v))) next
      median_val <- median(v, na.rm = TRUE)
      mad_val <- mad(v, na.rm = TRUE)
      upper_bound <- median_val + threshold * mad_val

      # Only consider positive outliers (values above upper bound)
      col_outliers <- v > upper_bound
      col_outliers[is.na(col_outliers)] <- FALSE
      outlier_indices <- outlier_indices | col_outliers
    }
  }
  outlier_indices
}

#' Filter rows by IQR outlier status (positive/high-value side only)
#'
#' Flags a row as an outlier if any of `cols` exceeds `Q3 + multiplier * IQR`
#' (Tukey, 1977).
#'
#' @details
#' A row is flagged if it crosses the fence in *any one* of `cols`, so the
#' effective per-row false-positive rate grows with the number of columns
#' (roughly `1 - (1 - p)^k` for `k` columns; see the module source header).
#' Tukey's fence assumes a roughly symmetric distribution - on the
#' right-skewed measurements this app filters (wt%, ECD, area) it flags
#' part of the legitimate upper tail. Set `log_transform = TRUE` to fit and
#' apply the fence on `log10(value)` instead, which suits right-skewed
#' positive data; non-positive and non-finite entries are then excluded
#' from the fit and never flagged.
#'
#' @param data A data frame.
#' @param cols Character vector of numeric column names to check.
#' @param multiplier IQR multiplier for the threshold. Default 1.5.
#' @param keep_outliers If `TRUE`, keep only the flagged rows; if `FALSE`
#'   (default), remove them.
#' @param log_transform If `TRUE`, compute the fence on `log10(value)`
#'   (recommended for right-skewed positive data). Default `FALSE`.
#' @return The filtered data frame.
#' @export
apply_iqr_filter <- function(data, cols, multiplier = 1.5, keep_outliers = FALSE, log_transform = FALSE) {
  # A cleared/backspaced numericInput reports as NA_real_ in Shiny, not
  # NULL - is.numeric(NA_real_) is TRUE, so without an explicit is.na()
  # check `multiplier < 0` silently evaluates to NA and if(NA) is a hard
  # "missing value where TRUE/FALSE needed" crash rather than this
  # function's intended graceful stop(), matching the exact hazard already
  # fixed in validate_mahalanobis_inputs() (multivariate.R).
  if (!is.numeric(multiplier) || is.na(multiplier) || multiplier < 0) stop("multiplier must be a non-negative number.")
  # IQR-based outlier filtering - only considers positive outliers (values > Q3 + multiplier*IQR)
  outlier_indices <- get_iqr_outlier_flags(data, cols, multiplier, log_transform)

  if (keep_outliers) {
    # Keep only positive outliers
    filtered_data <- data[outlier_indices, , drop = FALSE]
  } else {
    # Remove positive outliers
    filtered_data <- data[!outlier_indices, , drop = FALSE]
  }

  return(filtered_data)
}

#' Filter rows by Z-score outlier status (positive/high-value side only)
#'
#' Flags a row as an outlier if any of `cols` has a Z-score above `threshold`.
#'
#' @details
#' Two properties of the sample z-score matter here. First, a row is
#' flagged if it crosses `threshold` in *any one* of `cols`, so the
#' effective false-positive rate grows with the number of columns (see the
#' module source header). Second - and specific to this method - the
#' largest \eqn{|z|} any single point can reach in a sample of size `n` is
#' \eqn{(n-1)/\sqrt{n}} (Shiffler, 1988). With the default `threshold = 3`
#' no row can be flagged until `n >= 11`, and the estimate stays heavily
#' masked (the outlier inflates the mean and SD it is judged against) for
#' `n` into the hundreds. This function emits a `warning()` when `n` is
#' below the ceiling for the chosen `threshold`. IQR and MAD do not share
#' this failure mode. `log_transform = TRUE` fits the z-score on
#' `log10(value)`; NA/non-positive entries are ignored and never flagged.
#'
#' @param data A data frame.
#' @param cols Character vector of numeric column names to check.
#' @param threshold Z-score threshold. Default 3.
#' @param keep_outliers If `TRUE`, keep only the flagged rows; if `FALSE`
#'   (default), remove them.
#' @param log_transform If `TRUE`, compute z-scores on `log10(value)`.
#'   Default `FALSE`.
#' @return The filtered data frame.
#' @export
apply_zscore_filter <- function(data, cols, threshold = 3, keep_outliers = FALSE, log_transform = FALSE) {
  # Same NA_real_-from-a-cleared-numericInput hazard as apply_iqr_filter()
  # above - see its comment for the full explanation.
  if (!is.numeric(threshold) || is.na(threshold) || threshold < 0) stop("threshold must be a non-negative number.")

  # Small-sample ceiling: (n-1)/sqrt(n) caps the largest attainable |z|, so
  # below n_min no row can possibly cross `threshold` no matter how extreme
  # it is. Warn rather than fail (the call is still well-defined, it just
  # returns everything/nothing depending on keep_outliers).
  if (is.data.frame(data) && nrow(data) > 0) {
    n_min <- .zscore_sample_ceiling(threshold)
    if (nrow(data) < n_min) {
      max_z <- (nrow(data) - 1) / sqrt(nrow(data))
      warning(sprintf(
        paste0("apply_zscore_filter(): with n = %d row(s) the largest Z-score any single point can reach is %.2f, ",
               "below the threshold of %g - no row can be flagged. Use IQR or MAD, lower the threshold, ",
               "or run on n >= %d rows."),
        nrow(data), max_z, threshold, n_min), call. = FALSE)
    }
  }

  # Z-score based outlier filtering - only considers positive outliers (z-scores > threshold)
  outlier_indices <- get_zscore_outlier_flags(data, cols, threshold, log_transform)

  if (keep_outliers) {
    # Keep only positive outliers
    filtered_data <- data[outlier_indices, , drop = FALSE]
  } else {
    # Remove positive outliers
    filtered_data <- data[!outlier_indices, , drop = FALSE]
  }

  return(filtered_data)
}

#' Filter rows by MAD outlier status (positive/high-value side only)
#'
#' Flags a row as an outlier if any of `cols` exceeds
#' `median + threshold * MAD` (Leys et al., 2013).
#'
#' @details
#' `mad()` uses R's default `constant = 1.4826`, so for normal data
#' `median + 3*MAD` approximates `median + 3*sigma`. A row is flagged if it
#' crosses the fence in *any one* of `cols`, so the effective
#' false-positive rate grows with the number of columns (see the module
#' source header). MAD about the median is more robust than the IQR or
#' z-score fences but still assumes symmetry; `log_transform = TRUE` fits
#' it on `log10(value)` for right-skewed positive data, excluding
#' non-positive and non-finite entries from the fit.
#'
#' @param data A data frame.
#' @param cols Character vector of numeric column names to check.
#' @param threshold MAD multiplier for the threshold. Default 3.
#' @param keep_outliers If `TRUE`, keep only the flagged rows; if `FALSE`
#'   (default), remove them.
#' @param log_transform If `TRUE`, compute the fence on `log10(value)`.
#'   Default `FALSE`.
#' @return The filtered data frame.
#' @export
apply_mad_filter <- function(data, cols, threshold = 3, keep_outliers = FALSE, log_transform = FALSE) {
  # Same NA_real_-from-a-cleared-numericInput hazard as apply_iqr_filter()
  # above - see its comment for the full explanation.
  if (!is.numeric(threshold) || is.na(threshold) || threshold < 0) stop("threshold must be a non-negative number.")
  # Median Absolute Deviation (MAD) based filtering - only considers positive outliers (values > median + threshold*MAD)
  outlier_indices <- get_mad_outlier_flags(data, cols, threshold, log_transform)

  if (keep_outliers) {
    # Keep only positive outliers
    filtered_data <- data[outlier_indices, , drop = FALSE]
  } else {
    # Remove positive outliers
    filtered_data <- data[!outlier_indices, , drop = FALSE]
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

# generate_stats()/compute_correlation() used to live here too - both
# @export'ed, but confirmed dead: a full cross-reference of every call site
# in R/, tests/, and vignettes/ found zero real callers anywhere in the
# current app. Their only actual callers were in legacy/App6.0.1.R (the
# pre-modularization script - not sourced by anything, see this package's
# own "Repository hygiene" audit history for why that's healthy archival
# practice rather than live code). The app's own current, more specialized
# machinery has since grown past what these did: build_descriptive_stats_
# table() (stats_display_utils.R) covers generate_stats()'s mean/median/SD,
# and the Data Comparison tab's own correlation handlers
# (server_data_comparison_stats.R) call stats::cor() directly rather than
# through this wrapper. Confirmed via the user before removing, since both
# were real, documented public API (exported, with their own man/ pages)
# even though nothing internal used them.

# Note: Functions are exported via NAMESPACE file
