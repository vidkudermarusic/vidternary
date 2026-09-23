# ---- Multivariate Analysis Module ----
# This module handles all multivariate analysis methods including:
# - Standard Mahalanobis Distance
# - Mahalanobis Distance
# - Isolation Forest
# - Data validation for multivariate analysis

#' Validate Mahalanobis distance parameters
#'
#' Raises an error (via `stop()`) for invalid values, and a `warning()` for
#' valid-but-suspicious ones (e.g. an extreme lambda/omega gap, or a very
#' high manual threshold).
#'
#' @param lambda Sensitivity parameter for the automatic threshold formula (non-negative).
#' @param omega Leniency parameter for the automatic threshold formula (non-negative).
#' @param custom_mdthresh Manual distance threshold, required (positive numeric) when `mdthresh_mode == "manual"`.
#' @param mdthresh_mode Either `"auto"` or `"manual"`.
#' @param selected_columns Optional character vector of column names; if
#'   given, must have at least 2 entries.
#' @return `TRUE`, invisibly, if all checks pass.
#' @export
validate_mahalanobis_inputs <- function(lambda, omega, custom_mdthresh, mdthresh_mode, selected_columns) {
  # Validate threshold mode and parameters
  # A cleared/backspaced numericInput reports as NA_real_ in Shiny, not
  # NULL - is.numeric(NA_real_) is TRUE, so without an explicit is.na()
  # check the comparisons below (<=0, <0) silently evaluate to NA and
  # if(NA) is a hard error ("missing value where TRUE/FALSE needed")
  # rather than the intended validate()-style stop(). Reachable simply by
  # clicking into lambda/omega/the manual threshold field and pressing
  # backspace - no malformed file or edge-case dataset needed.
  if (mdthresh_mode == "manual") {
    if (is.null(custom_mdthresh) || !is.numeric(custom_mdthresh) || is.na(custom_mdthresh) || custom_mdthresh <= 0) {
      stop("Custom threshold must be a positive numeric value")
    }
    if (custom_mdthresh > 10000) {
      warning("Custom threshold value is very high (", custom_mdthresh, "). This may result in no outliers being detected.")
    }
  } else {
    # Automatic mode validation
    if (!is.numeric(lambda) || is.na(lambda) || lambda < 0) {
      stop("Lambda parameter must be a non-negative numeric value")
    }
    if (!is.numeric(omega) || is.na(omega) || omega < 0) {
      stop("Omega parameter must be a non-negative numeric value")
    }
    # The automatic threshold formula divides by (100 + lambda - omega) inside
    # a sqrt() - at or below this boundary that denominator is non-positive,
    # so sqrt() silently returns NaN (no R warning) instead of a threshold.
    if (lambda - omega <= -100) {
      stop("Omega is too large relative to lambda (lambda - omega = ", lambda - omega,
           "). This combination makes the automatic threshold formula undefined - ",
           "increase lambda, reduce omega, or use a manual threshold instead.")
    }
    if (lambda - omega < -50) {
      warning("Lambda - omega difference is very negative (", lambda - omega, "). This may result in a very lenient threshold (few or no outliers detected).")
    }
    if (lambda - omega > 100) {
      warning("Lambda - omega difference is very positive (", lambda - omega, "). This may result in a very strict threshold (many points flagged as outliers).")
    }
  }
  
  # Validate selected columns if provided
  if (!is.null(selected_columns)) {
    if (!is.character(selected_columns) || length(selected_columns) == 0) {
      stop("Selected columns must be a non-empty character vector")
    }
    if (length(selected_columns) < 2) {
      stop("At least 2 columns must be selected for Mahalanobis distance calculation")
    }
  }
  
  return(TRUE)
}

#' Validate and clean two datasets for multivariate analysis
#'
#' Checks that `selected_columns` exist, are numeric, and number at least
#' 2, in both datasets; drops incomplete rows; checks each dataset has
#' enough observations relative to the number of variables
#' (`min_obs_ratio`); and warns (without erroring) on zero-variance
#' columns, high pairwise correlations, or a poorly-conditioned covariance
#' matrix in `data2`.
#'
#' @param data1 First dataset, as a data frame.
#' @param data2 Second (typically reference) dataset, as a data frame.
#' @param selected_columns Character vector of numeric column names to
#'   use, mandatory (length >= 2).
#' @param method Method name used only in the sample-size error message.
#'   Default `"Multivariate Analysis"`.
#' @param min_obs_ratio Minimum ratio of complete observations to
#'   variables required in each dataset. Default 2.
#' @return A list: `data1_clean`, `data2_clean` (complete-case data
#'   frames), `common_cols`, `n_vars`, `n_obs1`, `n_obs2`,
#'   `condition_number`, `high_correlations`, `zero_var_cols`.
#' @export
validate_multivariate_data <- function(data1, data2, selected_columns, method = "Multivariate Analysis", min_obs_ratio = 2) {
  # MANDATORY COLUMN SELECTION: User must select columns for multivariate analysis
  if (is.null(selected_columns) || length(selected_columns) == 0) {
    stop("Column selection is MANDATORY for multivariate analysis. Please select at least 2 numeric columns before proceeding.")
  }
  
  # Validate that selected columns exist in both datasets
  missing_in_data1 <- setdiff(selected_columns, colnames(data1))
  missing_in_data2 <- setdiff(selected_columns, colnames(data2))
  
  if (length(missing_in_data1) > 0) {
    stop("Selected columns missing in data1: ", paste(missing_in_data1, collapse = ", "))
  }
  if (length(missing_in_data2) > 0) {
    stop("Selected columns missing in data2: ", paste(missing_in_data2, collapse = ", "))
  }
  
  # Check that selected columns are numeric
  non_numeric_in_data1 <- selected_columns[!sapply(data1[, selected_columns, drop = FALSE], is.numeric)]
  non_numeric_in_data2 <- selected_columns[!sapply(data2[, selected_columns, drop = FALSE], is.numeric)]
  
  if (length(non_numeric_in_data1) > 0) {
    stop("Non-numeric selected columns in data1: ", paste(non_numeric_in_data1, collapse = ", "))
  }
  if (length(non_numeric_in_data2) > 0) {
    stop("Non-numeric selected columns in data2: ", paste(non_numeric_in_data2, collapse = ", "))
  }
  
  # Ensure minimum number of columns
  if (length(selected_columns) < 2) {
    stop("At least 2 numeric columns must be selected for multivariate analysis. Currently selected: ", length(selected_columns))
  }
  
  common_cols <- selected_columns
  
  # Extract common columns and remove rows with missing values
  data1_clean <- data1[, common_cols, drop = FALSE]
  data2_clean <- data2[, common_cols, drop = FALSE]
  
  # Remove rows with missing values
  complete_cases1 <- complete.cases(data1_clean)
  complete_cases2 <- complete.cases(data2_clean)
  
  data1_clean <- data1_clean[complete_cases1, , drop = FALSE]
  data2_clean <- data2_clean[complete_cases2, , drop = FALSE]
  
  # Check sample size requirements
  n_vars <- length(common_cols)
  n_obs1 <- nrow(data1_clean)
  n_obs2 <- nrow(data2_clean)
  
  if (n_obs1 < n_vars * min_obs_ratio) {
    stop(sprintf("Insufficient observations in data1: %d observations for %d variables (need at least %d)", 
                 n_obs1, n_vars, n_vars * min_obs_ratio))
  }
  
  if (n_obs2 < n_vars * min_obs_ratio) {
    stop(sprintf("Insufficient observations in data2: %d observations for %d variables (need at least %d)", 
                 n_obs2, n_vars, n_vars * min_obs_ratio))
  }
  
  # Check for zero variance columns
  zero_var_cols <- sapply(common_cols, function(col) {
    var1 <- var(data1_clean[[col]], na.rm = TRUE)
    var2 <- var(data2_clean[[col]], na.rm = TRUE)
    # var() doesn't error on a column containing Inf/-Inf - it silently
    # returns NaN instead (Inf - Inf = NaN inside the sum of squares). A
    # plain `var1 == 0 || var2 == 0` then evaluates to NA rather than
    # FALSE, which sapply() carries into zero_var_cols, and any(zero_var_cols)
    # below propagates that NA straight into `if (any(...))` - a hard
    # "missing value where TRUE/FALSE needed" crash on R >= 4.3, reachable
    # by any dataset containing a single Inf value in a selected column (no
    # malformed file needed - e.g. a stray division-by-zero further
    # upstream). An unusable (Inf-corrupted, or otherwise non-computable)
    # variance is just as much a reason to warn as an exactly-zero one, so
    # it's folded into the same TRUE result here instead of being left to
    # propagate as NA.
    is.na(var1) || is.na(var2) || var1 == 0 || var2 == 0
  })
  
  if (any(zero_var_cols)) {
    zero_var_names <- common_cols[zero_var_cols]
    warning("Zero variance columns detected: ", paste(zero_var_names, collapse = ", "), 
            ". These may cause issues in multivariate analysis.")
  }
  
  # Check for high correlations (potential multicollinearity)
  high_correlations <- NULL
  if (n_vars > 2) {
    cor_matrix1 <- cor(data1_clean, use = "pairwise.complete.obs")
    cor_matrix2 <- cor(data2_clean, use = "pairwise.complete.obs")
    
    # Find high correlations (>0.9) excluding diagonal
    high_cor1 <- which(abs(cor_matrix1) > 0.9 & cor_matrix1 != 1, arr.ind = TRUE)
    high_cor2 <- which(abs(cor_matrix2) > 0.9 & cor_matrix2 != 1, arr.ind = TRUE)
    
    if (nrow(high_cor1) > 0 || nrow(high_cor2) > 0) {
      high_correlations <- list(
        data1 = if (nrow(high_cor1) > 0) {
          data.frame(
            var1 = rownames(cor_matrix1)[high_cor1[, 1]],
            var2 = colnames(cor_matrix1)[high_cor1[, 2]],
            correlation = cor_matrix1[high_cor1]
          )
        } else NULL,
        data2 = if (nrow(high_cor2) > 0) {
          data.frame(
            var1 = rownames(cor_matrix2)[high_cor2[, 1]],
            var2 = colnames(cor_matrix2)[high_cor2[, 2]],
            correlation = cor_matrix2[high_cor2]
          )
        } else NULL
      )
      
      warning("High correlations (>0.9) detected. This may cause multicollinearity issues.")
    }
  }
  
  # Check condition number for numerical stability. condition_number is
  # pre-declared here (rather than only inside the tryCatch) because the
  # error handler is its own function closure - a plain `<-` inside it
  # would create a throwaway local that vanishes when the handler returns,
  # leaving condition_number undefined in this frame and crashing the
  # `!is.na(condition_number)` check below on the very error path this was
  # meant to handle gracefully. `<<-` in the handler now finds this
  # pre-declared binding and updates it in place instead.
  condition_number <- NA
  tryCatch({
    # Use data2 (reference) for condition number calculation
    cov_matrix <- cov(data2_clean, use = "complete.obs")
    eigenvals <- eigen(cov_matrix, only.values = TRUE)$values
    condition_number <- max(eigenvals) / min(eigenvals)
  }, error = function(e) {
    condition_number <<- NA
    warning("Could not calculate condition number: ", e$message)
  })
  
  if (!is.na(condition_number) && condition_number > 1e10) {
    warning("High condition number (", format(condition_number, scientific = TRUE), 
            ") detected. This may indicate numerical instability.")
  }
  
  return(list(
    data1_clean = data1_clean,
    data2_clean = data2_clean,
    common_cols = common_cols,
    n_vars = n_vars,
    n_obs1 = n_obs1,
    n_obs2 = n_obs2,
    condition_number = condition_number,
    high_correlations = high_correlations,
    zero_var_cols = common_cols[zero_var_cols]
  ))
}

#' Compute Mahalanobis distances and flag outliers
#'
#' Computes the Mahalanobis distance of each `data1` row from `data2`'s
#' distribution over `selected_columns`, then flags outliers against
#' either a manual threshold or the automatic formula `MDthresh = MDmean +
#' sqrt(100/(100 + lambda - omega)) * stdMD` (Vode et al., 2022,
#' <https://doi.org/10.3390/ma15020684>). `MDmean`/`stdMD` are the mean/SD
#' of `data2`'s own distances from itself (its baseline spread as a
#' "normal" reference), not of the `data1` distances being tested -
#' otherwise outliers already present in `data1` would inflate the very
#' mean/SD used to judge them.
#'
#' @param data1 Dataset whose points are scored, as a data frame.
#' @param data2 Reference dataset the distribution is fit to, as a data frame.
#' @param lambda Sensitivity parameter for the automatic threshold. Default 1.
#' @param omega Leniency parameter for the automatic threshold. Default 0.
#' @param keep_outliers Currently stored on the result but not used to
#'   filter within this function. Default `FALSE`.
#' @param custom_mdthresh Manual distance threshold, used when `mdthresh_mode == "manual"`.
#' @param selected_columns Character vector of at least 2 numeric column names to use.
#' @param mdthresh_mode Either `"auto"` (default) or `"manual"`.
#' @return A list: `distances`, `MDthresh`, `MDmean`, `stdMD`, `lambda`,
#'   `omega`, `outlier_95`/`outlier_99`/`outlier_custom` (counts),
#'   `total_points`, `df`, `common_cols`, `outlier_indices`,
#'   `keep_outliers`, `threshold_method`, `threshold_formula`.
#' @export
compute_mahalanobis_distance <- function(data1, data2, lambda = 1, omega = 0, keep_outliers = FALSE, custom_mdthresh = NULL, selected_columns, mdthresh_mode = "auto") {
  # Input validation
  validate_mahalanobis_inputs(lambda, omega, custom_mdthresh, mdthresh_mode, selected_columns)
  
  # UNIFIED VALIDATION: Apply consistent pre-checks across all multivariate methods
  validation_result <- validate_multivariate_data(data1, data2, selected_columns, method = "Mahalanobis Distance", min_obs_ratio = 2)
  
  # Extract validated data
  data1_clean <- validation_result$data1_clean
  data2_clean <- validation_result$data2_clean
  common_cols <- validation_result$common_cols
  
  # Log validation results for debugging
  debug_log("DEBUG: Mahalanobis validation passed")
  debug_log("DEBUG: Variables: %d", validation_result$n_vars)
  debug_log("DEBUG: Observations (data): %d", validation_result$n_obs1)
  debug_log("DEBUG: Observations (reference): %d", validation_result$n_obs2)
  debug_log("DEBUG: Condition number: %.2e", validation_result$condition_number)
  if (!is.null(validation_result$high_correlations)) {
    debug_log("DEBUG: High correlations detected")
  }
  
  # Calculate covariance matrix from reference dataset (data2)
  cov_matrix <- cov(data2_clean, use = "complete.obs")
  
  # Check for singular matrix and handle it. cov_inv is pre-declared here so
  # the error handler's `cov_inv <<-` (below) finds and updates this local
  # binding - without it, since the try-block's own `cov_inv <- solve(...)`
  # never completes on the error path, `<<-` would fall through past this
  # function's frame entirely and create/overwrite a variable in the global
  # environment instead (cov_inv itself is never read after this block -
  # mahalanobis() uses cov_matrix directly - so this was harmless in
  # practice, but a real scoping mistake worth not leaving in place).
  cov_inv <- NULL
  tryCatch({
    # Try to calculate the inverse of the covariance matrix
    cov_inv <- solve(cov_matrix)
  }, error = function(e) {
    if (grepl("singular", e$message, ignore.case = TRUE)) {
      # If matrix is singular, try using a regularized version
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Singular covariance matrix detected. Applying regularization.\n")
      }
      
      # Add small regularization to diagonal
      n_vars <- ncol(cov_matrix)
      regularization <- 1e-6 * diag(n_vars)
      cov_matrix <<- cov_matrix + regularization
      
      # Try again
      cov_inv <<- solve(cov_matrix)
      
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Regularization applied successfully.\n")
      }
    } else {
      stop("Covariance matrix error: ", e$message)
    }
  })
  
  # Calculate Mahalanobis distances for data1 relative to data2 - these are
  # the per-row scores actually compared against MDthresh below.
  mahal_distances <- mahalanobis(data1_clean,
                                 center = colMeans(data2_clean),
                                 cov = cov_matrix)

  # Calculate threshold based on mode. MDmean/stdMD calibrate the threshold
  # and MUST come from the reference dataset's own distances (data2 scored
  # against itself), not from mahal_distances (data1 scored against data2)
  # above. Using data1's own distances here would let genuine outliers in
  # data1 inflate the very mean/SD used to judge them - the same masking
  # effect this codebase already guards against for the Z-score filter (see
  # statistical_filters.R's module header). This also keeps "self" reference
  # mode unchanged: when data1 and data2 are the same dataset (self-reference
  # in apply_multivariate_filtering()), ref_distances and mahal_distances are
  # identical, so MDmean/stdMD come out exactly as before.
  ref_distances <- mahalanobis(data2_clean,
                               center = colMeans(data2_clean),
                               cov = cov_matrix)
  MDmean <- mean(ref_distances)
  stdMD <- sd(ref_distances)
  
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: Threshold calculation:\n")
    cat("DEBUG: mdthresh_mode =", mdthresh_mode, "\n")
    cat("DEBUG: custom_mdthresh =", if (is.null(custom_mdthresh)) "NULL" else custom_mdthresh, "\n")
    cat("DEBUG: lambda =", lambda, ", omega =", omega, "\n")
    cat("DEBUG: MDmean =", MDmean, ", stdMD =", stdMD, "\n")
  }
  
  if (mdthresh_mode == "manual" && !is.null(custom_mdthresh)) {
    MDthresh <- custom_mdthresh
    threshold_method <- "Manual"
    threshold_formula <- sprintf("Manual threshold: %.3f", custom_mdthresh)
    if (getOption("ternary.debug", FALSE)) cat("DEBUG: Using MANUAL threshold:", MDthresh, "\n")
  } else {
    # Default to automatic mode
    if (getOption("ternary.debug", FALSE)) {
      cat("DEBUG: Mode selection: Defaulting to AUTOMATIC mode\n")
      cat("DEBUG: mdthresh_mode was:", mdthresh_mode, "(not 'manual')\n")
    }
    # This automatic threshold (MDmean + sqrt(100/(100+lambda-omega)) * stdMD)
    # is not an ad-hoc heuristic - it is the empirical Mahalanobis-distance
    # cutoff formula from:
    #   Vode, F., Tehovnik, F., Kosec, G., Steiner Petrovic, D. (2022).
    #   "Classification of Hot-Rolled Plates Using the Mahalanobis Distance
    #   of NMIs in Ti-Stabilized Austenitic Stainless-Steel Produced by
    #   Secondary Metallurgy." Materials, 15(2), 684. MDPI.
    #   https://doi.org/10.3390/ma15020684
    # lambda/omega are the article's sensitivity/leniency parameters, not
    # general-purpose statistical knobs, which is why they are kept separate
    # from the Z-score/MAD thresholds elsewhere in this codebase.
    MDthresh <- MDmean + sqrt(100/(100 + lambda - omega)) * stdMD
    threshold_method <- "Automatic (MDthresh=MDmean+sqrt(100/(100+lambda-omega))xstdMD)"
    threshold_formula <- sprintf("MDthresh = %.3f + sqrt(100/(100+%.1f-%.1f)) x %.3f = %.3f", 
                               MDmean, lambda, omega, stdMD, MDthresh)
    if (getOption("ternary.debug", FALSE)) cat("DEBUG: Using AUTOMATIC threshold:", MDthresh, "\n")
  }
  
  # Calculate additional thresholds for reference
  threshold_95 <- qchisq(0.95, df = length(common_cols))
  threshold_99 <- qchisq(0.99, df = length(common_cols))
  
  # Identify outliers based on custom threshold. A comparison against NA/NaN
  # yields NA, and indexing a data frame with an NA logical silently inserts
  # a phantom all-NA row instead of dropping or keeping it - so NA results
  # here must not flow through uncoerced, matching how every other filter in
  # this codebase (statistical_filters.R, compute_isolation_forest() below)
  # already treats this exact hazard.
  outlier_indices <- mahal_distances > MDthresh
  outlier_indices[is.na(outlier_indices)] <- FALSE
  
  if (getOption("ternary.debug", FALSE)) {
    cat("DEBUG: Outlier detection:\n")
    cat("DEBUG: Threshold used:", MDthresh, "\n")
    cat("DEBUG: Min distance:", min(mahal_distances), "\n")
    cat("DEBUG: Max distance:", max(mahal_distances), "\n")
    cat("DEBUG: Outliers detected:", sum(outlier_indices), "out of", length(mahal_distances), "\n")
    cat("DEBUG: Outlier percentage:", round(100 * sum(outlier_indices) / length(mahal_distances), 1), "%\n")
  }
  
  return(list(
    distances = mahal_distances,
    MDthresh = MDthresh,
    MDmean = MDmean,
    stdMD = stdMD,
    lambda = lambda,
    omega = omega,
    # na.rm = TRUE matches the NA-coercion outlier_indices already gets a
    # few lines up: an Inf value anywhere in the selected columns can make
    # mahal_distances contain NA/NaN entries, and a plain sum() over a
    # logical vector containing NA returns NA itself instead of a count.
    outlier_95 = sum(mahal_distances > threshold_95, na.rm = TRUE),
    outlier_99 = sum(mahal_distances > threshold_99, na.rm = TRUE),
    outlier_custom = sum(outlier_indices),
    total_points = length(mahal_distances),
    df = length(common_cols),
    common_cols = common_cols,
    outlier_indices = outlier_indices,
    keep_outliers = keep_outliers,
    threshold_method = threshold_method,
    threshold_formula = threshold_formula
  ))
}

# Robust Mahalanobis function removed - use standard Mahalanobis instead


#' Flag outliers in one dataset using an Isolation Forest trained on another
#'
#' Trains an isolation forest (`isotree::isolation.forest()`) on `data2`
#' (the reference), derives an anomaly-score threshold from `data2`'s own
#' scores at the `1 - contamination` quantile, then scores `data1` against
#' that threshold. Liu, Ting & Zhou (2008), <https://doi.org/10.1109/ICDM.2008.17>.
#'
#' @param data1 Dataset to score for outliers, as a data frame.
#' @param data2 Reference dataset the model is trained on, as a data frame.
#' @param selected_columns Character vector of column names to use for analysis.
#' @param contamination Expected proportion of outliers (0-1). Default 0.10.
#'   User-adjustable in the UI (Ternary Plots' Isolation Forest panel,
#'   Data Comparison's Isolation Forest/Comprehensive panels) - both surface
#'   whatever value was actually used in their results text, since a
#'   reviewer reading the app's own output should be able to see it without
#'   reading source.
#' @param keep_outliers If `TRUE`, `kept_mask`/`filtered_data1` keep the
#'   flagged outliers instead of the inliers. Default `FALSE`.
#' @param ntrees Number of trees in the isolation forest. Default 200.
#'   User-adjustable in the UI, same as `contamination` above.
#' @param score_type Score type passed to `predict.isolation_forest()`, `"score"` or `"outlier"`. Default `"score"`.
#' @param seed Random seed for reproducibility, passed through directly to
#'   `isotree::isolation.forest()`'s own `seed` argument (which governs its
#'   randomness independently of R's `set.seed()` - confirmed empirically,
#'   not assumed). Default 42. User-adjustable in the UI, same as
#'   `contamination`/`ntrees` above - change it to sensitivity-check
#'   whether outlier calls are stable across different random forest
#'   realizations.
#' @param sample_size Number of reference rows each tree is trained on.
#'   `NULL` (default) uses every complete reference row - simple and fully
#'   reproducible, but a departure from the algorithm as published: Liu,
#'   Ting & Zhou (2008) sub-sample (their default `psi = 256`) specifically
#'   so trees stay shallow and the anomaly-score normalisation `c(psi)`
#'   matches the sub-sample. Pass a whole number `>= 2` to sub-sample that
#'   many rows per tree instead; a value above the number of complete
#'   reference rows is clamped down to it. The value actually used is
#'   reported back in `sample_size`.
#' @return A list: `model` (the fitted isolation forest), `columns_used`,
#'   `threshold`, `contamination`, `ntrees`, `seed`, `sample_size` (the row count
#'   each tree was actually trained on - either every complete reference
#'   row, or the sub-sample size requested, clamped to the rows available),
#'   `scores` (length `nrow(data1)`), `outlier_indices`,
#'   `kept_mask`, `filtered_data1`, `ref_scores_sum`.
#' @export
compute_isolation_forest <- function(
  data1, data2, selected_columns,
  contamination = 0.10,
  keep_outliers = FALSE,
  ntrees = 200,
  score_type = "score",
  seed = 42,
  sample_size = NULL
) {
  stopifnot(is.data.frame(data1), is.data.frame(data2))

  # A cleared/backspaced numericInput reports as NA_real_ in Shiny (see
  # validate_mahalanobis_inputs() for the fuller explanation of this exact
  # hazard), and an out-of-(0,1)-range contamination reaches
  # stats::quantile(scores_ref, 1 - contamination) below and fails there
  # with a raw "'probs' outside [0,1]" error - or, for NA specifically,
  # doesn't error at all: quantile() with an NA prob silently returns
  # NA_real_ as the threshold, which then makes `scores1 >= threshold` NA
  # for every row and outlier_indices FALSE for every row - a silently
  # broken "zero outliers detected" result with no error or warning at
  # all. Both are caught here with one clear, friendly stop() instead.
  if (!is.numeric(contamination) || length(contamination) != 1L || is.na(contamination) ||
      contamination <= 0 || contamination >= 1) {
    stop("contamination must be a single numeric value strictly between 0 and 1.")
  }

  # Same NA_real_-from-a-cleared-numericInput hazard as contamination above -
  # now that ntrees is user-adjustable via the UI (previously a fixed
  # internal default), it needs the identical guard. isotree::isolation.
  # forest(ntrees = NA) doesn't fail cleanly - it errors deep inside the
  # C++ backend with a message naming no R-level argument at all.
  if (!is.numeric(ntrees) || length(ntrees) != 1L || is.na(ntrees) || ntrees < 1 || ntrees != round(ntrees)) {
    stop("ntrees must be a single positive whole number.")
  }

  # sample_size: NULL keeps the original "use every complete reference row"
  # behaviour; a number requests classic sub-sampling. Same
  # NA-from-a-cleared-numericInput guard as ntrees above. Clamped to the
  # available row count below (once X2c is known), not here.
  if (!is.null(sample_size)) {
    if (!is.numeric(sample_size) || length(sample_size) != 1L || is.na(sample_size) ||
        sample_size < 2 || sample_size != round(sample_size)) {
      stop("sample_size must be NULL (use all reference rows) or a single whole number >= 2.")
    }
  }

  # Same NA_real_-from-a-cleared-numericInput hazard as ntrees/contamination
  # above - now that seed is also user-adjustable via the UI (previously a
  # fixed internal default), it needs the identical guard.
  # isotree::isolation.forest(seed = NA) doesn't fail cleanly either.
  if (!is.numeric(seed) || length(seed) != 1L || is.na(seed) || seed != round(seed)) {
    stop("seed must be a single whole number.")
  }

  # Check for isotree package
  if (!requireNamespace("isotree", quietly = TRUE)) {
    stop("Package 'isotree' is required for isolation forest outlier detection. Please install it first.")
  }

  # 1) Subset and clean
  common_cols <- intersect(selected_columns, intersect(colnames(data1), colnames(data2)))
  if (length(common_cols) < 2L) stop("Too few numeric variables in common between the two datasets.")

  X1 <- data1[, common_cols, drop = FALSE]
  X2 <- data2[, common_cols, drop = FALSE]

  # Non-numeric selected columns produce a clear stop() here, matching the
  # Mahalanobis path (validate_multivariate_data()), which hard-stops with
  # a message naming exactly which selected columns are non-numeric.
  non_numeric <- common_cols[!vapply(X1, is.numeric, logical(1)) | !vapply(X2, is.numeric, logical(1))]
  if (length(non_numeric) > 0) {
    stop("Selected columns are not numeric: ", paste(non_numeric, collapse = ", "))
  }

  # keep only numeric columns (safety net - by this point the check
  # above has already confirmed every selected column is numeric)
  num_cols <- names(X1)[vapply(X1, is.numeric, logical(1))]
  X1 <- X1[, num_cols, drop = FALSE]
  X2 <- X2[, num_cols, drop = FALSE]
  if (ncol(X1) < 2L) stop("Too few columns remain after keeping only the numeric ones.")

  # remove constants / NA rows
  nzv <- vapply(X2, function(v) length(unique(na.omit(v))) > 1L, logical(1))
  X1 <- X1[, nzv, drop = FALSE]
  X2 <- X2[, nzv, drop = FALSE]
  # The column-count check above runs before this near-zero-variance
  # filter, so it can't catch the filter itself dropping columns below 2 -
  # re-checked here with the same friendly-error convention as the rest
  # of this function, instead of surfacing a raw isotree error.
  if (ncol(X2) < 2L) stop("Too few variables remain after dropping zero-variance columns.")

  cc1 <- complete.cases(X1); cc2 <- complete.cases(X2)
  X1c <- X1[cc1, , drop = FALSE]
  X2c <- X2[cc2, , drop = FALSE]
  # Unlike the Mahalanobis path (validate_multivariate_data(), which
  # enforces a minimum observation count before reaching this point), the
  # reference dataset here needs its own explicit check for at least 2
  # complete rows before isotree::isolation.forest() is called.
  if (nrow(X2c) < 2L) stop("The reference dataset has too few complete rows for the isolation forest (at least 2 are required).")
  # Mirrors the reference-dataset guard just above, but for the TARGET
  # dataset (data1): unlike the reference (which needs >= 2 rows to fit a
  # meaningful model), scoring a single complete row against an
  # already-fitted model is well-defined, so the floor here is 1 row.
  if (nrow(X1c) < 1L) stop("The target dataset has no complete rows for the isolation forest (the selected columns are missing in every row).")

  # 2) Training on the reference
  #
  # sample_size (rows each tree trains on): NULL -> every complete
  # reference row (nrow(X2c)); a number -> that many rows per tree, the
  # classic Isolation Forest sub-sampling (Liu, Ting & Zhou 2008, psi=256),
  # clamped to the rows actually available. `ss` is what was really used and
  # is reported back so no caller's results text is ever out of step with
  # it.
  # isotree::isolation.forest()'s randomness is governed entirely by its
  # OWN internal `seed` argument (own default 1) - confirmed empirically,
  # not assumed: R's set.seed() has zero effect on its output regardless
  # of what it's set to.
  ss <- if (is.null(sample_size)) nrow(X2c) else min(as.integer(sample_size), nrow(X2c))
  iso_model <- isotree::isolation.forest(
    X2c,
    ntrees = ntrees,
    sample_size = ss,
    seed = seed
  )

  # 3) Threshold from REFERENCE scores
  scores_ref <- as.numeric(predict(iso_model, X2c, type = score_type))
  threshold  <- as.numeric(stats::quantile(scores_ref, 1 - contamination, na.rm = TRUE))

  # 4) Scores for data1 + flagging outliers
  scores1_c  <- as.numeric(predict(iso_model, X1c, type = score_type))
  # map back to original order
  scores1 <- rep(NA_real_, nrow(X1)); scores1[cc1] <- scores1_c
  outlier_indices <- !is.na(scores1) & (scores1 >= threshold)

  # 5) Export of filtered data (optional)
  kept <- if (keep_outliers) outlier_indices else !outlier_indices
  kept[is.na(kept)] <- FALSE

  return(list(
    model            = iso_model,
    columns_used     = colnames(X1c),
    threshold        = threshold,
    contamination    = contamination,
    ntrees           = ntrees,
    seed             = seed,                      # echoed back so callers can show it, matching ntrees/contamination/sample_size
    sample_size      = ss,                        # == nrow(X2c) - see comment above
    scores           = scores1,                  # length = nrow(data1)
    outlier_indices  = outlier_indices,          # logical vector for data1
    kept_mask        = kept,                     # what is kept, per keep_outliers
    filtered_data1   = data1[kept, , drop = FALSE],
    ref_scores_sum   = summary(scores_ref)       # for QC
  ))
}

# Note: Functions are exported via NAMESPACE file

