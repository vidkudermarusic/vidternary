# ---- Compositional Data Analysis (CLR / ILR) ----
# Wt% chemistry columns are compositional data: they're constrained to sum
# to (approximately) a constant, so they live on a simplex, not in
# ordinary Euclidean space. Applying ordinary statistics/PCA/correlation
# directly to raw percentages is technically inappropriate (the "closure
# problem" - e.g. spurious negative correlations are induced purely by the
# sum constraint). The standard fix is a log-ratio transform before doing
# any multivariate analysis.
#
# CLR (centered log-ratio): clr_i = ln(x_i) - mean(ln(x)) = ln(x_i / g(x))
# where g(x) is the geometric mean of the row. Each CLR coordinate still
# corresponds directly to one original element, so loadings/biplots stay
# directly interpretable - but the CLR covariance matrix is singular
# (the D coordinates always sum to 0), which some downstream methods
# don't tolerate.
#
# ILR (isometric log-ratio): an orthonormal-basis transform to D-1
# coordinates with a non-singular covariance matrix, at the cost of each
# coordinate being an abstract contrast between subsets of elements rather
# than one single element. Uses the standard sequential-binary-partition
# ("pivot coordinate") basis: part 1 contrasted against parts 2..D, then
# part 2 against 3..D, etc.
#
# Zero-handling: zeros/NAs are replaced with a small pseudo-count before
# taking logs (default: half the smallest positive value found across the
# selected columns). This is a simplified version of the standard
# multiplicative zero-replacement approach in the CoDA literature (e.g.
# Martín-Fernández et al.) - documented here rather than implemented in
# full, since the full method needs a detection-limit per element that
# this app's data doesn't carry.

# Replace zeros/NA with a small pseudo-count so logs are always defined.
.coda_replace_zeros <- function(mat, zero_replacement = NULL) {
  if (is.null(zero_replacement)) {
    positive_vals <- mat[mat > 0 & is.finite(mat)]
    if (length(positive_vals) == 0) stop("No positive values found in the selected compositional columns.")
    zero_replacement <- min(positive_vals) / 2
  }
  mat[is.na(mat) | mat <= 0] <- zero_replacement
  mat
}

# Centered log-ratio transform. Returns a data frame with one column per
# selected part, same column names, CLR-transformed values (each row
# sums to ~0).
#' Centered log-ratio (CLR) transform of compositional columns
#'
#' `clr_i = ln(x_i) - mean(ln(x))`. Each output column still corresponds to
#' one original element, so loadings/biplots computed from it stay directly
#' interpretable - but the transformed columns always sum to 0 per row, so
#' their covariance matrix is singular (see `ilr_transform()`).
#'
#' @param data A data frame containing the compositional columns.
#' @param parts Character vector of column names to transform (at least 2).
#' @param zero_replacement Value substituted for zero/NA entries before
#'   taking logs. Defaults to half the smallest positive value found across
#'   `parts`.
#' @return A data frame of CLR-transformed values, one column per `parts` entry.
#' @export
clr_transform <- function(data, parts, zero_replacement = NULL) {
  if (length(parts) < 2) stop("At least 2 compositional parts (columns) are required.")
  mat <- as.matrix(data[, parts, drop = FALSE])
  storage.mode(mat) <- "double"
  mat <- .coda_replace_zeros(mat, zero_replacement)

  log_mat <- log(mat)
  row_means <- rowMeans(log_mat)
  clr_mat <- log_mat - row_means
  as.data.frame(clr_mat)
}

# Isometric log-ratio transform via the default sequential-binary-partition
# ("pivot coordinate") basis. Returns a list with the transformed
# coordinates (D-1 columns, named ilr_1..ilr_(D-1)) and the basis matrix V
# (D x (D-1)) such that clr = V %*% t(ilr) reconstructs the CLR values.
#' Isometric log-ratio (ILR) transform of compositional columns
#'
#' An orthonormal re-expression of the CLR coordinates into `D-1`
#' coordinates with a non-singular covariance matrix, using the standard
#' sequential-binary-partition ("pivot coordinate") basis: coordinate `j`
#' contrasts the geometric mean of the first `j` parts against part `j+1`.
#' ILR is an isometry of CLR, so PCA on either basis gives identical
#' variance-explained and scores (up to sign) - only the loadings'
#' interpretation differs (per-element for CLR, an abstract balance for ILR).
#'
#' @param data A data frame containing the compositional columns.
#' @param parts Character vector of column names to transform (at least 2).
#'   The order determines what each `ilr_j` balance represents.
#' @param zero_replacement Value substituted for zero/NA entries before
#'   taking logs. Defaults to half the smallest positive value found across
#'   `parts`.
#' @return A list with `ilr` (data frame, columns `ilr_1..ilr_(D-1)`),
#'   `basis` (the `D x (D-1)` orthonormal basis matrix `V` such that
#'   `clr = V %*% t(ilr)`), and `parts`.
#' @export
ilr_transform <- function(data, parts, zero_replacement = NULL) {
  if (length(parts) < 2) stop("At least 2 compositional parts (columns) are required.")
  D <- length(parts)
  mat <- as.matrix(data[, parts, drop = FALSE])
  storage.mode(mat) <- "double"
  mat <- .coda_replace_zeros(mat, zero_replacement)

  # Balance j compares the group of the first j parts (size j) against
  # part j+1 alone (size 1). For a group of size r vs a group of size s,
  # the (unit-norm) orthonormal coefficient is sqrt(r*s/(r+s)) split as
  # 1/r per member of the first group and -1/s per member of the second;
  # here r=j, s=1, which reduces to coeff = sqrt(j/(j+1)).
  log_mat <- log(mat)
  n <- nrow(mat)
  ilr_mat <- matrix(NA_real_, nrow = n, ncol = D - 1)
  for (j in seq_len(D - 1)) {
    left_cols <- seq_len(j)
    right_col <- j + 1
    left_mean <- rowMeans(log_mat[, left_cols, drop = FALSE])
    coeff <- sqrt(j / (j + 1))
    ilr_mat[, j] <- coeff * (left_mean - log_mat[, right_col])
  }
  colnames(ilr_mat) <- paste0("ilr_", seq_len(D - 1))

  # Basis matrix V (D x (D-1)), orthonormal (V'V = I_{D-1}) so that
  # clr = V %*% ilr exactly reconstructs the CLR coordinates - verified
  # empirically during development (see verification notes for this file).
  V <- matrix(0, nrow = D, ncol = D - 1)
  for (j in seq_len(D - 1)) {
    coeff <- sqrt(j / (j + 1))
    V[seq_len(j), j] <- coeff / j
    V[j + 1, j] <- -coeff
  }
  rownames(V) <- parts

  list(ilr = as.data.frame(ilr_mat), basis = V, parts = parts)
}

# Thin wrapper around prcomp() for the transformed (CLR or ILR) data.
#' Run PCA on CLR- or ILR-transformed compositional data
#'
#' Thin wrapper around `stats::prcomp()` (centered, unscaled).
#'
#' @param transformed_data A data frame of CLR- or ILR-transformed values,
#'   as returned by `clr_transform()` or `ilr_transform()$ilr`.
#' @return A list with `pca` (the raw `prcomp` object), `scores` (data
#'   frame of PC scores), `loadings` (data frame of PC loadings), and
#'   `var_explained` (numeric vector, percent variance per component).
#' @export
compositional_pca <- function(transformed_data) {
  pca <- stats::prcomp(transformed_data, center = TRUE, scale. = FALSE)
  var_explained <- (pca$sdev^2) / sum(pca$sdev^2) * 100
  list(
    pca = pca,
    scores = as.data.frame(pca$x),
    loadings = as.data.frame(pca$rotation),
    var_explained = var_explained
  )
}

# PCA biplot: score points (PC1 vs PC2) with loading vectors overlaid,
# scaled to be visible alongside the scores.
#' Build a PCA biplot (PC1 vs PC2 scores with loading vectors)
#'
#' @param pca_result A result from `compositional_pca()`.
#' @param scale_loadings Numeric factor to scale loading arrows by, for
#'   visibility alongside the scores. Defaults to an automatically chosen
#'   scale (80% of the score range relative to the loading range).
#' @return A `ggplot` object.
#' @export
create_coda_biplot <- function(pca_result, scale_loadings = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 package is required for plotting")
  scores <- pca_result$scores
  loadings <- pca_result$loadings
  ve <- pca_result$var_explained

  if (is.null(scale_loadings)) {
    score_range <- max(abs(range(scores[, 1:2], na.rm = TRUE)))
    loading_range <- max(abs(range(loadings[, 1:2], na.rm = TRUE)))
    scale_loadings <- if (loading_range > 0) 0.8 * score_range / loading_range else 1
  }
  loadings_scaled <- loadings * scale_loadings
  loadings_scaled$label <- rownames(loadings)

  ggplot2::ggplot() +
    ggplot2::geom_point(data = scores, ggplot2::aes(x = PC1, y = PC2), alpha = 0.5, color = "#357ABD") +
    ggplot2::geom_segment(data = loadings_scaled, ggplot2::aes(x = 0, y = 0, xend = PC1, yend = PC2),
                           arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")), color = "#d32f2f") +
    ggplot2::geom_text(data = loadings_scaled, ggplot2::aes(x = PC1, y = PC2, label = label),
                        color = "#d32f2f", vjust = -0.5, size = 3.5) +
    ggplot2::labs(
      title = "Compositional PCA Biplot",
      x = sprintf("PC1 (%.1f%% variance)", ve[1]),
      y = sprintf("PC2 (%.1f%% variance)", ve[2])
    ) +
    ggplot2::theme_minimal()
}
