# ---- Zero diagnostics for Compositional Analysis ----
# Log-ratios cannot be taken of zeros, so clr_transform()/ilr_transform()
# replace them with a small pseudo-count. That is harmless when zeros are
# rare, but SEM/EDS spectra of single particles list an element only if it
# was detected, so most columns are 30-90% zeros. The pseudo-counts then
# put every particle where an element was not detected far from the
# others, and the principal components describe which elements were
# detected rather than how much of each is present. On real files with
# all 19 Wt% columns, "which columns are zero" explained 97-98% of the
# variance of PC1-PC3; the share only dropped once columns with more than
# 10% zeros were left out. The functions below make this visible and pick
# a default selection that avoids it.

# Parts with a larger share of zeros are not pre-selected and are reported.
COMPOSITIONAL_MAX_ZERO_SHARE <- 0.10
# The zero pattern explaining at least this share of PC1 or PC2 is reported.
COMPOSITIONAL_MAX_ZERO_R2 <- 0.5

# Zero, missing, negative or non-finite entries: what the log-ratio
# transforms treat as "no value" (see .coda_replace_zeros()).
.coda_is_zero <- function(x) is.na(x) | !is.finite(x) | x <= 0

#' Zero share and pseudo-count of each compositional part
#'
#' @param data A data frame.
#' @param parts Column names.
#' @return A data frame: `Part`, `Zero_pct` (share of zero/missing entries)
#'   and `Pseudo_count` (half the smallest positive value, as used by
#'   [clr_transform()]; `NA` when a part has no positive value).
#' @export
coda_zero_summary <- function(data, parts) {
  cols <- lapply(parts, function(p) suppressWarnings(as.numeric(data[[p]])))
  data.frame(
    Part = parts,
    Zero_pct = vapply(cols, function(x) 100 * mean(.coda_is_zero(x)), numeric(1)),
    Pseudo_count = vapply(cols, function(x) if (any(!.coda_is_zero(x))) min(x[!.coda_is_zero(x)]) / 2 else NA_real_, numeric(1)),
    row.names = NULL, stringsAsFactors = FALSE
  )
}

#' Parts pre-selected in the Compositional Analysis tab
#'
#' The Wt% columns with at most `max_zero_share` zeros, or all of them when
#' fewer than `min_parts` qualify.
#'
#' @param data A data frame.
#' @param candidates Candidate column names (e.g. the Wt% columns).
#' @param max_zero_share Largest allowed share of zeros (0-1).
#' @param min_parts Fewest parts needed for an analysis.
#' @return A character vector of column names.
#' @export
coda_default_parts <- function(data, candidates, max_zero_share = COMPOSITIONAL_MAX_ZERO_SHARE, min_parts = 3) {
  if (length(candidates) == 0) return(character(0))
  share <- coda_zero_summary(data, candidates)$Zero_pct / 100
  keep <- candidates[share <= max_zero_share]
  if (length(keep) >= min_parts) keep else candidates
}

#' Share of each principal component explained by the zero pattern
#'
#' R-squared of a regression of each of the first `n_pc` PC scores on the
#' zero indicators (1 where a part is zero in that row) of the parts. A high
#' value means the component mostly separates particles by which parts were
#' not detected.
#'
#' @param data The rows the PCA was run on.
#' @param parts The compositional columns.
#' @param scores PC scores (rows in the same order as `data`).
#' @param n_pc Number of components.
#' @return A numeric vector of length `n_pc` (0 when no part has both zero
#'   and non-zero entries).
#' @export
coda_zero_pattern_r2 <- function(data, parts, scores, n_pc = 3) {
  scores <- as.matrix(scores)
  n_pc <- min(n_pc, ncol(scores))
  z <- vapply(parts, function(p) as.numeric(.coda_is_zero(suppressWarnings(as.numeric(data[[p]])))), numeric(nrow(data)))
  z <- matrix(z, nrow = nrow(data))
  z <- z[, colSums(z) > 0 & colSums(z) < nrow(z), drop = FALSE]
  if (ncol(z) == 0) return(rep(0, n_pc))
  x <- cbind(1, z)
  vapply(seq_len(n_pc), function(i) {
    y <- scores[, i]
    tss <- sum((y - mean(y))^2)
    if (tss <= 0) return(0)
    1 - sum(stats::lm.fit(x, y)$residuals^2) / tss
  }, numeric(1))
}

#' Warnings about zeros in a Compositional Analysis run
#'
#' @param zero_summary From [coda_zero_summary()].
#' @param zero_r2 From [coda_zero_pattern_r2()].
#' @param max_zero_share,max_zero_r2 Limits for a part's zero share and for the
#'   zero pattern's R-squared on PC1 or PC2.
#' @return A character vector (empty when nothing needs saying).
#' @export
coda_zero_notes <- function(zero_summary, zero_r2,
                            max_zero_share = COMPOSITIONAL_MAX_ZERO_SHARE,
                            max_zero_r2 = COMPOSITIONAL_MAX_ZERO_R2) {
  msgs <- character(0)
  many <- zero_summary[zero_summary$Zero_pct > 100 * max_zero_share, , drop = FALSE]
  if (nrow(many) > 0) {
    msgs <- c(msgs, sprintf(
      "%d of the selected parts have more than %.0f%% zeros: %s. Zeros are replaced by pseudo-counts, so components can reflect which elements were detected rather than how much of each is present.",
      nrow(many), 100 * max_zero_share,
      paste(sprintf("%s (%.0f%%)", sub("\\.?\\(Wt%\\)$", "", many$Part), many$Zero_pct), collapse = ", ")))
  }
  if (length(zero_r2) >= 2 && max(zero_r2[1:2]) >= max_zero_r2) {
    msgs <- c(msgs, sprintf(
      "Which parts are zero in a particle explains %.0f%% of PC1 and %.0f%% of PC2 (R-squared). Read the biplot as a detection pattern, or select parts with few zeros (at most %.0f%%) and run again.",
      100 * zero_r2[1], 100 * zero_r2[2], 100 * max_zero_share))
  }
  msgs
}
