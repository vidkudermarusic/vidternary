# ---- Plotting Utilities: Correlation Heatmap ----
# The app's actual ternary diagrams are built via general_ternary_plot()/
# the Ternary package (see ternary_plot.R), not via ggplot2 - this file
# only holds the one plotting helper that's genuinely wired into the app:
# the Data Comparison tab's correlation heatmap.

#' Draw a `corrplot` correlation heatmap, base-graphics style
#'
#' Constant (zero-variance) columns are dropped first (their correlation
#' is undefined and would crash `corrplot`'s hierarchical-clustering
#' column ordering); if a pairwise `NA` still remains, falls back to the
#' original column order instead of clustering.
#'
#' @details
#' The correlation is computed on the columns exactly as supplied. When
#' those columns are constant-sum compositional data (wt% chemistry that
#' sums to ~100), the closure constraint induces spurious correlations -
#' most visibly a negative bias, since an increase in one part forces the
#' others down - so a Pearson `r` between two wt% columns is not a clean
#' measure of their association. This is the problem the CoDA tab handles
#' with a log-ratio transform (`compositional_data_analysis.R`). A caption
#' line on the plot states this; for a quick rank-based alternative that is
#' less distorted, pass `method = "spearman"`. Non-compositional columns
#' (area, ECD, aspect ratio, coordinates) are unaffected.
#'
#' @param data A data frame of numeric columns.
#' @param method Correlation method passed to `cor()`. Default `"pearson"`.
#'   `"spearman"` (rank) is less affected by compositional closure.
#' @param title Plot title.
#' @param color_palette `corrplot::COL2()` palette name. Default `"RdBu"`.
#' @return `TRUE` (invisibly `FALSE` if fewer than 2 non-constant numeric
#'   columns remain). Called for its plotting side effect on the current
#'   graphics device.
#' @export
create_correlation_plot <- function(data, method = "pearson",
                                  title = "Correlation Matrix",
                                  color_palette = "RdBu") {

  if (!requireNamespace("corrplot", quietly = TRUE)) {
    stop("corrplot package is required for correlation plotting")
  }

  # A constant (zero-variance) column has an undefined correlation with
  # anything (division by zero -> NaN). corrplot's order="hclust" step calls
  # stats::hclust() on the correlation matrix, whose underlying Fortran
  # routine rejects any NA/NaN/Inf outright ("NA/NaN/Inf in foreign function
  # call (arg 10)") - so such columns are dropped before plotting rather than
  # crashing the whole heatmap.
  col_sds <- vapply(data, function(x) stats::sd(x, na.rm = TRUE), numeric(1))
  constant_cols <- names(col_sds)[is.na(col_sds) | col_sds == 0]
  if (length(constant_cols) > 0) {
    data <- data[, setdiff(names(data), constant_cols), drop = FALSE]
  }

  if (ncol(data) < 2) {
    plot.new()
    text(0.5, 0.5, "Not enough variable columns to plot a correlation matrix\n(need at least 2 non-constant numeric columns)", cex = 0.9)
    return(invisible(FALSE))
  }

  # Calculate correlation matrix. Deliberately listwise (complete.obs), NOT
  # pairwise.complete.obs like build_correlation_pairs_table()'s own
  # equivalent table (stats_display_utils.R): this matrix feeds hclust()
  # below for column reordering, and a pairwise-deleted correlation matrix
  # isn't guaranteed positive-semi-definite when missingness differs across
  # column pairs - which can produce an invalid "distance" for clustering.
  # The plain table has no such requirement, so it uses the more
  # data-efficient pairwise convention instead.
  cor_matrix <- cor(data, use = "complete.obs", method = method)

  # A pairwise NA can still remain (e.g. two columns share no complete rows
  # in common under "complete.obs") even after dropping constant columns -
  # hclust ordering can't handle that either, so fall back to the original
  # column order rather than crashing.
  order_method <- if (anyNA(cor_matrix)) "original" else "hclust"

  # Caption lines printed below the heatmap: always a note that the
  # coefficient is computed on the raw columns (so closure distorts it for
  # constant-sum wt% data - see this function's @details), plus, when
  # relevant, which constant columns were dropped. Reserve bottom-margin
  # room per line so mtext() doesn't overlap the heatmap's own labels.
  method_label <- c(pearson = "Pearson r", spearman = "Spearman rho",
                    kendall = "Kendall tau")[tolower(method)]
  if (is.na(method_label)) method_label <- method
  notes <- sprintf(
    "%s on raw columns - for constant-sum (wt%%) data, closure distorts this; use the CoDA tab for a log-ratio analysis.",
    method_label)
  if (length(constant_cols) > 0) {
    notes <- c(notes, paste("Excluded constant column(s):", paste(constant_cols, collapse = ", ")))
  }
  bottom_margin <- 2 + 1.4 * length(notes)

  # Create correlation plot
  corrplot::corrplot(cor_matrix,
    method = "color",
                     type = "upper",
                     order = order_method,
                     tl.cex = 0.7,
    tl.col = "black",
    tl.srt = 45,
                     col = corrplot::COL2(color_palette, 200),
    title = title,
                     mar = c(bottom_margin, 0, 2, 0))

  for (i in seq_along(notes)) {
    mtext(notes[i], side = 1, line = 0.6 + 1.3 * (i - 1), cex = 0.6, col = "grey40")
  }

  return(TRUE)
}
