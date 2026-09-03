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
#' @param data A data frame of numeric columns.
#' @param method Correlation method passed to `cor()`. Default `"pearson"`.
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

  # Calculate correlation matrix
  cor_matrix <- cor(data, use = "complete.obs", method = method)

  # A pairwise NA can still remain (e.g. two columns share no complete rows
  # in common under "complete.obs") even after dropping constant columns -
  # hclust ordering can't handle that either, so fall back to the original
  # column order rather than crashing.
  order_method <- if (anyNA(cor_matrix)) "original" else "hclust"

  # Reserve extra room in the bottom margin when a note about excluded
  # columns needs to be printed below the plot, so mtext() doesn't overlap
  # the heatmap's own bottom row/labels.
  bottom_margin <- if (length(constant_cols) > 0) 3 else 0

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

  if (length(constant_cols) > 0) {
    mtext(paste("Excluded constant column(s):", paste(constant_cols, collapse = ", ")),
          side = 1, line = 1.5, cex = 0.6, col = "grey40")
  }

  return(TRUE)
}
