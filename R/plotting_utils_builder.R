# ---- Plotting Utilities: Custom Plot Builder ----
# One generic chart builder for the "Plot Builder" tab, as opposed to the
# six fixed per-type builders in plotting_utils_compare.R (used by the
# "Multiple Plot Types" tab). Column names are arbitrary/user-chosen (this
# app has no fixed data schema), so every axis/color argument is a plain
# string column name resolved via rlang::sym(), matching the existing
# tidy-eval style already used in plotting_utils_compare.R.

build_custom_plot <- function(data, type, x, y = NULL, color_by = "none",
                               log_x = FALSE, log_y = FALSE, percent = FALSE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 package is required for plotting")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("rlang package is required for plotting")

  has_color <- !is.null(color_by) && color_by != "none"
  fill_var <- if (has_color) color_by else x

  p <- switch(type,
    "violin" = ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y), fill = !!rlang::sym(fill_var))) +
      ggplot2::geom_violin(alpha = 0.7, scale = "width") +
      ggplot2::geom_boxplot(width = 0.1, alpha = 0.5) +
      ggplot2::labs(x = x, y = y, fill = fill_var),
    "box" = ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y), fill = !!rlang::sym(fill_var))) +
      ggplot2::geom_boxplot(alpha = 0.7) +
      ggplot2::labs(x = x, y = y, fill = fill_var),
    "hist" = {
      p0 <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(x)))
      if (has_color) p0 <- p0 + ggplot2::aes(fill = !!rlang::sym(color_by))
      p0 + ggplot2::geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
        ggplot2::labs(x = x, y = "Count", fill = color_by)
    },
    "scatter" = {
      p0 <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y)))
      if (has_color) p0 <- p0 + ggplot2::aes(color = !!rlang::sym(color_by))
      p0 + ggplot2::geom_point(alpha = 0.6) +
        ggplot2::labs(x = x, y = y, color = color_by)
    },
    "bar" = if (percent) {
      d2 <- as.data.frame(table(data[[x]], data[[fill_var]]), stringsAsFactors = FALSE)
      names(d2) <- c(x, fill_var, "n")
      d2 <- d2[d2$n > 0, , drop = FALSE]
      totals <- stats::ave(d2$n, d2[[x]], FUN = sum)
      d2$pct <- d2$n / totals * 100
      ggplot2::ggplot(d2, ggplot2::aes(x = !!rlang::sym(x), y = pct, fill = !!rlang::sym(fill_var))) +
        ggplot2::geom_col(position = "stack") +
        ggplot2::labs(x = x, y = "Percentage (%)", fill = fill_var)
    } else {
      d2 <- as.data.frame(table(data[[x]]), stringsAsFactors = FALSE)
      names(d2) <- c(x, "n")
      ggplot2::ggplot(d2, ggplot2::aes(x = stats::reorder(!!rlang::sym(x), n), y = n, fill = !!rlang::sym(x))) +
        ggplot2::geom_col(alpha = 0.8) + ggplot2::coord_flip() +
        ggplot2::labs(x = x, y = "Count") + ggplot2::theme(legend.position = "none")
    },
    stop(paste("Unknown plot type:", type))
  )

  if (isTRUE(log_x)) p <- p + ggplot2::scale_x_log10()
  if (isTRUE(log_y)) p <- p + ggplot2::scale_y_log10()

  p + ggplot2::theme_minimal() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}
