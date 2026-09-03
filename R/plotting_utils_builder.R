# ---- Plotting Utilities: Custom Plot Builder ----
# One generic chart builder for the "Plot Builder" tab: violin, box, bar,
# histogram, scatter, or rose diagram, all through the same function.
# Column names are arbitrary/user-chosen (this app has no fixed data
# schema), so every axis/color argument is a plain string column name
# resolved via rlang::sym() (tidy-eval style) rather than a hardcoded aes().

#' Build a Plot Builder chart from arbitrary column names
#'
#' The generic, schema-agnostic chart builder behind the "Plot Builder"
#' tab: violin, box, bar (counts), histogram, scatter, or rose diagram,
#' with every axis/color argument a plain column-name string resolved via
#' `rlang::sym()`. Selecting multiple `y` columns for `type` `"violin"`/
#' `"box"` reshapes the data to compare them side by side within each `x` group.
#'
#' @param data A data frame.
#' @param type Chart type: `"violin"`, `"box"`, `"bar"`, `"hist"`,
#'   `"scatter"`, or `"rose"`.
#' @param x Name of the X-axis / grouping / direction column, depending on `type`.
#' @param y Name(s) of the Y-axis / value column(s). A character vector of
#'   length > 1 is only meaningful for `"violin"`/`"box"`.
#' @param color_by Name of a column to color/group by, or `"none"` to disable. Default `"none"`.
#' @param log_x Log-scale the X axis (ignored for `"rose"`). Default `FALSE`.
#' @param log_y Log-scale the Y axis (ignored for `"rose"`). Default `FALSE`.
#' @param percent For `type = "bar"`, show percentages instead of raw counts. Default `FALSE`.
#' @param bar_values Optional character vector restricting which
#'   categories are shown for `type = "bar"`.
#' @param rose_bin_width Bin width in degrees, for `type = "rose"`. Default 10.
#' @param hist_bins Number of bins, for `type = "hist"`. Default 30.
#' @return A `ggplot` object.
#' @export
build_custom_plot <- function(data, type, x, y = NULL, color_by = "none",
                               log_x = FALSE, log_y = FALSE, percent = FALSE,
                               bar_values = NULL, rose_bin_width = 10, hist_bins = 30) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 package is required for plotting")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("rlang package is required for plotting")

  has_color <- !is.null(color_by) && color_by != "none"
  fill_var <- if (has_color) color_by else x
  y_label <- if (length(y) == 1) y else NULL
  fill_label <- fill_var

  # Multi-column Y (violin/box only): reshape wide -> long, turning each
  # selected column into its own sub-group of rows instead of one Y value
  # per plot - e.g. picking several elements' Wt% columns compares them
  # side by side within each X group ("4 elements x 3 files" instead of
  # being limited to one Y column and the existing Color/group by choice,
  # which is what this replaces here).
  if (type %in% c("violin", "box") && length(y) > 1) {
    id_cols <- setdiff(names(data), y)
    long_rows <- lapply(y, function(col) {
      d_col <- data[, id_cols, drop = FALSE]
      d_col$.builder_element <- col
      d_col$.builder_value <- suppressWarnings(as.numeric(data[[col]]))
      d_col
    })
    data <- do.call(rbind, long_rows)
    data <- data[!is.na(data$.builder_value), , drop = FALSE]
    fill_var <- ".builder_element"
    fill_label <- "Column"
    y <- ".builder_value"
    y_label <- "Value"
  } else if (length(y) > 1) {
    y <- y[1]
    y_label <- y
  }

  p <- switch(type,
    # X axis (and fill, when it defaults to the same column) is wrapped in
    # factor() so any column works as the grouping axis, not just ones
    # already stored as character/factor - a numeric column (e.g. an
    # element's Wt%) would otherwise be treated as a continuous axis,
    # collapsing every row onto one pooled violin/box instead of one per
    # distinct value.
    "violin" = ggplot2::ggplot(data, ggplot2::aes(x = factor(!!rlang::sym(x)), y = !!rlang::sym(y), fill = factor(!!rlang::sym(fill_var)))) +
      # geom_violin defaults to position "dodge" and geom_boxplot to "dodge2" -
      # two different dodge algorithms that don't share x-offsets when there
      # are multiple fill groups per x category, so the boxplots drift out of
      # alignment with their violins unless both are pinned to the same
      # explicit dodge width.
      ggplot2::geom_violin(alpha = 0.7, scale = "width", position = ggplot2::position_dodge(width = 0.9)) +
      ggplot2::geom_boxplot(width = 0.1, alpha = 0.5, position = ggplot2::position_dodge(width = 0.9)) +
      ggplot2::labs(x = x, y = y_label, fill = fill_label),
    "box" = ggplot2::ggplot(data, ggplot2::aes(x = factor(!!rlang::sym(x)), y = !!rlang::sym(y), fill = factor(!!rlang::sym(fill_var)))) +
      ggplot2::geom_boxplot(alpha = 0.7) +
      ggplot2::labs(x = x, y = y_label, fill = fill_label),
    "hist" = {
      p0 <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(x)))
      if (has_color) p0 <- p0 + ggplot2::aes(fill = !!rlang::sym(color_by))
      p0 + ggplot2::geom_histogram(alpha = 0.7, bins = hist_bins, position = "identity") +
        ggplot2::labs(x = x, y = "Count", fill = color_by)
    },
    "scatter" = {
      p0 <- ggplot2::ggplot(data, ggplot2::aes(x = !!rlang::sym(x), y = !!rlang::sym(y)))
      if (has_color) p0 <- p0 + ggplot2::aes(color = !!rlang::sym(color_by))
      p0 + ggplot2::geom_point(alpha = 0.6, size = 1.2) +
        ggplot2::labs(x = x, y = y, color = color_by)
    },
    "bar" = {
      # Restrict to the user-chosen subset of category values before
      # counting. When a Color / group by breakdown is active, this filters
      # THAT dimension's values (e.g. "which chemical classes to show, per
      # file") rather than the X axis's own values - X is normally the file/
      # group being compared, so the categories the user wants to narrow
      # down are the breakdown values, not which X groups exist.
      filter_col <- if (has_color) fill_var else x
      if (!is.null(bar_values)) data <- data[data[[filter_col]] %in% bar_values, , drop = FALSE]
      if (nrow(data) == 0) {
        # A "Filter by value" selection that matches nothing legitimately
        # leaves a 0-row data frame here. table() on a length-0 *plain*
        # vector (as opposed to a factor, which keeps its declared levels
        # at count 0) returns a dimnames-less "table of extent 0" -
        # as.data.frame(table(...)) on that produces only a bare Freq
        # column, with no x-axis variable column at all - so every one of
        # this block's four count-computing branches below would error on
        # its own names(d2) <- c(x, ...) ("'names' attribute [N] must be
        # the same length as the vector [N-1]") instead of showing a
        # graceful empty-data state. Handled once, here, before any of
        # them run.
        ggplot2::ggplot() +
          ggplot2::annotate("text", x = 0.5, y = 0.5, label = "No data matches the selected filter", size = 5, color = "grey40") +
          ggplot2::theme_void()
      } else if (percent && has_color) {
        # Percentage within each X group's own breakdown (stacked bars sum
        # to 100% per X category) - fill_var is a real second dimension
        # here, distinct from x.
        d2 <- as.data.frame(table(data[[x]], data[[fill_var]]), stringsAsFactors = FALSE)
        names(d2) <- c(x, fill_var, "n")
        d2 <- d2[d2$n > 0, , drop = FALSE]
        totals <- stats::ave(d2$n, d2[[x]], FUN = sum)
        d2$pct <- d2$n / totals * 100
        ggplot2::ggplot(d2, ggplot2::aes(x = !!rlang::sym(x), y = pct, fill = !!rlang::sym(fill_var))) +
          ggplot2::geom_col(position = "stack") +
          ggplot2::labs(x = x, y = "Percentage (%)", fill = fill_var)
      } else if (percent) {
        # No Color/group-by set: fill_var defaults to x itself, so a 2-way
        # table(x, fill_var) would just cross-tabulate x against itself -
        # every category's own count would equal its own "total" and every
        # bar would render at a meaningless 100%. Percentage here instead
        # means "share of all rows", computed against the one real total.
        d2 <- as.data.frame(table(data[[x]]), stringsAsFactors = FALSE)
        names(d2) <- c(x, "n")
        d2$pct <- d2$n / sum(d2$n) * 100
        ggplot2::ggplot(d2, ggplot2::aes(x = stats::reorder(!!rlang::sym(x), pct), y = pct, fill = !!rlang::sym(x))) +
          ggplot2::geom_col(alpha = 0.8) + ggplot2::coord_flip() +
          ggplot2::labs(x = x, y = "Percentage (%)") + ggplot2::theme(legend.position = "none")
      } else if (has_color) {
        # Grouped (dodged) counts: one sub-bar per selected category,
        # within each X group - e.g. several categories compared side by
        # side for each file, mirroring the multi-column violin/box feature.
        d2 <- as.data.frame(table(data[[x]], data[[fill_var]]), stringsAsFactors = FALSE)
        names(d2) <- c(x, fill_var, "n")
        d2 <- d2[d2$n > 0, , drop = FALSE]
        ggplot2::ggplot(d2, ggplot2::aes(x = !!rlang::sym(x), y = n, fill = !!rlang::sym(fill_var))) +
          ggplot2::geom_col(position = "dodge") +
          ggplot2::labs(x = x, y = "Count", fill = fill_var)
      } else {
        d2 <- as.data.frame(table(data[[x]]), stringsAsFactors = FALSE)
        names(d2) <- c(x, "n")
        ggplot2::ggplot(d2, ggplot2::aes(x = stats::reorder(!!rlang::sym(x), n), y = n, fill = !!rlang::sym(x))) +
          ggplot2::geom_col(alpha = 0.8) + ggplot2::coord_flip() +
          ggplot2::labs(x = x, y = "Count") + ggplot2::theme(legend.position = "none")
      }
    },
    "rose" = {
      # Windrose / polar histogram of a direction column (degrees): bin
      # into fixed-width angular sectors on a 0-360 grid (independent of
      # the data's own range - orientation data is often only measured
      # 0-180 since a line has no front/back, and the empty half of the
      # circle is exactly what shows that), count per bin, and draw as
      # radial bars. "Color / group by" facets into one rose per category
      # here instead of coloring within a single plot - overlapping wedges
      # from several categories in one polar plot would be unreadable,
      # unlike a stacked/dodged Cartesian bar chart.
      bin_width <- if (is.numeric(rose_bin_width) && length(rose_bin_width) == 1 && rose_bin_width > 0) rose_bin_width else 10
      breaks <- seq(0, 360, by = bin_width)
      dir_vals <- data[[x]] %% 360
      bins <- cut(dir_vals, breaks = breaks, right = FALSE, include.lowest = TRUE)
      bin_mid <- breaks[-length(breaks)] + bin_width / 2

      # coord_polar's start is a radian offset of the theta=0 point from 12
      # o'clock, applied in the plot's rotation direction (clockwise by
      # default). start = -pi/2 moves theta=0 from 12 o'clock to 9 o'clock
      # (the left), so 0/360 renders on the left, 90 at top, 180 on the
      # right, and 270 at the bottom, sweeping clockwise as direction
      # increases - matching the standard rose-diagram layout.
      rose_start <- -pi / 2

      if (has_color) {
        d2 <- as.data.frame(table(bins, data[[color_by]]), stringsAsFactors = FALSE)
        names(d2) <- c("bin", color_by, "n")
        d2$bin_mid <- bin_mid[match(d2$bin, levels(bins))]
        ggplot2::ggplot(d2, ggplot2::aes(x = bin_mid, y = n, fill = !!rlang::sym(color_by))) +
          ggplot2::geom_col(width = bin_width * 0.95, show.legend = FALSE) +
          ggplot2::coord_polar(theta = "x", start = rose_start) +
          ggplot2::scale_x_continuous(limits = c(0, 360), breaks = seq(0, 330, by = 30), expand = c(0, 0)) +
          ggplot2::facet_wrap(stats::as.formula(paste0("~ `", color_by, "`"))) +
          ggplot2::labs(x = "Direction (degrees)", y = "Count")
      } else {
        d2 <- as.data.frame(table(bins), stringsAsFactors = FALSE)
        names(d2) <- c("bin", "n")
        d2$bin_mid <- bin_mid[match(d2$bin, levels(bins))]
        ggplot2::ggplot(d2, ggplot2::aes(x = bin_mid, y = n)) +
          ggplot2::geom_col(width = bin_width * 0.95, fill = "#357ABD") +
          ggplot2::coord_polar(theta = "x", start = rose_start) +
          ggplot2::scale_x_continuous(limits = c(0, 360), breaks = seq(0, 330, by = 30), expand = c(0, 0)) +
          ggplot2::labs(x = "Direction (degrees)", y = "Count")
      }
    },
    stop(paste("Unknown plot type:", type))
  )

  # log_x/log_y and the rotated axis-text theme don't apply meaningfully to
  # a polar (circular) axis, so a rose diagram skips both and keeps its own
  # theme_minimal() + upright angular labels.
  if (type != "rose") {
    if (isTRUE(log_x)) p <- p + ggplot2::scale_x_log10()
    if (isTRUE(log_y)) p <- p + ggplot2::scale_y_log10()
    p <- p + ggplot2::theme_minimal() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  } else {
    p <- p + ggplot2::theme_minimal()
  }

  p
}
