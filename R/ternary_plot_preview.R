# ---- Ternary Plot: Preview Render (split out of ternary_plot.R) ----
# Draws the ternary plot to whatever graphics device is currently active.
# general_ternary_plot() always calls this, then separately decides
# whether to also call save_ternary_plot_to_file().
#
# Takes the list returned by prepare_ternary_plot_data() and renders from
# its fields via with(pd, {...}).
render_ternary_plot_preview <- function(pd) {
  with(pd, {
    # Create the ternary plot
    log_operation("INFO", "Starting to plot", paste(nrow(ternary_points1), "points"))

    # Set outer margins to prevent clipping of multi-line titles and notes.
    # Top margin for titles; bottom margin scales with how many lines the
    # tallest plot-notes column actually needs (notes_bottom_margin,
    # computed in prepare_ternary_plot_data()) - see
    # draw_plot_notes_column()'s own comment for the full explanation.
    op <- par(oma = c(notes_bottom_margin, 0, 3, 0))
    on.exit(par(op))

    # Check if Ternary package is available
    if (!requireNamespace("Ternary", quietly = TRUE)) {
      stop("Ternary package is required for ternary plotting. Please install it first.")
    }

    # Create the ternary plot using the Ternary package
    Ternary::TernaryPlot(
      atip = clean_labels_A, btip = clean_labels_B, ctip = clean_labels_C,
      alab = paste(axis_labels_A, "->"), blab = paste(axis_labels_B, "->"), clab = paste("<-", axis_labels_C),
      col = "white",
      grid.lines = 5,
      grid.lty = "dotted",
      grid.minor.lines = 1,
      grid.minor.lty = "dotted"
    )

    # Plot all points
    if (getOption("ternary.debug", FALSE)) cat("DEBUG: Plotting all points with TernaryPoints\n")

    # Safety check: ensure we have data to plot
    if (nrow(ternary_points1) == 0) {
      if (getOption("ternary.debug", FALSE)) cat("DEBUG: No data to plot - skipping TernaryPoints\n")
      # Add a message to the plot
      text(0.5, 0.5, "No data to display\n(No groups selected or all data filtered out)",
           cex = 1.2, col = "red", adj = 0.5)
    } else {
      # Ensure all vectors have the same length as ternary_points1
      n_points <- nrow(ternary_points1)
      if (length(pointSize) != n_points) {
        pointSize <- rep(pointSize[1], n_points)
      }
      if (length(pointCol) != n_points) {
        pointCol <- rep(pointCol[1], n_points)
      }
      if (length(pointType) != n_points) {
        pointType <- rep(pointType[1], n_points)
      }

      Ternary::TernaryPoints(ternary_points1, cex = pointSize, col = pointCol, pch = pointType)
      if (getOption("ternary.debug", FALSE)) cat("DEBUG: All points plotted successfully\n")
    }

    # Enhanced title plotting for multi-line titles
    if (grepl("\n", plot_title)) {
      # Multi-line title: adjust line position and size
      title(main = plot_title, cex.main = 0.7, line = 0.8)
    } else {
      # Single-line title: standard formatting
      title(main = plot_title, cex.main = 0.8, line = 0.5)
    }

    # Add legends for optional parameters if selected
    if (!is.null(optional_param1) && !use_manual_point_size) {
      # Legend for optional parameter 1 (point size) - top right
      if (length(optional_param1$col) == 1 && optional_param1_representation == "point_size") {
        if (!any(is.finite(param1_values))) {
          # No non-NA values for the size parameter in this dataset -
          # min/max(na.rm=TRUE) would return -Inf/Inf here, and seq()/
          # SizeLegend() on a non-finite range fails with "'from' must be
          # a finite number". Note it instead of crashing the whole plot.
          legend("topright",
                 title = paste(optional_param1$col, collapse = "+"),
                 legend = "No data",
                 bty = "n",
                 cex = 0.7)
        } else if (requireNamespace("PlotTools", quietly = TRUE)) {
          PlotTools::SizeLegend(
            "topright",
            width = c(MIN_POINT_SIZE, MAX_POINT_SIZE),
            lend = "round",
            legend = paste(
              signif(seq(max(param1_values, na.rm = TRUE), min(param1_values, na.rm = TRUE), length.out = 5), digits = 3)
            ),
            title = paste(optional_param1$col, collapse = "+"),
            bty = "n",
            cex = 0.7
          )
        } else {
          # Fallback to regular legend
          size_range <- seq(min(param1_values, na.rm = TRUE), max(param1_values, na.rm = TRUE), length.out = 5)
          legend_sizes <- size_range * (MAX_POINT_SIZE - MIN_POINT_SIZE) / max(param1_values, na.rm = TRUE) + MIN_POINT_SIZE

          legend("topright",
                 title = paste(optional_param1$col, collapse = "+"),
                 legend = paste("Size:", signif(size_range, 3)),
                 pch = 16,
                 pt.cex = legend_sizes,
                 cex = 0.7)
        }
      } else if (length(optional_param1$col) > 1) {
        # Multiple columns - show combined legend
        legend("topright",
               title = paste(optional_param1$col, collapse = "+"),
               legend = "Combined",
               pt.cex = 1.5,
               pch = 16,
               cex = 0.7)
      }
    }

    if (!is.null(optional_param2)) {
      # Legend for optional parameter 2 (color) - handle both categorical and numeric
      if (is_categorical_group && !is.null(selected_groups) && length(selected_groups) > 0) {
        # Categorical group legend
        create_group_legend(unique_groups, group_colors, group_counts)
      } else if (length(optional_param2$col) == 1) {
        # Numeric data legend - show color legend with exactly 5 bins
        # Reuses param2_colors/param2_breaks - the same objects
        # compute_point_styling() already computed for the points
        # themselves - so the legend can't drift from the actual point
        # colors (quantile-binned breaks, palette sized to the real bin
        # count after unique() dedup, which can be under 5).
        if (optional_param2$col == "Aspect.Ratio") {
          legend_labels <- c("1-1.5", "1.5-3", "3-5", "5-10", "10+")
        } else if (is.numeric(param2_values) && all(is.finite(param2_values), na.rm = TRUE) && length(param2_breaks) >= 2) {
          legend_labels <- paste0(round(param2_breaks[-length(param2_breaks)], 3), " - ", round(param2_breaks[-1], 3))
        } else {
          legend_labels <- "All"
        }

        legend("topleft",
               legend = legend_labels,
               col = param2_colors,
               pch = 16,
               title = paste(optional_param2$col, collapse = "+"),
               cex = 0.7,
               y.intersp = 1.2)
      } else {
        # Multiple columns - show combined legend
        legend("topleft",
               title = paste(optional_param2$col, collapse = "+"),
               legend = "Combined",
               fill = if (color_palette == "blue") "#357ABD" else if (color_palette == "red") "#FF6666" else if (color_palette == "viridis") "#440154" else if (color_palette == "rainbow") "#FF0000" else "grey",
               cex = 0.7)
      }
    }

    # Add comprehensive plot notes if requested (text already built by
    # prepare_ternary_plot_data(); this just draws it to the active device)
    if (include_plot_notes) {
      # Column 1 (left) - Elements and filters
      draw_plot_notes_column(col1_text, side = 1, start_line = line_pos, cex = text_cex, col = "darkblue", adj = 0)

      # Column 2 (center) - Optional parameters
      draw_plot_notes_column(col2_text, side = 1, start_line = line_pos, cex = text_cex, col = "darkgreen", adj = 0.5)

      # Column 3 (right) - Analysis methods
      draw_plot_notes_column(col3_text, side = 1, start_line = line_pos, cex = text_cex, col = "darkred", adj = 1)

      # Add a debug message to confirm plot notes are being added
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Plot notes added - Elements:", length(elements_summary), "Optional:", length(optional_summary), "Analysis:", length(analysis_summary), "\n")
        cat("DEBUG: Plot notes text lengths - col1:", nchar(col1_text), "col2:", nchar(col2_text), "col3:", nchar(col3_text), "\n")
        cat("DEBUG: Plot notes positioning - line_pos:", line_pos, "text_cex:", text_cex, "\n")
      }
    }

    invisible(NULL)
  })
}
