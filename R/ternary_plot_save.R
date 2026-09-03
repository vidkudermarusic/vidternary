# ---- Ternary Plot: Save-to-File Render (split out of ternary_plot.R) ----
# Opens an image device, redraws the ternary plot onto it (a second,
# independent draw from ternary_plot_preview.R's - this duplication is
# original behavior, not introduced by this split; see the plan notes on
# why it wasn't deduplicated here), and closes the device. Returns the
# saved file path.
#
# Called by general_ternary_plot() only when `!preview && !is.null(output_dir)`
# - that gate lives in the orchestrator now, so this function can assume it
# should always produce a file.
#
# NOTE: this render's optional-parameter-1 legend has a point_type branch
# that ternary_plot_preview.R's does NOT have (preview never showed a point-
# shape legend, only save does) - that's a pre-existing inconsistency
# between the two draws in the original single-file code, preserved as-is
# here rather than silently "fixed" by unifying them.
save_ternary_plot_to_file <- function(pd) {
  with(pd, {
    # Create output directory if it doesn't exist
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Generate simple filename. extract_file_base() (file_management.R) is
    # the safe version - it basename()s xlsx_display_name before stripping
    # the extension, so a crafted upload filename containing path
    # separators can't steer this save outside output_dir. It's already
    # used on the preview path via create_ternary_output_dir(); this was
    # the one production Save path still building file_base inline without
    # that guard.
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    file_base <- extract_file_base(xlsx_file, xlsx_display_name)
    filename <- file.path(output_dir, paste0("charge_", file_base, "_", timestamp, ".", output_format))

    # Calculate plot dimensions based on title length
    plot_dims <- calculate_plot_dimensions(title_parts)

    # Save plot with calculated dimensions
    if (output_format == "png") {
      png(filename, width = plot_dims$width, height = plot_dims$height, res = 200)
    } else if (output_format == "jpeg") {
      jpeg(filename, width = plot_dims$width, height = plot_dims$height, res = 200, quality = 95)
    } else if (output_format == "pdf") {
      pdf(filename, width = plot_dims$width/100, height = plot_dims$height/100)
    } else if (output_format == "tiff") {
      tiff(filename, width = plot_dims$width, height = plot_dims$height, res = 200, compression = "lzw")
    }
    # Guarantee the device is closed even if an error occurs anywhere below
    # (a bad legend call, non-finite data, etc.) - previously only the
    # dev.off() at the very end of this function closed it, so any error in
    # between left the device open for the lifetime of the R process, and
    # R has a hard cap on simultaneously open devices.
    on.exit(dev.off(), add = TRUE)

    # Set outer margins to prevent clipping of multi-line titles and notes
    op <- par(oma = c(4, 0, 3, 0))

    # Recreate the entire plot on the file device
    # Check if Ternary package is available
    if (!requireNamespace("Ternary", quietly = TRUE)) {
      stop("Ternary package is required for ternary plotting. Please install it first.")
    }

    # Create the ternary plot using the Ternary package
    Ternary::TernaryPlot(
      atip = clean_labels_A, btip = clean_labels_B, ctip = clean_labels_C,
      alab = paste(axis_labels_A, "→"), blab = paste(axis_labels_B, "→"), clab = paste("←", axis_labels_C),
      col = "white",
      grid.lines = 5,
      grid.lty = "dotted",
      grid.minor.lines = 1,
      grid.minor.lty = "dotted"
    )

    # Plot all points
    if (getOption("ternary.debug", FALSE)) cat("DEBUG: Plotting all points with TernaryPoints for file save\n")

    # Safety check: ensure we have data to plot
    if (nrow(ternary_points1) == 0) {
      if (getOption("ternary.debug", FALSE)) cat("DEBUG: No data to plot for file save - skipping TernaryPoints\n")
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
      if (getOption("ternary.debug", FALSE)) cat("DEBUG: All points plotted successfully for file save\n")
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
      } else if (length(optional_param1$col) == 1 && optional_param1_representation == "point_type") {
        # Point type representation - show different point types
        if (!is.null(param1_bins) && length(levels(param1_bins)) > 1) {
          point_types <- c(16, 17, 15, 18, 19)  # circle, triangle, square, diamond, filled diamond
          legend("topright",
                 title = paste(optional_param1$col, collapse = "+"),
                 legend = levels(param1_bins),
                 pch = point_types[seq_len(length(levels(param1_bins)))],
                 pt.cex = 1.0,
                 cex = 0.7)
        } else {
          legend("topright",
                 title = paste(optional_param1$col, collapse = "+"),
                 legend = "All",
                 pch = 16,
                 pt.cex = 1.0,
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
      # Legend for optional parameter 2 (color) - handle both categorical and numeric for file save
      if (is_categorical_group && !is.null(selected_groups) && length(selected_groups) > 0) {
        # Categorical group legend for file save
        create_group_legend(unique_groups, group_colors, group_counts)
      } else if (length(optional_param2$col) == 1) {
        # Numeric data legend - show color legend with exactly 5 bins
        # Generate 5 colors for the legend using the selected palette
        if (color_palette == "blue") {
          legend_colors <- colorRampPalette(c("#357ABD", "#002147"))(5)
        } else if (color_palette == "red") {
          legend_colors <- colorRampPalette(c("#FF6666", "#990000"))(5)
        } else if (color_palette == "viridis") {
          if (!requireNamespace("viridisLite", quietly = TRUE)) install.packages("viridisLite")
          legend_colors <- viridisLite::viridis(5)
        } else if (color_palette == "rainbow") {
          legend_colors <- rainbow(5)
        } else {
          legend_colors <- rep("grey", 5)
        }

        # Create legend labels based on whether it's Aspect.Ratio or not
        if (optional_param2$col == "Aspect.Ratio") {
          # Use hardcoded labels for Aspect.Ratio
          legend_labels <- c("1-1.5", "1.5-3", "3-5", "5-10", "10+")
        } else {
          # Use dynamic range labels for other columns
          # Check if data is actually numeric
          if (is.numeric(param2_values) && all(is.finite(param2_values), na.rm = TRUE)) {
            param2_range <- range(param2_values, na.rm = TRUE)
            param2_breaks_legend <- seq(param2_range[1], param2_range[2], length.out = 6)
            legend_labels <- paste0(round(param2_breaks_legend[1:5], 3), " - ", round(param2_breaks_legend[2:6], 3))
          } else {
            # Fallback for non-numeric data
            legend_labels <- "All"
          }
        }

        legend("topleft",
               legend = legend_labels,
               col = legend_colors,
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

    # Add comprehensive plot notes if requested
    if (include_plot_notes && nchar(col1_text) > 0) {
      # Add plot notes in three columns below the plot with intelligent positioning
      # Column 1 (left) - Elements and filters
      mtext(col1_text, side = 1, line = line_pos, cex = text_cex, col = "darkblue", outer = TRUE, adj = 0)

      # Column 2 (center) - Optional parameters
      mtext(col2_text, side = 1, line = line_pos, cex = text_cex, col = "darkgreen", outer = TRUE, adj = 0.5)

      # Column 3 (right) - Analysis methods
      mtext(col3_text, side = 1, line = line_pos, cex = text_cex, col = "darkred", outer = TRUE, adj = 1)

      # Add a debug message to confirm plot notes are being added
      if (getOption("ternary.debug", FALSE)) {
        cat("DEBUG: Plot notes added for file save - Elements:", length(elements_summary), "Optional:", length(optional_summary), "Analysis:", length(analysis_summary), "\n")
        cat("DEBUG: Plot notes text lengths for file save - col1:", nchar(col1_text), "col2:", nchar(col2_text), "col3:", nchar(col3_text), "\n")
        cat("DEBUG: Plot notes positioning for file save - line_pos:", line_pos, "text_cex:", text_cex, "\n")
      }
    }

    # Restore original par settings
    par(op)

    log_operation("SUCCESS", "Plot saved", filename)

    filename
  })
}
