# ---- Inclusion add-on: drawing a preset diagram ----

#' Draw an inclusion preset diagram
#'
#' Ternary of the kept particles coloured by class, with the classification
#' lines and the reference phases. Uses the same `Ternary` calls and grid
#' style as the existing ternary plots and draws to the active device.
#'
#' @param result From [analyze_inclusion_preset()].
#' @param show_lines Draw the classification lines.
#' @param show_reference Mark and label the reference phases (phases that
#'   sit on a corner are not labelled; the corner label already names them).
#' @param cex,pch Point size and symbol.
#' @param legend Draw the class legend with counts.
#' @param notes Print the settings under the diagram.
#' @param main Title; default the preset name.
#' @return The class table (invisibly).
#' @export
plot_inclusion_preset <- function(result, show_lines = TRUE, show_reference = TRUE,
                                  cex = 0.5, pch = 16, legend = TRUE, notes = TRUE, main = NULL) {
  if (!requireNamespace("Ternary", quietly = TRUE)) stop("Ternary package is required.", call. = FALSE)
  pr <- result$preset
  lab <- pr$labels
  op <- par(oma = c(if (notes) 4 else 0, 0, 0, 0)); on.exit(par(op))
  Ternary::TernaryPlot(
    atip = lab[1], btip = lab[2], ctip = lab[3],
    alab = paste(lab[1], "->"), blab = paste(lab[2], "->"), clab = paste("<-", lab[3]),
    col = "white", grid.lines = 5, grid.lty = "dotted", grid.minor.lines = 1, grid.minor.lty = "dotted"
  )
  if (show_lines) {
    for (s in preset_boundary_segments(pr)) Ternary::TernaryLines(list(s[1, ], s[2, ]), col = "grey35", lty = 2, lwd = 1)
  }
  cols <- inclusion_class_colors(pr)
  pts <- result$coords[result$keep, , drop = FALSE]
  if (nrow(pts) > 0) {
    Ternary::TernaryPoints(pts, col = cols[result$class[result$keep]], pch = pch, cex = cex)
  } else {
    text(0.5, 0.5, "No particles pass the threshold", col = "red")
  }
  if (show_reference && nrow(pr$references) > 0) {
    refs <- pr$references
    inner <- refs[pmax(refs$A, refs$B, refs$C) < 0.999, , drop = FALSE]
    if (nrow(inner) > 0) {
      Ternary::TernaryPoints(inner[, c("A", "B", "C")], pch = 4, cex = 0.8, col = "black")
      Ternary::TernaryText(inner[, c("A", "B", "C")], labels = inner$phase, cex = 0.55, pos = 3, offset = 0.4)
    }
  }
  title(main = if (is.null(main)) pr$name else main, cex.main = 0.9)
  tab <- result$summary
  if (legend && nrow(tab) > 0) {
    legend("topright", title = "Class", legend = sprintf("%s (n=%d)", tab$class, tab$n),
           col = cols[tab$class], pch = 16, cex = 0.6, bty = "n")
  }
  if (notes) mtext(paste(result$settings, collapse = "\n"), side = 1, outer = TRUE, line = -1, cex = 0.5, adj = 0.02)
  invisible(tab)
}
