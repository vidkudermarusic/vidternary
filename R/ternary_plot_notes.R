# ---- Ternary Plot: Plot-Notes Drawing (split out of ternary_plot_data_prep.R) ----
# Shared mtext()-drawing helper used by both ternary_plot_preview.R and
# ternary_plot_save.R to draw the col1/col2/col3_text columns that
# prepare_ternary_plot_data() computes - not called by prepare_ternary_plot_data()
# itself, so it doesn't belong with the orchestrator either.

# Draws one plot-notes column (col1/col2/col3_text from
# prepare_ternary_plot_data(), each a "\n"-joined block of lines) as a
# stack of individual mtext() calls, one per line, each with its OWN
# explicit `line=` position - not a single mtext() call on the whole
# "\n"-joined string. mtext() anchors a multi-line "\n" string by its LAST
# line at the given `line=` value, with earlier lines extending TOWARD the
# plot, not away from it - so three columns of different lengths (e.g. a
# short "Elements" column next to a long "Outlier Detection:
# Mahalanobis..." column once Mahalanobis/Isolation Forest are active)
# would have their LAST lines converge on the same outer position and
# overlap, regardless of how many lines each column actually needed above
# that shared bottom anchor. Anchoring every column's FIRST line at the
# same `start_line` instead, and stepping each subsequent line further
# outward, keeps columns of any length from ever colliding based on how
# many lines their neighbors happen to have.
#
# The `cex * 1.1` per-line step (not `cex` alone) was tuned empirically
# against a real 7-line Mahalanobis column: mtext()'s `line=` units are a
# fixed physical size independent of the `cex` passed to that call, so a
# step of exactly `cex` visually crowded adjacent lines together at small
# cex - `cex * 1.1` leaves a small, consistent gap at every size tested
# (0.5-0.6, this file's own three cex tiers).
#' Draw one plot-notes column as a stack of individually-positioned mtext() lines
#'
#' @param text A `"\n"`-joined block of lines (e.g. `col1_text`/`col2_text`/
#'   `col3_text` from `prepare_ternary_plot_data()`). Empty lines are
#'   skipped.
#' @param side Passed to `mtext()` - which plot margin to draw in.
#' @param start_line Where the FIRST line is drawn (`mtext()`'s `line=` for
#'   that line); each subsequent line is drawn further out by `cex * 1.1`.
#' @param cex Text size, passed to `mtext()`.
#' @param col Text color, passed to `mtext()`.
#' @param adj Horizontal justification, passed to `mtext()` (`0` = left,
#'   `0.5` = center, `1` = right).
#' @param outer Passed to `mtext()`. Default `TRUE` (this app always draws
#'   plot notes in the outer margin).
#' @return `invisible(NULL)`. Called for its plotting side effect on the
#'   current graphics device.
#' @export
draw_plot_notes_column <- function(text, side, start_line, cex, col, adj, outer = TRUE) {
  lines <- strsplit(text, "\n")[[1]]
  for (i in seq_along(lines)) {
    if (nzchar(lines[i])) {
      mtext(lines[i], side = side, line = start_line + (i - 1) * cex * 1.1,
            cex = cex, col = col, outer = outer, adj = adj)
    }
  }
  invisible(NULL)
}
