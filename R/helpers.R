# ---- Helper Functions Module ----
# This module contains core utility functions used throughout the app:
# logging, debug output, column-name cleaning, and plot-title/summary text.
#
# Related helper functions live in a sibling module, split out for size:
#   helpers_filters.R - filter collection & application

# Note: MIN_POINT_SIZE and MAX_POINT_SIZE constants are defined in options.R

# `%||%`: use y when x is NULL. Base R only provides %||% from R >= 4.4.0;
# this package declares Depends: R (>= 4.0.0), so without a local
# definition, code using %||% (server_ternary_plots_groups.R) would fail
# with "could not find function '%||%'" on any R between 4.0.0 and
# 4.3.x - masked in this development environment only because it happens
# to run R 4.4.2, which already provides it. Defined here (rather than
# imported from a package like rlang, which also provides one) to keep
# the package self-sufficient across its whole declared R-version range.
# Not exported/documented (matches this file's convention for
# internal-only helpers, e.g. generate_distinct_colors() below) - ordinary
# lexical scoping means the package's own functions calling %||%
# unqualified find this definition first, regardless of R version.
`%||%` <- function(x, y) if (is.null(x)) y else x

# Debug Mode Control
# Set this to TRUE to enable debug output. Programmatic only - there used to
# be an "Enable Debug Mode" UI checkbox, but it never actually flipped this
# option (input$debug_mode was never read anywhere), so it was removed
# rather than wired up; toggle this by calling
# options(ternary.debug = TRUE) directly in an R console before launching
# the app.
options(ternary.debug = FALSE)

#' Print a debug message when debug mode is enabled
#'
#' Wraps `cat(sprintf(...))`, gated behind `getOption("ternary.debug", FALSE)`,
#' so verbose diagnostic output can be toggled on/off without removing the
#' calls. Usage: `debug_log("Processing %d items", length(items))`.
#'
#' @param message A `sprintf()`-style format string.
#' @param ... Values to interpolate into `message`.
#' @return `NULL`, invisibly. Called for its `cat()` side effect.
#' @export
debug_log <- function(message, ...) {
  if (getOption("ternary.debug", FALSE)) {
    cat(sprintf(message, ...), "\n")
  }
}

# Enhanced logging system with structured logging and performance optimization
#
# log_operation() is defined at package top level but is called from deep
# inside every create_server_*() factory function, where `rv` is a local
# parameter - and sometimes from plain helper functions that those
# observers/renderers call synchronously (e.g. safe_execute()).
# `rv` is never reachable via plain `exists("rv")` (that resolves through
# log_operation's own *lexical* scope - the package namespace - not the
# caller's). Instead, walk the live call stack: for each active frame,
# look up `rv` via ordinary (lexical) scoping starting from that frame.
# A direct call from an observer/renderer finds `rv` immediately (the
# handler expression is lexically nested inside its create_server_*()
# closure); a call via an intermediate helper function finds it once the
# walk reaches that still-executing observer's own frame further up the
# stack. Every access is wrapped in shiny::isolate() - reading a
# reactiveValues field from an *active* reactive context (e.g. the
# observer that ends up calling log_operation()) registers a read
# dependency for that context; the subsequent write to the same field
# would then invalidate that same context, which re-runs, calls
# log_operation() again, and so on - a self-sustaining infinite reactive
# loop (this was hit and confirmed during development: a call site that
# errors on every invocation produced 10000+ log_operation() calls in
# seconds). isolate() suppresses dependency registration for whichever
# context happens to be calling log_operation(), while the write still
# correctly invalidates unrelated, already-subscribed consumers (e.g. the
# Analysis Log tab's own display). Also guarded with tryCatch since
# log_operation() is sometimes called from non-reactive contexts, where
# reactiveValues access throws outright.
#' Record a structured log entry, and print it to the console
#'
#' Appends to the calling Shiny session's `rv$analysis_log` (found by
#' walking the live call stack for a lexically-reachable `rv`, wrapped in
#' `shiny::isolate()` to avoid a reactive read-then-write feedback loop -
#' see this function's inline comments for the full reasoning), then
#' `cat()`s the entry regardless of whether a reactive context was found.
#'
#' @param level Log level label, e.g. `"INFO"`, `"WARNING"`, `"ERROR"`.
#' @param message Log message text.
#' @param details Optional additional detail text.
#' @return `NULL`, invisibly.
#' @export
log_operation <- function(level, message, details = NULL) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry <- list(
    timestamp = timestamp,
    level = level,
    message = message,
    details = details
  )

  tryCatch({
    n <- sys.nframe()
    if (n > 1) {
      for (i in rev(seq_len(n - 1))) {
        frame_env <- sys.frame(i)
        if (exists("rv", envir = frame_env, inherits = TRUE)) {
          rv_obj <- get("rv", envir = frame_env, inherits = TRUE)
          logged <- shiny::isolate({
            current_log <- tryCatch(rv_obj$analysis_log, error = function(e) NULL)
            if (!is.null(current_log)) {
              rv_obj$analysis_log <- c(current_log, list(log_entry))
              if (length(rv_obj$analysis_log) > 10000) {
                rv_obj$analysis_log <- rv_obj$analysis_log[-(1:(length(rv_obj$analysis_log) - 10000))]
              }
              TRUE
            } else {
              FALSE
            }
          })
          if (isTRUE(logged)) break
        }
      }
    }
  }, error = function(e) NULL)

  # Console output for debugging
  cat(sprintf("[%s] %s: %s\n", timestamp, level, message))
  if (!is.null(details)) {
    cat(sprintf("  Details: %s\n", details))
  }
}

#' Clean column names for display
#'
#' Strips a `.(Wt%)` suffix, replaces dots/underscores with spaces, and
#' title-cases each word.
#'
#' @param col_names Character vector of raw column names.
#' @return Character vector of cleaned, display-friendly names.
#' @export
clean_column_names <- function(col_names) {
  # Remove .(Wt%) suffix first
  cleaned <- gsub("\\.\\(Wt%\\)", "", col_names)
  # Replace dots with spaces
  cleaned <- gsub("\\.", " ", cleaned)
  # Replace underscores with spaces
  cleaned <- gsub("_", " ", cleaned)
  # Capitalize first letter of each word
  cleaned <- gsub("\\b([a-z])", "\\U\\1", cleaned, perl = TRUE)
  return(cleaned)
}

#' Show a timestamped message on the console
#'
#' Within a running app, `create_server_logic()` overrides this with a
#' version that also pushes a toast to the browser via
#' `session$sendCustomMessage()`; this top-level definition is the fallback
#' used outside a Shiny session.
#'
#' @param message Message text.
#' @param type Message type label, e.g. `"info"`, `"error"`. Default `"info"`.
#' @return `NULL`, invisibly.
#' @export
show_message <- function(message, type = "info") {
  # In a Shiny context, this would typically use showNotification or similar
  # For now, we'll just print to console
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s: %s\n", timestamp, toupper(type), message))
}

# create_multi_line_title()/preview_title_layout()/calculate_plot_dimensions()
# used to live here too - all three dead code, none exported, confirmed via
# a full-tree grep to have zero real callers anywhere. Each shared its name
# with a completely different, actually-used implementation: ternary_plot_
# data_prep.R's prepare_ternary_plot_data() defines its own local
# preview_title_layout() (deliberately kept separate and passed as
# build_ternary_plot_title()'s title_layout_fn callback - see that
# function's own roxygen for why) and its own local calculate_plot_
# dimensions() (base 1200x1400px, grows *height* per extra title line -
# genuinely used, passed through as pd$calculate_plot_dimensions() and
# called for real by ternary_plot_save.R) - both bearing no resemblance to
# these top-level versions (base 10x8in, grows *width* past 50 characters).
# create_multi_line_title() itself had no independent caller of its own -
# only these other two dead functions ever called it - so it cascaded into
# the same removal once they were gone, rather than being left behind as
# an orphan of an orphan. Same class of leftover-from-an-incomplete-
# refactor as the dead apply_individual_filters() found and removed
# elsewhere in this audit.

# Function to generate distinct colors for categorical groups
generate_distinct_colors <- function(n_groups) {
  if (n_groups <= 0) return(character(0))

  # Use ColorBrewer palettes for maximum distinction
  if (n_groups <= 12) {
    colors <- RColorBrewer::brewer.pal(n_groups, "Set3")
  } else if (n_groups <= 24) {
    colors <- c(RColorBrewer::brewer.pal(12, "Set3"),
                RColorBrewer::brewer.pal(min(12, n_groups-12), "Paired"))
  } else if (n_groups <= 32) {
    colors <- c(RColorBrewer::brewer.pal(12, "Set3"),
                RColorBrewer::brewer.pal(12, "Paired"),
                RColorBrewer::brewer.pal(min(8, n_groups-24), "Dark2"))
  } else {
    # For >32 groups, use viridis sampling. viridisLite::viridis() (not the
    # full viridis package's own version - the two are not the same
    # dependency) provides the identical function, and viridisLite is
    # already the package's established choice for this colormap
    # elsewhere (ternary_plot_data_prep.R, ternary_plot_save.R,
    # ternary_plot_preview.R) - using it here too avoids adding a second,
    # heavier dependency (the full viridis package pulls in gridExtra)
    # just to reach the same function.
    colors <- viridisLite::viridis(n_groups)
  }
  # RColorBrewer::brewer.pal() has an undocumented-to-callers-here floor of
  # 3 - it silently returns 3 colors (with its own warning) for a requested
  # n of 1 or 2, rather than erroring. All three branches above request
  # brewer.pal() at least once with a count that can legitimately be as low
  # as 1 (n_groups itself in the first branch; n_groups-12/n_groups-24, the
  # "how many more are needed" remainder, in the other two) - so n_groups
  # of 1, 2, 13, 14, 25, or 26 all silently returned 3/15/27 colors instead
  # of the requested count, confirmed empirically for each. Truncating
  # here, once, after every branch has run, fixes all three uniformly
  # rather than patching each brewer.pal() call site separately; it's a
  # no-op for viridisLite::viridis() (which has no such floor and already
  # returns exactly n_groups) and for every n_groups that never hit the
  # floor to begin with.
  colors[seq_len(n_groups)]
}

# Function to create group legend
create_group_legend <- function(groups, colors, counts) {
  if (length(groups) == 0) return()

  # Sort groups by frequency (most frequent first)
  group_order <- order(counts[groups], decreasing = TRUE)
  sorted_groups <- groups[group_order]
  sorted_colors <- colors[group_order]

  # Create multi-column legend
  legend("topright",
         legend = sorted_groups,
         col = sorted_colors,
         pch = 16,
         title = "Groups",
         cex = 0.6,
         ncol = 2, # 2 columns
         y.intersp = 0.8)
}

# Note: Functions are exported via NAMESPACE file
