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
# Programmatic only: toggle by calling options(ternary.debug = TRUE)
# directly in an R console before launching the app.
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
# observers/renderers call synchronously (e.g. safe_execute()). `rv` is
# never reachable via plain `exists("rv")` (that resolves through
# log_operation's own *lexical* scope - the package namespace - not the
# caller's). Instead, walk the live call stack: for each active frame,
# look up `rv` via ordinary (lexical) scoping starting from that frame.
# Every access is wrapped in shiny::isolate() - reading a reactiveValues
# field from an *active* reactive context registers a read dependency for
# that context, and the subsequent write to the same field would then
# invalidate that same context, re-running it and calling log_operation()
# again in a self-sustaining infinite reactive loop. isolate() suppresses
# dependency registration for whichever context happens to be calling
# log_operation(), while the write still correctly invalidates unrelated,
# already-subscribed consumers (e.g. the Analysis Log tab's own display).
# Also guarded with tryCatch since log_operation() is sometimes called
# from non-reactive contexts, where reactiveValues access throws outright.
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

# Function to generate distinct colors for categorical groups
generate_distinct_colors <- function(n_groups) {
  if (n_groups <= 0) return(character(0))

  # Use ColorBrewer palettes for maximum distinction. Wrapped in
  # suppressWarnings(): brewer.pal()'s own "minimal value for n is 3"
  # warning below is real but permanently benign here (see the comment
  # after this if/else) - already-correct behavior, not something to fix,
  # just noise worth quieting.
  if (n_groups <= 12) {
    colors <- suppressWarnings(RColorBrewer::brewer.pal(n_groups, "Set3"))
  } else if (n_groups <= 24) {
    colors <- c(RColorBrewer::brewer.pal(12, "Set3"),
                suppressWarnings(RColorBrewer::brewer.pal(min(12, n_groups-12), "Paired")))
  } else if (n_groups <= 32) {
    colors <- c(RColorBrewer::brewer.pal(12, "Set3"),
                RColorBrewer::brewer.pal(12, "Paired"),
                suppressWarnings(RColorBrewer::brewer.pal(min(8, n_groups-24), "Dark2")))
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

#' Build a combined-upload reactive for a multi-file `fileInput`
#'
#' Reads every row of `input[[file_input_id]]` via
#' `openxlsx::read.xlsx(sheet = 1)`, drops files that failed to read, tags a
#' `source_file` column (from each file's own name, via
#' `tools::file_path_sans_ext()`) when more than one file was read, and - if
#' more than one data frame remains - row-binds them on their common columns.
#'
#' @param input The Shiny `input` object.
#' @param file_input_id Character; the `fileInput` id to read from.
#' @param allow_multiple If `FALSE`, more than one uploaded file is rejected
#'   with a validation message instead of being combined (used by EVS and
#'   the spatial tabs, where pooling specimens is invalid). Default `TRUE`.
#' @return A `shiny::reactive({...})` yielding the combined data frame.
#' @export
make_combined_upload_reactive <- function(input, file_input_id, allow_multiple = TRUE) {
  shiny::reactive({
    req(input[[file_input_id]])
    n_files <- nrow(input[[file_input_id]])
    shiny::validate(shiny::need(allow_multiple || n_files == 1,
      "Upload one file (one specimen) only - this analysis can't pool several specimens."))
    dfs <- lapply(seq_len(n_files), function(i) {
      d <- tryCatch(openxlsx::read.xlsx(input[[file_input_id]]$datapath[i], sheet = 1), error = function(e) NULL)
      if (is.null(d)) return(NULL)
      if (n_files > 1) d$source_file <- tools::file_path_sans_ext(input[[file_input_id]]$name[i])
      d
    })
    dfs <- Filter(Negate(is.null), dfs)
    shiny::validate(shiny::need(length(dfs) > 0, "None of the selected files could be read."))
    if (length(dfs) == 1) return(dfs[[1]])
    common_cols <- Reduce(intersect, lapply(dfs, names))
    shiny::validate(shiny::need(length(common_cols) > 0, "The selected files have no columns in common."))
    do.call(rbind, lapply(dfs, function(d) d[, common_cols, drop = FALSE]))
  })
}

#' Safely invoke a reactive for a download handler
#'
#' Calls `reactive_thunk()` fresh (so callers pass a zero-arg closure like
#' `function() result()` rather than an already-evaluated value), converting
#' any error into a download-friendly message: a non-empty error message is
#' prefixed with "Could not generate this download: "; an empty one (e.g.
#' from `req()`/`shiny::validate()`) is replaced with `placeholder_msg`.
#'
#' @param reactive_thunk Zero-arg function that invokes the underlying reactive.
#' @param placeholder_msg Message to use when the underlying error has no text.
#' @return Whatever `reactive_thunk()` returns, on success.
#' @export
safe_reactive_result <- function(reactive_thunk, placeholder_msg) {
  tryCatch(reactive_thunk(), error = function(e) {
    if (nzchar(e$message)) {
      stop("Could not generate this download: ", e$message)
    }
    stop(placeholder_msg)
  })
}

#' Render a gated status message from inside a `renderText()` block
#'
#' Returns `placeholder_msg` until `input[[button_id]]` has been clicked;
#' after that, calls `build_message_fn()` and returns its result, mapping an
#' empty-message `shiny.silent.error` (from `req()`/`shiny::validate()`) back
#' to `placeholder_msg` while re-throwing any other error so Shiny's own
#' output machinery can display it.
#'
#' @param input The Shiny `input` object.
#' @param button_id Character; id of the gating `actionButton`.
#' @param placeholder_msg Message to show before the button is clicked, or on an empty-message error.
#' @param build_message_fn Zero-arg function that computes and returns the real status message string.
#' @return Character status message.
#' @export
render_gated_status <- function(input, button_id, placeholder_msg, build_message_fn) {
  if (is.null(input[[button_id]]) || input[[button_id]] == 0) {
    return(placeholder_msg)
  }
  tryCatch({
    build_message_fn()
  }, shiny.silent.error = function(e) {
    if (!nzchar(conditionMessage(e))) return(placeholder_msg)
    stop(e)
  })
}

#' Return the first element matching a pattern, or `NULL`
#'
#' @param x Character vector to search.
#' @param pattern Regex pattern, matched case-insensitively.
#' @return The first matching element of `x`, or `NULL` if none match.
#' @export
first_match_or_null <- function(x, pattern) {
  hit <- x[grepl(pattern, x, ignore.case = TRUE)]
  if (length(hit) == 0) NULL else hit[1]
}

# Note: Functions are exported via NAMESPACE file
