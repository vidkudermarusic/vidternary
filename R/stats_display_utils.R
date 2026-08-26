# ---- Statistics Display Utilities ----
# Tidy, DT-ready data frames for descriptive/correlation statistics in the
# Data Comparison tab. Replace raw summary()/print() dumps into
# verbatimTextOutput with sortable, searchable, exportable tables.

#' Build a tidy descriptive-statistics table, one row per numeric variable
#'
#' N, missing count, five-number summary, mean, SD, and coefficient of
#' variation (SD as % of mean) - useful for comparing spread across
#' variables on very different scales.
#'
#' @param df A data frame.
#' @param numeric_cols Columns to summarize. Defaults to all numeric
#'   columns of `df`.
#' @return A data frame with one row per variable: `Variable`, `N`,
#'   `Missing`, `Min`, `Q1`, `Median`, `Mean`, `SD`, `Q3`, `Max`, `CV_pct`.
#' @export
build_descriptive_stats_table <- function(df, numeric_cols = NULL) {
  if (is.null(numeric_cols)) numeric_cols <- names(df)[sapply(df, is.numeric)]
  if (length(numeric_cols) == 0) {
    return(data.frame(Variable = character(0), N = integer(0), Missing = integer(0),
                       Min = numeric(0), Q1 = numeric(0), Median = numeric(0), Mean = numeric(0),
                       SD = numeric(0), Q3 = numeric(0), Max = numeric(0), CV_pct = numeric(0)))
  }

  rows <- lapply(numeric_cols, function(col) {
    x <- df[[col]]
    x_valid <- x[!is.na(x)]
    has_data <- length(x_valid) > 0
    mean_val <- if (has_data) mean(x_valid) else NA_real_
    sd_val <- if (length(x_valid) > 1) stats::sd(x_valid) else NA_real_
    data.frame(
      Variable = col,
      N = length(x),
      Missing = sum(is.na(x)),
      Min = if (has_data) min(x_valid) else NA_real_,
      Q1 = if (has_data) stats::quantile(x_valid, 0.25, names = FALSE) else NA_real_,
      Median = if (has_data) stats::median(x_valid) else NA_real_,
      Mean = mean_val,
      SD = sd_val,
      Q3 = if (has_data) stats::quantile(x_valid, 0.75, names = FALSE) else NA_real_,
      Max = if (has_data) max(x_valid) else NA_real_,
      CV_pct = if (!is.na(sd_val) && !is.na(mean_val) && mean_val != 0) 100 * sd_val / mean_val else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Build a combined descriptive-statistics table for 2+ datasets
#'
#' `build_descriptive_stats_table()` for each dataset, stacked long (one
#' `Dataset` column) so all datasets sort/filter as a single table.
#'
#' @param dfs A named list of 2+ data frames; names become the `Dataset` column's values.
#' @return A data frame with one row per (Dataset, Variable) pair.
#' @export
build_descriptive_stats_comparison_table <- function(dfs) {
  rows <- lapply(names(dfs), function(nm) {
    t <- build_descriptive_stats_table(dfs[[nm]])
    if (nrow(t) > 0) t <- cbind(Dataset = nm, t, stringsAsFactors = FALSE)
    t
  })
  result <- do.call(rbind, rows)
  if (is.null(result) || nrow(result) == 0) return(result)
  result[order(result$Variable, result$Dataset), ]
}

#' Build a long-format correlation table, one row per variable pair
#'
#' Sorted by `|Correlation|` descending, so the strongest relationships
#' appear first - easier to sort/scan than a full n x n matrix.
#'
#' @param df A data frame.
#' @param numeric_cols Columns to correlate. Defaults to all numeric
#'   columns of `df`. Needs at least 2.
#' @return A data frame: `Variable_1`, `Variable_2`, `Correlation`.
#' @export
build_correlation_pairs_table <- function(df, numeric_cols = NULL) {
  if (is.null(numeric_cols)) numeric_cols <- names(df)[sapply(df, is.numeric)]
  if (length(numeric_cols) < 2) {
    return(data.frame(Variable_1 = character(0), Variable_2 = character(0), Correlation = numeric(0)))
  }
  m <- suppressWarnings(stats::cor(df[, numeric_cols, drop = FALSE], use = "complete.obs"))
  pairs <- utils::combn(numeric_cols, 2, simplify = FALSE)
  rows <- lapply(pairs, function(p) {
    data.frame(Variable_1 = p[1], Variable_2 = p[2], Correlation = m[p[1], p[2]], stringsAsFactors = FALSE)
  })
  result <- do.call(rbind, rows)
  result[order(-abs(result$Correlation)), ]
}

#' Compare the internal correlation structure of 2+ datasets
#'
#' `build_correlation_pairs_table()` for each dataset over `common_cols`,
#' stacked long (one `Dataset` column) so it stays a single sortable table
#' regardless of dataset count. Does not compute a direct row-wise
#' correlation *between* datasets - that isn't meaningful when datasets
#' have different row counts (independent samples with no natural pairing).
#'
#' @param dfs A named list of 2+ data frames; names become the `Dataset` column's values.
#' @param common_cols Numeric columns present in all of `dfs`, to correlate. Needs at least 2.
#' @return A data frame: `Dataset`, `Variable_1`, `Variable_2`, `Correlation`.
#' @export
build_correlation_comparison_table <- function(dfs, common_cols) {
  if (length(common_cols) < 2) {
    return(data.frame(Dataset = character(0), Variable_1 = character(0),
                       Variable_2 = character(0), Correlation = numeric(0)))
  }
  rows <- lapply(names(dfs), function(nm) {
    pairs_table <- build_correlation_pairs_table(dfs[[nm]], common_cols)
    if (nrow(pairs_table) > 0) cbind(Dataset = nm, pairs_table, stringsAsFactors = FALSE) else pairs_table
  })
  result <- do.call(rbind, rows)
  result[order(result$Variable_1, result$Variable_2, result$Dataset), ]
}

#' Render a stats/correlation data frame as a `DT::datatable()`
#'
#' Adds copy/CSV/Excel export buttons (configured to export all pages, not
#' just the visible one) and optional per-column rounding.
#'
#' @param df A data frame to display.
#' @param round_cols Optional character vector of numeric columns to round
#'   for display.
#' @param digits Decimal places for `round_cols`. Default 3.
#' @param page_length Rows per page. Default 15.
#' @param escape_html Whether to HTML-escape cell content. Set `FALSE` when
#'   a column holds raw `<img>` markup (e.g. from
#'   `add_distribution_column()`). Default `TRUE`.
#' @return A `DT::datatable` htmlwidget.
#' @export
render_stats_datatable <- function(df, round_cols = NULL, digits = 3, page_length = 15, escape_html = TRUE) {
  export_all_pages <- list(exportOptions = list(modifier = list(page = "all")))
  dt <- DT::datatable(
    df,
    rownames = FALSE,
    escape = escape_html,
    extensions = "Buttons",
    options = list(
      dom = "Bfrtip",
      buttons = list(
        c(list(extend = "copy"), export_all_pages),
        c(list(extend = "csv"), export_all_pages),
        c(list(extend = "excel"), export_all_pages)
      ),
      pageLength = page_length,
      scrollX = TRUE
    )
  )
  if (!is.null(round_cols)) {
    round_cols <- intersect(round_cols, names(df))
    if (length(round_cols) > 0) dt <- DT::formatRound(dt, columns = round_cols, digits = digits)
  }
  dt
}

#' Build small styled "stat cards" (one per variable) for a quick visual scan
#'
#' The full sortable/exportable table (`render_stats_datatable()`) remains
#' the source of truth; these cards are a faster-to-scan summary above it.
#'
#' @param stats_table A result from `build_descriptive_stats_table()` (or
#'   the comparison variant).
#' @return A `shiny::div()` of responsively-wrapping flexbox cards, or a
#'   "No statistics computed yet." message if `stats_table` is empty.
#' @export
build_stat_cards <- function(stats_table) {
  if (is.null(stats_table) || nrow(stats_table) == 0) return(tags$p("No statistics computed yet."))

  cards <- lapply(seq_len(nrow(stats_table)), function(i) {
    row <- stats_table[i, ]
    mean_txt <- if (is.na(row$Mean)) "NA" else sprintf("%.3g", row$Mean)
    sd_txt <- if (is.na(row$SD)) "NA" else sprintf("%.3g", row$SD)
    median_txt <- if (is.na(row$Median)) "NA" else sprintf("%.3g", row$Median)
    dataset_txt <- if ("Dataset" %in% names(stats_table)) paste0(" (", row$Dataset, ")") else ""

    div(style = "display: inline-block; vertical-align: top; width: 160px; margin: 4px; padding: 10px; border: 1px solid #dee2e6; border-radius: 6px; background-color: #f8f9fa;",
      div(style = "font-weight: bold; font-size: 13px; color: #343a40;", paste0(row$Variable, dataset_txt)),
      div(style = "font-size: 16px; color: #002147; margin-top: 4px;", paste0(mean_txt, " ± ", sd_txt)),
      div(style = "font-size: 11px; color: #6c757d;", paste("Median:", median_txt)),
      div(style = "font-size: 11px; color: #6c757d;", paste("N:", row$N, if (row$Missing > 0) paste0(" (", row$Missing, " missing)") else ""))
    )
  })
  div(style = "display: flex; flex-wrap: wrap;", cards)
}

#' Render a tiny base64-encoded PNG histogram as an `<img>` tag
#'
#' Used to embed a per-row distribution sparkline in a DT column.
#'
#' @param values Numeric vector to histogram.
#' @param width_px Image width in pixels. Default 120.
#' @param height_px Image height in pixels. Default 32.
#' @return An HTML `<img>` tag string with the histogram as a base64 data
#'   URI, or `""` if there are fewer than 2 finite values or zero variance.
#' @export
render_mini_histogram_base64 <- function(values, width_px = 120, height_px = 32) {
  values <- values[is.finite(values)]
  if (length(values) < 2 || stats::sd(values) == 0) return("")

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp, width = width_px, height = height_px, bg = "transparent")
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::hist(values, breaks = 12, col = "#357ABD", border = NA, main = NULL, axes = FALSE, xlab = "", ylab = "")
  grDevices::dev.off()

  raw_bytes <- readBin(tmp, "raw", file.info(tmp)$size)
  b64 <- jsonlite::base64_enc(raw_bytes)
  sprintf('<img src="data:image/png;base64,%s" width="%d" height="%d" alt="distribution"/>', b64, width_px, height_px)
}

#' Add a per-row mini-histogram "Distribution" column to a stats table
#'
#' Only meaningful for a single-dataset table (no `Dataset` column), since
#' that's the only case where each row maps 1:1 to one column of `df`.
#'
#' @param stats_table A result from `build_descriptive_stats_table()`.
#' @param df The original data frame the stats were computed from (for raw values).
#' @return `stats_table` with an added `Distribution` column (HTML `<img>` tags).
#' @export
add_distribution_column <- function(stats_table, df) {
  if (nrow(stats_table) == 0) return(stats_table)
  stats_table$Distribution <- vapply(stats_table$Variable, function(v) {
    if (!v %in% names(df)) return("")
    render_mini_histogram_base64(suppressWarnings(as.numeric(df[[v]])))
  }, character(1))
  stats_table
}
