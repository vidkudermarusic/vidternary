# ---- Statistics Display Utilities ----
# Tidy, DT-ready data frames for descriptive/correlation statistics in the
# Data Comparison tab. Richer than helpers_reporting.R's
# create_statistical_summary()/create_correlation_matrix() (those are
# shaped for Excel export sheets, not interactive display), and replace
# raw summary()/print() dumps into verbatimTextOutput with sortable,
# searchable, exportable tables.

# One row per numeric variable: N, missing count, five-number summary,
# mean, SD, and coefficient of variation (SD as % of mean) - CV is a
# genuinely new metric versus the old summary()-based display, useful for
# comparing spread across variables on very different scales (e.g.
# comparing element wt% variability).
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

# Same table for two datasets side by side (long format with a Dataset
# column) so it can be sorted/filtered as one table instead of reading two
# separately printed summary() blocks.
build_descriptive_stats_comparison_table <- function(df1, df2) {
  t1 <- build_descriptive_stats_table(df1)
  t2 <- build_descriptive_stats_table(df2)
  if (nrow(t1) > 0) t1 <- cbind(Dataset = "Dataset 1", t1, stringsAsFactors = FALSE)
  if (nrow(t2) > 0) t2 <- cbind(Dataset = "Dataset 2", t2, stringsAsFactors = FALSE)
  result <- rbind(t1, t2)
  result[order(result$Variable, result$Dataset), ]
}

# One row per variable pair (not a full matrix - a full n x n matrix is
# awkward to sort/scan; a long "pairs" table sorted by |r| puts the
# strongest relationships at the top).
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

# Compares the internal correlation structure of two datasets over their
# common numeric columns, one row per pair, sorted by |difference| - this
# replaces printing three full matrices (dataset 1, dataset 2, and their
# element-wise difference) as text and asking the user to spot the large
# differences themselves. Also drops the old "direct row-wise correlation
# between dataset 1 and dataset 2" metric: that paired complete.cases()
# row-by-row between the two datasets, which isn't meaningful when the two
# datasets have different row counts (they're independent samples, e.g.
# two different steel heats - there's no natural pairing between row i of
# one and row i of the other).
build_correlation_comparison_table <- function(df1, df2, common_cols) {
  if (length(common_cols) < 2) {
    return(data.frame(Variable_1 = character(0), Variable_2 = character(0),
                       Dataset1_r = numeric(0), Dataset2_r = numeric(0), Difference = numeric(0)))
  }
  m1 <- suppressWarnings(stats::cor(df1[, common_cols, drop = FALSE], use = "complete.obs"))
  m2 <- suppressWarnings(stats::cor(df2[, common_cols, drop = FALSE], use = "complete.obs"))
  pairs <- utils::combn(common_cols, 2, simplify = FALSE)
  rows <- lapply(pairs, function(p) {
    r1 <- m1[p[1], p[2]]
    r2 <- m2[p[1], p[2]]
    data.frame(Variable_1 = p[1], Variable_2 = p[2], Dataset1_r = r1, Dataset2_r = r2,
               Difference = r1 - r2, stringsAsFactors = FALSE)
  })
  result <- do.call(rbind, rows)
  result[order(-abs(result$Difference)), ]
}

# Shared DT options: CSV/Excel/copy export buttons built into the table
# itself, sane paging, and column-specific rounding for numeric columns.
# escape_html = FALSE is needed when a column holds raw <img> markup (the
# mini-histogram "Distribution" column added by add_distribution_column()).
render_stats_datatable <- function(df, round_cols = NULL, digits = 3, page_length = 15, escape_html = TRUE) {
  dt <- DT::datatable(
    df,
    rownames = FALSE,
    escape = escape_html,
    extensions = "Buttons",
    options = list(
      dom = "Bfrtip",
      buttons = c("copy", "csv", "excel"),
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

# One small styled "card" per numeric variable (name, mean +/- SD, median,
# N) for a fast visual scan, instead of having to read every row of the
# full stats table. Returns a tagList of divs laid out with CSS flexbox
# (wraps responsively; the table below remains the sortable/exportable
# source of truth).
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

# Tiny base64-encoded PNG histogram for one numeric vector, as an <img>
# tag - used to embed a per-row distribution sparkline in a DT column
# (jsonlite::base64_enc() is already a declared dependency, avoiding a new
# one just for this).
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

# Adds a "Distribution" column (mini-histogram per row) to a descriptive
# stats table, using the *raw* column values from df (the stats table
# itself only holds aggregates). Only meaningful for a single-dataset
# table (no "Dataset" column) since that's the only case where each row
# maps 1:1 to one column of one data frame.
add_distribution_column <- function(stats_table, df) {
  if (nrow(stats_table) == 0) return(stats_table)
  stats_table$Distribution <- vapply(stats_table$Variable, function(v) {
    if (!v %in% names(df)) return("")
    render_mini_histogram_base64(suppressWarnings(as.numeric(df[[v]])))
  }, character(1))
  stats_table
}
