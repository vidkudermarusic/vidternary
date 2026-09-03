# Tests for R/server_analysis_log.R.
#
# Regression coverage for a bug fixed in this pass: several log_operation()
# call sites across the ternary-plot pipeline (ternary_plot.R,
# ternary_plot_data_prep.R, ternary_plot_save.R, ternary_plot_preview.R)
# passed a free-text category ("Data loading", "Column validation", ...) as
# the `level` argument instead of "INFO"/"WARNING"/"ERROR"/"SUCCESS". Those
# entries were invisible to the Log Level filter (only "All" showed them)
# and were silently excluded from log_stats' INFO/WARNING/ERROR tally. This
# file locks down that the Analysis Log tab's filter and stats now handle
# all four real levels - including "SUCCESS", which was already used
# throughout the app (e.g. server_ternary_plots.R, server_hex_ternary.R) but
# had no filter option and wasn't counted in log_stats before this pass.
#
# create_server_analysis_log() is exercised through the real
# moduleServer("analysis_log", ...) wrapper (matching server_logic.R's own
# wiring), so inputs/outputs below are read/written under the
# "analysis_log-" namespace prefix that testServer() applies.
library(shiny)

make_analysis_log_entries <- function() {
  list(
    list(timestamp = "2026-01-01 00:00:01", level = "SUCCESS", message = "Generated ternary coordinates", details = "42 points"),
    list(timestamp = "2026-01-01 00:00:02", level = "INFO", message = "Loading source data", details = "File: a.xlsx"),
    list(timestamp = "2026-01-01 00:00:03", level = "INFO", message = "All required columns found", details = NULL),
    list(timestamp = "2026-01-01 00:00:04", level = "WARNING", message = "Multiple filters selected", details = "used mahalanobis"),
    list(timestamp = "2026-01-01 00:00:05", level = "ERROR", message = "Missing required columns", details = "Mn")
  )
}

make_analysis_log_server <- function(entries) {
  rv <- shiny::reactiveValues(analysis_log = entries)
  show_message <- function(message, type = "info") invisible(NULL)
  log_operation <- function(...) invisible(NULL)
  function(input, output, session) {
    shiny::moduleServer("analysis_log", function(input, output, session) {
      create_server_analysis_log(input, output, session, rv, show_message, log_operation)
    })
  }
}

test_that("log_stats counts SUCCESS/INFO/WARNING/ERROR entries and they sum to the total", {
  testServer(make_analysis_log_server(make_analysis_log_entries()), {
    session$setInputs(`analysis_log-log_level` = "all", `analysis_log-log_search` = "")
    stats <- output[["analysis_log-log_stats"]]
    expect_match(stats, "Total Entries: 5")
    expect_match(stats, "SUCCESS: 1")
    expect_match(stats, "INFO: 2")
    expect_match(stats, "WARNING: 1")
    expect_match(stats, "ERROR: 1")
  })
})

test_that("Log Level filter can isolate SUCCESS entries (previously not offered as a filter option)", {
  testServer(make_analysis_log_server(make_analysis_log_entries()), {
    session$setInputs(`analysis_log-log_level` = "SUCCESS", `analysis_log-log_search` = "")
    log_text <- output[["analysis_log-analysis_log"]]
    expect_match(log_text, "Generated ternary coordinates")
    expect_false(grepl("Loading source data", log_text))
    expect_false(grepl("Missing required columns", log_text))

    stats <- output[["analysis_log-log_stats"]]
    expect_match(stats, "Total Entries: 1")
    expect_match(stats, "SUCCESS: 1")
    expect_match(stats, "ERROR: 0")
  })
})

test_that("Log Level filter isolates ERROR entries the same way", {
  testServer(make_analysis_log_server(make_analysis_log_entries()), {
    session$setInputs(`analysis_log-log_level` = "ERROR", `analysis_log-log_search` = "")
    log_text <- output[["analysis_log-analysis_log"]]
    expect_match(log_text, "Missing required columns")
    expect_false(grepl("Generated ternary coordinates", log_text))
  })
})

test_that("an empty log reports zero counts across the board, not an error", {
  testServer(make_analysis_log_server(list()), {
    session$setInputs(`analysis_log-log_level` = "all", `analysis_log-log_search` = "")
    expect_equal(output[["analysis_log-log_stats"]], "No log entries")
    expect_equal(output[["analysis_log-analysis_log"]], "No activities logged yet. Start using the app to see activity history.")
  })
})
