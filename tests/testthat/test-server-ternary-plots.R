# Tests for the "Ternary Plots" tab's core server files - R/server_ternary_
# plots.R, R/server_ternary_plots_groups.R, R/server_file_handlers.R - all
# three registered from the same moduleServer("ternary_plots", ...) wrapper
# in server_logic.R. ("Multiple Ternary Creator", server_ternary_plots_
# batch.R, is a genuinely separate tab/module and is not covered here.)
#
# Before this pass, coverage on this whole subsystem was narrow and
# crash-shaped: test-pass5-regressions.R has real end-to-end checks for a
# handful of specific proven bugs (the multi-column && crash, "Select All"'s
# checkbox values, negative point-size clipping, the filter-parser
# consolidation) but nothing exercising the ordinary, non-crashing behavior
# of file upload, the mutual-exclusivity checkboxes, the dynamic per-element
# filter UI, a real Save Plot 1/2/Both round-trip, or Copy Settings - and
# several bugs fixed earlier this audit (Save Both Plots' independent
# tryCatch, Copy Settings copying filters/palette, the sibling-file group-
# state reset) were verified by hand at the time but never locked into a
# permanent regression test.
#
# Real files are written to isolated temp working/output directories -
# never the real repo - matching this project's own established convention.
library(shiny)

make_upload <- function(d, name = "sample.xlsx") {
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  data.frame(name = name, size = file.info(path)$size, type = "",
             datapath = path, stringsAsFactors = FALSE)
}

make_ternary_data <- function(n = 20, seed = 1, extra_cols = list()) {
  set.seed(seed)
  d <- data.frame(
    Al = abs(stats::rnorm(n, 10, 2)) + 1,
    Si = abs(stats::rnorm(n, 10, 2)) + 1,
    Mn = abs(stats::rnorm(n, 10, 2)) + 1
  )
  for (nm in names(extra_cols)) d[[nm]] <- extra_cols[[nm]]
  d
}

make_ternary_plots_server <- function(show_message = function(message, type = "info") invisible(NULL)) {
  rv <- shiny::reactiveValues()
  log_operation <- function(...) invisible(NULL)
  list(
    app = function(input, output, session) {
      shiny::moduleServer("ternary_plots", function(input, output, session) {
        create_server_file_handlers(input, output, session, rv, show_message, log_operation)
        create_server_ternary_plots(input, output, session, rv, show_message, log_operation)
      })
    },
    rv = rv
  )
}

make_message_capture <- function() {
  messages <- list()
  list(
    fn = function(message, type = "info") messages[[length(messages) + 1]] <<- list(message = message, type = type),
    get = function() messages
  )
}

# ---- File upload (server_file_handlers.R) ----

test_that("uploading Dataset 1 populates rv$df1, the element choices, and shows a success message", {
  cap <- make_message_capture()
  server <- make_ternary_plots_server(show_message = cap$fn)
  upload <- make_upload(make_ternary_data())
  testServer(server$app, {
    orig <- session$sendInputMessage
    sent <- list()
    session$sendInputMessage <- function(inputId, message) { sent[[inputId]] <<- message; orig(inputId, message) }

    session$setInputs(`ternary_plots-xlsx_file1` = upload)

    expect_equal(nrow(server$rv$df1), 20)
    expect_match(sent[["element_A1"]]$options, 'value="Al"')
    expect_match(sent[["element_B1"]]$options, 'value="Si"')
  })
  messages <- cap$get()
  expect_true(any(sapply(messages, function(m) m$message == "Dataset 1 loaded successfully!" && m$type == "success")))
})

test_that("re-uploading Dataset 1 resets stale categorical-group state from the previous file (sibling-regression fix)", {
  server <- make_ternary_plots_server()
  d1 <- make_ternary_data(seed = 1, extra_cols = list(Shape = sample(c("Oxide", "Sulfide"), 20, replace = TRUE)))
  d2 <- make_ternary_data(seed = 2, extra_cols = list(Shape = sample(c("Round", "Angular"), 20, replace = TRUE)))
  testServer(server$app, {
    session$setInputs(`ternary_plots-xlsx_file1` = make_upload(d1, "first.xlsx"))
    session$setInputs(`ternary_plots-optional_param2_1` = "Shape")
    expect_true(server$rv$is_categorical_group_1)
    expect_true(!is.null(server$rv$group_counts_1))
    expect_true(all(c("Oxide", "Sulfide") %in% names(server$rv$group_counts_1)))

    # A fresh upload must clear that state immediately, not just eventually
    # via the optional_param2_1 reset observer re-firing.
    session$setInputs(`ternary_plots-xlsx_file1` = make_upload(d2, "second.xlsx"))
    expect_null(server$rv$group_counts_1)
    expect_null(server$rv$group_selections_1)
    expect_false(server$rv$is_categorical_group_1)
  })
})

test_that("a text column is only detected as categorical up to 50 distinct values, not unconditionally", {
  # Before this fix, is.character(column_data) alone granted unconditional
  # categorical status regardless of cardinality - a per-row text
  # identifier (e.g. a Sample_ID column) got treated as categorical the
  # same as a real 2-3-value grouping column, producing an unbounded
  # group-selection checklist/legend. Character and factor are never
  # is.numeric() in R, so the fixed detection (!is.numeric() && count <=
  # 50) still catches both - just with the same cap every other
  # non-numeric type already had.
  d_under_cap <- make_ternary_data(seed = 3, extra_cols = list(Label = paste0("id_", 1:20)))
  d_over_cap <- make_ternary_data(seed = 4, n = 60, extra_cols = list(Label = paste0("id_", 1:60)))

  server1 <- make_ternary_plots_server()
  testServer(server1$app, {
    session$setInputs(`ternary_plots-xlsx_file1` = make_upload(d_under_cap))
    session$setInputs(`ternary_plots-optional_param2_1` = "Label")
    expect_true(server1$rv$is_categorical_group_1)
  })

  server2 <- make_ternary_plots_server()
  testServer(server2$app, {
    session$setInputs(`ternary_plots-xlsx_file1` = make_upload(d_over_cap))
    session$setInputs(`ternary_plots-optional_param2_1` = "Label")
    expect_false(server2$rv$is_categorical_group_1)
  })
})

test_that("Copy Settings copies element selections, per-element filters, the color palette, and multivariate checkboxes to Dataset 2", {
  server <- make_ternary_plots_server()
  d <- make_ternary_data()
  testServer(server$app, {
    session$setInputs(`ternary_plots-xlsx_file1` = make_upload(d, "one.xlsx"))
    session$setInputs(`ternary_plots-xlsx_file2` = make_upload(d, "two.xlsx"))
    session$setInputs(
      `ternary_plots-element_A1` = "Al", `ternary_plots-element_B1` = "Si", `ternary_plots-element_C1` = "Mn",
      `ternary_plots-color_palette1` = "viridis",
      `ternary_plots-use_iqr_filter` = TRUE
    )
    session$setInputs(`ternary_plots-filter_A1_Al` = "> 5")

    orig <- session$sendInputMessage
    sent <- list()
    session$sendInputMessage <- function(inputId, message) { sent[[inputId]] <<- message; orig(inputId, message) }

    session$setInputs(`ternary_plots-copy_settings` = 1)

    expect_equal(sent[["element_A2"]]$value, "Al")
    expect_equal(sent[["element_B2"]]$value, "Si")
    expect_equal(sent[["element_C2"]]$value, "Mn")
    expect_equal(sent[["color_palette2"]]$value, "viridis")
    expect_equal(sent[["use_iqr_filter"]]$value, TRUE)
    expect_equal(sent[["filter_A2_Al"]]$value, "> 5")
  })
})

# ---- Categorical group selection (server_ternary_plots_groups.R) ----

test_that("picking a categorical column as Optional Parameter 2 renders the group-selection UI with real counts", {
  server <- make_ternary_plots_server()
  d <- make_ternary_data(extra_cols = list(Shape = c(rep("Oxide", 14), rep("Sulfide", 6))))
  testServer(server$app, {
    session$setInputs(`ternary_plots-xlsx_file1` = make_upload(d))
    session$setInputs(`ternary_plots-optional_param2_1` = "Shape")

    ui_html <- output[["ternary_plots-group_selection_ui_1"]]$html
    expect_match(ui_html, "Select Groups to Display")

    expect_equal(as.numeric(server$rv$group_counts_1[["Oxide"]]), 14)
    expect_equal(as.numeric(server$rv$group_counts_1[["Sulfide"]]), 6)
  })
})

test_that("group_count_1 reports 'Showing X of Y groups' as the checklist selection changes", {
  server <- make_ternary_plots_server()
  d <- make_ternary_data(extra_cols = list(Shape = c(rep("Oxide", 14), rep("Sulfide", 6))))
  testServer(server$app, {
    session$setInputs(`ternary_plots-xlsx_file1` = make_upload(d))
    session$setInputs(`ternary_plots-optional_param2_1` = "Shape")
    session$setInputs(`ternary_plots-selected_groups_1` = "Oxide (14 samples)")
    expect_equal(output[["ternary_plots-group_count_1"]], "Showing 1 of 2 groups")
  })
})

# ---- Mutual exclusivity & dynamic filter UI (server_ternary_plots.R) ----

test_that("checking one filter method unchecks any other that was already checked", {
  server <- make_ternary_plots_server()
  testServer(server$app, {
    session$setInputs(`ternary_plots-use_iqr_filter` = TRUE)

    orig <- session$sendInputMessage
    sent <- list()
    session$sendInputMessage <- function(inputId, message) { sent[[inputId]] <<- message; orig(inputId, message) }

    session$setInputs(`ternary_plots-use_mahalanobis` = TRUE)
    expect_equal(sent[["use_iqr_filter"]]$value, FALSE)
  })
})

test_that("selecting two columns for Element A renders two per-element filter inputs, one per column", {
  server <- make_ternary_plots_server()
  testServer(server$app, {
    session$setInputs(`ternary_plots-element_A1` = c("Al", "Si"))
    html <- output[["ternary_plots-dynamic_filters_A1"]]$html
    expect_match(html, "Filter for Al")
    expect_match(html, "Filter for Si")
    expect_match(html, 'id="ternary_plots-filter_A1_Al"')
    expect_match(html, 'id="ternary_plots-filter_A1_Si"')
  })
})

# ---- Save Plot 1/2/Both (server_ternary_plots.R) ----
# Each is a downloadHandler now (browser Save dialog instead of a
# pre-chosen server-side folder - see the vidternary Structural Audit's
# §03). testServer()'s own output$id accessor both runs the handler's
# content() function and returns the path Shiny wrote its content to -
# confirmed directly against a toy downloadHandler before relying on it
# here: reading output$id executes content() and hands back a real file
# path (or re-throws content()'s own stop() as a normal R error, catchable
# with expect_error()), so no session$setInputs(...) click simulation is
# needed - reading the output IS the click.

test_that("Save Plot 1 writes a real file and reports its location; a genuine error is surfaced instead of a crash", {
  server <- make_ternary_plots_server()
  d <- make_ternary_data()
  # The real plot-notes text includes "->"/"<-" style arrows rendered as
  # actual Unicode glyphs; this Windows R session's console encoding can't
  # represent them and emits a real but harmless mbcsToSbcs substitution
  # warning on every such save - suppressed once confirmed expected, same
  # convention as elsewhere in this suite, rather than left to inflate the
  # warning count.
  suppressWarnings(testServer(server$app, {
    session$setInputs(`ternary_plots-xlsx_file1` = make_upload(d),
                       `ternary_plots-element_A1` = "Al", `ternary_plots-element_B1` = "Si", `ternary_plots-element_C1` = "Mn")
    path <- output[["ternary_plots-plot1"]]
    expect_true(file.exists(path))
    expect_gt(file.info(path)$size, 0)
    status <- output[["ternary_plots-status"]]
    expect_match(status, "Plot 1 saved successfully")

    session$setInputs(`ternary_plots-element_A1` = "NoSuchColumn")
    expect_error(output[["ternary_plots-plot1"]], "missing")
    status2 <- output[["ternary_plots-status"]]
    expect_match(status2, "^Error saving Plot 1:")
  }))
})

test_that("Save Both Plots: when Plot 1 succeeds and Plot 2 genuinely errors, the zip still contains Plot 1 plus an errors.txt naming Plot 2's failure", {
  server <- make_ternary_plots_server()
  d1 <- make_ternary_data(seed = 1)
  d2 <- make_ternary_data(seed = 2)
  suppressWarnings(testServer(server$app, {
    session$setInputs(`ternary_plots-xlsx_file1` = make_upload(d1, "one.xlsx"),
                       `ternary_plots-element_A1` = "Al", `ternary_plots-element_B1` = "Si", `ternary_plots-element_C1` = "Mn")
    session$setInputs(`ternary_plots-xlsx_file2` = make_upload(d2, "two.xlsx"),
                       `ternary_plots-element_A2` = "Al", `ternary_plots-element_B2` = "Si", `ternary_plots-element_C2` = "NoSuchColumn")

    path <- output[["ternary_plots-plot_both"]]
    expect_true(file.exists(path))
    entries <- zip::zip_list(path)$filename
    expect_length(entries[grepl("\\.png$", entries)], 1)
    expect_true("errors.txt" %in% entries)

    status <- output[["ternary_plots-status"]]
    expect_match(status, "One plot saved, one failed")
    expect_match(status, "Plot 2:")
  }))
})

test_that("Save Both Plots: a genuine success on both zips exactly two files with no errors.txt", {
  server <- make_ternary_plots_server()
  d1 <- make_ternary_data(seed = 1)
  d2 <- make_ternary_data(seed = 2)
  suppressWarnings(testServer(server$app, {
    session$setInputs(`ternary_plots-xlsx_file1` = make_upload(d1, "one.xlsx"),
                       `ternary_plots-element_A1` = "Al", `ternary_plots-element_B1` = "Si", `ternary_plots-element_C1` = "Mn")
    session$setInputs(`ternary_plots-xlsx_file2` = make_upload(d2, "two.xlsx"),
                       `ternary_plots-element_A2` = "Al", `ternary_plots-element_B2` = "Si", `ternary_plots-element_C2` = "Mn")

    path <- output[["ternary_plots-plot_both"]]
    expect_true(file.exists(path))
    entries <- zip::zip_list(path)$filename
    expect_length(entries[grepl("\\.png$", entries)], 2)
    expect_false("errors.txt" %in% entries)

    status <- output[["ternary_plots-status"]]
    expect_match(status, "Both plots saved successfully")
  }))
})

# ---- Analysis report ----

test_that("the analysis report only renders once at least one filter method is active, matching its own req() gate", {
  server <- make_ternary_plots_server()
  d <- make_ternary_data()
  testServer(server$app, {
    session$setInputs(`ternary_plots-xlsx_file1` = make_upload(d),
                       `ternary_plots-element_A1` = "Al", `ternary_plots-element_B1` = "Si", `ternary_plots-element_C1` = "Mn")
    # req(a || b || c || d || e) needs every operand to already be a real
    # TRUE/FALSE, not NULL - true in the live app (every checkboxInput's
    # first client render always sends its initial boolean, never nothing
    # at all) but not automatically true here, where an input testServer
    # never explicitly sets stays NULL; set every method checkbox to FALSE
    # first to match that real initial state rather than an untested one.
    session$setInputs(`ternary_plots-use_mahalanobis` = FALSE, `ternary_plots-use_isolation_forest` = FALSE,
                       `ternary_plots-use_iqr_filter` = FALSE, `ternary_plots-use_zscore_filter` = FALSE,
                       `ternary_plots-use_mad_filter` = FALSE)
    res <- tryCatch({ output[["ternary_plots-analysis_report"]]; list(ok = TRUE) },
                     error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    expect_false(res$ok)
    expect_equal(res$msg, "")

    # outlier_mode_iqr is a radioButtons() with a real selected = FALSE
    # default in the live app (ui_ternary_plots_tab.R) - same testServer
    # gap as the checkboxes above, so it needs setting explicitly too.
    session$setInputs(`ternary_plots-use_iqr_filter` = TRUE, `ternary_plots-outlier_mode_iqr` = FALSE)
    report <- output[["ternary_plots-analysis_report"]]
    expect_match(report, "ANALYSIS METHODS REPORT")
    expect_match(report, "Element A: Al")
    expect_match(report, "IQR FILTER")
  })
})
