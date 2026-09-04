# Tests for the "Data Comparison" tab (R/server_data_comparison*.R: the
# top-level dispatcher plus its 4 sibling handler modules - upload, stats,
# multivariate, preview). Before this pass, the only coverage on this whole
# subsystem was test-pass5-regressions.R's single silent-recompute-on-
# keystroke regression check for the Mahalanobis panel; everything else -
# upload/disambiguation, the target/reference/columns selectors, the
# descriptive-stats and correlation paths (including their warning branches),
# the Isolation Forest and Comprehensive panels, and the missing/outlier
# preview - had zero coverage.
#
# None of these handlers use shiny::validate() (they report problems via
# show_message() and plain renderText()/renderPrint() text instead), so this
# file doesn't need the validate()-message-swallowing checks the Analysis
# Log/Plot Builder test files needed - but every assumption about *shape*
# below (disambiguation, selector defaults, message text) was still
# confirmed against the real module rather than guessed.
library(shiny)

make_upload <- function(d, name = "sample.xlsx") {
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  data.frame(name = name, size = file.info(path)$size, type = "",
             datapath = path, stringsAsFactors = FALSE)
}

make_comparison_data <- function(n = 20, seed = 1, cols = c("Al", "Si", "Mn")) {
  set.seed(seed)
  as.data.frame(setNames(
    lapply(cols, function(i) abs(stats::rnorm(n, 10, 2)) + 1),
    cols
  ))
}

# Returns a list(fn, get): `fn` is a show_message-shaped closure that
# appends to an internal list via `<<-`, `get()` returns that list. Kept as
# a matched pair (rather than a plain `messages <- list()` + inline closure
# per test) after an earlier version of this file built the closure *inside*
# make_data_comparison_server() from a `messages` parameter - which closes
# over that function's own local copy, not the caller's variable, so
# `<<-` silently wrote into a binding nothing else could ever read.
make_message_capture <- function() {
  messages <- list()
  list(
    fn = function(message, type = "info") messages[[length(messages) + 1]] <<- list(message = message, type = type),
    get = function() messages
  )
}

# `show_message` is passed in already-built (a closure over the caller's own
# `messages` list, via `<<-`) rather than reconstructed here from a
# `messages` argument - building it here would close over *this function's*
# local parameter, not the caller's variable, silently capturing nothing.
make_data_comparison_server <- function(show_message = function(message, type = "info") invisible(NULL)) {
  rv <- shiny::reactiveValues()
  log_operation <- function(...) invisible(NULL)
  function(input, output, session) {
    shiny::moduleServer("data_comparison", function(input, output, session) {
      create_server_data_comparison(input, output, session, rv, show_message, log_operation)
    })
  }
}

# ---- Upload & selectors ----

test_that("uploading files loads rv$comparison_data with disambiguated names and a success message naming the count", {
  cap <- make_message_capture()
  server <- make_data_comparison_server(cap$fn)
  d <- make_comparison_data()
  upload1 <- make_upload(d, "sample.xlsx")
  upload2 <- make_upload(d, "sample.xlsx")
  testServer(server, {
    session$setInputs(`data_comparison-comparison_files` = rbind(upload1, upload2))
    html <- output[["data_comparison-comparison_dataset_selector_ui"]]$html
    expect_match(html, 'value="sample"')
    expect_match(html, 'value="sample #1"')
  })
  messages <- cap$get()
  expect_equal(messages[[length(messages)]]$message, "Loaded 2 dataset(s) for comparison")
  expect_equal(messages[[length(messages)]]$type, "success")
})

test_that("an unreadable file among several loads the good ones and warns by name about the bad one", {
  cap <- make_message_capture()
  server <- make_data_comparison_server(cap$fn)
  good <- make_upload(make_comparison_data(), "good.xlsx")
  bad_path <- tempfile(fileext = ".xlsx")
  writeLines("not a real xlsx file", bad_path)
  bad <- data.frame(name = "bad.xlsx", size = file.info(bad_path)$size, type = "",
                     datapath = bad_path, stringsAsFactors = FALSE)
  suppressWarnings(testServer(server, {
    session$setInputs(`data_comparison-comparison_files` = rbind(good, bad))
    html <- output[["data_comparison-comparison_dataset_selector_ui"]]$html
    expect_match(html, 'value="good"')
    expect_false(grepl('value="bad"', html))
  }))
  msgs <- sapply(cap$get(), function(m) m$message)
  expect_true(any(grepl("^Could not read: bad\\.xlsx$", msgs)))
  expect_true(any(grepl("^Loaded 1 dataset\\(s\\)", msgs)))
})

test_that("after upload, target/reference/columns default correctly: target = first dataset, reference = Self, columns = target's numeric columns", {
  server <- make_data_comparison_server()
  upload <- make_upload(make_comparison_data(cols = c("Al", "Si")))
  testServer(server, {
    # update*Input() doesn't loop back into input$... inside a testServer()
    # session (established elsewhere in this suite) - the interceptor has to
    # be in place *before* the triggering setInputs() call, and its effect
    # is read from the captured message, not from `input` afterward.
    orig <- session$sendInputMessage
    sent <- list()
    session$sendInputMessage <- function(inputId, message) { sent[[inputId]] <<- message; orig(inputId, message) }

    session$setInputs(`data_comparison-comparison_files` = upload)
    expect_equal(sent[["comparison_mv_target"]]$value, "sample")

    # comparison_mv_reference/comparison_mv_columns are populated by
    # observers gated on input$comparison_mv_target itself - in the real
    # browser, the updateSelectInput() call just above sets that for real;
    # under testServer() it doesn't loop back (same limitation as above), so
    # the reactive chain needs a real session$setInputs() here to continue,
    # exactly as the client-side effect of that update would.
    session$setInputs(`data_comparison-comparison_mv_target` = "sample")
    expect_equal(sent[["comparison_mv_reference"]]$value, "__self__")
    # updateSelectizeInput() explicitly sets selected = character(0) (never
    # pre-select analysis columns) - options still lists the target's real
    # numeric columns.
    expect_match(sent[["comparison_mv_columns"]]$options, 'value="Al"')
    expect_match(sent[["comparison_mv_columns"]]$options, 'value="Si"')
  })
})

test_that("data_readiness_status reports the right state for 0, 1, and 2+ datasets with shared numeric columns", {
  server <- make_data_comparison_server()
  testServer(server, {
    # The 0-dataset state is what every user sees on this tab's very first
    # load, before uploading anything - its renderPrint() block used to end
    # a bare `return()` (a VISIBLE NULL) rather than `return(invisible())`,
    # which renderPrint() then explicitly printed as a literal trailing
    # "NULL" line right under the intended message (confirmed directly
    # against the real shiny::renderPrint() mechanism, and against a real
    # running instance of the app in-browser, before fixing). expect_match()
    # alone doesn't catch this - the intended text is still present either
    # way - so the exact string is checked here instead of just a substring.
    expect_equal(output[["data_comparison-data_readiness_status"]],
                 "\U0001F4CB Please upload one or more Excel files to begin.")

    upload1 <- make_upload(make_comparison_data(cols = c("Al", "Si")), "one.xlsx")
    session$setInputs(`data_comparison-comparison_files` = upload1)
    expect_match(output[["data_comparison-data_readiness_status"]], "One dataset loaded")

    upload2 <- make_upload(make_comparison_data(seed = 2, cols = c("Al", "Si")), "two.xlsx")
    session$setInputs(`data_comparison-comparison_files` = rbind(upload1, upload2))
    status <- output[["data_comparison-data_readiness_status"]]
    expect_match(status, "Ready for comparison and multivariate analysis")
    expect_match(status, "Al, Si")
  })
})

# ---- Descriptive stats & correlation ----

test_that("Compute Descriptive Statistics renders a table for one selected dataset, with no numeric columns warned about instead of crashing", {
  cap <- make_message_capture()
  server <- make_data_comparison_server(cap$fn)
  upload <- make_upload(make_comparison_data())
  testServer(server, {
    session$setInputs(`data_comparison-comparison_files` = upload)
    session$setInputs(`data_comparison-comparison_selected` = "sample")
    session$setInputs(`data_comparison-compute_stats` = 1)
    res <- tryCatch({ output[["data_comparison-descriptive_stats_output"]]; "ok" },
                     error = function(e) conditionMessage(e))
    expect_equal(res, "ok")

    text_only <- make_upload(data.frame(field = c("a", "b", "c")), "textonly.xlsx")
    session$setInputs(`data_comparison-comparison_files` = text_only)
    session$setInputs(`data_comparison-comparison_selected` = "textonly")
    session$setInputs(`data_comparison-compute_stats` = 2)
  })
  expect_true(any(grepl("^No numeric columns found in textonly$", sapply(cap$get(), `[[`, "message"))))
})

test_that("Compute Correlations: 2 datasets show the real heatmap, 3+ datasets show the 'needs exactly 2' warning but the comparison table still covers all of them", {
  cap <- make_message_capture()
  server <- make_data_comparison_server(cap$fn)
  cols <- c("Al", "Si", "Mn")
  u1 <- make_upload(make_comparison_data(seed = 1, cols = cols), "one.xlsx")
  u2 <- make_upload(make_comparison_data(seed = 2, cols = cols), "two.xlsx")
  u3 <- make_upload(make_comparison_data(seed = 3, cols = cols), "three.xlsx")
  testServer(server, {
    session$setInputs(`data_comparison-comparison_files` = rbind(u1, u2, u3))
    session$setInputs(`data_comparison-comparison_selected` = c("one", "two"))
    session$setInputs(`data_comparison-compute_correlations` = 1)
    res2 <- tryCatch({ output[["data_comparison-correlation_output"]]; "ok" }, error = function(e) conditionMessage(e))
    expect_equal(res2, "ok")

    session$setInputs(`data_comparison-comparison_selected` = c("one", "two", "three"))
    session$setInputs(`data_comparison-compute_correlations` = 2)
    res3 <- tryCatch({ output[["data_comparison-correlation_output"]]; "ok" }, error = function(e) conditionMessage(e))
    expect_equal(res3, "ok")
  })
  messages <- cap$get()
  last_warning <- messages[[length(messages)]]
  expect_equal(last_warning$type, "warning")
  expect_match(last_warning$message, "needs exactly 2 selected datasets \\(3 selected\\)")
  expect_match(last_warning$message, "still covers all 3 datasets")
})

test_that("Compute Correlations warns instead of crashing when fewer than 2 common numeric columns are shared", {
  cap <- make_message_capture()
  server <- make_data_comparison_server(cap$fn)
  u1 <- make_upload(make_comparison_data(cols = c("Al", "Si")), "one.xlsx")
  u2 <- make_upload(data.frame(Al = 1:10, OnlyHere = 11:20), "two.xlsx")
  testServer(server, {
    session$setInputs(`data_comparison-comparison_files` = rbind(u1, u2))
    session$setInputs(`data_comparison-comparison_selected` = c("one", "two"))
    session$setInputs(`data_comparison-compute_correlations` = 1)
  })
  messages <- cap$get()
  expect_equal(messages[[length(messages)]]$message, "Need at least 2 common numeric columns across the selected datasets")
})

# ---- Multivariate ----

test_that("selecting a column missing from the reference dataset shows the specific 'Missing in ...' message, not a crash, on all three panels", {
  server <- make_data_comparison_server()
  target <- make_upload(make_comparison_data(cols = c("Al", "Si", "Mn")), "target.xlsx")
  reference <- make_upload(data.frame(Al = 1:10, Si = 11:20), "reference.xlsx")
  testServer(server, {
    session$setInputs(`data_comparison-comparison_files` = rbind(target, reference))
    session$setInputs(`data_comparison-comparison_mv_target` = "target")
    session$setInputs(`data_comparison-comparison_mv_reference` = "reference")
    session$setInputs(`data_comparison-comparison_mv_columns` = c("Al", "Si", "Mn"))

    session$setInputs(`data_comparison-mahalanobis_analysis` = 1)
    expect_match(output[["data_comparison-mahalanobis_output"]], "Missing in reference: Mn")

    session$setInputs(`data_comparison-isolation_forest_analysis` = 1)
    expect_match(output[["data_comparison-isolation_forest_output"]], "Missing in reference: Mn")

    session$setInputs(`data_comparison-comparison_mv_run_comprehensive` = 1)
    expect_match(output[["data_comparison-mahalanobis_info"]], "Missing in reference: Mn")
  })
})

test_that("Isolation Forest single-method panel shows its own real fields (not Mahalanobis's), for a genuine run", {
  server <- make_data_comparison_server()
  upload <- make_upload(make_comparison_data(n = 30, cols = c("Al", "Si", "Mn")))
  testServer(server, {
    session$setInputs(`data_comparison-comparison_files` = upload)
    session$setInputs(`data_comparison-comparison_mv_target` = "sample")
    session$setInputs(`data_comparison-comparison_mv_reference` = "__self__")
    session$setInputs(`data_comparison-comparison_mv_columns` = c("Al", "Si", "Mn"))
    session$setInputs(`data_comparison-isolation_forest_analysis` = 1)
    out <- output[["data_comparison-isolation_forest_output"]]
    expect_match(out, "Analysis completed successfully")
    expect_match(out, "Total points analyzed: 30")
    expect_match(out, "Threshold value:")
    expect_match(out, "Outliers detected:")
  })
})

test_that("Comprehensive Analysis Results panel shows both Mahalanobis and Isolation Forest sections, with the right self/cross-reference interpretation", {
  server <- make_data_comparison_server()
  target <- make_upload(make_comparison_data(seed = 1, cols = c("Al", "Si", "Mn")), "target.xlsx")
  reference <- make_upload(make_comparison_data(seed = 2, cols = c("Al", "Si", "Mn")), "reference.xlsx")
  testServer(server, {
    session$setInputs(`data_comparison-comparison_files` = rbind(target, reference))
    session$setInputs(`data_comparison-comparison_mv_target` = "target")
    session$setInputs(`data_comparison-comparison_mv_columns` = c("Al", "Si", "Mn"))

    session$setInputs(`data_comparison-comparison_mv_reference` = "__self__")
    session$setInputs(`data_comparison-comparison_mv_run_comprehensive` = 1)
    self_out <- output[["data_comparison-mahalanobis_info"]]
    expect_match(self_out, "Mahalanobis Distance:")
    expect_match(self_out, "Isolation Forest:")
    expect_match(self_out, "Self-reference: points flagged as outliers stand out within target itself")

    session$setInputs(`data_comparison-comparison_mv_reference` = "reference")
    session$setInputs(`data_comparison-comparison_mv_run_comprehensive` = 2)
    cross_out <- output[["data_comparison-mahalanobis_info"]]
    # paste("...relative to", td$reference_name, "'s distribution.") inserts
    # its own space before "'s" - not a typo to "fix" here, just the real
    # rendered text this test locks down.
    expect_match(cross_out, "Cross-reference: points in target are flagged relative to reference 's distribution")
  })
})

# ---- Preview: missing/outlier summary & Excel preview ----

test_that("the missing/outlier summary and Excel preview toggle correctly, and reset when the target dataset changes", {
  server <- make_data_comparison_server()
  d <- make_comparison_data(n = 10, cols = c("Al", "Si"))
  d$Al[1:2] <- NA
  upload1 <- make_upload(d, "one.xlsx")
  upload2 <- make_upload(make_comparison_data(seed = 2, cols = c("Al", "Si")), "two.xlsx")
  testServer(server, {
    session$setInputs(`data_comparison-comparison_files` = rbind(upload1, upload2))
    session$setInputs(`data_comparison-comparison_preview_target` = "one")

    expect_null(output[["data_comparison-comparison_preview_output"]])

    session$setInputs(`data_comparison-show_missing_selected` = 1)
    missing_ui <- output[["data_comparison-comparison_preview_output"]]$html
    expect_match(missing_ui, "comparison_preview_validation")
    validation_text <- output[["data_comparison-comparison_preview_validation"]]
    expect_match(validation_text, "Missing/Outlier Summary")
    expect_match(validation_text, "Al")
    expect_match(validation_text, "Total rows: 10")

    session$setInputs(`data_comparison-show_excel_selected` = 1)
    excel_ui <- output[["data_comparison-comparison_preview_output"]]$html
    expect_match(excel_ui, "comparison_preview_excel")

    # Changing the target dataset resets the toggle back to nothing shown.
    session$setInputs(`data_comparison-comparison_preview_target` = "two")
    expect_null(output[["data_comparison-comparison_preview_output"]])
  })
})
