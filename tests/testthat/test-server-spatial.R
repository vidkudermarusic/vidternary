# Tests for R/server_spatial.R's download handlers - that they surface a
# clear message instead of an uncaught blank error when clicked before
# "Analyze Spatial Pattern" is ever pressed.
#
# All 3 downloadButtons in ui_spatial_tab.R (scatter plot, NND histogram,
# NND values table) are clickable at any time - none sit inside a
# conditionalPanel - and each handler called result(), an eventReactive
# gated on input$spatial_analyze, with no error handling of its own.
# Before that button is clicked, result() throws a
# shiny::validate()/req() condition whose $message is always "" by design
# (the real text lives elsewhere - see output$spatial_status's own comment
# for the full explanation of that mechanism). Same defect class, same
# fix, as server_plot_builder.R's output$builder_download.
#
# library(shiny) is required here (unlike the package's other test files):
# vidternary's NAMESPACE doesn't import shiny (dependencies are attached at
# app-launch time via dependencies.R's initialize_packages(), which only
# runs from R/app.R), and server_spatial.R's reactive/render bodies call
# shiny functions unqualified.
library(shiny)

make_spatial_server <- function() {
  rv <- shiny::reactiveValues()
  show_message <- function(message, type = "info") invisible(NULL)
  log_operation <- function(...) invisible(NULL)
  function(input, output, session) {
    shiny::moduleServer("spatial", function(input, output, session) {
      create_server_spatial(input, output, session, rv, show_message, log_operation)
    })
  }
}

# Writes a small xlsx with StageX/StageY coordinate columns, returning the
# input$spatial_files-shaped data.frame Shiny's fileInput produces.
make_spatial_upload <- function(n = 30, seed = 1) {
  set.seed(seed)
  d <- data.frame(StageX = stats::runif(n, 0, 1000), StageY = stats::runif(n, 0, 1000))
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  data.frame(name = basename(path), size = file.info(path)$size, type = "",
             datapath = path, stringsAsFactors = FALSE)
}

test_that("clicking any download before Analyze Spatial Pattern is ever clicked gives a clear message, not an uncaught blank error", {
  testServer(make_spatial_server(), {
    for (out in c("spatial-spatial_download_scatter", "spatial-spatial_download_histogram",
                   "spatial-spatial_download_table")) {
      res <- tryCatch({ output[[out]]; list(ok = TRUE) },
                       error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
      expect_false(res$ok)
      expect_equal(res$msg, "Upload data, choose X/Y coordinate columns, and click \"Analyze Spatial Pattern\" before downloading.")
    }
  })
})

test_that("a normal, successful analysis still downloads real plots and a real values table", {
  testServer(make_spatial_server(), {
    session$setInputs(`spatial-spatial_files` = make_spatial_upload())
    session$setInputs(`spatial-spatial_x_col` = "StageX")
    session$setInputs(`spatial-spatial_y_col` = "StageY")
    session$setInputs(`spatial-spatial_analyze` = 1)

    for (out in c("spatial-spatial_download_scatter", "spatial-spatial_download_histogram",
                   "spatial-spatial_download_table")) {
      path <- output[[out]]
      expect_true(file.exists(path))
      expect_gt(file.info(path)$size, 0)
    }
  })
})

# ---- Pre-Analysis Data Filter (helpers_pre_analysis_filter.R) ----
# Same convention as test-server-evs.R's own filter tests: the shared
# helper's pure logic is already covered in
# test-helpers-pre-analysis-filter.R; what matters here is that
# server_spatial.R actually wires it in - filtered_data() feeds result()
# (not the raw combined_data()), and the reported row counts are real.

# Writes an xlsx with a "Size" column: half the rows < 1, half > 1 -
# lets a "Size > 1" filter cleanly halve the point count, a real,
# checkable change rather than a coincidence.
make_spatial_upload_with_size <- function(n = 40, seed = 1) {
  set.seed(seed)
  half <- n %/% 2
  d <- data.frame(
    StageX = stats::runif(n, 0, 1000), StageY = stats::runif(n, 0, 1000),
    Size = c(stats::runif(half, 0.01, 0.5), stats::runif(n - half, 2, 10))
  )
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  data.frame(name = basename(path), size = file.info(path)$size, type = "",
             datapath = path, stringsAsFactors = FALSE)
}

test_that("the pre-analysis filter genuinely changes which points reach the Clark-Evans test", {
  testServer(make_spatial_server(), {
    session$setInputs(`spatial-spatial_files` = make_spatial_upload_with_size(n = 40))
    session$setInputs(`spatial-spatial_x_col` = "StageX")
    session$setInputs(`spatial-spatial_y_col` = "StageY")
    session$setInputs(`spatial-spatial_filter_cols` = "Size")
    session$setInputs(`spatial-spatial_filter_Size` = "> 1")
    session$setInputs(`spatial-spatial_analyze` = 1)

    status <- output[["spatial-spatial_status"]]
    expect_match(status, "^n = 20 points")  # only the "large" half survives

    tbl <- output[["spatial-spatial_summary_table"]]
    expect_match(tbl, "Rows before pre-analysis filter")
    expect_match(tbl, "40")
    expect_match(tbl, "Rows after pre-analysis filter")
    expect_match(tbl, "20")
  })
})

test_that("a pre-analysis filter that removes every point surfaces a clear message instead of crashing downstream", {
  testServer(make_spatial_server(), {
    session$setInputs(`spatial-spatial_files` = make_spatial_upload_with_size(n = 40))
    session$setInputs(`spatial-spatial_x_col` = "StageX")
    session$setInputs(`spatial-spatial_y_col` = "StageY")
    session$setInputs(`spatial-spatial_filter_cols` = "Size")
    session$setInputs(`spatial-spatial_filter_Size` = "> 999999")
    session$setInputs(`spatial-spatial_analyze` = 1)

    err <- expect_error(output[["spatial-spatial_status"]])
    expect_match(conditionMessage(err), "No rows remain after applying the pre-analysis filter", fixed = TRUE)
  })
})

test_that("leaving the filter columns unselected behaves exactly as before this feature existed (no regression)", {
  testServer(make_spatial_server(), {
    session$setInputs(`spatial-spatial_files` = make_spatial_upload())
    session$setInputs(`spatial-spatial_x_col` = "StageX")
    session$setInputs(`spatial-spatial_y_col` = "StageY")
    session$setInputs(`spatial-spatial_analyze` = 1)

    status <- output[["spatial-spatial_status"]]
    expect_match(status, "^n = 30 points")  # make_spatial_upload()'s default n
  })
})
