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
