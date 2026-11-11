# Tests for R/server_coda.R's download handlers - that they surface a
# clear message instead of an uncaught blank error when clicked before
# "Transform & Run PCA" is ever pressed.
#
# All 4 downloadButtons in ui_coda_tab.R (CLR/ILR data, CLR/ILR biplots)
# are clickable at any time - none sit inside a conditionalPanel - and each
# handler called result(), an eventReactive gated on input$coda_run, with
# no error handling of its own. Before that button is clicked, result()
# throws a shiny::validate()/req() condition whose $message is always ""
# by design (the real text lives elsewhere - see output$coda_status's own
# comment for the full explanation of that mechanism). Same defect class,
# same fix, as server_plot_builder.R's output$builder_download.
#
# library(shiny) is required here (unlike the package's other test files):
# vidternary's NAMESPACE doesn't import shiny (dependencies are attached at
# app-launch time via dependencies.R's initialize_packages(), which only
# runs from R/app.R), and server_coda.R's reactive/render bodies call shiny
# functions unqualified.
library(shiny)

make_coda_server <- function() {
  rv <- shiny::reactiveValues()
  show_message <- function(message, type = "info") invisible(NULL)
  log_operation <- function(...) invisible(NULL)
  function(input, output, session) {
    shiny::moduleServer("coda", function(input, output, session) {
      create_server_coda(input, output, session, rv, show_message, log_operation)
    })
  }
}

# Writes a small xlsx with 3 numeric ("compositional") columns, returning
# the input$coda_files-shaped data.frame Shiny's fileInput produces.
make_coda_upload <- function(n = 20, seed = 1) {
  set.seed(seed)
  d <- data.frame(
    Al = abs(stats::rnorm(n, 10, 2)) + 1,
    Si = abs(stats::rnorm(n, 10, 2)) + 1,
    Mn = abs(stats::rnorm(n, 10, 2)) + 1
  )
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  data.frame(name = basename(path), size = file.info(path)$size, type = "",
             datapath = path, stringsAsFactors = FALSE)
}

test_that("clicking any download before Transform & Run PCA is ever clicked gives a clear message, not an uncaught blank error", {
  testServer(make_coda_server(), {
    for (out in c("coda-coda_download_clr", "coda-coda_download_ilr",
                   "coda-coda_download_biplot", "coda-coda_download_biplot_ilr")) {
      res <- tryCatch({ output[[out]]; list(ok = TRUE) },
                       error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
      expect_false(res$ok)
      expect_equal(res$msg, "Select at least 3 compositional parts and click \"Transform & Run PCA\" before downloading.")
    }
  })
})

test_that("a normal, successful run still downloads real CLR/ILR data and biplots", {
  testServer(make_coda_server(), {
    session$setInputs(`coda-coda_files` = make_coda_upload())
    session$setInputs(`coda-coda_parts` = c("Al", "Si", "Mn"))
    session$setInputs(`coda-coda_run` = 1)

    for (out in c("coda-coda_download_clr", "coda-coda_download_ilr",
                   "coda-coda_download_biplot", "coda-coda_download_biplot_ilr")) {
      path <- output[[out]]
      expect_true(file.exists(path))
      expect_gt(file.info(path)$size, 0)
    }
  })
})

test_that("a run with zero-heavy parts shows the zero warning and the per-part zero table", {
  set.seed(5)
  n <- 60
  d <- data.frame(`Fe (Wt%)` = runif(n, 50, 70), `Cr (Wt%)` = runif(n, 10, 20), `Ni (Wt%)` = runif(n, 5, 10),
                  `Ca (Wt%)` = runif(n, 1, 3), check.names = FALSE)
  d[["Ca (Wt%)"]][seq_len(30)] <- 0
  path <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(d, path)
  up <- data.frame(name = basename(path), size = file.info(path)$size, type = "", datapath = path, stringsAsFactors = FALSE)
  testServer(make_coda_server(), {
    session$setInputs(`coda-coda_files` = up)
    session$setInputs(`coda-coda_parts` = c("Fe.(Wt%)", "Cr.(Wt%)", "Ni.(Wt%)", "Ca.(Wt%)"))
    session$setInputs(`coda-coda_run` = 1)
    warn <- output[["coda-coda_zero_warning"]]$html
    expect_match(warn, "have more than 10% zeros: Ca (50%)", fixed = TRUE)
    tbl <- output[["coda-coda_zero_table"]]
    expect_match(tbl, "Ca.(Wt%)", fixed = TRUE)
    expect_match(tbl, "50.0", fixed = TRUE)
  })
})
