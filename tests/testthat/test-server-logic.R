# Tests for R/server_logic.R's create_server_logic() - the app's top-level
# orchestrator that builds the shared rv reactiveValues and wires every
# tab's create_server_*()/register_*_handlers() factory into one shared
# reactive graph. Every other test file in this suite calls an individual
# tab's server function directly through its own hand-built
# moduleServer(...) wrapper; nothing exercised the real orchestrator itself
# before this - so a bug in create_server_logic()'s own wiring (a module
# never actually called, a wrong argument passed to one of them) could have
# gone unnoticed even with every individual tab fully covered elsewhere.
#
# Presets load from a hardcoded relative path at server-creation time (see
# R/plot_builder_presets.R) - every test here runs from an isolated temp
# working directory, matching this project's established convention.
library(shiny)

make_full_app <- function() {
  function(input, output, session) {
    rv <- create_server_logic(input, output, session)
    session$userData$rv <- rv
  }
}

test_that("create_server_logic() wires the whole app without error and initializes rv with its documented defaults", {
  old_wd <- getwd()
  tmp <- tempfile("server_logic_init"); dir.create(tmp); setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)

  testServer(make_full_app(), {
    rv <- session$userData$rv
    expect_null(rv$df1)
    expect_null(rv$df2)
    expect_equal(rv$comparison_data, list())
    expect_equal(rv$analysis_log, list())
    expect_null(rv$group_selections_1)
    expect_null(rv$group_counts_1)
    expect_false(rv$is_categorical_group_1)
    expect_false(rv$is_categorical_group_2)
    # No plot_builder_presets.json in this fresh isolated directory.
    expect_equal(rv$plot_presets, list())

    expect_equal(output[["project_status"]], "Project status: No project loaded")
  })
})

test_that("plot_presets is loaded from a real pre-existing plot_builder_presets.json at server-creation time", {
  old_wd <- getwd()
  tmp <- tempfile("server_logic_presets"); dir.create(tmp); setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)

  preset <- list(type = "scatter", x = "Al", y = "Si", color_by = "none",
                  log_x = FALSE, log_y = FALSE, percent = FALSE,
                  bar_values = NULL, rose_bin_width = NULL, hist_bins = NULL)
  save_builder_presets(list(PreExisting = preset))

  testServer(make_full_app(), {
    rv <- session$userData$rv
    expect_true("PreExisting" %in% names(rv$plot_presets))
    expect_equal(rv$plot_presets[["PreExisting"]]$x, "Al")
  })
})

test_that("a real file upload through the full orchestrator (not a hand-built module wrapper) reaches rv$df1", {
  old_wd <- getwd()
  tmp <- tempfile("server_logic_upload"); dir.create(tmp); setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)

  d <- data.frame(Al = 1:5, Si = 6:10, Mn = 11:15)
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  upload <- data.frame(name = "sample.xlsx", size = file.info(path)$size, type = "",
                        datapath = path, stringsAsFactors = FALSE)

  testServer(make_full_app(), {
    rv <- session$userData$rv
    session$setInputs(`ternary_plots-xlsx_file1` = upload)
    expect_equal(nrow(rv$df1), 5)
    expect_equal(names(rv$df1), c("Al", "Si", "Mn"))
  })
})
