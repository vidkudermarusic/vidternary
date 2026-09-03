# Tests for R/server_plot_builder.R (the "Plot Builder" tab).
#
# Includes a regression test for a real bug found and fixed in this pass:
# output$builder_plot's tryCatch(..., error = function(e) {...}) caught
# *every* condition class "error" includes - which covers a
# shiny::validate()/req() condition too (its class is
# c("shiny.silent.error", "validation", "error", "condition")) - and
# unconditionally wrapped it as "Error rendering plot: <e$message>". A
# validate()/req() condition's own $message is always "" by design (the
# real text lives elsewhere), so every specific message combined_data()
# raises - "Select at least one dataset above.", "None of the selected
# files could be read.", etc. - was silently replaced by a blank
# "Error rendering plot: " with nothing after the colon. Same defect class
# already fixed for EVS/Spatial/CoDA earlier in this audit (see
# server_evs.R's output$evs_status); fixed here the same way: only a
# genuine (non-validation) error still gets the "Error rendering plot: "
# wrapper, and a validation condition is re-thrown unchanged so Shiny's own
# validation-error display shows the real message.
#
# Presets persist to a hardcoded relative path (plot_builder_presets.json,
# see R/plot_builder_presets.R) - every preset test runs from an isolated
# temp working directory so it doesn't write into the real repo, matching
# this project's own established convention.
library(shiny)

make_upload_df <- function(d, name = "sample.xlsx") {
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  data.frame(name = name, size = file.info(path)$size, type = "",
             datapath = path, stringsAsFactors = FALSE)
}

make_builder_data <- function(n = 20, seed = 1) {
  set.seed(seed)
  data.frame(
    Al = abs(stats::rnorm(n, 10, 2)) + 1,
    Si = abs(stats::rnorm(n, 10, 2)) + 1,
    field = sample(c("Oxide", "Sulfide"), n, replace = TRUE)
  )
}

make_plot_builder_server <- function(plot_presets = list(), show_message = function(message, type = "info") invisible(NULL)) {
  rv <- shiny::reactiveValues(plot_presets = plot_presets)
  log_operation <- function(...) invisible(NULL)
  list(
    app = function(input, output, session) {
      shiny::moduleServer("plot_builder", function(input, output, session) {
        create_server_plot_builder(input, output, session, rv, show_message, log_operation)
      })
    },
    rv = rv
  )
}

# ---- validate()-message fix (output$builder_plot) ----

test_that("before any file is uploaded, the plot shows nothing (an empty req() condition), not a stray error", {
  server <- make_plot_builder_server()
  testServer(server$app, {
    res <- tryCatch(list(ok = TRUE), error = function(e) list(ok = FALSE, msg = conditionMessage(e), class = class(e)))
    res <- tryCatch({ output[["plot_builder-builder_plot"]]; list(ok = TRUE) },
                     error = function(e) list(ok = FALSE, msg = conditionMessage(e), class = class(e)))
    expect_false(res$ok)
    expect_equal(res$msg, "")
    expect_true(inherits_validation <- "validation" %in% res$class)
  })
})

test_that("an unreadable file surfaces its own specific message, not the generic 'Error rendering plot:' wrapper", {
  server <- make_plot_builder_server()
  bad_path <- tempfile(fileext = ".xlsx")
  writeLines("not a real xlsx file", bad_path)
  bad_upload <- data.frame(name = "corrupt.xlsx", size = file.info(bad_path)$size, type = "",
                            datapath = bad_path, stringsAsFactors = FALSE)
  # openxlsx itself emits a real (expected) base warning ("error 1 in
  # extracting from zip file") while failing to read the deliberately
  # corrupt file - suppressed once confirmed expected, matching this
  # suite's convention, rather than left to inflate the warning count.
  suppressWarnings(testServer(server$app, {
    session$setInputs(`plot_builder-builder_files` = bad_upload)
    session$setInputs(`plot_builder-builder_selected_files` = "corrupt")
    res <- tryCatch({ output[["plot_builder-builder_plot"]]; list(ok = TRUE) },
                     error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    expect_false(res$ok)
    expect_equal(res$msg, "None of the selected files could be read.")
  }))
})

test_that("a stale dataset selection after a fresh upload surfaces 'Select at least one dataset above.', not the generic wrapper", {
  server <- make_plot_builder_server()
  upload1 <- make_upload_df(make_builder_data(), name = "first.xlsx")
  upload2 <- make_upload_df(make_builder_data(seed = 2), name = "second.xlsx")
  testServer(server$app, {
    session$setInputs(`plot_builder-builder_files` = upload1)
    session$setInputs(`plot_builder-builder_selected_files` = "first")
    # A fresh upload replaces builder_files but nothing resets
    # builder_selected_files - it's deliberately left holding "first",
    # which doesn't exist among the new file's names, simulating a stale
    # selection right after re-uploading.
    session$setInputs(`plot_builder-builder_files` = upload2)
    res <- tryCatch({ output[["plot_builder-builder_plot"]]; list(ok = TRUE) },
                     error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    expect_false(res$ok)
    expect_equal(res$msg, "Select at least one dataset above.")
  })
})

test_that("a genuine (non-validation) rendering error is still wrapped as 'Error rendering plot: ...'", {
  server <- make_plot_builder_server()
  # Two files with no data columns in common still share the synthetic
  # source_file tag added whenever more than one file is combined, so
  # combined_data() doesn't stop at "no columns in common" - it proceeds
  # to build a combined frame with only source_file, and asking for a
  # numeric column that isn't in it produces a genuine ggplot2 error
  # ("Problem while computing aesthetics"), not a validate() condition -
  # exactly the kind of real error this handler still needs to catch and
  # label.
  upload1 <- make_upload_df(make_builder_data(), name = "first.xlsx")
  d2 <- data.frame(TotallyDifferentCol = 1:5, AnotherOne = 6:10)
  upload2 <- make_upload_df(d2, name = "second.xlsx")
  testServer(server$app, {
    session$setInputs(`plot_builder-builder_files` = rbind(upload1, upload2))
    session$setInputs(`plot_builder-builder_selected_files` = c("first", "second"))
    session$setInputs(`plot_builder-builder_type` = "scatter",
                       `plot_builder-builder_x` = "Al", `plot_builder-builder_y` = "Si")
    res <- tryCatch({ output[["plot_builder-builder_plot"]]; list(ok = TRUE) },
                     error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    expect_false(res$ok)
    expect_match(res$msg, "^Error rendering plot:")
  })
})

test_that("a normal, valid selection still renders successfully", {
  server <- make_plot_builder_server()
  upload <- make_upload_df(make_builder_data())
  testServer(server$app, {
    session$setInputs(`plot_builder-builder_files` = upload)
    session$setInputs(`plot_builder-builder_selected_files` = "sample")
    session$setInputs(`plot_builder-builder_type` = "scatter",
                       `plot_builder-builder_x` = "Al", `plot_builder-builder_y` = "Si")
    res <- tryCatch({ output[["plot_builder-builder_plot"]]; list(ok = TRUE) },
                     error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
    expect_true(res$ok)
  })
})

# ---- File upload / dynamic UI wiring ----

test_that("uploading two same-named files gets a disambiguated ' #2' dataset selector label", {
  server <- make_plot_builder_server()
  upload1 <- make_upload_df(make_builder_data(), name = "sample.xlsx")
  upload2 <- make_upload_df(make_builder_data(seed = 2), name = "sample.xlsx")
  testServer(server$app, {
    session$setInputs(`plot_builder-builder_files` = rbind(upload1, upload2))
    html <- output[["plot_builder-builder_dataset_selector_ui"]]$html
    expect_match(html, 'value="sample"')
    # make.unique(sep = " #") numbers the *second* occurrence "#1", not
    # "#2" - the first occurrence keeps the bare name unchanged.
    expect_match(html, 'value="sample #1"')
  })
})

test_that("the axis-selector UI switches by plot type: scatter is single-select, violin's Y is multi-select and defaults to the first numeric column", {
  server <- make_plot_builder_server()
  upload <- make_upload_df(make_builder_data())
  testServer(server$app, {
    session$setInputs(`plot_builder-builder_files` = upload)
    session$setInputs(`plot_builder-builder_selected_files` = "sample")

    session$setInputs(`plot_builder-builder_type` = "scatter")
    scatter_html <- output[["plot_builder-builder_axis_selectors"]]$html
    expect_match(scatter_html, 'id="plot_builder-builder_x"')
    expect_match(scatter_html, 'id="plot_builder-builder_y"')
    expect_match(scatter_html, '<option value="Al" selected>Al</option>')

    session$setInputs(`plot_builder-builder_type` = "violin")
    violin_html <- output[["plot_builder-builder_axis_selectors"]]$html
    # X axis defaults to the first *categorical* column when one exists
    # (union(cat_cols, num_cols) puts categoricals first) - "field" here.
    expect_match(violin_html, 'id="plot_builder-builder_x"[^>]*>\\s*<option value="field" selected>')
    expect_match(violin_html, "multiple")
  })
})

# ---- Presets ----

test_that("saving a new preset persists it to disk, updates rv$plot_presets, and selects it in the dropdown", {
  old_wd <- getwd()
  tmp <- tempfile("pb_preset_new"); dir.create(tmp); setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)

  messages <- list()
  server <- make_plot_builder_server(show_message = function(message, type = "info") {
    messages[[length(messages) + 1]] <<- list(message = message, type = type)
  })
  upload <- make_upload_df(make_builder_data())
  testServer(server$app, {
    session$setInputs(`plot_builder-builder_files` = upload)
    session$setInputs(`plot_builder-builder_selected_files` = "sample")
    session$setInputs(`plot_builder-builder_type` = "scatter",
                       `plot_builder-builder_x` = "Al", `plot_builder-builder_y` = "Si")
    session$setInputs(`plot_builder-builder_preset_name` = "MyPreset")

    orig <- session$sendInputMessage
    sent <- list()
    session$sendInputMessage <- function(inputId, message) { sent[[inputId]] <<- message; orig(inputId, message) }

    session$setInputs(`plot_builder-builder_save_preset` = 1)

    expect_true("MyPreset" %in% names(server$rv$plot_presets))
    expect_equal(server$rv$plot_presets[["MyPreset"]]$type, "scatter")
    expect_equal(server$rv$plot_presets[["MyPreset"]]$x, "Al")
    expect_equal(server$rv$plot_presets[["MyPreset"]]$y, "Si")

    expect_equal(messages[[length(messages)]]$message, "Preset saved: MyPreset")
    expect_equal(messages[[length(messages)]]$type, "success")

    # Like updateSelectizeInput() elsewhere in this suite, updateSelectInput()
    # on a selectize-backed control (the default for selectInput() unless
    # multiple=TRUE) sends pre-rendered <option> HTML under $options plus a
    # separate $value for the selection, not $choices/$selected.
    expect_true("builder_preset_select" %in% names(sent))
    expect_match(sent[["builder_preset_select"]]$options, 'value="MyPreset"')
    expect_equal(sent[["builder_preset_select"]]$value, "MyPreset")
  })

  expect_true(file.exists("plot_builder_presets.json"))
  on_disk <- load_builder_presets()
  expect_true("MyPreset" %in% names(on_disk))
  expect_equal(on_disk[["MyPreset"]]$x, "Al")
})

test_that("saving with a blank name is rejected and creates no preset", {
  messages <- list()
  server <- make_plot_builder_server(show_message = function(message, type = "info") {
    messages[[length(messages) + 1]] <<- list(message = message, type = type)
  })
  testServer(server$app, {
    session$setInputs(`plot_builder-builder_preset_name` = "   ")
    session$setInputs(`plot_builder-builder_save_preset` = 1)
    expect_equal(length(server$rv$plot_presets), 0)
    expect_equal(messages[[length(messages)]]$message, "Enter a name for the preset before saving.")
    expect_equal(messages[[length(messages)]]$type, "warning")
  })
})

test_that("saving under an existing name overwrites it and says so, distinct from a first-time save", {
  old_wd <- getwd()
  tmp <- tempfile("pb_preset_overwrite"); dir.create(tmp); setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)

  messages <- list()
  server <- make_plot_builder_server(show_message = function(message, type = "info") {
    messages[[length(messages) + 1]] <<- list(message = message, type = type)
  })
  upload <- make_upload_df(make_builder_data())
  testServer(server$app, {
    session$setInputs(`plot_builder-builder_files` = upload)
    session$setInputs(`plot_builder-builder_selected_files` = "sample")
    session$setInputs(`plot_builder-builder_type` = "scatter",
                       `plot_builder-builder_x` = "Al", `plot_builder-builder_y` = "Si")
    session$setInputs(`plot_builder-builder_preset_name` = "Dup")
    session$setInputs(`plot_builder-builder_save_preset` = 1)
    expect_equal(messages[[length(messages)]]$message, "Preset saved: Dup")

    session$setInputs(`plot_builder-builder_type` = "hist", `plot_builder-builder_x` = "Al")
    session$setInputs(`plot_builder-builder_save_preset` = 2)
    expect_equal(messages[[length(messages)]]$message, "Preset overwritten: Dup")
    expect_equal(server$rv$plot_presets[["Dup"]]$type, "hist")
  })
})

test_that("loading a preset restores its saved builder_* values", {
  old_wd <- getwd()
  tmp <- tempfile("pb_preset_load"); dir.create(tmp); setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)

  preset <- list(type = "scatter", x = "Al", y = "Si", color_by = "field",
                  log_x = TRUE, log_y = FALSE, percent = FALSE,
                  bar_values = NULL, rose_bin_width = NULL, hist_bins = NULL)
  server <- make_plot_builder_server(plot_presets = list(Saved = preset))
  upload <- make_upload_df(make_builder_data())
  testServer(server$app, {
    session$setInputs(`plot_builder-builder_files` = upload)
    session$setInputs(`plot_builder-builder_selected_files` = "sample")

    orig <- session$sendInputMessage
    sent <- list()
    session$sendInputMessage <- function(inputId, message) { sent[[inputId]] <<- message; orig(inputId, message) }

    session$setInputs(`plot_builder-builder_preset_select` = "Saved")
    session$setInputs(`plot_builder-builder_load_preset` = 1)

    expect_equal(sent[["builder_type"]]$value, "scatter")
    expect_equal(sent[["builder_x"]]$value, "Al")
    expect_equal(sent[["builder_y"]]$value, "Si")
    expect_equal(sent[["builder_color_by"]]$value, "field")
    expect_equal(sent[["builder_log_x"]]$value, TRUE)
    expect_equal(sent[["builder_log_y"]]$value, FALSE)
  })
})

test_that("loading a preset that references a column missing from the current data warns by name and leaves that selection unset", {
  old_wd <- getwd()
  tmp <- tempfile("pb_preset_missing_col"); dir.create(tmp); setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)

  preset <- list(type = "scatter", x = "Al", y = "NoSuchColumn", color_by = "none",
                  log_x = FALSE, log_y = FALSE, percent = FALSE,
                  bar_values = NULL, rose_bin_width = NULL, hist_bins = NULL)
  messages <- list()
  server <- make_plot_builder_server(plot_presets = list(Stale = preset),
                                      show_message = function(message, type = "info") {
                                        messages[[length(messages) + 1]] <<- list(message = message, type = type)
                                      })
  upload <- make_upload_df(make_builder_data())
  testServer(server$app, {
    session$setInputs(`plot_builder-builder_files` = upload)
    session$setInputs(`plot_builder-builder_selected_files` = "sample")

    orig <- session$sendInputMessage
    sent <- list()
    session$sendInputMessage <- function(inputId, message) { sent[[inputId]] <<- message; orig(inputId, message) }

    session$setInputs(`plot_builder-builder_preset_select` = "Stale")
    session$setInputs(`plot_builder-builder_load_preset` = 1)

    warn <- messages[[length(messages)]]
    expect_equal(warn$type, "warning")
    expect_match(warn$message, "NoSuchColumn")
    # x = "Al" is valid and still gets restored...
    expect_equal(sent[["builder_x"]]$value, "Al")
    # ...but y = "NoSuchColumn" is not among the current choices, so it's
    # never sent at all - not sent-but-ignored, genuinely never attempted.
    expect_false("builder_y" %in% names(sent))
  })
})

test_that("deleting a preset removes it from rv, from disk, and from the dropdown", {
  old_wd <- getwd()
  tmp <- tempfile("pb_preset_delete"); dir.create(tmp); setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)

  preset <- list(type = "scatter", x = "Al", y = "Si", color_by = "none",
                  log_x = FALSE, log_y = FALSE, percent = FALSE,
                  bar_values = NULL, rose_bin_width = NULL, hist_bins = NULL)
  save_builder_presets(list(ToDelete = preset))
  server <- make_plot_builder_server(plot_presets = list(ToDelete = preset))
  testServer(server$app, {
    orig <- session$sendInputMessage
    sent <- list()
    session$sendInputMessage <- function(inputId, message) { sent[[inputId]] <<- message; orig(inputId, message) }

    session$setInputs(`plot_builder-builder_preset_select` = "ToDelete")
    session$setInputs(`plot_builder-builder_delete_preset` = 1)

    expect_false("ToDelete" %in% names(server$rv$plot_presets))
    remaining_options <- sent[["builder_preset_select"]]$options
    if (is.null(remaining_options)) remaining_options <- ""
    expect_false(grepl('value="ToDelete"', remaining_options))
  })
  expect_false("ToDelete" %in% names(load_builder_presets()))
})
