# Tests for R/server_ternary_plots_batch.R ("Multiple Ternary Creator" - a
# genuinely separate tab/module from "Ternary Plots", see that file's own
# header comment). Before this pass, the only coverage here was
# test-pass5-regressions.R's crash-scenario check for
# extract_ternary_params(multiple_mode = TRUE); ordinary upload/column-
# choice population, the dynamic filter UI, and a real Create & Save
# round-trip (success, mixed, and total-failure outcomes) were all untested.
#
# Includes a regression test for a real bug found and fixed in this pass:
# the final status message unconditionally let an "Errors encountered"
# block overwrite whatever the success branch had just set - so a batch
# where some files succeeded and others failed showed *only* the error
# text, with zero indication that some plots had actually been saved (even
# though the files genuinely existed). Same defect class, and same fix, as
# server_ternary_plots.R's "Save Both Plots" handler earlier in this audit.
#
# "Create & Save" is a downloadHandler now (one zip download instead of a
# subfolder under a pre-chosen server-side Output Directory - see the
# vidternary Structural Audit's §03). testServer()'s own output$id
# accessor both runs the handler's content() function and returns the path
# Shiny wrote its content to (basename reflecting the handler's own
# filename() function - confirmed directly before relying on it here), or
# re-throws content()'s own stop() as a normal R error, catchable with
# expect_error() - so no session$setInputs(...) click simulation is needed
# for the save itself, and per-file output is verified via zip::zip_list()
# rather than scanning a real output folder.
library(shiny)

make_upload_multi <- function(dfs, names) {
  paths <- vapply(dfs, function(d) {
    p <- tempfile(fileext = ".xlsx")
    openxlsx::write.xlsx(d, p)
    p
  }, character(1))
  data.frame(name = names, size = file.info(paths)$size, type = "",
             datapath = paths, stringsAsFactors = FALSE)
}

make_ternary_data <- function(n = 20, seed = 1, cols = c("Al", "Si", "Mn")) {
  set.seed(seed)
  as.data.frame(setNames(lapply(cols, function(i) abs(stats::rnorm(n, 10, 2)) + 1), cols))
}

make_batch_server <- function() {
  rv <- shiny::reactiveValues()
  show_message <- function(message, type = "info") invisible(NULL)
  log_operation <- function(...) invisible(NULL)
  function(input, output, session) {
    shiny::moduleServer("multiple_ternary", function(input, output, session) {
      register_ternary_plots_batch_handlers(input, output, session, rv, show_message, log_operation)
    })
  }
}

test_that("uploading multiple files populates Element A/B/C and Optional Parameter choices from the first file's columns", {
  server <- make_batch_server()
  upload <- make_upload_multi(list(make_ternary_data(cols = c("Al", "Si", "Mn")),
                                    make_ternary_data(cols = c("Fe", "Cu"))),
                               c("first.xlsx", "second.xlsx"))
  testServer(server, {
    orig <- session$sendInputMessage
    sent <- list()
    session$sendInputMessage <- function(inputId, message) { sent[[inputId]] <<- message; orig(inputId, message) }
    session$setInputs(`multiple_ternary-multiple_xlsx_files` = upload)

    for (id in c("multiple_element_A", "multiple_element_B", "multiple_element_C")) {
      expect_match(sent[[id]]$options, 'value="Al"')
      expect_match(sent[[id]]$options, 'value="Mn"')
      expect_false(grepl('value="Fe"', sent[[id]]$options))
    }
  })
})

test_that("selecting two columns for Element A renders a filter input for each", {
  server <- make_batch_server()
  testServer(server, {
    session$setInputs(`multiple_ternary-multiple_element_A` = c("Al", "Si"))
    html <- output[["multiple_ternary-multiple_filters_A"]]$html
    expect_match(html, "Filter for Al")
    expect_match(html, "Filter for Si")
    expect_match(html, 'id="multiple_ternary-multiple_filter_A_Al"')
    expect_match(html, 'id="multiple_ternary-multiple_filter_A_Si"')
  })
})

test_that("Create & Save with an incomplete element selection errors clearly instead of silently saving nothing", {
  server <- make_batch_server()
  upload <- make_upload_multi(list(make_ternary_data()), "one.xlsx")
  testServer(server, {
    session$setInputs(`multiple_ternary-multiple_xlsx_files` = upload)
    session$setInputs(`multiple_ternary-multiple_element_A` = "Al", `multiple_ternary-multiple_element_B` = "Si")
    # Element C left unset.
    expect_error(output[["multiple_ternary-create_save_multiple_ternary"]], "elements A, B, and C")
    # content() errors before ever touching multiple_ternary_status, so it
    # stays at its untouched baseline text.
    expect_equal(output[["multiple_ternary-multiple_ternary_status"]],
                 "No multiple ternary plots created yet. Click 'Create & Save all ternary plots (zip)' to start.")
  })
})

test_that("a genuine all-success batch reports the full-success message and zips one real PNG per file", {
  server <- make_batch_server()
  upload <- make_upload_multi(list(make_ternary_data(seed = 1), make_ternary_data(seed = 2), make_ternary_data(seed = 3)),
                               c("one.xlsx", "two.xlsx", "three.xlsx"))
  suppressWarnings(testServer(server, {
    session$setInputs(`multiple_ternary-multiple_xlsx_files` = upload)
    session$setInputs(`multiple_ternary-multiple_element_A` = "Al", `multiple_ternary-multiple_element_B` = "Si",
                       `multiple_ternary-multiple_element_C` = "Mn")
    path <- output[["multiple_ternary-create_save_multiple_ternary"]]
    expect_true(file.exists(path))
    entries <- zip::zip_list(path)$filename
    expect_length(entries[grepl("\\.png$", entries)], 3)
    expect_false("errors.txt" %in% entries)

    status <- output[["multiple_ternary-multiple_ternary_status"]]
    expect_match(status, "^Successfully saved 3 ternary plots")
  }))
})

test_that("a mixed batch (some succeed, some fail) reports BOTH the save count and the errors - the fixed bug", {
  server <- make_batch_server()
  good <- make_ternary_data(seed = 1, cols = c("Al", "Si", "Mn"))
  bad <- make_ternary_data(seed = 2, cols = c("Al", "Si"))  # missing Mn - will fail
  upload <- make_upload_multi(list(good, bad), c("good.xlsx", "bad.xlsx"))
  suppressWarnings(testServer(server, {
    session$setInputs(`multiple_ternary-multiple_xlsx_files` = upload)
    session$setInputs(`multiple_ternary-multiple_element_A` = "Al", `multiple_ternary-multiple_element_B` = "Si",
                       `multiple_ternary-multiple_element_C` = "Mn")
    path <- output[["multiple_ternary-create_save_multiple_ternary"]]
    expect_true(file.exists(path))
    entries <- zip::zip_list(path)$filename
    expect_length(entries[grepl("\\.png$", entries)], 1)
    expect_true("errors.txt" %in% entries)

    status <- output[["multiple_ternary-multiple_ternary_status"]]
    # Before the fix, this would show ONLY the error text, with the "Saved 1"
    # part silently discarded even though the file genuinely got saved.
    expect_match(status, "^Saved 1 ternary plot")
    expect_match(status, "Errors encountered:")
    expect_match(status, "bad\\.xlsx")
  }))
})

test_that("a total-failure batch errors and reports the plain error message, matching the original wording", {
  server <- make_batch_server()
  bad1 <- make_ternary_data(seed = 1, cols = c("Al", "Si"))
  bad2 <- make_ternary_data(seed = 2, cols = c("Al", "Si"))
  upload <- make_upload_multi(list(bad1, bad2), c("bad1.xlsx", "bad2.xlsx"))
  testServer(server, {
    session$setInputs(`multiple_ternary-multiple_xlsx_files` = upload)
    session$setInputs(`multiple_ternary-multiple_element_A` = "Al", `multiple_ternary-multiple_element_B` = "Si",
                       `multiple_ternary-multiple_element_C` = "Mn")
    expect_error(output[["multiple_ternary-create_save_multiple_ternary"]], "Failed to save any ternary plots")

    status <- output[["multiple_ternary-multiple_ternary_status"]]
    expect_match(status, "^Error saving multiple ternary plots: Errors encountered:")
    expect_match(status, "bad1\\.xlsx")
    expect_match(status, "bad2\\.xlsx")
  })
})

test_that("Create & Save's zip filename uses the custom name field, and a sensible default when blank", {
  server <- make_batch_server()
  upload <- make_upload_multi(list(make_ternary_data()), "one.xlsx")
  suppressWarnings(testServer(server, {
    session$setInputs(`multiple_ternary-multiple_xlsx_files` = upload,
                       `multiple_ternary-multiple_output_folder` = "my_batch_run")
    session$setInputs(`multiple_ternary-multiple_element_A` = "Al", `multiple_ternary-multiple_element_B` = "Si",
                       `multiple_ternary-multiple_element_C` = "Mn")
    path <- output[["multiple_ternary-create_save_multiple_ternary"]]
    expect_match(basename(path), "^my_batch_run_.*\\.zip$")

    session$setInputs(`multiple_ternary-multiple_output_folder` = "")
    path2 <- output[["multiple_ternary-create_save_multiple_ternary"]]
    expect_match(basename(path2), "^multiple_ternary_plots_.*\\.zip$")
  }))
})
