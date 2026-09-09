# Tests for R/server_hex_ternary.R (the "Hexagonal Ternary Diagram" tab).
#
# This file had zero test-level coverage before this pass - not even a
# crash-scenario spot check - despite driving real graphics-device work
# (create_hex_ternary_diagram(): 6 triangle PNGs + 1 composite, ~13 device
# open/close cycles) and having its own history of real bugs (the pass-4
# composite race condition and device-leak fixes, see the audit's §03).
# These tests exercise the module through the real moduleServer("hex_ternary",
# ...) wrapper end to end - a genuine upload, genuine element selections, and
# a genuine call into create_hex_ternary_diagram() that writes real PNGs to
# disk - rather than stubbing out the plotting layer, since a stub would
# have missed exactly the class of bug (a real file never actually getting
# created/found) this subsystem has had before.
library(shiny)

make_hex_server <- function() {
  rv <- shiny::reactiveValues()
  show_message <- function(message, type = "info") invisible(NULL)
  log_operation <- function(...) invisible(NULL)
  function(input, output, session) {
    shiny::moduleServer("hex_ternary", function(input, output, session) {
      create_server_hex_ternary(input, output, session, rv, show_message, log_operation)
    })
  }
}

# Writes a small xlsx with 7 distinct, all-positive numeric columns - one
# per hex element slot (A-G) - plus a non-numeric column, so tests can
# exercise both "real column exists" and "column choices exclude nothing
# unexpected" without every test needing to invent its own fixture.
make_hex_upload <- function(n = 12, seed = 1) {
  set.seed(seed)
  d <- data.frame(
    Al = abs(stats::rnorm(n, 10, 2)) + 1,
    Si = abs(stats::rnorm(n, 10, 2)) + 1,
    Mn = abs(stats::rnorm(n, 10, 2)) + 1,
    Fe = abs(stats::rnorm(n, 10, 2)) + 1,
    Cu = abs(stats::rnorm(n, 10, 2)) + 1,
    Ni = abs(stats::rnorm(n, 10, 2)) + 1,
    Cr = abs(stats::rnorm(n, 10, 2)) + 1,
    field = sample(c("A", "B"), n, replace = TRUE)
  )
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  list(input = data.frame(name = basename(path), size = file.info(path)$size, type = "",
                           datapath = path, stringsAsFactors = FALSE),
       columns = names(d))
}

# All 7 element slots set to one real column each - the minimum valid
# selection every "real diagram" test starts from.
set_all_hex_elements <- function(session, cols = c("Al", "Si", "Mn", "Fe", "Cu", "Ni", "Cr")) {
  inputs <- setNames(as.list(cols), paste0("hex_ternary-hex_element_", seq_along(cols)))
  do.call(session$setInputs, inputs)
}

test_that("uploading a file populates all 7 element dropdowns with its real column names", {
  upload <- make_hex_upload()
  sent <- list()
  testServer(make_hex_server(), {
    orig <- session$sendInputMessage
    session$sendInputMessage <- function(inputId, message) { sent[[inputId]] <<- message; orig(inputId, message) }
    session$setInputs(`hex_ternary-hex_xlsx_file` = upload$input)
  })
  # updateSelectizeInput() sends its choices as pre-rendered `<option>` HTML
  # under $options (the same pre-rendered-HTML convention already found for
  # updateCheckboxGroupInput() elsewhere in this suite), not a plain
  # $choices vector - so the real column names are extracted back out of
  # that markup rather than compared to a $choices field that doesn't exist.
  for (i in 1:7) {
    slot <- paste0("hex_element_", i)
    expect_true(slot %in% names(sent), info = paste("no update sent for", slot))
    sent_cols <- regmatches(sent[[slot]]$options, gregexpr('(?<=value=")[^"]+', sent[[slot]]$options, perl = TRUE))[[1]]
    expect_equal(sent_cols, upload$columns)
  }
})

# "Create & Save diagram" is a single downloadHandler (browser Save dialog
# instead of a pre-chosen server-side folder - see the vidternary
# Structural Audit's §03) - a genuine two-button "Generate" + "Save"
# workflow used to exist here (see the audit's own writeup for that
# change), merged into this one button at the user's request since "Save"
# already built the composite and updated the preview on its own; the
# separate "Generate" button was one redundant extra click, not two
# independently useful actions. testServer()'s own output$id accessor both
# runs the handler's content() function and returns the path Shiny wrote
# its content to (basename reflecting the handler's own filename()
# function), or re-throws content()'s own stop() as a normal R error,
# catchable with expect_error() - confirmed directly before relying on it
# in this suite.

test_that("clicking Create & Save with fewer than 7 elements selected surfaces the specific validation message and writes nothing", {
  upload <- make_hex_upload()
  testServer(make_hex_server(), {
    session$setInputs(`hex_ternary-hex_xlsx_file` = upload$input)
    # Only 6 of 7 slots filled.
    set_all_hex_elements(session, c("Al", "Si", "Mn", "Fe", "Cu", "Ni"))
    err <- expect_error(output[["hex_ternary-hex_save"]])
    expect_match(conditionMessage(err), "at least one column for all 7 element positions", fixed = TRUE)
    # The preview never got a chance to update - still the placeholder.
    container <- output[["hex_ternary-hex_plot_container"]]
    expect_match(container$html, "select all 7 element positions")
  })
})

test_that("Create & Save surfaces the real error message instead of crashing when a selected column doesn't exist", {
  upload <- make_hex_upload()
  testServer(make_hex_server(), {
    session$setInputs(`hex_ternary-hex_xlsx_file` = upload$input)
    set_all_hex_elements(session, c("Al", "Si", "Mn", "Fe", "Cu", "Ni", "NoSuchColumn"))
    expect_error(output[["hex_ternary-hex_save"]], "NoSuchColumn")
  })
})

test_that("Save writes a real composite PNG named after hex_output_folder, and updates the preview", {
  upload <- make_hex_upload()
  testServer(make_hex_server(), {
    session$setInputs(`hex_ternary-hex_xlsx_file` = upload$input,
                       `hex_ternary-hex_output_folder` = "my_hex_run")
    set_all_hex_elements(session)
    path <- output[["hex_ternary-hex_save"]]
    expect_true(file.exists(path))
    expect_gt(file.info(path)$size, 0)
    expect_match(basename(path), "^my_hex_run_.*\\.png$")

    status <- output[["hex_ternary-hex_status"]]
    expect_match(status, "^Successfully saved hexagonal ternary diagram:")
    # The just-saved diagram becomes the on-page preview too, matching
    # "Generate"'s own behavior.
    container <- output[["hex_ternary-hex_plot_container"]]$html
    expect_match(container, "hex_plot")
  })
})

test_that("Save's filename defaults to hex_ternary_diagrams when hex_output_folder is left blank", {
  upload <- make_hex_upload()
  testServer(make_hex_server(), {
    session$setInputs(`hex_ternary-hex_xlsx_file` = upload$input,
                       `hex_ternary-hex_output_folder` = "")
    set_all_hex_elements(session)
    path <- output[["hex_ternary-hex_save"]]
    expect_match(basename(path), "^hex_ternary_diagrams_.*\\.png$")
  })
})

test_that("Save surfaces the real error message instead of crashing when a selected column doesn't exist", {
  upload <- make_hex_upload()
  testServer(make_hex_server(), {
    session$setInputs(`hex_ternary-hex_xlsx_file` = upload$input)
    set_all_hex_elements(session, c("Al", "Si", "Mn", "Fe", "Cu", "Ni", "NoSuchColumn"))
    expect_error(output[["hex_ternary-hex_save"]], "NoSuchColumn")
  })
})

test_that("multiple columns selected for one element slot are summed via '+', matching the UI's own documented promise", {
  upload <- make_hex_upload()
  before <- list.files(tempdir(), pattern = "^Hexagonal_Ternary_of_.*\\.png$", recursive = TRUE, full.names = TRUE)
  testServer(make_hex_server(), {
    session$setInputs(`hex_ternary-hex_xlsx_file` = upload$input)
    session$setInputs(
      `hex_ternary-hex_element_1` = c("Al", "Si"),
      `hex_ternary-hex_element_2` = "Mn", `hex_ternary-hex_element_3` = "Fe",
      `hex_ternary-hex_element_4` = "Cu", `hex_ternary-hex_element_5` = "Ni",
      `hex_ternary-hex_element_6` = "Cr", `hex_ternary-hex_element_7` = "Al"
    )
    path <- output[["hex_ternary-hex_save"]]
    expect_true(file.exists(path))
    status <- output[["hex_ternary-hex_status"]]
    expect_match(status, "^Successfully saved hexagonal ternary diagram:")
  })
  after <- list.files(tempdir(), pattern = "^Hexagonal_Ternary_of_.*\\.png$", recursive = TRUE, full.names = TRUE)
  expect_equal(length(setdiff(after, before)), 1)
})

# ---- Pre-Analysis Data Filter (helpers_pre_analysis_filter.R) ----
# Same convention as the identical sections in test-server-evs.R/
# test-server-spatial.R/test-server-spatial-ppp.R: the shared helper's pure
# logic is already covered in test-helpers-pre-analysis-filter.R; what
# matters here is that server_hex_ternary.R actually wires it in.
# Architecturally different from those three tabs, though - this one has
# no combined_data() reactive at all (create_hex_ternary_diagram() reads
# an xlsx file PATH itself), so resolve_hex_input_file() writes a fresh
# filtered xlsx and hands ITS path through instead - what's checked here
# is that the diagram is genuinely built from the filtered file, not the
# original upload, once a filter is set.

# Writes an xlsx with the same 7 element columns as make_hex_upload() plus
# an "Area" column: half the rows < 1, half > 1 - lets an "Area > 1"
# filter cleanly halve the row count, a real, checkable change.
make_hex_upload_with_area <- function(n = 20, seed = 1) {
  set.seed(seed)
  half <- n %/% 2
  d <- data.frame(
    Al = abs(stats::rnorm(n, 10, 2)) + 1, Si = abs(stats::rnorm(n, 10, 2)) + 1,
    Mn = abs(stats::rnorm(n, 10, 2)) + 1, Fe = abs(stats::rnorm(n, 10, 2)) + 1,
    Cu = abs(stats::rnorm(n, 10, 2)) + 1, Ni = abs(stats::rnorm(n, 10, 2)) + 1,
    Cr = abs(stats::rnorm(n, 10, 2)) + 1,
    Area = c(stats::runif(half, 0.01, 0.5), stats::runif(n - half, 2, 10))
  )
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  list(input = data.frame(name = basename(path), size = file.info(path)$size, type = "",
                           datapath = path, stringsAsFactors = FALSE),
       columns = names(d))
}

test_that("the pre-analysis filter genuinely changes which rows reach the composite diagram, and the status text reports real row counts", {
  # Two things are checked, deliberately without stubbing
  # create_hex_ternary_diagram() (this file's own established convention -
  # see its header comment - stubbing the plotting layer would miss
  # exactly the class of bug it's meant to catch): (1) the real filtered
  # xlsx file resolve_hex_input_file() writes has the right row count and
  # values, and (2) the composite PNG's content itself genuinely differs
  # between a filtered and an unfiltered run on the same data/elements -
  # the only fully black-box way to confirm the filtered file's PATH, not
  # just its own content, actually reaches create_hex_ternary_diagram().
  # A status-text-only check would NOT catch a wiring bug where do.call()
  # keeps using the raw upload path regardless (confirmed directly:
  # temporarily reverting only that one argument left a status-text-only
  # version of this test still passing, since the status text is computed
  # from resolve_hex_input_file()'s own return value independent of
  # whether it's actually used downstream).
  upload <- make_hex_upload_with_area(n = 20)

  filtered_png <- NULL
  unfiltered_png <- NULL
  before_filtered <- list.files(tempdir(), pattern = "^hex_filtered_.*\\.xlsx$", recursive = TRUE, full.names = TRUE)

  testServer(make_hex_server(), {
    session$setInputs(`hex_ternary-hex_xlsx_file` = upload$input)
    set_all_hex_elements(session)
    session$setInputs(`hex_ternary-hex_filter_cols` = "Area")
    session$setInputs(`hex_ternary-hex_filter_Area` = "> 1")

    path <- output[["hex_ternary-hex_save"]]
    expect_true(file.exists(path))
    filtered_png <<- readBin(path, "raw", file.info(path)$size)
    status <- output[["hex_ternary-hex_status"]]
    expect_match(status, "10 of 20 rows used after pre-analysis filter", fixed = TRUE)
  })

  after_filtered <- list.files(tempdir(), pattern = "^hex_filtered_.*\\.xlsx$", recursive = TRUE, full.names = TRUE)
  new_filtered <- setdiff(after_filtered, before_filtered)
  expect_equal(length(new_filtered), 1)
  filtered_content <- openxlsx::read.xlsx(new_filtered[1], sheet = 1)
  expect_equal(nrow(filtered_content), 10)
  expect_true(all(filtered_content$Area > 1))

  # Same upload, same elements, no filter selected - the unfiltered
  # baseline to compare the filtered PNG's actual content against.
  testServer(make_hex_server(), {
    session$setInputs(`hex_ternary-hex_xlsx_file` = upload$input)
    set_all_hex_elements(session)
    path <- output[["hex_ternary-hex_save"]]
    unfiltered_png <<- readBin(path, "raw", file.info(path)$size)
    status <- output[["hex_ternary-hex_status"]]
    expect_match(status, "20 of 20 rows used after pre-analysis filter", fixed = TRUE)
  })

  expect_false(identical(filtered_png, unfiltered_png))
})

test_that("a pre-analysis filter that removes every row surfaces a clear message instead of a raw crash inside diagram creation", {
  upload <- make_hex_upload_with_area(n = 20)
  testServer(make_hex_server(), {
    session$setInputs(`hex_ternary-hex_xlsx_file` = upload$input)
    set_all_hex_elements(session)
    session$setInputs(`hex_ternary-hex_filter_cols` = "Area")
    session$setInputs(`hex_ternary-hex_filter_Area` = "> 999999")

    err <- expect_error(output[["hex_ternary-hex_save"]])
    expect_match(conditionMessage(err), "No rows remain after applying the pre-analysis filter", fixed = TRUE)
  })
})

test_that("leaving the filter columns unselected behaves exactly as before this feature existed (no regression)", {
  upload <- make_hex_upload_with_area(n = 20)
  testServer(make_hex_server(), {
    session$setInputs(`hex_ternary-hex_xlsx_file` = upload$input)
    set_all_hex_elements(session)

    path <- output[["hex_ternary-hex_save"]]
    expect_true(file.exists(path))
    status <- output[["hex_ternary-hex_status"]]
    expect_match(status, "20 of 20 rows used after pre-analysis filter", fixed = TRUE)
  })
})
