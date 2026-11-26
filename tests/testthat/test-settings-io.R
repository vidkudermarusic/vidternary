# Tests for R/settings_io.R: saving and loading the settings of a tab.
library(shiny)

test_that("settings are the plain input values, without uploads and buttons", {
  values <- list(
    evs_area_col = "Area", evs_return_period = 380, evs_filter_cols = c("Area", "Fe"), evs_flag = TRUE,
    evs_files = data.frame(name = "a.xlsx", datapath = "x"),
    evs_fit = structure(3L, class = c("shinyActionButtonValue", "integer")),
    evs_nothing_selected = NULL, evs_secret = "x"
  )
  s <- collect_settings(values, exclude = "secret")
  expect_equal(names(s), c("evs_area_col", "evs_filter_cols", "evs_flag", "evs_nothing_selected", "evs_return_period"))
  expect_equal(s$evs_return_period, 380)
  expect_equal(s$evs_filter_cols, c("Area", "Fe"))
  # an empty selection is kept as empty, so loading clears it again
  expect_equal(s$evs_nothing_selected, character(0))
})

test_that("settings survive being written and read back, vectors and empty selections included", {
  s <- list(a_col = "Area.(sq..um)", a_number = 0.1 + 0.2, a_flag = FALSE, many = c("x", "y"), one_of_many = "x",
            none = character(0), empty_number = NA_real_)
  path <- tempfile(fileext = ".json")
  write_settings_file(s, "evs", path)
  back <- read_settings_file(path, "evs")
  expect_equal(back$tab, "evs")
  expect_match(back$saved, "^[0-9]{4}-[0-9]{2}-[0-9]{2} ")
  expect_equal(back$settings$a_col, "Area.(sq..um)")
  expect_equal(back$settings$a_number, 0.1 + 0.2, tolerance = 1e-14)   # 15 significant digits
  expect_false(back$settings$a_flag)
  expect_equal(back$settings$many, c("x", "y"))
  expect_equal(back$settings$one_of_many, "x")
  expect_equal(back$settings$none, character(0))
  # an empty number box is written as null and left alone on loading
  expect_false("empty_number" %in% names(back$settings))
})

test_that("a file from another tab, or not a settings file, is refused", {
  path <- tempfile(fileext = ".json")
  write_settings_file(list(a = 1), "evs", path)
  expect_error(read_settings_file(path, "cleanliness"), "saved from the 'evs' tab and cannot be loaded into 'cleanliness'")
  expect_equal(read_settings_file(path)$tab, "evs")            # no tab given: not checked
  bad <- tempfile(fileext = ".json"); writeLines("{not json", bad)
  expect_error(read_settings_file(bad), "not valid JSON")
  other <- tempfile(fileext = ".json"); writeLines('{"kind": "import profile"}', other)
  expect_error(read_settings_file(other), "not a vidternary settings file")
})

# A module with the settings buttons; `record$sent` collects what would be
# set in the browser.
settings_server <- function(record) {
  function(input, output, session) {
    register_settings_io(input, output, session, tab = "demo", prefix = "demo",
                         send = function(id, value) record$sent[[length(record$sent) + 1]] <- list(id = id, value = value))
  }
}
as_upload <- function(path) data.frame(name = basename(path), size = 1, type = "", datapath = path, stringsAsFactors = FALSE)

test_that("saving writes the current settings of the tab", {
  testServer(settings_server(new.env()), {
    session$setInputs(demo_area_col = "Area", demo_threshold = 2.5, demo_filter_cols = c("Area", "Fe"),
                      demo_files = data.frame(name = "a.xlsx", datapath = "x"), demo_run = 1)
    back <- read_settings_file(output$demo_settings_save, "demo")
    expect_equal(back$settings, list(demo_area_col = "Area", demo_filter_cols = c("Area", "Fe"), demo_threshold = 2.5))
  })
})

# A module whose `send` is recorded in `record$sent`, with short waits.
recording_server <- function(record) {
  record$sent <- list()
  function(input, output, session) {
    io <- register_settings_io(input, output, session, tab = "demo", prefix = "demo", retry_ms = 100,
                               send = function(id, value) record$sent[[length(record$sent) + 1]] <- list(id = id, value = value))
    output$waiting <- renderText(paste(sort(names(io$pending())), collapse = ","))
  }
}
sent_ids <- function(record, from = 1) {
  if (length(record$sent) < from) return(character(0))
  vapply(record$sent[from:length(record$sent)], function(s) s$id, character(1))
}

test_that("loading sets every input, and sends again what has not arrived", {
  path <- tempfile(fileext = ".json")
  write_settings_file(list(demo_area_col = "Area", demo_filter_cols = "Area", demo_filter_Area = "> 1",
                           demo_threshold = 8, demo_cleared = character(0), demo_run = 5), "demo", path)
  record <- new.env()
  testServer(recording_server(record), {
    session$setInputs(demo_area_col = "Other", demo_threshold = 2)
    session$setInputs(demo_settings_load = as_upload(path))
    # everything is sent at once - but not the button
    expect_setequal(sent_ids(record), c("demo_area_col", "demo_filter_cols", "demo_filter_Area", "demo_threshold", "demo_cleared"))
    values <- stats::setNames(lapply(record$sent, function(s) s$value), sent_ids(record))
    expect_equal(values$demo_area_col, "Area")
    expect_equal(values$demo_cleared, character(0))
    expect_match(output$demo_settings_status, "Loaded 5 settings saved on ")

    # the browser applies what is on the page; the filter box is not there yet
    session$setInputs(demo_area_col = "Area", demo_threshold = 8L, demo_filter_cols = "Area")
    n <- length(record$sent)
    session$elapse(100)
    expect_equal(sent_ids(record, n + 1), "demo_filter_Area")     # only what has not arrived is sent again
    expect_equal(record$sent[[n + 1]]$value, "> 1")

    # now the box exists and takes the value: nothing more is sent
    session$setInputs(demo_filter_Area = "> 1")
    n <- length(record$sent)
    session$elapse(100)
    session$elapse(100)
    expect_length(record$sent, n)
    expect_equal(output$waiting, "")
    expect_no_match(output$demo_settings_status, "could not be set")
  })
})

test_that("a value that never arrives is given up on and named", {
  path <- tempfile(fileext = ".json")
  write_settings_file(list(demo_area_col = "Area", demo_missing_col = "Not in this file"), "demo", path)
  record <- new.env()
  testServer(recording_server(record), {
    session$setInputs(demo_area_col = "Area")
    session$setInputs(demo_settings_load = as_upload(path))
    for (i in 1:5) session$elapse(100)
    # sent three times in all, then no more
    expect_equal(sum(sent_ids(record) == "demo_missing_col"), 3)
    expect_equal(sum(sent_ids(record) == "demo_area_col"), 1)
    expect_equal(output$waiting, "")
    expect_match(output$demo_settings_status,
                 "1 could not be set, because the input is not shown or does not offer that value: demo_missing_col.", fixed = TRUE)
  })
})

test_that("loading a second file starts afresh", {
  first <- tempfile(fileext = ".json"); write_settings_file(list(demo_missing_col = "x"), "demo", first)
  second <- tempfile(fileext = ".json"); write_settings_file(list(demo_area_col = "Area"), "demo", second)
  record <- new.env()
  testServer(recording_server(record), {
    session$setInputs(demo_area_col = "Area")
    session$setInputs(demo_settings_load = as_upload(first))
    for (i in 1:5) session$elapse(100)
    expect_match(output$demo_settings_status, "could not be set")
    session$setInputs(demo_settings_load = as_upload(second))
    session$elapse(100)
    expect_match(output$demo_settings_status, "^Loaded 1 settings saved on [-0-9: ]+[.]$")
  })
})

test_that("a settings file of another tab is reported, and nothing is set", {
  path <- tempfile(fileext = ".json")
  write_settings_file(list(evs_area_col = "Area"), "evs", path)
  record <- new.env()
  record$sent <- list()
  testServer(settings_server(record), {
    session$setInputs(demo_settings_load = as_upload(path))
    expect_match(output$demo_settings_status, "Could not load the settings: These settings were saved from the 'evs' tab")
    expect_length(record$sent, 0)
  })
})

test_that("the settings buttons are part of the analysis tabs", {
  ui <- as.character(create_main_ui())
  for (id in c("evs-evs", "cleanliness-clean", "spatial-spatial", "spatial_ppp-ppp", "coda-coda", "batch-batch")) {
    expect_match(ui, paste0(id, "_settings_save"), fixed = TRUE)
    expect_match(ui, paste0(id, "_settings_load"), fixed = TRUE)
  }
})
