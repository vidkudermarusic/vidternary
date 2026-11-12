# Tests for R/server_cleanliness.R and R/server_cleanliness_outputs.R: the
# Shiny wiring of the "Steel Cleanliness" tab, exercised through the real
# moduleServer("cleanliness", ...) wrapper as in R/server_logic.R.
#
# library(shiny) is required here - see test-server-evs.R for why.
library(shiny)

make_cleanliness_server <- function(log = function(...) invisible(NULL)) {
  rv <- shiny::reactiveValues()
  show_message <- function(message, type = "info") invisible(NULL)
  function(input, output, session) {
    shiny::moduleServer("cleanliness", function(input, output, session) {
      create_server_cleanliness(input, output, session, rv, show_message, log)
    })
  }
}

# Sets the inputs the browser would hold after a file is loaded (testServer
# does not apply updateSelectInput(), so the column choices are set here).
set_cleanliness_inputs <- function(session, upload = make_cleanliness_upload(), ...) {
  values <- utils::modifyList(list(
    clean_files = upload,
    clean_area_col = CLEAN_COLS$area, clean_length_col = CLEAN_COLS$length, clean_breadth_col = CLEAN_COLS$breadth,
    clean_x_col = CLEAN_COLS$x, clean_y_col = CLEAN_COLS$y, clean_direction_col = CLEAN_COLS$direction,
    clean_field_col = CLEAN_COLS$field, clean_coord_unit = "mm", clean_family_source = "eds",
    clean_standard = "ISO 4967", clean_size_breaks = "1, 2, 5, 10, 20", clean_measured = ""
  ), list(...))
  names(values) <- paste0("cleanliness-", names(values))
  do.call(session$setInputs, values)
}

clean_placeholder <- "Upload a file, check the columns, and click \"Run Cleanliness Analysis\"."

test_that("the status shows the placeholder before Run is clicked, and when it is clicked with no data", {
  testServer(make_cleanliness_server(), {
    expect_equal(output[["cleanliness-clean_status"]], clean_placeholder)
    session$setInputs(`cleanliness-clean_run` = 1)
    expect_equal(output[["cleanliness-clean_status"]], clean_placeholder)
    expect_null(output[["cleanliness-clean_notes"]])
  })
})

test_that("a run reports the rating, the notes and every table", {
  testServer(make_cleanliness_server(), {
    set_cleanliness_inputs(session)
    session$setInputs(`cleanliness-clean_run` = 1)

    status <- output[["cleanliness-clean_status"]]
    expect_match(status, "ISO 4967: 32 particles analysed (32 rows uploaded), 8 rated objects in 6 rating fields (2 complete), 1 stringers. Inspected area 2.00 mm2.", fixed = TRUE)

    notes <- output[["cleanliness-clean_notes"]]$html
    expect_match(notes, "The inspected area is 2.00 mm2", fixed = TRUE)
    expect_match(notes, "22 of 32 particles", fixed = TRUE)

    rating <- output[["cleanliness-clean_rating_table"]]
    expect_match(rating, "Worst-field index (method A)", fixed = TRUE)
    expect_match(rating, "64 um total length", fixed = TRUE)
    expect_match(rating, "4 particles", fixed = TRUE)
    expect_match(rating, "6 (incomplete)", fixed = TRUE)     # the DS particle lies in an edge field

    expect_match(output[["cleanliness-clean_rating_family_table"]], "Ti-rich", fixed = TRUE)
    expect_match(output[["cleanliness-clean_stringer_table"]], "aluminate", fixed = TRUE)
    expect_match(output[["cleanliness-clean_indices_table"]], "Area fraction (ppm)", fixed = TRUE)
    expect_match(output[["cleanliness-clean_size_table"]], "&gt;= 20", fixed = TRUE)
    expect_match(output[["cleanliness-clean_stereology_table"]], "Per mm3 (Fullman)", fixed = TRUE)
    expect_match(output[["cleanliness-clean_saltykov_table"]], "Spheres per mm3", fixed = TRUE)
    expect_match(output[["cleanliness-clean_mass_phase_table"]], "Al2O3", fixed = TRUE)
    expect_match(output[["cleanliness-clean_mass_element_table"]], "Bound in inclusions", fixed = TRUE)
  })
})

test_that("the plots render after a run", {
  testServer(make_cleanliness_server(), {
    set_cleanliness_inputs(session)
    session$setInputs(`cleanliness-clean_run` = 1)
    for (plot in c("clean_map_plot", "clean_rose_plot", "clean_size_plot", "clean_saltykov_plot")) {
      expect_false(is.null(output[[paste0("cleanliness-", plot)]]$src), info = plot)
    }
  })
})

test_that("the rose diagram asks for a direction column when none is chosen", {
  testServer(make_cleanliness_server(), {
    set_cleanliness_inputs(session, clean_direction_col = "none")
    session$setInputs(`cleanliness-clean_run` = 1)
    expect_match(output[["cleanliness-clean_status"]], "8 rated objects", fixed = TRUE)
    err <- expect_error(output[["cleanliness-clean_rose_plot"]])
    expect_match(conditionMessage(err), "Choose a direction column", fixed = TRUE)
  })
})

test_that("an invalid column is named in the status instead of a generic failure", {
  testServer(make_cleanliness_server(), {
    set_cleanliness_inputs(session, clean_breadth_col = "not_a_column")
    session$setInputs(`cleanliness-clean_run` = 1)
    err <- expect_error(output[["cleanliness-clean_status"]])
    expect_match(conditionMessage(err), "Choose a valid column for: breadth", fixed = TRUE)
    # the supplementary notes stay silent rather than repeating the error
    expect_null(output[["cleanliness-clean_notes"]])
  })
})

test_that("badly typed size classes and bulk analysis are reported in plain words", {
  testServer(make_cleanliness_server(), {
    set_cleanliness_inputs(session, clean_size_breaks = "1, two, 5")
    session$setInputs(`cleanliness-clean_run` = 1)
    err <- expect_error(output[["cleanliness-clean_status"]])
    expect_match(conditionMessage(err), "Size-class limits must be positive numbers", fixed = TRUE)
  })
  testServer(make_cleanliness_server(), {
    set_cleanliness_inputs(session, clean_measured = "O 25")
    session$setInputs(`cleanliness-clean_run` = 1)
    err <- expect_error(output[["cleanliness-clean_status"]])
    expect_match(conditionMessage(err), "Cannot read 'O 25'", fixed = TRUE)
  })
})

test_that("the settings typed in the tab reach the analysis", {
  testServer(make_cleanliness_server(), {
    set_cleanliness_inputs(session, clean_standard = "ASTM E45", clean_inspected_area = 1.9,
                           clean_measured = "S=100, O=20", clean_axis_angle = 90)
    session$setInputs(`cleanliness-clean_run` = 1)
    status <- output[["cleanliness-clean_status"]]
    expect_match(status, "^ASTM E45: ")
    expect_match(status, "0 stringers", fixed = TRUE)          # working direction turned by 90 degrees
    expect_match(status, "Inspected area 1.90 mm2", fixed = TRUE)
    expect_match(output[["cleanliness-clean_mass_element_table"]], "100.0", fixed = TRUE)
    expect_match(output[["cleanliness-clean_stringer_table"]], "No particles lie close enough", fixed = TRUE)
  })
})

test_that("the pre-analysis filter changes the particles but not the inspected area", {
  testServer(make_cleanliness_server(), {
    set_cleanliness_inputs(session)
    session$setInputs(`cleanliness-clean_filter_cols` = CLEAN_COLS$area)
    filter_id <- paste0("cleanliness-clean_filter_", gsub("[^A-Za-z0-9]", "_", CLEAN_COLS$area))
    do.call(session$setInputs, stats::setNames(list("> 1"), filter_id))
    session$setInputs(`cleanliness-clean_run` = 1)
    status <- output[["cleanliness-clean_status"]]
    expect_match(status, "10 particles analysed (32 rows uploaded), 8 rated objects in 6 rating fields", fixed = TRUE)
    expect_match(status, "Inspected area 2.00 mm2", fixed = TRUE)
  })
  testServer(make_cleanliness_server(), {
    set_cleanliness_inputs(session)
    session$setInputs(`cleanliness-clean_filter_cols` = CLEAN_COLS$area)
    filter_id <- paste0("cleanliness-clean_filter_", gsub("[^A-Za-z0-9]", "_", CLEAN_COLS$area))
    do.call(session$setInputs, stats::setNames(list("> 999999"), filter_id))
    session$setInputs(`cleanliness-clean_run` = 1)
    err <- expect_error(output[["cleanliness-clean_status"]])
    expect_match(conditionMessage(err), "No rows remain after applying the pre-analysis filter", fixed = TRUE)
  })
})

test_that("a class column of the file can be used as the chemical family", {
  d <- make_cleanliness_data()
  d$MyClass <- ifelse(d[[CLEAN_COLS$breadth]] > 1, "oxide", "speck")
  testServer(make_cleanliness_server(), {
    set_cleanliness_inputs(session, upload = make_cleanliness_upload(d),
                           clean_family_source = "column", clean_family_col = "MyClass")
    session$setInputs(`cleanliness-clean_run` = 1)
    indices <- output[["cleanliness-clean_indices_table"]]
    expect_match(indices, "oxide", fixed = TRUE)
    expect_match(indices, "speck", fixed = TRUE)
    expect_no_match(indices, "Ti-rich", fixed = TRUE)
  })
})

test_that("downloads give a clear message before a run and real files after it", {
  testServer(make_cleanliness_server(), {
    for (out in c("cleanliness-clean_download_tables", "cleanliness-clean_download_map")) {
      res <- tryCatch({ output[[out]]; list(ok = TRUE) }, error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
      expect_false(res$ok)
      expect_equal(res$msg, "Upload a file and click \"Run Cleanliness Analysis\" before downloading.")
    }
    set_cleanliness_inputs(session)
    session$setInputs(`cleanliness-clean_run` = 1)
    tables <- output[["cleanliness-clean_download_tables"]]
    expect_true(file.exists(tables))
    sheets <- openxlsx::getSheetNames(tables)
    expect_true(all(c("Rating", "Objects", "Indices", "Saltykov", "Settings") %in% sheets))
    rating <- openxlsx::read.xlsx(tables, sheet = "Rating")
    expect_equal(rating$worst_index[rating$series == "D fine"], 1)
    map <- output[["cleanliness-clean_download_map"]]
    expect_gt(file.info(map)$size, 0)
  })
})

test_that("a run is written to the analysis log", {
  logged <- list()
  testServer(make_cleanliness_server(log = function(level, message, details = NULL) {
    logged[[length(logged) + 1]] <<- list(level = level, message = message, details = details)
  }), {
    set_cleanliness_inputs(session)
    session$setInputs(`cleanliness-clean_run` = 1)
    output[["cleanliness-clean_status"]]
    expect_length(logged, 1)
    expect_equal(logged[[1]]$message, "Steel cleanliness analysis")
    expect_match(logged[[1]]$details, "ISO 4967, 32 particles, 8 rated objects, 2.00 mm2", fixed = TRUE)
  })
})

test_that("the tab is part of the app", {
  ui <- as.character(create_main_ui())
  expect_match(ui, "Steel Cleanliness", fixed = TRUE)
  expect_match(ui, "cleanliness-clean_run", fixed = TRUE)
  expect_match(ui, "cleanliness-clean_filter_cols", fixed = TRUE)
  expect_match(paste(deparse(create_server_logic), collapse = "\n"), "create_server_cleanliness", fixed = TRUE)
})

test_that("the bulk analysis text is read, and empty text means none", {
  expect_null(parse_measured_ppm(""))
  expect_null(parse_measured_ppm(NULL))
  expect_null(parse_measured_ppm("   "))
  expect_equal(parse_measured_ppm("O=25, S=10"), c(O = 25, S = 10))
})
