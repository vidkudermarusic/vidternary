# Extracted from test-server-reports.R:65

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vidternary", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(shiny)
report_clean_server <- function() {
  rv <- shiny::reactiveValues()
  function(input, output, session) {
    shiny::moduleServer("cleanliness", function(input, output, session) {
      create_server_cleanliness(input, output, session, rv, function(...) invisible(NULL), function(...) invisible(NULL))
    })
  }
}
report_evs_server <- function() {
  rv <- shiny::reactiveValues()
  function(input, output, session) {
    shiny::moduleServer("evs", function(input, output, session) {
      create_server_evs(input, output, session, rv, function(...) invisible(NULL), function(...) invisible(NULL))
    })
  }
}
run_clean <- function(session, upload, ...) {
  values <- c(list(clean_files = upload, clean_area_col = CLEAN_COLS$area, clean_length_col = CLEAN_COLS$length,
                   clean_breadth_col = CLEAN_COLS$breadth, clean_x_col = CLEAN_COLS$x, clean_y_col = CLEAN_COLS$y,
                   clean_direction_col = CLEAN_COLS$direction, clean_field_col = CLEAN_COLS$field, clean_coord_unit = "mm",
                   clean_family_source = "eds", clean_standard = "ISO 4967", clean_size_breaks = "1, 2, 5, 10, 20",
                   clean_measured = ""), list(...))
  names(values) <- paste0("cleanliness-", names(values))
  do.call(session$setInputs, values)
  session$setInputs(`cleanliness-clean_run` = 1)
}
read_all <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

# test -------------------------------------------------------------------------
upload <- make_cleanliness_upload()
testServer(report_clean_server(), {
    session$setInputs(`cleanliness-clean_filter_cols` = CLEAN_COLS$area)
    filter_id <- paste0("cleanliness-clean_filter_", gsub("[^A-Za-z0-9]", "_", CLEAN_COLS$area))
    do.call(session$setInputs, stats::setNames(list("> 1"), filter_id))
    run_clean(session, upload, clean_standard = "ASTM E45", clean_max_offset = 15, clean_aspect_limit = 2)
    expect_match(output[["cleanliness-clean_status"]], "^ASTM E45: 10 particles analysed")

    script <- readLines(output[["cleanliness-clean_download_script"]], warn = FALSE, encoding = "UTF-8")
    expect_true(any(grepl('standard = "ASTM E45"', script, fixed = TRUE)))
    expect_true(any(grepl("apply_pre_filters(all_data, list(", script, fixed = TRUE)))
    # run it against the uploaded file
    script <- sub('^file <- .*$', paste0("file <- ", deparse(upload$datapath)), script)
    env <- new.env()
    eval(parse(text = script[!grepl("^library\\(", script)]), envir = env)
    expect_equal(nrow(env$result$particles), 10)
    expect_equal(env$result$standard$name, "ASTM E45")
    tables <- openxlsx::read.xlsx(output[["cleanliness-clean_download_tables"]], sheet = "Rating")
    expect_equal(env$result$rating_summary$worst_index, tables$worst_index)
  })
