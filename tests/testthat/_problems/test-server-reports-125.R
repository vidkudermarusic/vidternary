# Extracted from test-server-reports.R:125

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
evs_upload <- function() make_batch_upload(make_batches()["A"])
fit_evs_tab <- function(session, upload = evs_upload(), ...) {
  values <- c(list(evs_files = upload, evs_area_col = BATCH_COLS$area, evs_group_col = "Field",
                   evs_fields_per_area = 2, evs_return_period = 50), list(...))
  names(values) <- paste0("evs-", names(values))
  do.call(session$setInputs, values)
  session$setInputs(`evs-evs_fit` = 1)
}

# test -------------------------------------------------------------------------
upload <- evs_upload()
testServer(report_evs_server(), {
    fit_evs_tab(session, upload)
    script <- readLines(output[["evs-evs_download_script"]], warn = FALSE, encoding = "UTF-8")
    expect_true(any(grepl("assign_control_areas(data[[field_col]], 2", script, fixed = TRUE)))
    expect_true(any(grepl("predict_evs_max(fit, 50)", script, fixed = TRUE)))
    script <- sub('^file <- .*$', paste0("file <- ", deparse(upload$datapath)), script)
    env <- new.env()
    eval(parse(text = script[!grepl("^library\\(|^create_gumbel_plot", script)]), envir = env)
    summary <- output[["evs-evs_summary_table"]]
    expect_match(summary, sprintf("%.2f", env$prediction$predicted), fixed = TRUE)
    expect_match(summary, sprintf("%.4f", env$fit$slope), fixed = TRUE)
  })
