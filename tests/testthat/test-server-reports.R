# Tests for the report and script downloads of the Steel Cleanliness and
# Extreme Value Analysis tabs (R/server_cleanliness_outputs.R,
# R/server_evs.R). The cleanliness helpers come from
# test-server-cleanliness.R / helper-cleanliness.R; they are repeated here
# where needed so this file also runs on its own.
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
  values <- utils::modifyList(
    list(clean_files = upload, clean_area_col = CLEAN_COLS$area, clean_length_col = CLEAN_COLS$length,
         clean_breadth_col = CLEAN_COLS$breadth, clean_x_col = CLEAN_COLS$x, clean_y_col = CLEAN_COLS$y,
         clean_direction_col = CLEAN_COLS$direction, clean_field_col = CLEAN_COLS$field, clean_coord_unit = "mm",
         clean_family_source = "eds", clean_standard = "ISO 4967", clean_size_breaks = "1, 2, 5, 10, 20",
         clean_measured = ""), list(...))
  names(values) <- paste0("cleanliness-", names(values))
  do.call(session$setInputs, values)
  session$setInputs(`cleanliness-clean_run` = 1)
}
read_all <- function(path) paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

test_that("report and script of the cleanliness tab need a run first", {
  testServer(report_clean_server(), {
    for (out in c("clean_download_report", "clean_download_script")) {
      res <- tryCatch({ output[[paste0("cleanliness-", out)]]; "ok" }, error = function(e) conditionMessage(e))
      expect_equal(res, "Upload a file and click \"Run Cleanliness Analysis\" before downloading.", info = out)
    }
  })
})

test_that("the cleanliness report names the uploaded file and carries the result", {
  upload <- make_cleanliness_upload()
  upload$name <- "heat_4711.xlsx"
  testServer(report_clean_server(), {
    run_clean(session, upload)
    html <- read_all(output[["cleanliness-clean_download_report"]])
    expect_match(html, "<h1>Steel cleanliness report</h1>", fixed = TRUE)
    expect_match(html, "heat_4711.xlsx", fixed = TRUE)
    expect_match(html, unname(tools::md5sum(upload$datapath)), fixed = TRUE)
    expect_match(html, "64 um total length", fixed = TRUE)
    expect_match(html, 'file &lt;- "heat_4711.xlsx"', fixed = TRUE)
  })
})

test_that("the downloaded cleanliness script repeats the run of the tab, filter included", {
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
    script[grepl("^file <- ", script)] <- paste0("file <- ", deparse(upload$datapath))
    env <- new.env()
    eval(parse(text = script[!grepl("^library\\(", script)]), envir = env)
    expect_equal(nrow(env$result$particles), 10)
    expect_equal(env$result$standard$name, "ASTM E45")
    tables <- openxlsx::read.xlsx(output[["cleanliness-clean_download_tables"]], sheet = "Rating")
    expect_equal(env$result$rating_summary$worst_index, tables$worst_index)
  })
})

evs_upload <- function() make_batch_upload(make_batches()["A"])
fit_evs_tab <- function(session, upload = evs_upload(), ...) {
  values <- utils::modifyList(list(evs_files = upload, evs_area_col = BATCH_COLS$area, evs_group_col = "Field",
                                   evs_fields_per_area = 2, evs_return_period = 50), list(...))
  names(values) <- paste0("evs-", names(values))
  do.call(session$setInputs, values)
  session$setInputs(`evs-evs_fit` = 1)
}

test_that("report and script of the EVS tab need a fit first", {
  testServer(report_evs_server(), {
    for (out in c("evs_download_report", "evs_download_script")) {
      res <- tryCatch({ output[[paste0("evs-", out)]]; "ok" }, error = function(e) conditionMessage(e))
      expect_equal(res, "Upload data, choose the area and grouping columns, and click \"Fit Extreme Value Model\" before downloading.", info = out)
    }
  })
})

test_that("the EVS report carries the fit, and the fatigue limit once a hardness is entered", {
  testServer(report_evs_server(), {
    fit_evs_tab(session)
    plain <- read_all(output[["evs-evs_download_report"]])
    expect_match(plain, "<h1>Extreme value analysis report</h1>", fixed = TRUE)
    expect_match(plain, "10 control areas, return level", fixed = TRUE)
    expect_match(plain, "A.xlsx", fixed = TRUE)
    expect_no_match(plain, "Fatigue limit estimate", fixed = TRUE)

    session$setInputs(`evs-evs_hv` = 250, `evs-evs_incl_location` = "internal")
    with_fatigue <- read_all(output[["evs-evs_download_report"]])
    expect_match(with_fatigue, "<h2>Fatigue limit estimate (Murakami)</h2>", fixed = TRUE)
    expect_match(with_fatigue, 'location = "internal"', fixed = TRUE)
  })
})

test_that("the downloaded EVS script repeats the fit of the tab", {
  upload <- evs_upload()
  testServer(report_evs_server(), {
    fit_evs_tab(session, upload)
    script <- readLines(output[["evs-evs_download_script"]], warn = FALSE, encoding = "UTF-8")
    expect_true(any(grepl("assign_control_areas(data[[field_col]], 2", script, fixed = TRUE)))
    expect_true(any(grepl("predict_evs_max(fit, 50)", script, fixed = TRUE)))
    script[grepl("^file <- ", script)] <- paste0("file <- ", deparse(upload$datapath))
    env <- new.env()
    eval(parse(text = script[!grepl("^library\\(|^create_gumbel_plot", script)]), envir = env)
    summary <- output[["evs-evs_summary_table"]]
    expect_match(summary, sprintf("%.2f", env$prediction$predicted), fixed = TRUE)
    expect_match(summary, sprintf("%.4f", env$fit$slope), fixed = TRUE)
  })
})

test_that("an EVS report without a valid return period says what is missing", {
  testServer(report_evs_server(), {
    fit_evs_tab(session, evs_return_period = 1)
    res <- tryCatch({ output[["evs-evs_download_report"]]; "ok" }, error = function(e) conditionMessage(e))
    expect_match(res, "Set a return period T greater than 1", fixed = TRUE)
  })
})
