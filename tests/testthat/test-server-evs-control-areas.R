# Tests for EVS control areas of several fields (assign_control_areas()),
# the "T for the inspected area" note, and the warning when T is smaller
# than the number of inspected control areas.
library(shiny)

test_that("assign_control_areas groups numbered fields k at a time and leaves out an incomplete last group", {
  a <- assign_control_areas(1:10, 3)
  expect_equal(a$n_fields, 10)
  expect_equal(a$n_control_areas, 3)
  expect_equal(a$n_leftover_fields, 1)
  expect_equal(a$control_area, c(1, 1, 1, 2, 2, 2, 3, 3, 3, NA))
})

test_that("a missing field number counts as an inspected field with nothing detected", {
  a <- assign_control_areas(c(1, 2, 2, 5), 1)
  expect_equal(a$n_fields, 5)
  expect_equal(a$n_control_areas, 5)
  expect_equal(a$control_area, c(1, 2, 2, 5))
  expect_true(a$numeric_ids)
})

test_that("non-numeric field IDs are grouped in sorted order of the distinct values", {
  a <- assign_control_areas(c("B", "A", "D", "C", "A"), 2)
  expect_false(a$numeric_ids)
  expect_equal(a$n_fields, 4)
  expect_equal(a$control_area, c(1, 1, 2, 2, 1))
})

test_that("fields are counted on all_fields, so a filtered-out field still counts as inspected", {
  a <- assign_control_areas(c(1, 2, 3), 2, all_fields = 1:8)
  expect_equal(a$n_fields, 8)
  expect_equal(a$n_control_areas, 4)
  expect_equal(a$control_area, c(1, 1, 2))
})

test_that("an invalid fields-per-area value, or more fields per area than fields, is rejected", {
  expect_error(assign_control_areas(1:10, 0), "whole number of 1 or more")
  expect_error(assign_control_areas(1:10, 1.5), "whole number of 1 or more")
  expect_error(assign_control_areas(1:10, NA), "whole number of 1 or more")
  expect_error(assign_control_areas(1:3, 4), "fewer than the 4 fields per control area")
})

make_evs_ca_server <- function() {
  rv <- shiny::reactiveValues()
  function(input, output, session) {
    shiny::moduleServer("evs", function(input, output, session) {
      create_server_evs(input, output, session, rv, function(...) invisible(NULL), function(...) invisible(NULL))
    })
  }
}

# Value cell of the summary-table row whose label is `metric`.
summary_value <- function(tbl, metric) {
  row <- grep(metric, strsplit(tbl, "<tr>", fixed = TRUE)[[1]], fixed = TRUE, value = TRUE)[1]
  cells <- regmatches(row, gregexpr("<td[^>]*>[^<]*</td>", row))[[1]]
  trimws(gsub("<[^>]+>", "", cells))[2]
}

# `n_fields` numbered fields, `rows_per_field` inclusions each.
make_evs_numbered_upload <- function(n_fields = 16, rows_per_field = 5, seed = 7) {
  set.seed(seed)
  d <- data.frame(field = rep(seq_len(n_fields), each = rows_per_field),
                  area = stats::rlnorm(n_fields * rows_per_field, meanlog = 1, sdlog = 1))
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  data.frame(name = basename(path), size = file.info(path)$size, type = "",
             datapath = path, stringsAsFactors = FALSE)
}

test_that("fields per control area changes the block maxima the fit uses", {
  testServer(make_evs_ca_server(), {
    upload <- make_evs_numbered_upload(n_fields = 16)
    session$setInputs(`evs-evs_files` = upload)
    session$setInputs(`evs-evs_area_col` = "area", `evs-evs_group_col` = "field",
                      `evs-evs_fields_per_area` = 4)
    session$setInputs(`evs-evs_fit` = 1)

    expect_match(output[["evs-evs_status"]], "^Fit successful: n = 4 control areas \\(4 fields each\\)")

    d <- openxlsx::read.xlsx(upload$datapath, sheet = 1)
    expected <- sort(as.numeric(sqrt(tapply(d$area, ceiling(d$field / 4), max))))
    bm <- openxlsx::read.xlsx(output[["evs-evs_download_table"]])
    expect_equal(sort(bm$sqrt_area_max), expected)

    tbl <- output[["evs-evs_summary_table"]]
    expect_match(tbl, "Fields inspected")
    expect_match(tbl, "Control areas inspected \\(T for the inspected area\\)")
    expect_false(grepl("Fields left out", tbl))
  })
})

test_that("an incomplete last control area is reported in the summary table", {
  testServer(make_evs_ca_server(), {
    session$setInputs(`evs-evs_files` = make_evs_numbered_upload(n_fields = 14))
    session$setInputs(`evs-evs_area_col` = "area", `evs-evs_group_col` = "field",
                      `evs-evs_fields_per_area` = 4)
    session$setInputs(`evs-evs_fit` = 1)
    tbl <- output[["evs-evs_summary_table"]]
    expect_equal(summary_value(tbl, "Control areas inspected"), "3")
    expect_equal(summary_value(tbl, "Fields left out (incomplete last control area)"), "2")
  })
})

test_that("the note gives T for the inspected area, and warns only when T is smaller", {
  testServer(make_evs_ca_server(), {
    upload <- make_evs_numbered_upload(n_fields = 16)
    session$setInputs(`evs-evs_files` = upload)
    session$setInputs(`evs-evs_area_col` = "area", `evs-evs_group_col` = "field",
                      `evs-evs_fields_per_area` = 2, `evs-evs_return_period` = 100)
    session$setInputs(`evs-evs_fit` = 1)

    note <- output[["evs-evs_return_period_note"]]$html
    expect_match(note, "You inspected 8 control areas (16 fields, 2 per control area). T = 8 predicts", fixed = TRUE)
    expect_false(grepl("is smaller than", note))

    session$setInputs(`evs-evs_return_period` = 5)
    note <- output[["evs-evs_return_period_note"]]$html
    expect_match(note, "T = 5 is smaller than the 8 control areas you inspected.", fixed = TRUE)
    expect_match(note, sprintf("sqrtArea = %.2f um", max(sqrt(openxlsx::read.xlsx(upload$datapath, sheet = 1)$area))), fixed = TRUE)

    session$setInputs(`evs-evs_return_period` = 8)
    expect_false(grepl("is smaller than", output[["evs-evs_return_period_note"]]$html))
  })
})

test_that("the note is empty before a fit and an invalid fields-per-area value gives a clear message", {
  testServer(make_evs_ca_server(), {
    expect_null(output[["evs-evs_return_period_note"]])
    session$setInputs(`evs-evs_files` = make_evs_numbered_upload(n_fields = 16))
    session$setInputs(`evs-evs_area_col` = "area", `evs-evs_group_col` = "field",
                      `evs-evs_fields_per_area` = 0)
    session$setInputs(`evs-evs_fit` = 1)
    err <- expect_error(output[["evs-evs_status"]])
    expect_match(conditionMessage(err), "whole number of 1 or more", fixed = TRUE)
  })
})

test_that("a pre-analysis filter that empties a field does not shrink the inspected area", {
  testServer(make_evs_ca_server(), {
    session$setInputs(`evs-evs_files` = make_evs_numbered_upload(n_fields = 16))
    session$setInputs(`evs-evs_area_col` = "area", `evs-evs_group_col` = "field",
                      `evs-evs_filter_cols` = "field", `evs-evs_filter_field` = "<= 12")
    session$setInputs(`evs-evs_fit` = 1)
    tbl <- output[["evs-evs_summary_table"]]
    expect_equal(summary_value(tbl, "Control areas in the fit (n)"), "12")
    expect_equal(summary_value(tbl, "Control areas inspected"), "16")
  })
})
