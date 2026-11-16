# Tests for the Shiny wiring of the "Measurement Planning" tab
# (R/server_planning.R), the "Folder Summary" tab (R/server_folder.R) and
# the acceptance limits of the "Steel Cleanliness" tab
# (R/server_cleanliness.R, R/acceptance_inputs.R).
#
# library(shiny) is required here - see test-server-evs.R for why.
library(shiny)

module_server <- function(id, create, log = function(...) invisible(NULL)) {
  rv <- shiny::reactiveValues()
  function(input, output, session) {
    shiny::moduleServer(id, function(input, output, session) {
      create(input, output, session, rv, function(...) invisible(NULL), log)
    })
  }
}
set_inputs <- function(session, id, values) {
  names(values) <- paste0(id, "-", names(values))
  do.call(session$setInputs, values)
}

# ---- Measurement Planning ----

plan_inputs <- function(upload, ...) utils::modifyList(list(
  plan_files = upload, plan_area_col = BATCH_COLS$area, plan_field_col = BATCH_COLS$field, plan_x_col = BATCH_COLS$x,
  plan_y_col = BATCH_COLS$y, plan_coord_unit = "mm", plan_inspected_area = "", plan_precision = 10, plan_ratio = 1.2,
  plan_power = 80, plan_fields_per_area = 1, plan_return_period = 50), list(...))
plan_placeholder <- "Upload the pilot run, check the columns, and click \"Make the Plan\"."

test_that("planning: placeholder first, then the plan of the pilot run", {
  testServer(module_server("planning", create_server_planning), {
    expect_equal(output[["planning-plan_status"]], plan_placeholder)
    set_inputs(session, "planning", plan_inputs(make_batch_upload(make_batches()["A"])))
    expect_equal(output[["planning-plan_file_info"]], "A: 200 rows")
    session$setInputs(`planning-plan_run` = 1)
    expect_match(output[["planning-plan_status"]], "^200 particles on 2.00 mm2: 100.0 per mm2, known to \\+/- [0-9.]+ % now. For \\+/- 10 %: ")
    expect_match(output[["planning-plan_pilot_table"]], "Index of dispersion (counts per field)", fixed = TRUE)
    expect_match(output[["planning-plan_precision_table"]], "your target", fixed = TRUE)
    expect_match(output[["planning-plan_ratio_table"]], "Area per batch (mm2)", fixed = TRUE)
    expect_match(output[["planning-plan_evs_table"]], "Control areas", fixed = TRUE)
    expect_match(output[["planning-plan_repeat_density_table"]], "two or more runs", fixed = TRUE)
    expect_match(output[["planning-plan_notes"]]$html, "this specimen and section only", fixed = TRUE)
    expect_true(nzchar(output[["planning-plan_precision_plot"]]$src))
    sheets <- openxlsx::getSheetNames(output[["planning-plan_download_tables"]])
    expect_true(all(c("Pilot", "Area for precision", "Area for ratio") %in% sheets))
  })
})

test_that("planning: several files are repeats, and the targets are used", {
  logged <- list()
  testServer(module_server("planning", create_server_planning, function(...) logged[[length(logged) + 1]] <<- list(...)), {
    set_inputs(session, "planning", plan_inputs(make_batch_upload(make_batches()[c("A", "B")]), plan_precision = 4, plan_ratio = 1.5))
    session$setInputs(`planning-plan_run` = 1)
    expect_match(output[["planning-plan_status"]], "400 particles on 4.00 mm2", fixed = TRUE)
    expect_match(output[["planning-plan_status"]], "For +/- 4 %", fixed = TRUE)
    expect_match(output[["planning-plan_runs_table"]], "<td> A </td>", fixed = TRUE)
    expect_match(output[["planning-plan_repeat_density_table"]], "Repeatability limit", fixed = TRUE)
    expect_match(output[["planning-plan_repeat_other_table"]], "Median ECD (um)", fixed = TRUE)
    expect_equal(logged[[1]][[2]], "Measurement plan")
  })
})

test_that("planning: bad input shows a message instead of a result", {
  testServer(module_server("planning", create_server_planning), {
    set_inputs(session, "planning", plan_inputs(make_batch_upload(make_batches()["A"]), plan_ratio = 1))
    session$setInputs(`planning-plan_run` = 1)
    expect_error(output[["planning-plan_pilot_table"]], "other than 1")
    set_inputs(session, "planning", list(plan_ratio = 1.2, plan_inspected_area = "abc"))
    session$setInputs(`planning-plan_run` = 2)
    expect_error(output[["planning-plan_pilot_table"]], "Inspected areas must be positive numbers")
  })
})

# ---- Folder Summary ----

# The helper specimen twice (the second without its 15 um particle) and a
# file without the columns, as uploaded files.
make_folder_upload <- function() {
  full <- make_cleanliness_data()
  tables <- list(first = full, second = full[full[[CLEAN_COLS$length]] < 15 | full[[CLEAN_COLS$breadth]] < 15, ],
                 broken = data.frame(Something = 1:3))
  dir <- tempfile("folder_"); dir.create(dir)
  paths <- vapply(names(tables), function(name) {
    path <- file.path(dir, paste0(name, ".xlsx"))
    openxlsx::write.xlsx(tables[[name]], path)
    path
  }, character(1))
  writeLines("no table here", file.path(dir, "notes.xls"))
  paths <- c(paths, notes = file.path(dir, "notes.xls"))
  data.frame(name = basename(paths), size = 1, type = "", datapath = unname(paths), stringsAsFactors = FALSE)
}
folder_inputs <- function(upload, ...) utils::modifyList(list(
  folder_files = upload, folder_area_col = CLEAN_COLS$area, folder_length_col = CLEAN_COLS$length,
  folder_breadth_col = CLEAN_COLS$breadth, folder_x_col = CLEAN_COLS$x, folder_y_col = CLEAN_COLS$y,
  folder_direction_col = CLEAN_COLS$direction, folder_field_col = CLEAN_COLS$field, folder_coord_unit = "mm",
  folder_standard = "ISO 4967", folder_beam_kv = 15, folder_acc_ratings = ""), list(...))
folder_placeholder <- "Select the files, check the columns, and click \"Analyse All Files\"."

test_that("folder: files are listed, unreadable ones named", {
  testServer(module_server("folder", create_server_folder), {
    expect_equal(output[["folder-folder_status"]], folder_placeholder)
    session$setInputs(`folder-folder_files` = make_folder_upload())
    info <- output[["folder-folder_file_info"]]
    expect_match(info, "3 files read, 66 rows in all.", fixed = TRUE)
    expect_match(info, "Not read: notes.xls (", fixed = TRUE)
  })
})

test_that("folder: every file is analysed, failures are kept, limits give verdicts", {
  testServer(module_server("folder", create_server_folder), {
    set_inputs(session, "folder", folder_inputs(make_folder_upload(), folder_acc_per_mm2 = 1000, folder_acc_ratings = "DS=0"))
    session$setInputs(`folder-folder_run` = 1)
    expect_equal(output[["folder-folder_status"]], "2 of 3 files analysed. Not analysed: broken. Acceptance: 1 fail, 1 pass.")
    expect_match(output[["folder-folder_overview_table"]], "<td> first </td>", fixed = TRUE)
    expect_match(output[["folder-folder_overview_table"]], "Verdict", fixed = TRUE)
    expect_match(output[["folder-folder_failed_table"]], "Choose the area column.", fixed = TRUE)
    expect_match(output[["folder-folder_ratings_table"]], "D thick", fixed = TRUE)
    expect_match(output[["folder-folder_acceptance_table"]], "Chart index DS, worst field", fixed = TRUE)
    expect_match(output[["folder-folder_warnings_table"]], "The inspected area is", fixed = TRUE)
    expect_true(nzchar(output[["folder-folder_density_plot"]]$src))
    expect_true(nzchar(output[["folder-folder_area_plot"]]$src))

    expect_true(all(c("Summary", "Acceptance", "Limits") %in% openxlsx::getSheetNames(output[["folder-folder_download_tables"]])))
    report <- paste(readLines(output[["folder-folder_download_report"]], warn = FALSE), collapse = "\n")
    expect_match(report, "first.xlsx", fixed = TRUE)
    expect_match(report, "MD5 checksum", fixed = TRUE)
    expect_setequal(zip::zip_list(output[["folder-folder_download_archive"]])$filename,
                    c("first_report.html", "second_report.html", "folder_summary_report.html"))
  })
})

test_that("folder: a mistyped limit is reported, and downloads need a result", {
  testServer(module_server("folder", create_server_folder), {
    expect_error(output[["folder-folder_download_tables"]], "before downloading")
    set_inputs(session, "folder", folder_inputs(make_folder_upload(), folder_acc_ratings = "X=1"))
    session$setInputs(`folder-folder_run` = 1)
    expect_error(output[["folder-folder_overview_table"]], "Cannot read 'X=1'")
  })
})

# ---- Acceptance limits in the Steel Cleanliness tab ----

clean_inputs <- function(...) utils::modifyList(list(
  clean_files = make_cleanliness_upload(), clean_area_col = CLEAN_COLS$area, clean_length_col = CLEAN_COLS$length,
  clean_breadth_col = CLEAN_COLS$breadth, clean_x_col = CLEAN_COLS$x, clean_y_col = CLEAN_COLS$y,
  clean_direction_col = CLEAN_COLS$direction, clean_field_col = CLEAN_COLS$field, clean_coord_unit = "mm",
  clean_family_source = "eds", clean_standard = "ISO 4967", clean_size_breaks = "1, 2, 5, 10, 20", clean_measured = "",
  clean_acc_ratings = ""), list(...))

test_that("cleanliness: limits are judged without a new run", {
  testServer(module_server("cleanliness", create_server_cleanliness), {
    set_inputs(session, "cleanliness", clean_inputs())
    session$setInputs(`cleanliness-clean_run` = 1)
    expect_match(output[["cleanliness-clean_acceptance_table"]], "Enter one or more limits", fixed = TRUE)
    expect_null(output[["cleanliness-clean_acceptance_summary"]])

    set_inputs(session, "cleanliness", list(clean_acc_per_mm2 = 1000, clean_acc_ratings = "D fine=0.5"))
    expect_match(output[["cleanliness-clean_acceptance_table"]], "Chart index D fine, worst field", fixed = TRUE)
    expect_match(output[["cleanliness-clean_acceptance_summary"]]$html, "Does not meet the limits: Chart index D fine, worst field.", fixed = TRUE)
    expect_match(output[["cleanliness-clean_acceptance_summary"]]$html, "#f8d7da", fixed = TRUE)      # red box

    set_inputs(session, "cleanliness", list(clean_acc_ratings = "D fine=1"))
    expect_match(output[["cleanliness-clean_acceptance_summary"]]$html, "Meets all limits.", fixed = TRUE)
    expect_match(output[["cleanliness-clean_acceptance_summary"]]$html, "#d4edda", fixed = TRUE)      # green box

    # the extreme-value limit is fitted on the run's own fields
    set_inputs(session, "cleanliness", list(clean_acc_return_level = 1000, clean_acc_fields_per_area = 1, clean_acc_return_period = 50))
    expect_match(output[["cleanliness-clean_acceptance_table"]], "bootstrap of the extreme-value fit", fixed = TRUE)

    set_inputs(session, "cleanliness", list(clean_acc_ratings = "nonsense"))
    expect_error(output[["cleanliness-clean_acceptance_table"]], "Cannot read 'nonsense'")
  })
})

test_that("cleanliness: the export and the report carry the assessment", {
  testServer(module_server("cleanliness", create_server_cleanliness), {
    set_inputs(session, "cleanliness", clean_inputs(clean_acc_per_mm2 = 1000))
    session$setInputs(`cleanliness-clean_run` = 1)
    path <- output[["cleanliness-clean_download_tables"]]
    expect_equal(openxlsx::getSheetNames(path)[1], "Acceptance")
    expect_equal(openxlsx::read.xlsx(path, "Acceptance")$verdict, "pass")
    report <- paste(readLines(output[["cleanliness-clean_download_report"]], warn = FALSE), collapse = "\n")
    expect_match(report, "<h2>Acceptance limits</h2>", fixed = TRUE)

    # no limits: no sheet, no section
    set_inputs(session, "cleanliness", list(clean_acc_per_mm2 = NA))
    expect_false("Acceptance" %in% openxlsx::getSheetNames(output[["cleanliness-clean_download_tables"]]))
  })
})

test_that("the limits are read from the boxes of either tab", {
  input <- list(x_acc_per_mm2 = 50, x_acc_area_ppm = NA, x_acc_largest = NULL, x_acc_ratings = "DS=1",
                x_acc_return_level = 20, x_acc_fields_per_area = 4, x_acc_return_period = NA)
  acc <- read_acceptance_limits(input, "x")
  expect_equal(acc$limits, list(per_mm2 = 50, return_level_um = 20, ratings = c(DS = 1)))
  expect_equal(acc$fields_per_area, 4)
  expect_equal(acc$return_period, 100)
  html <- as.character(acceptance_limits_ui(shiny::NS("tab"), "x", "A note."))
  expect_match(html, "id=\"tab-x_acc_return_level\"", fixed = TRUE)
  expect_match(html, "A note.", fixed = TRUE)
})
