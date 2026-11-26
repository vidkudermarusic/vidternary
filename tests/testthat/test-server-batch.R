# Tests for R/server_batch.R and R/server_batch_outputs.R: the Shiny wiring
# of the "Batch Comparison" tab.
#
# library(shiny) is required here - see test-server-evs.R for why.
library(shiny)

make_batch_server <- function(log = function(...) invisible(NULL)) {
  rv <- shiny::reactiveValues()
  function(input, output, session) {
    shiny::moduleServer("batch", function(input, output, session) {
      create_server_batch(input, output, session, rv, function(...) invisible(NULL), log)
    })
  }
}

# Sets the inputs the browser would hold after the files are loaded.
set_batch_inputs <- function(session, upload = make_batch_upload(), ...) {
  values <- utils::modifyList(list(
    batch_files = upload, batch_area_col = BATCH_COLS$area, batch_field_col = BATCH_COLS$field,
    batch_x_col = BATCH_COLS$x, batch_y_col = BATCH_COLS$y, batch_coord_unit = "mm", batch_inspected_area = "",
    batch_parts = BATCH_COLS$parts, batch_fields_per_area = 2, batch_return_period = 50, batch_n_perm = 199
  ), list(...))
  names(values) <- paste0("batch-", names(values))
  do.call(session$setInputs, values)
}
batch_placeholder <- "Upload one file per batch, check the columns, and click \"Compare Batches\"."

test_that("the status shows the placeholder before the comparison is run", {
  testServer(make_batch_server(), {
    expect_equal(output[["batch-batch_status"]], batch_placeholder)
    session$setInputs(`batch-batch_run` = 1)
    expect_equal(output[["batch-batch_status"]], batch_placeholder)
    expect_null(output[["batch-batch_notes"]])
  })
})

test_that("the uploaded files are listed as batches", {
  testServer(make_batch_server(), {
    session$setInputs(`batch-batch_files` = make_batch_upload())
    expect_equal(output[["batch-batch_file_info"]], "A: 200 rows\nB: 200 rows\nC: 100 rows")
  })
})

test_that("a comparison fills the status, the notes and every table", {
  testServer(make_batch_server(), {
    set_batch_inputs(session)
    session$setInputs(`batch-batch_run` = 1)
    expect_equal(output[["batch-batch_status"]],
                 "3 batches compared: A, B, C. Particles: 200, 200, 100. Composition compared on 3 elements.")
    expect_match(output[["batch-batch_notes"]]$html, "adjusted for the number of pairs (Holm)", fixed = TRUE)

    summary <- output[["batch-batch_summary_table"]]
    expect_match(summary, "Index of dispersion (counts per field)", fixed = TRUE)
    expect_match(summary, "100.0", fixed = TRUE)
    overall <- output[["batch-batch_overall_table"]]
    expect_match(overall, "k-sample Anderson-Darling, permutation", fixed = TRUE)
    expect_match(overall, "Share of variation explained (R2)", fixed = TRUE)
    expect_match(output[["batch-batch_density_pairs_table"]], "A vs C", fixed = TRUE)
    expect_match(output[["batch-batch_density_pairs_table"]], "2.00", fixed = TRUE)
    expect_match(output[["batch-batch_size_quantiles_table"]], "99th percentile (um)", fixed = TRUE)
    expect_match(output[["batch-batch_size_pairs_table"]], "Median ratio", fixed = TRUE)
    expect_match(output[["batch-batch_clr_table"]], "Zero readings (%)", fixed = TRUE)
    expect_match(output[["batch-batch_composition_pairs_table"]], "Pseudo-F", fixed = TRUE)
    expect_match(output[["batch-batch_spread_table"]], "Mean distance to the batch centroid", fixed = TRUE)
    expect_match(output[["batch-batch_evs_table"]], "Return level sqrt(area) (um)", fixed = TRUE)
  })
})

test_that("the plots render after a comparison", {
  testServer(make_batch_server(), {
    set_batch_inputs(session)
    session$setInputs(`batch-batch_run` = 1)
    for (plot in c("batch_density_plot", "batch_area_plot", "batch_size_plot", "batch_clr_plot", "batch_evs_plot")) {
      expect_false(is.null(output[[paste0("batch-", plot)]]$src), info = plot)
    }
  })
})

test_that("without elements and field IDs the comparison still runs, and the plots say what is missing", {
  testServer(make_batch_server(), {
    set_batch_inputs(session, batch_parts = character(0), batch_field_col = "none")
    session$setInputs(`batch-batch_run` = 1)
    expect_match(output[["batch-batch_status"]], "Composition not compared (no elements chosen).", fixed = TRUE)
    expect_match(output[["batch-batch_composition_pairs_table"]], "Choose two or more chemistry columns", fixed = TRUE)
    expect_match(output[["batch-batch_evs_table"]], "Needs a SEM field ID column", fixed = TRUE)
    err <- expect_error(output[["batch-batch_clr_plot"]])
    expect_match(conditionMessage(err), "Choose two or more elements", fixed = TRUE)
    err <- expect_error(output[["batch-batch_evs_plot"]])
    expect_match(conditionMessage(err), "needs a SEM field ID column", fixed = TRUE)
    expect_false(is.null(output[["batch-batch_area_plot"]]$src))        # drawn without intervals
  })
})

test_that("wrong input is named in the status", {
  testServer(make_batch_server(), {
    set_batch_inputs(session, upload = make_batch_upload(make_batches()["A"]))
    session$setInputs(`batch-batch_run` = 1)
    err <- expect_error(output[["batch-batch_status"]])
    expect_match(conditionMessage(err), "Upload at least 2 files", fixed = TRUE)
  })
  testServer(make_batch_server(), {
    set_batch_inputs(session, batch_parts = "Ti.(Wt%)")
    session$setInputs(`batch-batch_run` = 1)
    err <- expect_error(output[["batch-batch_status"]])
    expect_match(conditionMessage(err), "Choose two or more elements to compare, or none.", fixed = TRUE)
  })
  testServer(make_batch_server(), {
    set_batch_inputs(session, batch_inspected_area = "1,9; two")
    session$setInputs(`batch-batch_run` = 1)
    err <- expect_error(output[["batch-batch_status"]])
    expect_match(conditionMessage(err), "Inspected areas must be positive numbers", fixed = TRUE)
  })
  testServer(make_batch_server(), {
    set_batch_inputs(session, batch_x_col = "none", batch_y_col = "none")
    session$setInputs(`batch-batch_run` = 1)
    err <- expect_error(output[["batch-batch_status"]])
    expect_match(conditionMessage(err), "Enter the inspected area, or choose the X and Y", fixed = TRUE)
  })
})

test_that("entered inspected areas are used", {
  testServer(make_batch_server(), {
    set_batch_inputs(session, batch_inspected_area = "2, 4, 1", batch_x_col = "none", batch_y_col = "none")
    session$setInputs(`batch-batch_run` = 1)
    pairs <- output[["batch-batch_density_pairs_table"]]
    expect_match(pairs, "A vs B", fixed = TRUE)
    # 100 per mm2 against 50 per mm2
    expect_match(pairs, "2.00", fixed = TRUE)
  })
})

test_that("the pre-analysis filter is applied to every batch", {
  testServer(make_batch_server(), {
    set_batch_inputs(session)
    session$setInputs(`batch-batch_filter_cols` = BATCH_COLS$area)
    filter_id <- paste0("batch-batch_filter_", gsub("[^A-Za-z0-9]", "_", BATCH_COLS$area))
    do.call(session$setInputs, stats::setNames(list("> 3"), filter_id))
    session$setInputs(`batch-batch_run` = 1)
    expected <- vapply(make_batches(), function(d) sum(d[[BATCH_COLS$area]] > 3), integer(1))
    expect_match(output[["batch-batch_status"]], paste0("Particles: ", paste(expected, collapse = ", "), "."), fixed = TRUE)
  })
  testServer(make_batch_server(), {
    set_batch_inputs(session)
    session$setInputs(`batch-batch_filter_cols` = BATCH_COLS$area)
    filter_id <- paste0("batch-batch_filter_", gsub("[^A-Za-z0-9]", "_", BATCH_COLS$area))
    do.call(session$setInputs, stats::setNames(list("> 99999"), filter_id))
    session$setInputs(`batch-batch_run` = 1)
    err <- expect_error(output[["batch-batch_status"]])
    expect_match(conditionMessage(err), "No rows remain after the pre-analysis filter(s) in: A, B, C.", fixed = TRUE)
  })
})

test_that("tables, report and script can be downloaded after a comparison, and not before", {
  testServer(make_batch_server(), {
    for (out in c("batch_download_tables", "batch_download_report", "batch_download_script")) {
      res <- tryCatch({ output[[paste0("batch-", out)]]; "ok" }, error = function(e) conditionMessage(e))
      expect_equal(res, "Upload the files and click \"Compare Batches\" before downloading.", info = out)
    }
    set_batch_inputs(session)
    session$setInputs(`batch-batch_run` = 1)

    tables <- output[["batch-batch_download_tables"]]
    expect_true(all(c("Summary", "Overall tests", "Size pairs", "Extreme values") %in% openxlsx::getSheetNames(tables)))
    expect_equal(openxlsx::read.xlsx(tables, sheet = "Summary")$per_mm2, c(100, 100, 50))

    report <- paste(readLines(output[["batch-batch_download_report"]], warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    expect_match(report, "<h1>Batch comparison report</h1>", fixed = TRUE)
    expect_match(report, "3 batches: A, B, C", fixed = TRUE)
    expect_match(report, "A.xlsx", fixed = TRUE)

    script <- readLines(output[["batch-batch_download_script"]], warn = FALSE, encoding = "UTF-8")
    expect_silent(parse(text = script))
    expect_true(any(grepl('names(all_batches) <- c("A", "B", "C")', script, fixed = TRUE)))
    expect_true(any(grepl("fields_per_area = 2", script, fixed = TRUE)))
  })
})

test_that("the tab is part of the app and a comparison is logged", {
  ui <- as.character(create_main_ui())
  expect_match(ui, "Batch Comparison", fixed = TRUE)
  expect_match(ui, "batch-batch_run", fixed = TRUE)
  expect_match(paste(deparse(create_server_logic), collapse = "\n"), "create_server_batch", fixed = TRUE)

  record <- new.env()
  testServer(make_batch_server(log = function(level, message, details = NULL) record$details <- details), {
    set_batch_inputs(session)
    session$setInputs(`batch-batch_run` = 1)
    output[["batch-batch_status"]]
    expect_equal(record$details, "3 batches: A, B, C")
  })
})

test_that("inspected areas and batch labels are read from text", {
  expect_null(parse_inspected_areas(""))
  expect_null(parse_inspected_areas(NULL))
  expect_equal(parse_inspected_areas("1.9"), 1.9)
  expect_equal(parse_inspected_areas("1.9, 1.9; 2.4"), c(1.9, 1.9, 2.4))
  expect_error(parse_inspected_areas("1.9, x"), "positive numbers")
  expect_error(parse_inspected_areas("0"), "positive numbers")
  expect_equal(batch_labels(c("heat 1.xlsx", "dir/heat 2.csv", "heat 1.xlsx")), c("heat 1", "heat 2", "heat 1 #1"))
})
