# Tests for R/folder_pipeline.R and R/folder_report.R: many files, one
# table. The files are variants of make_cleanliness_data()
# (helper-cleanliness.R): 32 particles on 2 mm2.

# Three specimens: the helper data, the same without its 15 um particle,
# and a file that lacks the columns.
make_folder_tables <- function() {
  full <- make_cleanliness_data()
  list(first = full, second = full[full[[CLEAN_COLS$length]] < 15 | full[[CLEAN_COLS$breadth]] < 15, ],
       broken = data.frame(Something = 1:3))
}
FOLDER_SETTINGS <- list(n_boot = 200, cleanliness = list(n_boot = 200))

test_that("every file gets a row with the numbers of its own analysis", {
  tables <- make_folder_tables()
  res <- run_folder_summary(tables[c("first", "second")], CLEAN_COLS, FOLDER_SETTINGS)
  s <- res$summary
  expect_equal(s$file, c("first", "second"))
  expect_equal(s$status, c("ok", "ok"))
  expect_equal(s$n, c(32, 31))
  own <- run_cleanliness_analysis(tables$first, CLEAN_COLS, list(n_boot = 200))
  all_row <- own$indices[own$indices$group == "All", ]
  expect_equal(s$per_mm2[1], all_row$per_mm2)
  expect_equal(s$area_ppm[1], all_row$area_ppm)
  expect_equal(s$max_sqrt_area_um[1], all_row$max_sqrt_area_um)
  expect_equal(s$inspected_area_mm2, c(2, 2))
  expect_equal(s$stringers, c(1L, 1L))
  # the ratings of the helper data; the second file has lost its DS particle
  expect_equal(c(s$index_A_fine[1], s$index_B_fine[1], s$index_C_thick[1], s$index_D_fine[1], s$index_DS[1]), c(0.5, 0.5, 0.5, 1, 0.5))
  expect_equal(s$index_DS[2], 0)
  expect_true(all(is.na(s$verdict)))
  expect_null(res$acceptance)
  expect_equal(names(res$results), c("first", "second"))
})

test_that("a file that cannot be analysed stays in the table with the reason", {
  res <- run_folder_summary(make_folder_tables(), CLEAN_COLS, FOLDER_SETTINGS)
  s <- res$summary
  expect_equal(s$status, c("ok", "ok", "failed"))
  expect_equal(s$problem[3], "Choose the area column.")
  expect_true(is.na(s$n[3]))
  expect_equal(names(res$results), c("first", "second"))
  expect_equal(folder_display_tables(res)$failed$File, "broken")
  # nothing analysable at all is still a result, not an error
  none <- run_folder_summary(make_folder_tables()["broken"], CLEAN_COLS, FOLDER_SETTINGS)
  expect_equal(none$summary$status, "failed")
  expect_equal(folder_display_tables(none)$overview$Note, "No file could be analysed.")
})

test_that("a file in another coordinate unit fails with the reason, and the others are still analysed", {
  tables <- make_folder_tables()[c("first", "second")]
  tables$second[[CLEAN_COLS$x]] <- tables$second[[CLEAN_COLS$x]] * 1000
  tables$second[[CLEAN_COLS$y]] <- tables$second[[CLEAN_COLS$y]] * 1000
  res <- run_folder_summary(tables, CLEAN_COLS, FOLDER_SETTINGS)
  expect_equal(res$summary$status, c("ok", "failed"))
  expect_match(res$summary$problem[2], "4,003,035 rating fields", fixed = TRUE)
  expect_match(res$summary$problem[2], "if the coordinates are in um, change it to um", fixed = TRUE)
})

test_that("limits give every file a verdict", {
  limits <- acceptance_limits(per_mm2 = 1000, ratings = parse_rating_limits("DS=0"))
  res <- run_folder_summary(make_folder_tables(), CLEAN_COLS, FOLDER_SETTINGS, limits)
  expect_equal(res$summary$verdict, c("fail", "pass", NA))
  expect_equal(res$acceptance$file, c("first", "first", "second", "second"))
  expect_equal(res$acceptance$verdict, c("pass", "fail", "pass", "pass"))
  expect_equal(names(res$assessments), c("first", "second"))
  expect_equal(folder_display_tables(res)$overview$Verdict, c("fail", "pass"))
})

test_that("the filter is applied to every file, and an emptied file is reported", {
  tables <- make_folder_tables()[c("first", "second")]
  filters <- list(`Length.(um)` = ">= 15")
  res <- run_folder_summary(tables, CLEAN_COLS, FOLDER_SETTINGS, filters = filters)
  expect_equal(res$summary$n, c(sum(tables$first[[CLEAN_COLS$length]] >= 15), sum(tables$second[[CLEAN_COLS$length]] >= 15)))
  # the inspected area is that of the whole file
  expect_equal(res$summary$inspected_area_mm2, c(2, 2))
  gone <- run_folder_summary(tables, CLEAN_COLS, FOLDER_SETTINGS, filters = list(`Length.(um)` = "> 1000"))
  expect_equal(gone$summary$problem, rep("no rows remain after the pre-analysis filter(s)", 2))
})

test_that("the largest-inclusion estimate is given where it can be fitted", {
  big <- lapply(list(a = 1, b = 2), function(seed) {
    d <- make_batch(300, seed = seed)
    ecd <- 2 * sqrt(d[[BATCH_COLS$area]] / pi)
    d[["Length.(um)"]] <- ecd; d[["Breadth.(um)"]] <- ecd
    d
  })
  cols <- list(area = BATCH_COLS$area, length = "Length.(um)", breadth = "Breadth.(um)", x = BATCH_COLS$x, y = BATCH_COLS$y, field = "Field")
  res <- run_folder_summary(big, cols, c(FOLDER_SETTINGS, list(return_period = 50)))
  ref <- acceptance_evs(big$a[[BATCH_COLS$area]], big$a$Field, return_period = 50, n_boot = 200)
  expect_equal(res$summary$return_level_um[1], ref$return_level)
  expect_true(all(res$summary$return_level_lower < res$summary$return_level_um))
  # too few control areas for a fit: left empty, not an error
  small <- run_folder_summary(big["a"], cols, c(FOLDER_SETTINGS, list(fields_per_area = 10)))
  expect_equal(small$summary$status, "ok")
  expect_true(is.na(small$summary$return_level_um))
  expect_equal(folder_display_tables(small)$overview$`Largest expected, EVS (um)`, "-")
})

test_that("progress is reported once per file, and bad input refused", {
  seen <- character(0)
  run_folder_summary(make_folder_tables(), CLEAN_COLS, FOLDER_SETTINGS, progress = function(name) seen <<- c(seen, name))
  expect_equal(seen, c("first", "second", "broken"))
  expect_error(run_folder_summary(list(), CLEAN_COLS), "No files to analyse")
  expect_error(run_folder_summary(list(make_cleanliness_data()), CLEAN_COLS), "Every file needs its own name")
})

test_that("the display tables, the export and the report are complete", {
  limits <- acceptance_limits(per_mm2 = 1000, ratings = parse_rating_limits("DS=0"))
  res <- run_folder_summary(make_folder_tables(), CLEAN_COLS, FOLDER_SETTINGS, limits, filters = list(`Length.(um)` = "> 0"))
  tab <- folder_display_tables(res)
  expect_equal(names(tab), c("overview", "ratings", "acceptance", "failed", "warnings"))
  expect_equal(tab$overview$File, c("first", "second"))
  expect_equal(names(tab$ratings), c("File", "A fine", "A thick", "B fine", "B thick", "C fine", "C thick", "D fine", "D thick", "DS"))
  expect_equal(tab$ratings$DS, c("0.5", "0.0"))
  expect_equal(nrow(tab$acceptance), 4)
  expect_true(all(c("first", "second") %in% tab$warnings$File))    # the small-area warning of each file

  sheets <- folder_result_tables(res)
  expect_equal(names(sheets), c("Summary", "Acceptance", "Limits", "Warnings", "Settings", "Filters"))
  expect_equal(sheets$Limits$limit, c("per_mm2", "chart index DS"))
  expect_equal(sheets$Filters$condition, "> 0")
  expect_true("area" %in% sheets$Settings$setting)
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(sheets, path)
  expect_true(file.exists(path))

  html <- folder_report(res, report_provenance(names(res$results)))
  expect_match(html, "<h1>Folder summary report</h1>", fixed = TRUE)
  expect_match(html, "3 files, 2 analysed", fixed = TRUE)
  expect_match(html, "Acceptance: 1 fail, 1 pass.", fixed = TRUE)
  expect_match(html, "1 of 3 files could not be analysed.", fixed = TRUE)
  expect_match(html, "data:image/png;base64,", fixed = TRUE)
  expect_no_match(folder_report(run_folder_summary(make_folder_tables()["first"], CLEAN_COLS, FOLDER_SETTINGS)),
                  "<h2>Acceptance limits</h2>", fixed = TRUE)
})

test_that("the archive holds one report per analysed file and the summary", {
  res <- run_folder_summary(make_folder_tables(), CLEAN_COLS, FOLDER_SETTINGS, acceptance_limits(per_mm2 = 1000))
  path <- tempfile(fileext = ".zip")
  folder_report_archive(res, path)
  expect_setequal(zip::zip_list(path)$filename, c("first_report.html", "second_report.html", "folder_summary_report.html"))
  out <- tempfile("unzipped_"); zip::unzip(path, exdir = out)
  expect_match(paste(readLines(file.path(out, "first_report.html"), warn = FALSE), collapse = "\n"), "<h2>Acceptance limits</h2>", fixed = TRUE)
  none <- run_folder_summary(make_folder_tables()["broken"], CLEAN_COLS, FOLDER_SETTINGS)
  expect_error(folder_report_archive(none, tempfile(fileext = ".zip")), "No file was analysed")
})

test_that("process_folder() reads a directory, guesses the columns and writes the report", {
  dir <- tempfile("folder_"); dir.create(dir)
  file.copy(example_data_files(), dir)
  writeLines("not a table", file.path(dir, "notes.txt"))
  report <- file.path(dir, "summary.html")
  res <- process_folder(dir, settings = FOLDER_SETTINGS, limits = acceptance_limits(per_mm2 = 300), report = report)
  expect_equal(res$summary$file, c("example_other_heat", "example_run_1", "example_run_2"))
  expect_equal(res$summary$status, rep("ok", 3))
  expect_equal(res$cols$area, "Area.(sq..um)")
  expect_equal(res$cols$field, "Field")
  expect_equal(res$files$rows, unname(res$summary$n))
  expect_equal(res$unreadable$name, "notes.txt")
  expect_equal(res$summary$verdict[1], "pass")                      # 255 per mm2
  expect_true(all(grepl("^fail", res$summary$verdict[2:3])))        # about 455 per mm2
  expect_true(file.exists(report))
  expect_match(paste(readLines(report, warn = FALSE), collapse = "\n"), "MD5 checksum", fixed = TRUE)

  expect_error(process_folder(file.path(dir, "nowhere")), "does not exist")
  empty <- tempfile("empty_"); dir.create(empty)
  expect_error(process_folder(empty), "No particle files")
  odd <- tempfile("odd_"); dir.create(odd)
  utils::write.csv(data.frame(a = 1:3, b = 4:6), file.path(odd, "x.csv"), row.names = FALSE)
  expect_error(process_folder(odd), "Could not find a column for: area, length, breadth, x, y")
})
