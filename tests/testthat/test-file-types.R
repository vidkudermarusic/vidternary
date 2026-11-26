# File types: every particle-data upload box offers what read_particle_table()
# reads, and the older tabs read text files too (audit pass 7, medium finding
# "File types are inconsistent between tabs and wrongly described").

test_that("particle_file_accept() lists exactly what the reader reads, without .xls", {
  acc <- particle_file_accept()
  expect_true(all(c(".xlsx", ".xlsm", ".csv", ".txt", ".tsv", ".tab", ".dat") %in% acc))
  expect_false(".xls" %in% acc)
  expect_false(anyDuplicated(acc) > 0)
})

# Walk every R source file and collect the `accept` argument of each
# fileInput() call. Settings / project / profile boxes take .json; every other
# box must use particle_file_accept(), so the tabs cannot drift apart again.
.collect_file_inputs <- function(expr, file, out = list()) {
  if (is.call(expr)) {
    fn <- expr[[1]]
    is_fi <- (is.name(fn) && identical(as.character(fn), "fileInput")) ||
      (is.call(fn) && identical(as.character(fn[[1]]), "::") && identical(as.character(fn[[3]]), "fileInput"))
    if (is_fi) out[[length(out) + 1]] <- list(file = file, accept = expr$accept)
    parts <- as.list(expr)[-1]
  } else if (is.expression(expr) || is.list(expr) || is.pairlist(expr)) {
    parts <- as.list(expr)
  } else {
    return(out)
  }
  # Empty arguments (e.g. `x[, 1]`, function formals without a default) are
  # the "missing" symbol and cannot be passed on; skip them by index.
  for (i in seq_along(parts)) {
    if (identical(parts[[i]], quote(expr = ))) next
    out <- .collect_file_inputs(parts[[i]], file, out)
  }
  out
}

test_that("every data upload box in the app uses particle_file_accept()", {
  r_dir <- testthat::test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "R sources not available (installed package)")
  calls <- list()
  for (f in list.files(r_dir, pattern = "[.]R$", full.names = TRUE)) {
    calls <- .collect_file_inputs(parse(f, keep.source = FALSE), basename(f), calls)
  }
  expect_gt(length(calls), 10)
  for (cl in calls) {
    acc <- cl$accept
    ok <- (is.character(acc) && identical(acc, ".json")) ||
      (is.call(acc) && identical(acc, quote(particle_file_accept())))
    expect_true(ok, info = paste(cl$file, "fileInput accept =", paste(deparse(acc), collapse = "")))
  }
})

test_that("read_particle_table() reads .xlsm and gives a clear .xls message with the real name", {
  d0 <- data.frame(Field = 1:3, `Al.(Wt%)` = c(1, NA, 3), check.names = FALSE)
  p <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d0, p)
  pm <- sub("[.]xlsx$", ".xlsm", p)
  file.copy(p, pm)
  d <- read_particle_table(pm)
  expect_equal(nrow(d), 3)
  expect_equal(d[["Al.(Wt%)"]], c(1, 0, 3))
  expect_error(read_particle_table(tempfile(fileext = ".xls"), name = "heat_7.xls"), "heat_7[.]xls")
})

test_that("extract_file_base() strips any data-file extension, not only .xlsx", {
  expect_equal(extract_file_base("x", "heat 1.csv"), "heat 1")
  expect_equal(extract_file_base("x", "heat1.XLSX"), "heat1")
  expect_equal(extract_file_base("x", "heat1.xlsm"), "heat1")
  expect_equal(extract_file_base("C:/tmp/0.txt"), "0")
  # other dots in the name are kept
  expect_equal(extract_file_base("x", "steel.v2.tsv"), "steel.v2")
  expect_equal(extract_file_base("x", "steel.v2"), "steel.v2")
})

test_that("the older tabs' loaders read a .csv copy of a file like the .xlsx", {
  xlsx <- example_data_files()[[1]]
  d_x <- read_particle_table(xlsx)
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(d_x, csv, row.names = FALSE, na = "")
  d_c <- read_particle_table(csv, name = "example_run_1.csv")
  expect_equal(dim(d_c), dim(d_x))
  expect_equal(names(d_c), names(d_x))
  num <- names(d_x)[vapply(d_x, is.numeric, logical(1))]
  expect_equal(d_c[num], d_x[num], tolerance = 1e-9, ignore_attr = TRUE)
})
