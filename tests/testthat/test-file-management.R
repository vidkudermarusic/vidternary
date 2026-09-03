# Tests for R/file_management.R - output-directory creation and filename
# resolution for a ternary plot save. create_ternary_output_dir() setwd()s
# into `working_dir` (restoring on exit) and creates real directories, so
# every real-directory test here runs from an isolated temp location, not
# the real repo, matching this project's own established convention.

test_that("extract_file_base() strips the .xlsx extension and prefers xlsx_display_name over the raw upload path", {
  expect_equal(extract_file_base("C:/tmp/upload_a1b2c3.xlsx"), "upload_a1b2c3")
  expect_equal(extract_file_base("C:/tmp/upload_a1b2c3.xlsx", "My Real Sample.xlsx"), "My Real Sample")
  # basename() strips the directory even when only xlsx_file is given.
  expect_false(grepl("/", extract_file_base("C:/tmp/nested/dir/sample.xlsx")))
})

test_that("create_ternary_output_dir() in preview mode creates nothing and returns a NULL custom_folder", {
  old_wd <- getwd()
  tmp <- tempfile("output_dir_preview"); dir.create(tmp)
  on.exit(setwd(old_wd), add = TRUE)

  out_root <- tempfile("preview_out")  # deliberately never created
  res <- create_ternary_output_dir("sample.xlsx", output_dir = out_root, preview = TRUE, working_dir = tmp)
  expect_null(res$custom_folder)
  expect_equal(res$file_base, "sample")
  expect_false(dir.exists(out_root))
  # working_dir was restored, not left changed, despite the internal setwd().
  expect_equal(normalizePath(getwd()), normalizePath(old_wd))
})

test_that("create_ternary_output_dir() with a real output_dir creates <output_dir>/charge<file_base>, and restores the working directory afterward", {
  old_wd <- getwd()
  tmp <- tempfile("output_dir_working"); dir.create(tmp)
  out_root <- tempfile("real_out"); dir.create(out_root)
  on.exit(setwd(old_wd), add = TRUE)

  res <- create_ternary_output_dir("sample.xlsx", output_dir = out_root, preview = FALSE, working_dir = tmp)
  expect_equal(basename(res$custom_folder), "chargesample")
  expect_true(dir.exists(res$custom_folder))
  expect_equal(normalizePath(getwd()), normalizePath(old_wd))
})

test_that("create_ternary_output_dir() appends a timestamp instead of colliding when the target folder already exists", {
  old_wd <- getwd()
  tmp <- tempfile("output_dir_collide"); dir.create(tmp)
  out_root <- tempfile("collide_out"); dir.create(out_root)
  on.exit(setwd(old_wd), add = TRUE)

  first <- create_ternary_output_dir("sample.xlsx", output_dir = out_root, preview = FALSE, working_dir = tmp)
  second <- create_ternary_output_dir("sample.xlsx", output_dir = out_root, preview = FALSE, working_dir = tmp)
  expect_true(dir.exists(first$custom_folder))
  expect_true(dir.exists(second$custom_folder))
  expect_false(identical(first$custom_folder, second$custom_folder))
  expect_match(basename(second$custom_folder), "^chargesample_\\d{8}_\\d{6}$")
})

test_that("create_ternary_output_dir() falls back to <working_dir>/plots2 when output_dir is NULL and not in preview mode", {
  old_wd <- getwd()
  tmp <- tempfile("output_dir_fallback"); dir.create(tmp)
  on.exit(setwd(old_wd), add = TRUE)

  res <- create_ternary_output_dir("sample.xlsx", output_dir = NULL, preview = FALSE, working_dir = tmp)
  expect_true(dir.exists(file.path(tmp, "plots2")))
  expect_equal(normalizePath(dirname(res$custom_folder)), normalizePath(file.path(tmp, "plots2")))
  expect_equal(normalizePath(getwd()), normalizePath(old_wd))
})
