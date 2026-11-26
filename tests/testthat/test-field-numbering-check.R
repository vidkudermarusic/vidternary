# Tests for R/field_numbering_check.R and the tabs that use it: is the SEM
# field numbering one consecutive series, and is it said when it is not?

test_that("a consecutive numbering gives no message", {
  r <- field_numbering_notes(c(1, 1, 2, 3, 3, 3, 4, 5))
  expect_length(r$warnings, 0)
  expect_length(r$notes, 0)
  expect_equal(r$step, 1)
  expect_equal(r$n_fields, 5L)
  # missing values and an order other than the scan order do not matter
  expect_length(field_numbering_notes(c(5, NA, 3, 1, 2, 4))$notes, 0)
  expect_length(field_numbering_notes(NULL)$warnings, 0)
  expect_length(field_numbering_notes(c(NA, NA))$warnings, 0)
})

test_that("a few fields without particles are noted, not warned about", {
  # 2 of 50 numbers missing, as in a real map (2 of 608 in the project data)
  r <- field_numbering_notes(setdiff(1:50, c(17, 33)))
  expect_length(r$warnings, 0)
  expect_equal(r$notes, "2 of the 50 SEM fields hold no particle (their numbers are missing from the file); they are counted as inspected fields where nothing was detected.")
  expect_equal(r$n_fields, 50L)
})

test_that("IDs that step by a common number are found, and the real count is given", {
  r <- field_numbering_notes(c(100, 200, 200, 300, 500, 800))
  expect_equal(r$step, 100)
  expect_equal(r$n_fields, 701L)
  expect_length(r$warnings, 1)
  expect_match(r$warnings, "step by 100 (100, 200, 300, ...)", fixed = TRUE)
  expect_match(r$warnings, "701 fields, of which 5 hold particles", fixed = TRUE)
  expect_match(r$warnings, "there are about 5 fields", fixed = TRUE)
  expect_match(r$warnings, "divide the IDs by 100", fixed = TRUE)
})

test_that("separate plates (large gaps without a common step) are warned about", {
  r <- field_numbering_notes(c(101:106, 201:206))
  expect_equal(r$step, 1)
  expect_length(r$warnings, 1)
  expect_match(r$warnings, "Only 12 of the 106 field numbers between the first and last SEM field ID hold particles (89 % are missing, longest gap 94)", fixed = TRUE)
  expect_match(r$warnings, "IDs 101-106 and 201-206", fixed = TRUE)
  # the threshold is the one the EVS tab uses
  just_below <- field_numbering_notes(setdiff(1:100, c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)))      # 10 % missing
  expect_length(just_below$warnings, 0)
  expect_length(field_numbering_notes(setdiff(1:100, seq(2, 22, 2)))$warnings, 1)                   # 11 % missing
})

test_that("text IDs are noted: empty fields cannot be counted", {
  r <- field_numbering_notes(c("F1", "F2", "F10", "F2"))
  expect_length(r$warnings, 0)
  expect_match(r$notes, "are text, so only fields that hold at least one particle can be counted (3)", fixed = TRUE)
})

test_that("the label names the file, batch or run", {
  expect_match(field_numbering_notes(c(10, 20, 30), label = "batch 'B'")$warnings, "The SEM field IDs of batch 'B' step by 10", fixed = TRUE)
  expect_match(field_numbering_notes(setdiff(1:20, 7), label = "run 'x'")$notes, "1 of the 20 SEM fields of run 'x' hold no particle", fixed = TRUE)
})

test_that("Steel Cleanliness warns, and the acceptance intervals point to it", {
  d <- make_cleanliness_data()
  plain <- run_cleanliness_analysis(d, CLEAN_COLS, list(n_boot = 200))
  expect_false(any(grepl("field ID", plain$notes$warnings)))
  expect_length(plain$field_check$warnings, 0)

  d$Field <- d$Field * 100
  res <- run_cleanliness_analysis(d, CLEAN_COLS, list(n_boot = 200))
  expect_equal(res$n_fields, 701L)
  expect_match(res$notes$warnings, "The SEM field IDs step by 100", fixed = TRUE, all = FALSE)
  expect_match(res$notes$warnings, "there are about 8 fields", fixed = TRUE, all = FALSE)

  a <- assess_acceptance(res, acceptance_limits(per_mm2 = 1000, area_ppm = 1e6))$table
  expect_match(a$interval[2], "bootstrap over SEM fields; check the SEM field numbering (see the warnings)", fixed = TRUE)
  expect_no_match(assess_acceptance(plain, acceptance_limits(area_ppm = 1e6))$table$interval, "numbering")

  # no field column: nothing to check
  no_field <- run_cleanliness_analysis(make_cleanliness_data(), CLEAN_COLS[names(CLEAN_COLS) != "field"], list(n_boot = 200))
  expect_null(no_field$field_check)
})

test_that("the check uses the whole file, not the filtered rows", {
  d <- make_cleanliness_data()
  big <- d[d[[CLEAN_COLS$area]] > 1, ]           # leaves only some of the 8 fields
  res <- run_cleanliness_analysis(big, CLEAN_COLS, list(n_boot = 200), all_data = d)
  expect_length(res$field_check$warnings, 0)
  expect_length(res$field_check$notes, 0)
})

test_that("Batch Comparison names the batch whose numbering is off", {
  b <- make_batches()
  b$B$Field <- b$B$Field * 10
  res <- run_batch_comparison(b, BATCH_COLS, list(n_perm = 99, n_boot = 100))
  expect_match(res$notes$warnings, "The SEM field IDs of batch 'B' step by 10", fixed = TRUE, all = FALSE)
  expect_false(any(grepl("batch 'A'|batch 'C'", res$notes$warnings)))
  expect_equal(names(res$field_checks), c("A", "B", "C"))
})

test_that("Measurement Planning warns, naming the run only when there are several", {
  a <- make_batch(200)
  a$Field <- a$Field * 10
  cols <- BATCH_COLS[c("area", "field", "x", "y")]
  one <- plan_measurement(list(pilot = a), cols, list(n_sim = 50))
  expect_match(one$notes$warnings, "The SEM field IDs step by 10", fixed = TRUE, all = FALSE)
  two <- plan_measurement(list(first = make_batch(200, seed = 2), second = a), cols, list(n_sim = 50))
  expect_match(two$notes$warnings, "The SEM field IDs of run 'second' step by 10", fixed = TRUE, all = FALSE)
})

test_that("Folder Summary counts the warning for the file it belongs to", {
  d <- make_cleanliness_data()
  odd <- d
  odd$Field <- odd$Field * 100
  res <- run_folder_summary(list(plain = d, odd = odd), CLEAN_COLS, list(n_boot = 200, cleanliness = list(n_boot = 200)))
  expect_equal(res$summary$warnings[2], res$summary$warnings[1] + 1)
  expect_true(any(res$warnings$file == "odd" & grepl("step by 100", res$warnings$text)))
  expect_false(any(res$warnings$file == "plain" & grepl("step by", res$warnings$text)))
})
