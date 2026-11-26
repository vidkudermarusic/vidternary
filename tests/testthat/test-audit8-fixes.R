# Tests for the fixes of audit pass 8: the limit on the number of SEM
# fields, blank element cells set to 0 when a file is read, and the
# Anderson-Darling test when all values are equal.
library(shiny)

# ---- limit on the number of SEM fields ----

test_that("a field ID range far beyond any specimen is refused at once", {
  expect_equal(check_sem_field_count(c(1, 2, 3, 608)), 608)
  expect_equal(check_sem_field_count(c(NA, NA)), 0)
  expect_error(check_sem_field_count(c(1:31, 3e6)),
               "The SEM field IDs run from 1 to 3,000,000, so 3,000,000 fields would be counted (32 of them hold particles; at most 100,000 are allowed).", fixed = TRUE)
  expect_error(check_sem_field_count(c(1, 2e5), label = "batch 'B'"), "The SEM field IDs of batch 'B' run from 1 to 200,000", fixed = TRUE)
  # the limit is a number of fields: exactly 100000 is allowed
  expect_equal(check_sem_field_count(c(1, 1e5)), 1e5)
  expect_error(check_sem_field_count(c(1, 1e5 + 1)), "100,001 fields")
  expect_equal(check_sem_field_count(c(1, 1e8), max_fields = Inf), 1e8)
  # text IDs are counted as they are
  expect_error(check_sem_field_count(paste0("F", 1:12), max_fields = 10), "holds 12 different IDs (at most 10 are allowed)", fixed = TRUE)
})

test_that("Steel Cleanliness stops before building anything per field", {
  d <- make_cleanliness_data()
  d$Field[1] <- 1e8
  elapsed <- system.time(
    expect_error(run_cleanliness_analysis(d, CLEAN_COLS, list(n_boot = 100)), "run from 1 to 100,000,000")
  )[["elapsed"]]
  expect_lt(elapsed, 5)
  # the whole file is checked, so a filter cannot hide the stray ID
  expect_error(run_cleanliness_analysis(d[-1, ], CLEAN_COLS, list(n_boot = 100), all_data = d), "100,000,000")
  # raised limit: the analysis runs (on a smaller stray ID, to keep the test fast)
  d$Field[1] <- 2e5
  expect_equal(run_cleanliness_analysis(d, CLEAN_COLS, list(n_boot = 20, max_sem_fields = Inf))$n_fields, 2e5)
})

test_that("Batch Comparison, Measurement Planning and Folder Summary name what is wrong", {
  b <- make_batches()
  b$B$Field[1] <- 5e5
  expect_error(run_batch_comparison(b, BATCH_COLS, list(n_perm = 99, n_boot = 100)), "The SEM field IDs of batch 'B' run from 1 to 500,000", fixed = TRUE)
  cols <- BATCH_COLS[c("area", "field", "x", "y")]
  expect_error(plan_measurement(list(pilot = b$B), cols, list(n_sim = 20)), "The SEM field IDs run from 1 to 500,000", fixed = TRUE)
  expect_error(plan_measurement(list(one = b$A, two = b$B), cols, list(n_sim = 20)), "of run 'two' run from 1", fixed = TRUE)
  d <- make_cleanliness_data(); odd <- d; odd$Field[1] <- 1e8
  res <- run_folder_summary(list(good = d, odd = odd), CLEAN_COLS, list(n_boot = 100, cleanliness = list(n_boot = 100)))
  expect_equal(res$summary$status, c("ok", "failed"))
  expect_match(res$summary$problem[2], "run from 1 to 100,000,000", fixed = TRUE)
})

# ---- blank element cells are 0 from the moment a file is read ----

blank_copy <- function(d) {
  el <- grep("Wt%", names(d), fixed = TRUE, value = TRUE)
  for (cn in el) d[[cn]][d[[cn]] == 0] <- NA
  d
}

test_that("only element columns are filled, and a wholly blank column becomes zeros", {
  d <- data.frame(`Al.(Wt%)` = c(10, NA), `Wt% Si` = c(NA, 5), `Fe at%` = c(NA, 1), Y = c(NA, 2), B = c(NA, 1),
                  `Mo.(Wt%)` = c(NA, NA), Area = c(NA, 3), check.names = FALSE)
  f <- fill_blank_element_cells(d)
  expect_equal(f$`Al.(Wt%)`, c(10, 0))
  expect_equal(f$`Wt% Si`, c(0, 5))
  expect_equal(f$`Fe at%`, c(0, 1))
  expect_equal(f$`Mo.(Wt%)`, c(0, 0))
  expect_true(is.numeric(f$`Mo.(Wt%)`))
  # not element percentages: left alone
  expect_equal(f$Y, c(NA, 2))
  expect_equal(f$B, c(NA, 1))
  expect_equal(f$Area, c(NA, 3))
  expect_equal(attr(f, "blank_element_cells"), 5)
})

test_that("read_particle_table fills the blanks of Excel and text files and says how many", {
  d <- example_data("example_run_1")
  blank <- blank_copy(d)
  n_blank <- sum(is.na(blank))
  xl <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(blank, xl)
  csv <- tempfile(fileext = ".csv"); utils::write.csv(blank, csv, row.names = FALSE, na = "")
  from_xl <- read_particle_table(xl)
  from_csv <- read_particle_table(csv)
  expect_equal(attr(from_xl, "import_info")$blank_element_cells, n_blank)
  expect_equal(attr(from_csv, "import_info")$blank_element_cells, n_blank)
  el <- grep("Wt%", names(d), fixed = TRUE, value = TRUE)
  expect_equal(from_xl[el], d[el], ignore_attr = TRUE)
  expect_equal(from_csv[el], d[el], ignore_attr = TRUE, tolerance = 1e-6)
})

test_that("Ternary Plots, Inclusion Presets and the hexagonal diagram see the same particles with blanks as with zeros", {
  d <- example_data("example_run_1")
  zero_file <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(d, zero_file)
  blank_file <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(blank_copy(d), blank_file)
  A <- list(col = "Al.(Wt%)"); B <- list(col = "Si.(Wt%)"); C <- list(col = "Ca.(Wt%)")
  sel <- c("Al.(Wt%)", "Si.(Wt%)", "Ca.(Wt%)")
  points <- function(file) {
    invisible(capture.output({
      M <- load_and_validate_ternary_source_data(file, A, B, C, "png", preview = TRUE)
      out <- compute_ternary_coordinates(M, sel, A, B, C, NULL, NULL, FALSE, NULL)
    }))
    nrow(out$ternary_points1)
  }
  expect_equal(points(blank_file), points(zero_file))
  expect_equal(points(blank_file), 860)

  preset <- get_inclusion_preset("cao-al2o3-mgo")
  cls <- function(file) table(analyze_inclusion_preset(read_particle_table(file), preset)$class, useNA = "ifany")
  expect_equal(cls(blank_file), cls(zero_file))
  expect_equal(sum(!is.na(analyze_inclusion_preset(read_particle_table(blank_file), preset)$class)), 268)
})

test_that("the Ternary Plots upload fills the blanks too", {
  d <- example_data("example_run_1")
  blank_file <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(blank_copy(d), blank_file)
  old_wd <- getwd(); tmp <- tempfile("blank_upload"); dir.create(tmp); setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)
  testServer(function(input, output, session) session$userData$rv <- create_server_logic(input, output, session), {
    session$setInputs(`ternary_plots-xlsx_file1` = data.frame(name = "blank.xlsx", size = 1, type = "", datapath = blank_file))
    df1 <- session$userData$rv$df1
    expect_equal(nrow(df1), 916)
    expect_equal(sum(is.na(df1[grep("Wt%", names(df1), fixed = TRUE)])), 0)
  })
})

# ---- Anderson-Darling with equal values ----

test_that("all values equal gives statistic 0 and p = 1 instead of an error", {
  r <- ad_ksample_test(rep(2, 20), rep(c("a", "b"), 10), n_perm = 99)
  expect_equal(c(r$statistic, r$p_value, r$p_value_asymptotic), c(0, 1, 1))
  expect_equal(r$n, c(a = 10L, b = 10L))
  # one sample constant, the other not, still goes to kSamples
  expect_gt(ad_ksample_test(c(rep(2, 10), 1:10), rep(c("a", "b"), each = 10), n_perm = 99)$statistic, 0)
})

test_that("Batch Comparison runs when every particle has the same size", {
  b <- make_batches()
  for (n in names(b)) b[[n]][[BATCH_COLS$area]] <- pi / 4
  r <- run_batch_comparison(b, BATCH_COLS, list(n_perm = 99, n_boot = 100))
  expect_equal(r$size$overall$p_value, 1)
  expect_equal(r$size$pairs$p_value, c(1, 1, 1))
})
