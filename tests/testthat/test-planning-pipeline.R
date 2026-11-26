# Tests for R/planning_pipeline.R and R/planning_plots.R: the measurement
# plan from a pilot run. make_batch() (helper-batch.R) gives n particles on
# exactly 2 mm2 in 20 SEM fields.

PLAN_COLS <- BATCH_COLS[c("area", "field", "x", "y")]

test_that("the pilot summary holds what was measured", {
  res <- plan_measurement(list(pilot = make_batch(200)), PLAN_COLS, list(n_sim = 50))
  p <- res$pilot
  expect_equal(p$runs, 1L)
  expect_equal(p$n, 200)
  expect_equal(p$inspected_area_mm2, 2)
  expect_equal(p$per_mm2, 100)
  expect_equal(c(p$per_mm2_lower, p$per_mm2_upper), as.numeric(poisson_count_ci(200)) / 2)
  expect_equal(p$n_fields, 20L)
  expect_equal(p$field_area_mm2, 0.1)
  # the index of dispersion is that of the counts per field
  counts <- as.integer(table(factor(make_batch(200)$Field, levels = 1:20)))
  expect_equal(p$field_dispersion, stats::var(counts) / mean(counts))
  expect_equal(p$dispersion_used, max(1, p$field_dispersion))
  expect_equal(p$rel_halfwidth_now, density_relative_halfwidth(2, 100, p$dispersion_used))
  expect_null(res$repeatability)
})

test_that("the plan tables follow from the pilot numbers", {
  res <- plan_measurement(list(pilot = make_batch(200)), PLAN_COLS, list(n_sim = 50, target_precision_pct = 7, target_ratio = 1.3))
  d <- res$pilot$dispersion_used
  expect_equal(res$precision$precision_pct, c(50, 30, 20, 10, 7, 5))
  expect_equal(res$precision$area_mm2, area_for_density_precision(100, c(50, 30, 20, 10, 7, 5) / 100, d))
  expect_equal(res$precision$chosen, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(res$precision$fields, ceiling(res$precision$area_mm2 / 0.1))
  expect_equal(res$precision$particles_expected, 100 * res$precision$area_mm2)
  expect_equal(res$precision$times_pilot_area, res$precision$area_mm2 / 2)
  expect_equal(res$ratio$ratio, c(2, 1.5, 1.3, 1.2, 1.1))
  expect_equal(res$ratio$area_mm2, area_for_density_ratio(100, c(2, 1.5, 1.3, 1.2, 1.1), d))
  expect_equal(res$ratio$chosen, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  # the curve passes through the pilot point
  near <- which.min(abs(res$curve$area_mm2 - 2))
  expect_equal(res$curve$rel_halfwidth[near], res$pilot$rel_halfwidth_now, tolerance = 0.05)
})

test_that("clustered particles make every area larger, and the plan says so", {
  d <- make_batch(400)
  d$Field <- ifelse(d$Field <= 4, d$Field, 1)       # most particles in one field
  res <- plan_measurement(list(pilot = d), PLAN_COLS[c("area", "field")], list(n_sim = 50, inspected_area_mm2 = 2))
  expect_gt(res$pilot$field_dispersion, 2)
  expect_equal(res$precision$area_mm2[res$precision$chosen],
               res$pilot$field_dispersion * area_for_density_precision(200, 0.1))
  expect_match(res$notes$warnings, "the particles are clustered", all = FALSE)
  expect_equal(unique(res$curve$model)[2], "if the particles were randomly scattered")
})

test_that("without a field column the plan assumes random scatter and warns", {
  res <- plan_measurement(list(pilot = make_batch(200)), PLAN_COLS[c("area", "x", "y")], list(n_sim = 50))
  expect_true(is.na(res$pilot$field_dispersion))
  expect_equal(res$pilot$dispersion_used, 1)
  expect_true(all(is.na(res$precision$fields)))
  expect_null(res$evs)
  expect_match(res$notes$warnings, "No SEM field ID column", all = FALSE)
})

test_that("a small pilot run is flagged", {
  res <- plan_measurement(list(pilot = make_batch(20)), PLAN_COLS, list(n_sim = 50))
  expect_match(res$notes$warnings, "The pilot run holds 20 particles", all = FALSE)
})

test_that("the largest-inclusion plan starts at the pilot and narrows", {
  res <- plan_measurement(list(pilot = make_batch(400)), PLAN_COLS, list(n_sim = 200, return_period = 50))
  expect_equal(res$evs$n_areas, 20L * c(1, 2, 4, 8, 16))
  expect_equal(res$evs$times_pilot, c(1, 2, 4, 8, 16))
  expect_equal(res$evs$area_mm2, res$evs$n_areas * 0.1)
  expect_true(all(diff(res$evs$halfwidth_um) < 0))
  # too few control areas: no plan, with the reason
  few <- plan_measurement(list(pilot = make_batch(400)), PLAN_COLS, list(n_sim = 50, fields_per_area = 10))
  expect_null(few$evs)
  expect_match(few$notes$notes, "No plan for the largest-inclusion estimate: fewer than 3 control areas", all = FALSE)
})

test_that("several runs are pooled and their repeatability reported", {
  runs <- list(first = make_batch(200, seed = 1), second = make_batch(200, seed = 2))
  res <- plan_measurement(runs, PLAN_COLS, list(n_sim = 50))
  expect_equal(res$pilot$runs, 2L)
  expect_equal(res$pilot$n, 400)
  expect_equal(res$pilot$inspected_area_mm2, 4)
  expect_equal(res$pilot$n_fields, 40L)
  expect_equal(res$per_run$run, c("first", "second"))
  expect_equal(res$repeatability$density$runs$per_mm2, c(100, 100))
  expect_equal(res$repeatability$quantities$quantity,
               c("Area fraction (ppm)", "Median ECD (um)", "90th percentile ECD (um)", "Largest sqrt(area) (um)"))
  expect_match(res$notes$notes, "agree within counting scatter", all = FALSE)
  expect_match(res$notes$notes, "With 2 runs the scatter between runs is poorly known", all = FALSE)

  # a run with half the particles is not a repeat
  differ <- plan_measurement(list(first = make_batch(200, seed = 1), second = make_batch(100, seed = 2)), PLAN_COLS, list(n_sim = 50))
  expect_match(differ$notes$warnings, "differ by more than counting scatter explains (p < 0.001)", fixed = TRUE, all = FALSE)
})

test_that("a filter does not shrink the inspected area or the number of fields", {
  all_runs <- list(pilot = make_batch(200))
  big <- lapply(all_runs, function(d) d[d[[BATCH_COLS$area]] > 3, ])
  res <- plan_measurement(big, PLAN_COLS, list(n_sim = 50), all_runs = all_runs)
  expect_equal(res$pilot$inspected_area_mm2, 2)
  expect_equal(res$pilot$n_fields, 20L)
  expect_equal(res$pilot$n, nrow(big$pilot))
})

test_that("unusable input is refused with a message", {
  expect_error(plan_measurement(list(), PLAN_COLS), "Upload the pilot run")
  expect_error(plan_measurement(list(a = make_batch(50)), list(area = "nope")), "Choose the area column")
  expect_error(plan_measurement(list(a = make_batch(50)), PLAN_COLS, list(target_ratio = 1)), "other than 1")
  expect_error(plan_measurement(list(a = make_batch(50)), PLAN_COLS, list(target_precision_pct = 0)), "greater than 0 %")
  expect_error(plan_measurement(list(a = make_batch(50)), PLAN_COLS["area"]), "Enter the inspected area")
})

test_that("the display tables, the export and the plot are complete", {
  runs <- list(first = make_batch(200, seed = 1), second = make_batch(200, seed = 2))
  res <- plan_measurement(runs, PLAN_COLS, list(n_sim = 50))
  tab <- planning_display_tables(res)
  expect_equal(names(tab), c("pilot", "precision", "ratio", "evs", "runs", "repeat_density", "repeat_other"))
  expect_equal(tab$pilot$Value[tab$pilot$Quantity == "Particles per mm2"], "100.0")
  expect_equal(tab$precision[[6]][tab$precision[[1]] == "+/- 10 %"], "<- your target")
  expect_equal(names(tab$ratio)[2], "Area per batch (mm2)")
  expect_match(tab$repeat_density$Value[6], "per mm2 \\([0-9]+ %\\)")
  expect_equal(nrow(tab$repeat_other), 4)

  one <- planning_display_tables(plan_measurement(runs["first"], PLAN_COLS["area"], list(n_sim = 50, inspected_area_mm2 = 2)))
  expect_match(one$repeat_density$Note, "two or more runs")
  expect_equal(one$evs$Note, "Needs a SEM field ID column.")

  sheets <- planning_result_tables(res)
  expect_equal(names(sheets), c("Pilot", "Runs", "Area for precision", "Area for ratio", "Largest inclusion",
                                "Repeatability density", "Repeatability other", "Notes", "Settings"))
  expect_true(all(vapply(sheets, is.data.frame, logical(1))))
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(sheets, path)
  expect_true(file.exists(path))

  built <- ggplot2::ggplot_build(create_precision_curve(res))
  expect_equal(built$plot$labels$x, "Inspected area (mm2)")
})
