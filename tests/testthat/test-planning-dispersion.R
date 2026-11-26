# Measurement Planning: the index of dispersion is pooled within the
# repeated runs, not over one pile of all their counts per field (audit
# pass 7, medium finding "Measurement Planning overstates clustering when
# the uploaded runs differ"), and a plan without a measurable dispersion
# says so (pass 7, low finding).

PD_COLS <- list(area = "Area", field = "Field")

# One run with the given particle counts per field (fields 1..length(counts);
# the first and last count must be > 0 so every field is counted).
pd_run <- function(counts) {
  n <- sum(counts)
  # varied areas so the largest-inclusion fit is not degenerate; they do not
  # enter the index of dispersion
  data.frame(Field = rep(seq_along(counts), counts), Area = 2 + (seq_len(n) * 7) %% 11, check.names = FALSE)
}
pd_plan <- function(runs, area, ...) {
  plan_measurement(runs, PD_COLS, list(n_sim = 20, inspected_area_mm2 = area, ...))
}
old_pooled <- function(counts) { x <- unlist(counts); stats::var(x) / mean(x) }

test_that("pooled_within_dispersion() is the run's own index for one run and a weighted mean for several", {
  a <- c(3, 7, 5, 1, 9, 5)
  b <- c(20, 12, 15, 18, 10, 16, 14)
  expect_equal(pooled_within_dispersion(list(a))$dispersion, stats::var(a) / mean(a))
  w <- c((length(a) - 1) * mean(a), (length(b) - 1) * mean(b))
  d_ab <- c(stats::var(a) / mean(a), stats::var(b) / mean(b))
  res <- pooled_within_dispersion(list(a, b))
  expect_equal(res$dispersion, sum(w * d_ab) / sum(w))
  expect_equal(res$n_fields, 13)
  expect_equal(res$runs_used, 2L)
})

test_that("a shift between the runs' densities is not counted as clustering", {
  a <- c(4, 6, 5, 5, 3, 7, 5, 5)
  res <- pooled_within_dispersion(list(a, a + 10))
  # within each run the counts vary the same; only the means differ
  expect_lt(res$dispersion, 0.5)
  # one pile: the between-run variance (5^2) over the mean (10) is added
  expect_gt(old_pooled(list(a, a + 10)), 2.5)
})

test_that("runs that cannot give an index add nothing, and none at all gives NA", {
  expect_equal(pooled_within_dispersion(list(c(2, 4, 6), 7, c(0, 0)))$dispersion,
               stats::var(c(2, 4, 6)) / 4)
  expect_identical(pooled_within_dispersion(list(5, c(0, 0)))$dispersion, NA_real_)
  expect_equal(pooled_within_dispersion(list(5, c(0, 0)))$runs_used, 0L)
})

test_that("plan_measurement() pools within the runs: the audit case of 0.73 and 1.14", {
  withr::with_seed(3, {
    a <- stats::rpois(60, 4)
    b <- stats::rpois(60, 9)
  })
  a[c(1, 60)] <- pmax(a[c(1, 60)], 1)
  b[c(1, 60)] <- pmax(b[c(1, 60)], 1)
  res <- pd_plan(list(A = pd_run(a), B = pd_run(b)), c(6, 6))
  per_run <- res$per_run$field_dispersion
  expect_equal(per_run, c(stats::var(a) / mean(a), stats::var(b) / mean(b)))
  expect_equal(res$pilot$field_dispersion, pooled_within_dispersion(list(a, b))$dispersion)
  # between the two runs' own values, far below the one-pile value
  expect_gte(res$pilot$field_dispersion, min(per_run))
  expect_lte(res$pilot$field_dispersion, max(per_run))
  expect_gt(old_pooled(list(a, b)), 1.5 * res$pilot$field_dispersion)
  expect_equal(res$pilot$n_fields, 120)
  # the runs really differ, and the repeatability test finds it
  expect_lt(res$repeatability$density$p_value, 0.001)
})

test_that("one run gives the same index as before (variance over mean of its fields)", {
  x <- c(2, 5, 0, 3, 8, 1, 4, 6)
  res <- pd_plan(list(only = pd_run(x)), 0.8)
  expect_equal(res$pilot$field_dispersion, stats::var(x) / mean(x))
})

test_that("runs with different field sizes are not mixed", {
  # same density, run B at higher magnification: 4 times the fields, each a
  # quarter of the area
  withr::with_seed(5, {
    a <- stats::rpois(30, 20)
    b <- stats::rpois(120, 5)
  })
  b[c(1, 120)] <- pmax(b[c(1, 120)], 1)
  res <- pd_plan(list(A = pd_run(a), B = pd_run(b)), c(3, 3))
  expect_lt(res$pilot$field_dispersion, 1.5)
  expect_gt(old_pooled(list(a, b)), 5)
})

test_that("simulation: runs with different densities, no clustering", {
  # 300 pairs of runs, 40 fields each, Poisson (no clustering)
  sim <- function(m1, m2, k = 40) t(vapply(seq_len(300), function(i) {
    withr::with_seed(100 + i, { a <- stats::rpois(k, m1); b <- stats::rpois(k, m2) })
    new_d <- max(1, pooled_within_dispersion(list(a, b))$dispersion)
    old_d <- max(1, old_pooled(list(a, b)))
    p <- function(d) density_repeatability(c(sum(a), sum(b)), c(4, 4), d)$p_value
    c(new_d = new_d, old_d = old_d, new_p = p(new_d), old_p = p(old_d))
  }, numeric(4)))
  big <- sim(10, 15)
  # the new index stays near 1, the old one takes the difference as clustering
  expect_lt(mean(big[, "new_d"]), 1.15)
  expect_gt(mean(big[, "old_d"]), 1.4)
  # The old index hid part of a real difference from the repeatability test,
  # most with few dense fields (its inflation is about chi-squared / fields):
  # 5 fields of ~100 particles, 15 % apart - found in 0.55 of the runs with
  # the new index, 0.43 with the old one.
  few <- sim(100, 115, k = 5)
  expect_gt(mean(few[, "new_p"] < 0.05) - mean(few[, "old_p"] < 0.05), 0.06)
  # and with equal runs the test keeps its level
  expect_lt(mean(sim(10, 10)[, "new_p"] < 0.05), 0.08)
  expect_lt(mean(sim(100, 100, k = 5)[, "new_p"] < 0.05), 0.08)
})

test_that("a plan without a measurable index of dispersion warns", {
  res <- pd_plan(list(only = pd_run(25)), 0.1)
  expect_true(is.na(res$pilot$field_dispersion))
  expect_equal(res$pilot$dispersion_used, 1)
  expect_true(any(grepl("clustering could not be measured", res$notes$warnings)))
  expect_true(any(grepl("fewer than 2 SEM fields", res$notes$warnings)))
  two <- pd_plan(list(A = pd_run(25), B = pd_run(30)), c(0.1, 0.1))
  expect_true(any(grepl("no run has at least 2 SEM fields", two$notes$warnings)))
})

test_that("runs that differ beyond counting give the best precision one run can reach", {
  withr::with_seed(9, { a <- stats::rpois(50, 6); b <- stats::rpois(50, 12) })
  a[c(1, 50)] <- pmax(a[c(1, 50)], 1)
  b[c(1, 50)] <- pmax(b[c(1, 50)], 1)
  res <- pd_plan(list(A = pd_run(a), B = pd_run(b)), c(5, 5))
  w <- grep("differ by more than counting", res$notes$warnings, value = TRUE)
  expect_length(w, 1)
  rd <- res$repeatability$density
  floor_pct <- 100 * stats::qnorm(0.975) * rd$sd_excess / rd$mean_per_mm2
  expect_match(w, sprintf("below about +/- %.0f %%", floor_pct), fixed = TRUE)
})

test_that("the display table names the within-run index when there are several runs", {
  x <- c(2, 5, 1, 3)
  one <- planning_display_tables(pd_plan(list(only = pd_run(x)), 0.4))$pilot$Quantity
  two <- planning_display_tables(pd_plan(list(A = pd_run(x), B = pd_run(x + 1)), c(0.4, 0.4)))$pilot$Quantity
  expect_true("Index of dispersion (counts per field)" %in% one)
  expect_true("Index of dispersion (counts per field, within the runs)" %in% two)
})
