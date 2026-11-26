# Tests for R/acceptance_criteria.R: does the specimen meet the limits?
# make_cleanliness_data() (helper-cleanliness.R) gives 32 particles on
# 2 mm2 with the ratings A fine 0.5, B fine 0.5, C thick 0.5, D fine 1.0
# and DS 0.5.

clean_result <- function() run_cleanliness_analysis(make_cleanliness_data(), CLEAN_COLS, list(n_boot = 300))

test_that("chart-index limits are read as typed", {
  expect_null(parse_rating_limits(""))
  expect_null(parse_rating_limits(NULL))
  expect_equal(parse_rating_limits("A=2, DS=1"), c(`A fine` = 2, `A thick` = 2, DS = 1))
  # one series, lower case, semicolons; the result is in report order
  expect_equal(parse_rating_limits("ds = 1.5; d fine=0.5 ; B thick = 1"), c(`B thick` = 1, `D fine` = 0.5, DS = 1.5))
  # a series named after its type overrides the type
  expect_equal(parse_rating_limits("B=2, B fine=1"), c(`B fine` = 1, `B thick` = 2))
  expect_error(parse_rating_limits("E=1"), "Cannot read 'E=1'")
  expect_error(parse_rating_limits("A=1,5"), "Cannot read")
  expect_error(parse_rating_limits("DS fine=1"), "DS has no fine or thick series")
})

test_that("only the limits that were given are kept", {
  expect_length(acceptance_limits(), 0)
  expect_equal(acceptance_limits(per_mm2 = 20, area_ppm = NA, largest_um = NULL), list(per_mm2 = 20))
  lim <- acceptance_limits(return_level_um = 30, ratings = c(DS = 1))
  expect_equal(names(lim), c("return_level_um", "ratings"))
  expect_error(acceptance_limits(per_mm2 = -1), "cannot be negative")
})

test_that("a value is judged against its limit and its interval", {
  expect_equal(.acceptance_verdict(5, 10, 3, 8), "pass")
  expect_equal(.acceptance_verdict(5, 10, 3, 12), "pass, not certain")
  expect_equal(.acceptance_verdict(12, 10, 9, 15), "fail, not certain")
  expect_equal(.acceptance_verdict(12, 10, 11, 15), "fail")
  expect_equal(.acceptance_verdict(10, 10), "pass")              # at the limit is within it
  expect_equal(.acceptance_verdict(12, 10), "fail")              # no interval: the value alone
  expect_equal(.acceptance_verdict(NA_real_, 10), "not assessed")
})

test_that("each limit gets a row with the measured value", {
  res <- clean_result()
  all_row <- res$indices[res$indices$group == "All", ]
  a <- assess_acceptance(res, acceptance_limits(per_mm2 = 1000, area_ppm = 1e6, largest_um = 100,
                                                ratings = parse_rating_limits("A=1, D fine=0.5, DS=0.5")))
  t <- a$table
  expect_equal(t$criterion, c("Particles per mm2", "Area fraction (ppm)", "Largest inclusion found, sqrt(area) (um)",
                              "Chart index A fine, worst field", "Chart index A thick, worst field",
                              "Chart index D fine, worst field", "Chart index DS, worst field"))
  expect_equal(t$value, c(all_row$per_mm2, all_row$area_ppm, all_row$max_sqrt_area_um, 0.5, 0, 1, 0.5))
  expect_equal(t$verdict, c("pass", "pass", "pass", "pass", "pass", "fail", "pass"))
  expect_equal(c(t$lower[2], t$upper[2]), c(all_row$area_ppm_lower, all_row$area_ppm_upper))
  expect_true(all(is.na(t$lower[3:7])))
  expect_equal(a$verdict, "fail")
  expect_equal(a$summary, "Does not meet the limits: Chart index D fine, worst field.")
})

test_that("a limit inside the interval is not a certain verdict", {
  res <- clean_result()
  all_row <- res$indices[res$indices$group == "All", ]
  just_above <- assess_acceptance(res, acceptance_limits(per_mm2 = all_row$per_mm2 + 1))
  expect_equal(just_above$verdict, "pass, not certain")
  expect_match(just_above$summary, "not beyond the uncertainty of the measurement for: Particles per mm2")
  just_below <- assess_acceptance(res, acceptance_limits(per_mm2 = all_row$per_mm2 - 1))
  expect_equal(just_below$verdict, "fail, not certain")
  expect_match(just_below$summary, "A larger inspected area would decide it")
  expect_equal(assess_acceptance(res, acceptance_limits(per_mm2 = 10 * all_row$per_mm2))$summary, "Meets all limits.")
})

test_that("the density interval is widened when the particles are clustered", {
  d <- make_cleanliness_data()
  d$Field <- rep(c(1, 2, 3, 4), c(26, 2, 2, 2))             # most particles in one of 4 fields
  res <- run_cleanliness_analysis(d, CLEAN_COLS, list(n_boot = 300))
  all_row <- res$indices[res$indices$group == "All", ]
  t <- assess_acceptance(res, acceptance_limits(per_mm2 = 100))$table
  disp <- stats::var(c(26, 2, 2, 2)) / 8
  expect_match(t$interval, sprintf("widened for clustering (index of dispersion %.1f)", disp), fixed = TRUE)
  expect_gt(t$upper - t$lower, all_row$per_mm2_upper - all_row$per_mm2_lower)
  expect_equal(t$upper, all_row$per_mm2 + stats::qnorm(0.975) * sqrt(disp * 32) / res$inspected_area_mm2)

  # without field IDs the interval is the Poisson one, and says what it assumes
  no_field <- run_cleanliness_analysis(d, CLEAN_COLS[names(CLEAN_COLS) != "field"])
  t2 <- assess_acceptance(no_field, acceptance_limits(per_mm2 = 100, area_ppm = 1e6))$table
  expect_equal(t2$interval[1], "Poisson (assumes randomly scattered particles)")
  expect_equal(t2$interval[2], "none (needs a field ID column)")
  expect_equal(t2$verdict[2], "pass")
})

test_that("the extreme-value limit uses the return level and its interval", {
  res <- clean_result()
  evs <- data.frame(return_level = 20, ci_lower = -2, ci_upper = 26, problem = NA_character_)
  t <- assess_acceptance(res, acceptance_limits(return_level_um = 25), evs)$table
  expect_equal(t$value, 20)
  expect_equal(c(t$lower, t$upper), c(0, 26))                 # a size cannot be negative
  expect_equal(t$verdict, "pass, not certain")

  failed <- data.frame(return_level = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_, problem = "fewer than 3 control areas")
  a <- assess_acceptance(res, acceptance_limits(return_level_um = 25, per_mm2 = 1000), failed)
  expect_equal(a$table$verdict, c("pass", "not assessed"))
  expect_equal(a$table$interval[2], "no extreme-value fit: fewer than 3 control areas")
  expect_equal(a$verdict, "not assessed")
  expect_match(a$summary, "not assessed: Largest inclusion expected")
  none <- assess_acceptance(res, acceptance_limits(return_level_um = 25))
  expect_equal(none$table$interval, "no extreme-value fit: needs a SEM field ID column")
})

test_that("acceptance_evs() is the EVS tab's fit on one specimen", {
  withr::local_seed(5)
  d <- make_batch(400)
  e <- acceptance_evs(d[[BATCH_COLS$area]], d$Field, return_period = 50, n_boot = 200)
  ref <- batch_evs_table(list(specimen = d), BATCH_COLS$area, "Field", 1, 50, n_boot = 200)
  expect_equal(e$return_level, ref$return_level)
  expect_equal(e$n_control_areas, 20L)
  # too few control areas: the reason, not an error
  expect_match(acceptance_evs(d[[BATCH_COLS$area]], d$Field, fields_per_area = 10)$problem, "fewer than 3 control areas")
})

test_that("no limits give an empty assessment and a hint", {
  a <- assess_acceptance(clean_result(), acceptance_limits())
  expect_equal(a$verdict, "no limits")
  expect_equal(nrow(a$table), 0)
  expect_match(acceptance_display_table(a)$Note, "Enter one or more limits")
})

test_that("the display table is readable", {
  a <- assess_acceptance(clean_result(), acceptance_limits(per_mm2 = 1000, ratings = c(DS = 0.5)))
  d <- acceptance_display_table(a)
  expect_equal(names(d), c("Criterion", "Limit (at most)", "Measured", "95% interval", "Interval from", "Verdict"))
  expect_equal(d$`Limit (at most)`, c("1000", "0.5"))
  expect_equal(d$`95% interval`[2], "-")
  expect_match(d$`95% interval`[1], "^\\[[0-9.]+, [0-9.]+\\]$")
})

test_that("the cleanliness report carries the acceptance section", {
  res <- clean_result()
  a <- assess_acceptance(res, acceptance_limits(per_mm2 = 1000, ratings = c(`D fine` = 0.5)))
  html <- cleanliness_report(res, acceptance = a)
  expect_match(html, "<h2>Acceptance limits</h2>", fixed = TRUE)
  expect_match(html, "Does not meet the limits: Chart index D fine, worst field.", fixed = TRUE)
  expect_no_match(cleanliness_report(res), "<h2>Acceptance limits</h2>", fixed = TRUE)
  expect_no_match(cleanliness_report(res, acceptance = assess_acceptance(res, acceptance_limits())), "<h2>Acceptance limits</h2>", fixed = TRUE)
})
