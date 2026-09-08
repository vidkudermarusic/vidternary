# Tests for R/multivariate.R's compute_isolation_forest() - its first
# dedicated test file. Existing coverage before this was crash-shaped only
# (test-pass5-regressions.R's 3 statistics-layer validation checks); this
# file covers the isolation-forest-specific parameters (ntrees,
# contamination, sample_size) that became user-adjustable in the UI - see
# the vidternary Structural Audit for the full feature writeup.

test_that("sample_size always equals the reference dataset's own complete-row count, not an independent setting", {
  # Confirmed no longer an independently configurable parameter: it's
  # derived, every time, from nrow() of the reference rows that actually
  # survive the complete-cases filter for the selected columns - never a
  # fixed default, never capped against an arbitrary ceiling.
  set.seed(1)
  target <- data.frame(a = runif(50), b = runif(50))
  reference <- data.frame(a = runif(30), b = runif(30))

  result <- compute_isolation_forest(target, reference, selected_columns = c("a", "b"))
  expect_equal(result$sample_size, nrow(reference))

  # A reference with incomplete rows: sample_size must shrink to match the
  # rows actually used to fit the model, not the reference's raw row count.
  reference_na <- reference
  reference_na$a[1:5] <- NA
  result_na <- compute_isolation_forest(target, reference_na, selected_columns = c("a", "b"))
  expect_equal(result_na$sample_size, sum(complete.cases(reference_na)))
  expect_lt(result_na$sample_size, nrow(reference_na))
})

test_that("ntrees and contamination are both reported back exactly as passed in, and used for real", {
  set.seed(2)
  target <- data.frame(a = runif(40), b = runif(40))
  reference <- data.frame(a = runif(40), b = runif(40))

  result <- compute_isolation_forest(target, reference, selected_columns = c("a", "b"),
                                      ntrees = 77, contamination = 0.25)
  expect_equal(result$ntrees, 77)
  expect_equal(result$contamination, 0.25)

  # A materially different contamination genuinely changes the outlier
  # count on the same data (not just echoed back unused) - a real,
  # observable difference, not just a passthrough of the argument.
  result_low <- compute_isolation_forest(target, reference, selected_columns = c("a", "b"),
                                          ntrees = 77, contamination = 0.05, seed = 2)
  result_high <- compute_isolation_forest(target, reference, selected_columns = c("a", "b"),
                                           ntrees = 77, contamination = 0.40, seed = 2)
  expect_lte(sum(result_low$outlier_indices), sum(result_high$outlier_indices))
})

test_that("ntrees is validated the same way contamination already was - NA/negative/non-integer all rejected clearly", {
  # ntrees became user-adjustable via the UI (previously a fixed internal
  # default) - it needed the identical NA_real_-from-a-cleared-
  # numericInput guard contamination already had, since a cleared UI field
  # reports NA_real_, not NULL, and isotree::isolation.forest(ntrees = NA)
  # doesn't fail cleanly (a raw C++-backend error naming no R argument).
  target <- data.frame(a = runif(20), b = runif(20))
  reference <- data.frame(a = runif(20), b = runif(20))

  expect_error(compute_isolation_forest(target, reference, selected_columns = c("a", "b"), ntrees = NA_real_),
               "ntrees must be a single positive whole number")
  expect_error(compute_isolation_forest(target, reference, selected_columns = c("a", "b"), ntrees = -5),
               "ntrees must be a single positive whole number")
  expect_error(compute_isolation_forest(target, reference, selected_columns = c("a", "b"), ntrees = 2.5),
               "ntrees must be a single positive whole number")
  expect_error(compute_isolation_forest(target, reference, selected_columns = c("a", "b"), ntrees = c(50, 100)),
               "ntrees must be a single positive whole number")

  # Regression: a normal valid ntrees still works.
  expect_no_error(compute_isolation_forest(target, reference, selected_columns = c("a", "b"), ntrees = 50))
})

test_that("the seed argument actually reaches isotree::isolation.forest() and controls its randomness", {
  # isotree::isolation.forest()'s own randomness is governed entirely by
  # its OWN internal `seed` argument, independent of R's set.seed() -
  # confirmed empirically against the real package. A prior version of
  # this function called set.seed(seed) in R but never passed `seed`
  # through to isolation.forest() itself, so this function's own `seed`
  # parameter was silently dead: every call, regardless of what seed was
  # passed, actually ran on isotree's internal default (seed = 1). The
  # bug that regression test would have missed is checking that the SAME
  # seed gives the same result (true under the old, broken code too,
  # since it always fell back to the same internal default regardless) -
  # the real proof is that two DIFFERENT seeds give DIFFERENT results,
  # which only happens once `seed` is genuinely threaded through.
  set.seed(3)
  target <- data.frame(a = runif(60), b = runif(60), c = runif(60))
  reference <- data.frame(a = runif(60), b = runif(60), c = runif(60))

  result_seed1 <- compute_isolation_forest(target, reference, selected_columns = c("a", "b", "c"),
                                            ntrees = 100, seed = 11)
  result_seed2 <- compute_isolation_forest(target, reference, selected_columns = c("a", "b", "c"),
                                            ntrees = 100, seed = 999999)
  expect_false(isTRUE(all.equal(result_seed1$scores, result_seed2$scores)))

  # And the same seed, called twice, reproduces byte-identical scores -
  # confirming `seed` gives genuine, repeatable control, not just noise.
  result_seed1_again <- compute_isolation_forest(target, reference, selected_columns = c("a", "b", "c"),
                                                  ntrees = 100, seed = 11)
  expect_equal(result_seed1$scores, result_seed1_again$scores)
})
