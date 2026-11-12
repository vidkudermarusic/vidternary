# Tests for R/batch_composition.R: chemistry of several batches.

test_that("overlapping EDS lines among the chosen elements are named", {
  expect_equal(eds_overlaps(c("Ti", "N", "Fe")), "Ti - N")
  expect_equal(eds_overlaps(c("Cr", "O", "Ti", "V")), c("Cr - O", "Ti - V"))
  expect_equal(eds_overlaps(c("S", "Mo", "Pb", "Fe")), "S - Mo - Pb")
  expect_length(eds_overlaps(c("Ti", "Fe", "Cr", "Ni")), 0)
  expect_length(eds_overlaps(character(0)), 0)
})

test_that("pseudo-F and R2 match a case worked out by hand", {
  # x = 0, 1 | 2, 3: total SS 5, between 4, within 1 -> F = (4/1) / (1/2) = 8, R2 = 0.8
  t <- permanova_test(matrix(c(0, 1, 2, 3), ncol = 1), c("a", "a", "b", "b"), n_perm = 19)
  expect_equal(t$f, 8)
  expect_equal(t$r2, 0.8)
  expect_equal(t$df, c(1, 2))
})

test_that("the result matches vegan::adonis2 on Euclidean distances", {
  skip_if_not_installed("vegan")
  withr::with_seed(3, {
    coords <- matrix(stats::rnorm(180), ncol = 3)
    coords[41:60, 1] <- coords[41:60, 1] + 1
  })
  group <- rep(c("a", "b", "c"), each = 20)
  mine <- permanova_test(coords, group, n_perm = 99)
  ref <- withr::with_seed(1, vegan::adonis2(stats::dist(coords) ~ group, permutations = 99))
  expect_equal(mine$f, ref$F[1])
  expect_equal(mine$r2, ref$R2[1])
})

test_that("identical populations are not told apart, shifted ones are", {
  withr::with_seed(12, coords <- matrix(stats::rnorm(400), ncol = 2))
  group <- rep(c("a", "b"), each = 100)
  expect_gt(permanova_test(coords, group, n_perm = 299)$p_value, 0.05)
  shifted <- coords
  shifted[101:200, 2] <- shifted[101:200, 2] + 1
  t <- permanova_test(shifted, group, n_perm = 299)
  expect_equal(t$p_value, 1 / 300)
  expect_gt(t$r2, 0.1)
  expect_lt(t$r2, 1)
})

test_that("the permutation test is reproducible and refuses unusable input", {
  withr::with_seed(6, coords <- matrix(stats::rnorm(60), ncol = 2))
  group <- rep(c("a", "b", "c"), 10)
  expect_equal(permanova_test(coords, group, n_perm = 99)$p_value, permanova_test(coords, group, n_perm = 99)$p_value)
  expect_error(permanova_test(coords, rep("a", 30)), "At least 2 groups")
  expect_error(permanova_test(matrix(1, 4, 2), c("a", "a", "b", "b")), "identical")
  bad <- coords; bad[1, 1] <- NA
  expect_error(permanova_test(bad, group), "missing values")
})

test_that("batches of the same chemistry are not told apart, a Ti-rich batch is", {
  b <- make_batches()
  data <- do.call(rbind, b)
  batch <- rep(names(b), vapply(b, nrow, integer(1)))
  cmp <- compare_compositions(data, BATCH_COLS$parts, batch, n_perm = 199)

  expect_equal(cmp$n, c(A = 200L, B = 200L, C = 100L))
  expect_equal(paste(cmp$pairs$batch_a, cmp$pairs$batch_b), c("A B", "A C", "B C"))
  expect_gt(cmp$pairs$p_adjusted[1], 0.05)
  expect_lt(cmp$pairs$r2[1], 0.02)
  expect_equal(cmp$pairs$p_value[2:3], rep(1 / 200, 2))
  expect_gt(cmp$pairs$r2[2], 0.3)
  expect_equal(cmp$overall$p_value, 1 / 200)
  expect_equal(cmp$pairs$p_adjusted, stats::p.adjust(cmp$pairs$p_value, "holm"))
})

test_that("the clr means name the element that sets a batch apart", {
  b <- make_batches()
  data <- do.call(rbind, b)
  batch <- rep(names(b), vapply(b, nrow, integer(1)))
  m <- compare_compositions(data, BATCH_COLS$parts, batch, n_perm = 49)$clr_means
  expect_equal(unique(m$part), c("Ti", "Fe", "Cr"))      # element symbols, not column names
  diff <- function(batch, part) m$difference[m$batch == batch & m$part == part]
  expect_gt(diff("C", "Ti"), 0.2)
  expect_lt(diff("C", "Fe"), 0)
  expect_lt(diff("A", "Ti"), 0)
  # clr values sum to zero over the parts, so the differences of a batch do too
  expect_equal(sum(m$difference[m$batch == "C"]), 0, tolerance = 1e-10)
  # and the batch differences, weighted by size, cancel for each element
  expect_equal(200 * diff("A", "Ti") + 200 * diff("B", "Ti") + 100 * diff("C", "Ti"), 0, tolerance = 1e-8)
})

test_that("a batch that only differs in spread is flagged by the spread test", {
  withr::with_seed(17, {
    tight <- data.frame(a = 30 * exp(stats::rnorm(150, 0, 0.05)), b = 30 * exp(stats::rnorm(150, 0, 0.05)), c = 40)
    wide <- data.frame(a = 30 * exp(stats::rnorm(150, 0, 0.5)), b = 30 * exp(stats::rnorm(150, 0, 0.5)), c = 40)
  })
  cmp <- compare_compositions(rbind(tight, wide), c("a", "b", "c"), rep(c("tight", "wide"), each = 150), n_perm = 199)
  expect_equal(cmp$dispersion$p_value, 1 / 200)
  d <- cmp$dispersion$per_batch
  expect_gt(d$mean_distance[d$batch == "wide"], 5 * d$mean_distance[d$batch == "tight"])
  # parts that are not element columns keep their names
  expect_equal(unique(cmp$clr_means$part), c("a", "b", "c"))
})

test_that("zero readings are reported per part", {
  b <- make_batches()
  data <- do.call(rbind, b)
  data[["Cr.(Wt%)"]][1:100] <- 0
  cmp <- compare_compositions(data, BATCH_COLS$parts, rep(names(b), vapply(b, nrow, integer(1))), n_perm = 19)
  expect_equal(cmp$zeros$Zero_pct[cmp$zeros$Part == "Cr.(Wt%)"], 20)
  expect_equal(cmp$zeros$Zero_pct[cmp$zeros$Part == "Ti.(Wt%)"], 0)
})

test_that("too few parts or batches are refused", {
  b <- make_batches()
  expect_error(compare_compositions(b$A, "Ti.(Wt%)", rep("A", 200)), "At least 2 compositional parts")
  expect_error(compare_compositions(b$A, BATCH_COLS$parts, rep("A", 200)), "At least 2 batches")
})
