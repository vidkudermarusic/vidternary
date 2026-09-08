# Tests for R/spatial_point_pattern_analysis.R - Ripley's K/L functions,
# the nearest-neighbour G-function, the Monte Carlo CSR envelope test, and
# kernel intensity estimation, all built on spatstat.geom/spatstat.explore.
# Pure statistics, no Shiny dependency - same convention as
# test-spatial-clustering-analysis.R.

test_that("build_point_pattern requires at least 4 valid points", {
  expect_error(build_point_pattern(c(1, 2, 3), c(1, 2, 3)), "At least 4")
})

test_that("build_point_pattern rejects collinear/degenerate points with a clear message, not spatstat's raw internal error", {
  # Before this file's own defensive tryCatch(), spatstat.geom::convexhull.xy()
  # throws its own raw internal assertion ("is.owin(w) is not TRUE") on fully
  # collinear input rather than returning a zero-area window - confirmed
  # directly. Both the all-Y-constant case and a genuinely diagonal-collinear
  # case are checked, since they reach the failure through different paths.
  expect_error(build_point_pattern(c(1, 2, 3, 4), c(5, 5, 5, 5)),
               "non-zero area")
  expect_error(build_point_pattern(c(1, 2, 3, 4), c(1, 2, 3, 4)),
               "non-zero area")
})

test_that("build_point_pattern drops non-finite coordinates before building the pattern", {
  # The 4 surviving (finite-in-both) points - (1,1), (2,4), (3,2), (4,5) -
  # are deliberately non-collinear, unlike an earlier version of this test
  # which happened to leave exactly 4 points all on the line y=x and so
  # tripped the *other* new validation (non-zero area) instead of actually
  # exercising the non-finite-filtering behavior this test means to check.
  x <- c(1, 2, 3, 4, NA, 6)
  y <- c(1, 4, 2, 5, 5, NaN)
  pp <- build_point_pattern(x, y)
  expect_equal(spatstat.geom::npoints(pp), 4)
})

test_that("build_point_pattern's window is the convex hull, with a real positive area", {
  set.seed(1)
  x <- stats::runif(30, 0, 10); y <- stats::runif(30, 0, 10)
  pp <- build_point_pattern(x, y)
  area <- spatstat.geom::area.owin(spatstat.geom::Window(pp))
  expect_gt(area, 0)
  # The convex hull of points spread over [0,10]x[0,10] can't exceed the
  # bounding box's own area (100) - a real, checkable upper bound, not just
  # "some positive number".
  expect_lte(area, 100)
})

test_that("compute_ripley_k returns the CSR expectation pi*r^2 and a real observed column", {
  set.seed(1)
  x <- stats::runif(50, 0, 10); y <- stats::runif(50, 0, 10)
  pp <- build_point_pattern(x, y)
  k <- compute_ripley_k(pp)
  expect_true(all(c("r", "theo", "obs", "correction") %in% names(k)))
  expect_equal(k$theo, pi * k$r^2, tolerance = 1e-10)
  expect_true(all(is.finite(k$obs[-1])))  # r=0 row's obs can be exactly 0/0-derived; the rest must be real numbers
})

test_that("compute_l_function's obs_minus_r is exactly obs - r, and theo is CSR's r", {
  set.seed(1)
  x <- stats::runif(50, 0, 10); y <- stats::runif(50, 0, 10)
  pp <- build_point_pattern(x, y)
  l <- compute_l_function(pp)
  expect_true(all(c("r", "theo", "obs", "obs_minus_r", "correction") %in% names(l)))
  expect_equal(l$theo, l$r, tolerance = 1e-10)
  expect_equal(l$obs_minus_r, l$obs - l$r, tolerance = 1e-12)
})

test_that("compute_g_function returns a real CSR-comparable nearest-neighbour CDF", {
  set.seed(1)
  x <- stats::runif(50, 0, 10); y <- stats::runif(50, 0, 10)
  pp <- build_point_pattern(x, y)
  g <- compute_g_function(pp)
  expect_true(all(c("r", "theo", "obs", "correction") %in% names(g)))
  # Both G(r) curves are CDFs: non-decreasing, bounded in [0, 1] wherever
  # finite (a few trailing rows can be NaN/Inf past the data's own range -
  # a genuine spatstat behavior, not a bug in this wrapper, so checked only
  # where finite).
  finite_obs <- g$obs[is.finite(g$obs)]
  expect_true(all(finite_obs >= -1e-10 & finite_obs <= 1 + 1e-10))
  expect_true(all(diff(finite_obs) >= -1e-10))
})

test_that("compute_csr_envelope validates nsim the same way compute_isolation_forest() validates ntrees", {
  set.seed(1)
  x <- stats::runif(30, 0, 10); y <- stats::runif(30, 0, 10)
  pp <- build_point_pattern(x, y)
  expect_error(compute_csr_envelope(pp, nsim = NA_real_), "nsim must be a single positive whole number")
  expect_error(compute_csr_envelope(pp, nsim = -5), "nsim must be a single positive whole number")
  expect_error(compute_csr_envelope(pp, nsim = 2.5), "nsim must be a single positive whole number")
  expect_no_error(compute_csr_envelope(pp, nsim = 19))
})

test_that("compute_csr_envelope is reproducible with the same seed and differs with a different one", {
  set.seed(1)
  x <- stats::runif(40, 0, 10); y <- stats::runif(40, 0, 10)
  pp <- build_point_pattern(x, y)

  env1 <- compute_csr_envelope(pp, nsim = 19, seed = 11)
  env2 <- compute_csr_envelope(pp, nsim = 19, seed = 11)
  expect_equal(env1$lo, env2$lo)
  expect_equal(env1$hi, env2$hi)

  env3 <- compute_csr_envelope(pp, nsim = 19, seed = 999)
  expect_false(isTRUE(all.equal(env1$lo, env3$lo)))
})

test_that("compute_csr_envelope's _minus_r columns are exactly the corresponding column minus r, and doesn't perturb the caller's RNG state", {
  set.seed(1)
  x <- stats::runif(30, 0, 10); y <- stats::runif(30, 0, 10)
  pp <- build_point_pattern(x, y)

  # Same convention already verified for clark_evans_test()'s own Monte
  # Carlo simulation: the caller's global RNG stream should be untouched
  # after the call, not just "some other value".
  set.seed(123)
  before <- stats::runif(1)
  set.seed(123)
  env <- compute_csr_envelope(pp, nsim = 19, seed = 42)
  after <- stats::runif(1)
  expect_equal(before, after)

  expect_equal(env$obs_minus_r, env$obs - env$r, tolerance = 1e-12)
  expect_equal(env$lo_minus_r, env$lo - env$r, tolerance = 1e-12)
  expect_equal(env$hi_minus_r, env$hi - env$r, tolerance = 1e-12)
})

test_that("compute_kernel_intensity returns a finite x/y/intensity grid", {
  set.seed(1)
  x <- stats::runif(40, 0, 10); y <- stats::runif(40, 0, 10)
  pp <- build_point_pattern(x, y)
  dens <- compute_kernel_intensity(pp)
  expect_true(all(c("x", "y", "intensity") %in% names(dens)))
  expect_gt(nrow(dens), 0)
  expect_true(all(is.finite(dens$intensity)))
  expect_true(all(dens$intensity >= 0))
})

test_that("a tight double-clump pattern shows real clustering: L(r) - r positive at short range", {
  # Same real-configuration-correctness spirit as
  # test-spatial-clustering-analysis.R's own clump/grid checks - not just
  # confirming the function runs, but that it gives the statistically
  # correct verdict on a known, unambiguous pattern.
  set.seed(1)
  clump1 <- data.frame(x = stats::rnorm(25, 0, 0.05), y = stats::rnorm(25, 0, 0.05))
  clump2 <- data.frame(x = stats::rnorm(25, 5, 0.05), y = stats::rnorm(25, 5, 0.05))
  d <- rbind(clump1, clump2)
  pp <- build_point_pattern(d$x, d$y)
  l <- compute_l_function(pp)
  # At short distances (within a clump's own tight scale), a clustered
  # pattern has far more neighbours than CSR predicts - L(r) - r should be
  # clearly positive there.
  short_range <- l[l$r > 0 & l$r < 0.2, ]
  expect_gt(nrow(short_range), 0)
  expect_true(mean(short_range$obs_minus_r) > 0)
})

test_that("a regular grid shows real regularity: L(r) - r negative at short range", {
  g <- expand.grid(x = seq(0, 10, by = 1), y = seq(0, 10, by = 1))
  pp <- build_point_pattern(g$x, g$y)
  l <- compute_l_function(pp)
  short_range <- l[l$r > 0 & l$r < 1, ]
  expect_gt(nrow(short_range), 0)
  expect_true(mean(short_range$obs_minus_r) < 0)
})

test_that("all six plotting functions return real ggplot objects", {
  set.seed(1)
  x <- stats::runif(40, 0, 10); y <- stats::runif(40, 0, 10)
  mark <- sample(c("A", "B"), 40, replace = TRUE)
  pp <- build_point_pattern(x, y)

  k <- compute_ripley_k(pp)
  l <- compute_l_function(pp)
  g <- compute_g_function(pp)
  env <- compute_csr_envelope(pp, nsim = 19)
  dens <- compute_kernel_intensity(pp)

  expect_s3_class(create_point_pattern_plot(x, y), "ggplot")
  expect_s3_class(create_point_pattern_plot(x, y, mark, "Type"), "ggplot")
  expect_s3_class(create_ripley_k_plot(k), "ggplot")
  expect_s3_class(create_l_function_plot(l), "ggplot")
  expect_s3_class(create_g_function_plot(g), "ggplot")
  expect_s3_class(create_csr_envelope_plot(env), "ggplot")
  expect_s3_class(create_kernel_intensity_plot(dens), "ggplot")
})
