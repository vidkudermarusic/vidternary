# Tests for R/spatial_clustering_analysis.R - Clark-Evans nearest-neighbour
# test, both the "kdtree" (RANN::nn2) and "matrix" (stats::dist) nearest-
# neighbour methods. Pure statistics, no Shiny dependency.

test_that("compute_nearest_neighbor_distances requires at least 2 points", {
  expect_error(compute_nearest_neighbor_distances(1, 1))
})

test_that("compute_nearest_neighbor_distances is correct on a known configuration", {
  # 3 collinear points at x = 0, 1, 3: nearest-neighbour distances are 1, 1, 2.
  # "matrix" carries stats::dist()'s row-index names; "kdtree" doesn't - an
  # attribute-only difference the callers never rely on (both are always
  # reduced via mean()), so compare values only.
  x <- c(0, 1, 3); y <- c(0, 0, 0)
  expect_equal(unname(compute_nearest_neighbor_distances(x, y, method = "matrix")), c(1, 1, 2))
  expect_equal(unname(compute_nearest_neighbor_distances(x, y, method = "kdtree")), c(1, 1, 2))
})

test_that("kdtree and matrix methods give identical (not just close) nearest-neighbour distances", {
  set.seed(1)
  x <- stats::runif(200); y <- stats::runif(200)
  d_mat <- compute_nearest_neighbor_distances(x, y, method = "matrix")
  d_kd <- compute_nearest_neighbor_distances(x, y, method = "kdtree")
  expect_equal(unname(d_mat), unname(d_kd), tolerance = 1e-10)
})

test_that("clark_evans_test requires at least 3 points spanning a non-zero area", {
  expect_error(clark_evans_test(c(1, 2), c(1, 2)))
  expect_error(clark_evans_test(c(1, 1, 1), c(2, 2, 2)))
})

test_that("clark_evans_test drops non-finite coordinates before testing", {
  x <- c(1, 2, 3, NA, 5)
  y <- c(1, 2, 3, 4, NaN)
  ce <- clark_evans_test(x, y, n_sim = 50)
  expect_equal(ce$n, 3)
})

test_that("clark_evans_test flags two tight, well-separated clumps as clustered", {
  set.seed(1)
  clump1 <- data.frame(x = stats::rnorm(30, 0, 0.01), y = stats::rnorm(30, 0, 0.01))
  clump2 <- data.frame(x = stats::rnorm(30, 10, 0.01), y = stats::rnorm(30, 10, 0.01))
  d <- rbind(clump1, clump2)
  ce <- clark_evans_test(d$x, d$y, n_sim = 99, nn_method = "kdtree")
  expect_true(ce$R < 1)
  expect_true(ce$p_value_monte_carlo < 0.05)
  expect_match(ce$verdict, "CLUSTERED")
})

test_that("clark_evans_test flags a regular grid as dispersed", {
  g <- expand.grid(x = seq(0, 10, by = 1), y = seq(0, 10, by = 1))
  ce <- clark_evans_test(g$x, g$y, n_sim = 99, nn_method = "kdtree")
  expect_true(ce$R > 1)
  expect_match(ce$verdict, "REGULAR/DISPERSED")
})

test_that("window = \"rectangle\" is the default and is reported back unchanged", {
  set.seed(10)
  x <- stats::runif(60); y <- stats::runif(60)
  ce <- clark_evans_test(x, y, n_sim = 50)
  expect_equal(ce$window, "rectangle")
  # Default rectangle area is the bounding box.
  expect_equal(ce$area, (max(x) - min(x)) * (max(y) - min(y)))
})

test_that("window = \"convex_hull\" uses the hull's (smaller) area and changes the null model", {
  skip_if_not_installed("spatstat.geom")
  # Points filling only a triangular corner of their bounding box: the
  # convex hull is much smaller than the box, so intensity is higher and
  # the expected NND under CSR is smaller - a real, directional change.
  set.seed(11)
  x <- stats::runif(200); y <- stats::runif(200)
  keep <- y < x                       # lower-right triangle, ~half the box
  x <- x[keep]; y <- y[keep]

  ce_box  <- clark_evans_test(x, y, n_sim = 99, seed = 42, window = "rectangle")
  ce_hull <- clark_evans_test(x, y, n_sim = 99, seed = 42, window = "convex_hull")

  expect_equal(ce_hull$window, "convex_hull")
  expect_lt(ce_hull$area, ce_box$area)            # hull is strictly smaller
  expect_gt(ce_hull$density, ce_box$density)      # so intensity is higher
  # Same points, same NNDs, but a smaller expected-NND baseline -> the
  # hull's R is larger (less "clustered-looking") than the box's.
  expect_equal(ce_hull$Dobs, ce_box$Dobs)
  expect_gt(ce_hull$R, ce_box$R)
})

test_that("window = \"convex_hull\" rejects collinear points with a clear message", {
  skip_if_not_installed("spatstat.geom")
  expect_error(
    clark_evans_test(c(1, 2, 3, 4, 5), c(1, 2, 3, 4, 5), n_sim = 10, window = "convex_hull"),
    "collinear"
  )
})

test_that("clark_evans_test's R and Monte Carlo p-value agree exactly between nn_methods", {
  # Both methods compute exact nearest-neighbour distances, and the same
  # seed drives the same simulated point sets, so the two algorithms
  # (only a runtime optimization) must produce identical statistics.
  set.seed(1)
  x <- stats::runif(150); y <- stats::runif(150)
  ce_kd <- clark_evans_test(x, y, n_sim = 99, seed = 42, nn_method = "kdtree")
  ce_mat <- clark_evans_test(x, y, n_sim = 99, seed = 42, nn_method = "matrix")
  expect_equal(ce_kd$R, ce_mat$R, tolerance = 1e-10)
  expect_equal(ce_kd$p_value_monte_carlo, ce_mat$p_value_monte_carlo, tolerance = 1e-10)
})

test_that("clark_evans_test's default n_sim depends on nn_method", {
  set.seed(1)
  x <- stats::runif(100); y <- stats::runif(100)
  expect_equal(clark_evans_test(x, y, nn_method = "kdtree")$n_sim, 999)
  expect_equal(clark_evans_test(x, y, nn_method = "matrix")$n_sim, 299)
})

test_that("create_spatial_scatter_plot and create_nnd_histogram return ggplot objects", {
  set.seed(1)
  x <- stats::runif(50); y <- stats::runif(50)
  p1 <- create_spatial_scatter_plot(x, y)
  expect_s3_class(p1, "ggplot")

  p2 <- create_spatial_scatter_plot(x, y, color_by = stats::runif(50), color_label = "Value")
  expect_s3_class(p2, "ggplot")

  ce <- clark_evans_test(x, y, n_sim = 50)
  p3 <- create_nnd_histogram(ce)
  expect_s3_class(p3, "ggplot")
})
