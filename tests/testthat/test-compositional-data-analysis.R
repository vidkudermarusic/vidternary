# Tests for R/compositional_data_analysis.R - CLR/ILR log-ratio transforms
# and the PCA built on top of them. Pure statistics, no Shiny dependency.

test_that("clr_transform requires at least 2 parts", {
  d <- data.frame(a = c(1, 2, 3))
  expect_error(clr_transform(d, "a"))
})

test_that("clr_transform produces rows that sum to (approximately) zero", {
  set.seed(1)
  d <- data.frame(a = stats::runif(20, 1, 10), b = stats::runif(20, 1, 10), c = stats::runif(20, 1, 10))
  clr <- clr_transform(d, c("a", "b", "c"))
  expect_true(all(abs(rowSums(clr)) < 1e-10))
})

test_that("clr_transform replaces zeros/NAs with a pseudo-count instead of erroring", {
  d <- data.frame(a = c(0, 2, 4), b = c(1, NA, 3), c = c(2, 2, 2))
  clr <- clr_transform(d, c("a", "b", "c"))
  expect_true(all(is.finite(as.matrix(clr))))
})

test_that("clr_transform errors when no positive values are present", {
  d <- data.frame(a = c(0, 0), b = c(NA, NA))
  expect_error(clr_transform(d, c("a", "b")))
})

test_that("ilr_transform requires at least 2 parts", {
  d <- data.frame(a = c(1, 2, 3))
  expect_error(ilr_transform(d, "a"))
})

test_that("ilr_transform's basis is orthonormal (V'V = I)", {
  parts <- c("a", "b", "c", "d", "e")
  d <- as.data.frame(matrix(stats::runif(10 * 5, 1, 10), ncol = 5, dimnames = list(NULL, parts)))
  ilr_res <- ilr_transform(d, parts)
  VtV <- t(ilr_res$basis) %*% ilr_res$basis
  expect_equal(unname(VtV), diag(length(parts) - 1), tolerance = 1e-10)
})

test_that("ilr_transform's basis exactly reconstructs the CLR coordinates", {
  set.seed(2)
  parts <- c("a", "b", "c", "d")
  d <- as.data.frame(matrix(stats::runif(20 * 4, 1, 10), ncol = 4, dimnames = list(NULL, parts)))
  clr <- clr_transform(d, parts)
  ilr_res <- ilr_transform(d, parts)
  reconstructed <- as.matrix(ilr_res$ilr) %*% t(ilr_res$basis)
  colnames(reconstructed) <- parts
  expect_equal(unname(as.matrix(clr)), unname(reconstructed), tolerance = 1e-10)
})

test_that("compositional_pca's variance explained sums to 100", {
  set.seed(3)
  d <- data.frame(a = stats::runif(30, 1, 10), b = stats::runif(30, 1, 10),
                   c = stats::runif(30, 1, 10), e = stats::runif(30, 1, 10))
  clr <- clr_transform(d, c("a", "b", "c", "e"))
  pca <- compositional_pca(clr)
  expect_equal(sum(pca$var_explained), 100, tolerance = 1e-8)
})

test_that("PCA on CLR and PCA on ILR give identical variance-explained (ILR is an isometry of CLR)", {
  # Documented invariant in compositional_data_analysis.R, relied on by
  # server_coda.R to justify running PCA separately on both bases. CLR has
  # D components with the last carrying ~0 variance (the CLR covariance
  # matrix is singular - rank D-1); ILR has D-1 genuine components that
  # should match CLR's first D-1 exactly.
  set.seed(4)
  parts <- c("a", "b", "c", "e")
  d <- as.data.frame(matrix(stats::runif(40 * 4, 1, 10), ncol = 4, dimnames = list(NULL, parts)))
  clr <- clr_transform(d, parts)
  ilr_res <- ilr_transform(d, parts)
  pca_clr <- compositional_pca(clr)
  pca_ilr <- compositional_pca(ilr_res$ilr)

  n_shared <- length(pca_ilr$var_explained)
  expect_equal(pca_clr$var_explained[seq_len(n_shared)], pca_ilr$var_explained, tolerance = 1e-6)
  expect_equal(pca_clr$var_explained[n_shared + 1], 0, tolerance = 1e-6)
})

test_that("create_coda_biplot returns a ggplot object", {
  set.seed(5)
  d <- data.frame(a = stats::runif(15, 1, 10), b = stats::runif(15, 1, 10), c = stats::runif(15, 1, 10))
  clr <- clr_transform(d, c("a", "b", "c"))
  pca <- compositional_pca(clr)
  p <- create_coda_biplot(pca)
  expect_s3_class(p, "ggplot")
})
