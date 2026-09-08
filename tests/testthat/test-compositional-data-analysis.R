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

test_that("clr_transform replaces an Inf/-Inf value the same way as zero/NA instead of poisoning the whole row", {
  # Before this fix, .coda_replace_zeros() only guarded is.na()/<=0, missing
  # non-finite values - the same guard server_spatial.R's combined_data()
  # and extreme_value_analysis.R's compute_block_maxima() already apply to
  # their own numeric inputs. A single Inf didn't just corrupt its own
  # cell: log(Inf) fed into this transform's per-row mean, so rowMeans()
  # picked up that Inf and the ENTIRE row went non-finite (NaN for the Inf
  # column itself, -Inf for every other column in that row).
  d <- data.frame(a = c(1, 2, Inf, 4), b = c(2, 3, 4, 5), c = c(3, 4, 5, 6))
  clr <- clr_transform(d, c("a", "b", "c"))
  expect_true(all(is.finite(as.matrix(clr))))
  # A regression check that -Inf is caught too (it happened to already be
  # caught by the old `mat <= 0` condition, but confirmed here so it stays
  # covered alongside the +Inf fix rather than assumed).
  d2 <- data.frame(a = c(1, 2, -Inf, 4), b = c(2, 3, 4, 5), c = c(3, 4, 5, 6))
  clr2 <- clr_transform(d2, c("a", "b", "c"))
  expect_true(all(is.finite(as.matrix(clr2))))
})

test_that("clr_transform errors when no positive values are present", {
  d <- data.frame(a = c(0, 0), b = c(NA, NA))
  expect_error(clr_transform(d, c("a", "b")))
})

test_that("zero replacement is per-column, not one value shared across every part", {
  # Column "a" is a major element (values ~60), column "b" a trace element
  # (values ~0.01) - a single dataset-wide minimum would use b's tiny scale
  # to fill a's zero, which is exactly the miscalibration this fix removes.
  # Confirmed here by reconstructing what each column's own replacement
  # should independently be (half its own smallest positive value) and
  # checking clr_transform() actually used those two different numbers,
  # not one value borrowed from the other column.
  d <- data.frame(a = c(0, 58, 62, 60), b = c(0.02, 0, 0.03, 0.01), c = c(2, 2, 2, 2))
  a_repl <- min(d$a[d$a > 0]) / 2  # 29
  b_repl <- min(d$b[d$b > 0]) / 2  # 0.005
  expect_true(a_repl != b_repl)   # sanity: the two scales really do differ

  clr <- clr_transform(d, c("a", "b", "c"))
  # Reconstruct what raw "a"/"b" values (post zero-replacement) would need
  # to be, from the CLR output, and confirm they land on each column's OWN
  # replacement value, not the other column's (or a shared global one).
  # Row 1 has a's own zero (a=0, b=0.02); row 2 has b's own zero (a=58,
  # b=0) - each checked against the row's actual other values, not
  # against the other column's replacement.
  row1_reconstructed <- unlist(clr[1, ]) + log(prod(c(a_repl, d$b[1], d$c[1]))^(1/3))
  expect_equal(unname(row1_reconstructed["a"]), log(a_repl), tolerance = 1e-9)
  row2_reconstructed <- unlist(clr[2, ]) + log(prod(c(d$a[2], b_repl, d$c[2]))^(1/3))
  expect_equal(unname(row2_reconstructed["b"]), log(b_repl), tolerance = 1e-9)
})

test_that("an explicit single zero_replacement value still applies uniformly (back-compat)", {
  d <- data.frame(a = c(0, 58, 62, 60), b = c(0.02, 0, 0.03, 0.01), c = c(2, 2, 2, 2))
  clr <- clr_transform(d, c("a", "b", "c"), zero_replacement = 5)
  row1_reconstructed <- unlist(clr[1, ]) + log(prod(c(5, d$b[1], d$c[1]))^(1/3))
  expect_equal(unname(row1_reconstructed["a"]), log(5), tolerance = 1e-9)
  row2_reconstructed <- unlist(clr[2, ]) + log(prod(c(d$a[2], 5, d$c[2]))^(1/3))
  expect_equal(unname(row2_reconstructed["b"]), log(5), tolerance = 1e-9)
})

test_that("a column that is entirely zero/NA gets a clear per-column error, not a silent Inf", {
  d <- data.frame(a = c(0, 0, 0), b = c(1, 2, 3), c = c(2, 2, 2))
  err <- tryCatch(clr_transform(d, c("a", "b", "c")), error = function(e) e$message)
  expect_true(is.character(err))
  expect_true(grepl("a", err, fixed = TRUE))
})

test_that("ilr_transform also uses per-column zero replacement (shares .coda_replace_zeros with clr_transform)", {
  d <- data.frame(a = c(0, 58, 62, 60), b = c(0.02, 0, 0.03, 0.01), c = c(2, 2, 2, 2))
  ilr_res <- ilr_transform(d, c("a", "b", "c"))
  expect_true(all(is.finite(as.matrix(ilr_res$ilr))))
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
