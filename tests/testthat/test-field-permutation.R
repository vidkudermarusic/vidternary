# Tests for R/field_permutation.R: size and composition tests that permute
# whole SEM fields instead of single particles.

# Two or three batches drawn from ONE population in which particles of a
# field share a field effect (clustering): size on the log scale and the
# Ti share of the chemistry both vary from field to field.
clustered_batches <- function(seed, n_batches = 2, n_fields = 20, per_field = 15, field_sd = 0.6, shift = 0) {
  withr::with_seed(seed, {
    rows <- lapply(seq_len(n_batches), function(b) {
      effect <- stats::rnorm(n_fields, 0, field_sd)
      field <- rep(seq_len(n_fields), each = per_field)
      log_size <- log(2) + effect[field] + (b == n_batches) * shift + stats::rnorm(length(field), 0, 0.3)
      ti <- pmin(pmax(40 + 15 * effect[field] + (b == n_batches) * 30 * shift + stats::rnorm(length(field), 0, 5), 1), 90)
      cr <- (100 - ti) * 0.2
      data.frame(batch = LETTERS[b], field = field, size = exp(log_size),
                 Ti = ti, Fe = 100 - ti - cr, Cr = cr, stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
  })
}

test_that("the app's own statistic agrees with kSamples, so field and particle tests use the same one", {
  withr::with_seed(5, x <- c(stats::rnorm(15), stats::rnorm(12, 0.5), stats::rnorm(9, -0.3)))
  g <- rep(c("a", "b", "c"), c(15, 12, 9))
  for (values in list(x, round(x * 2) / 2, round(stats::setNames(exp(x), NULL), 1))) {
    distinct <- sort(unique(values))
    index <- match(values, distinct)
    own <- .ad_ksample_statistic(index, as.integer(factor(g)), 3, tabulate(index, nbins = length(distinct)))
    ref <- kSamples::ad.test(split(values, g), method = "asymptotic")
    from_ksamples <- unclass(ref$ad)[2, 2] * ref$sig + 2       # version 2, taken back from T.AD
    expect_equal(own, from_ksamples, tolerance = 1e-4)
    expect_equal(ad_field_permutation_test(values, g, seq_along(values), n_perm = 9)$statistic, own)
  }
})

test_that("with one particle per field the field test is the particle test", {
  withr::with_seed(7, x <- c(stats::rlnorm(40), stats::rlnorm(40, 0.4)))
  g <- rep(c("a", "b"), each = 40)
  by_field <- ad_field_permutation_test(x, g, seq_along(x), n_perm = 499, seed = 3)
  by_particle <- ad_ksample_test(x, g, n_perm = 499, seed = 3)
  expect_equal(by_field$statistic, by_particle$statistic, tolerance = 1e-4)
  # different random streams, the same test: p-values within Monte Carlo error (about 0.02 each at 499 permutations)
  expect_lt(abs(by_field$p_value - by_particle$p_value), 0.06)
  expect_equal(by_field$n_fields, c(a = 40L, b = 40L))
})

test_that("particles of one field move together", {
  d <- clustered_batches(1)
  r <- ad_field_permutation_test(d$size, d$batch, d$field, n_perm = 99)
  expect_equal(r$n_fields, c(A = 20L, B = 20L))
  expect_equal(r$n, c(A = 300L, B = 300L))
  expect_equal(r$unit, "SEM fields")
  # the same field number in two batches is two fields
  expect_equal(length(unique(.field_units(d$field, d$batch))), 40)
  # a particle without a field ID is a unit of its own
  expect_equal(length(unique(.field_units(c(1, 1, NA, NA), c("a", "a", "a", "a")))), 3)
  # same seed, same p; the caller's random stream is untouched
  set.seed(1); before <- stats::runif(1); set.seed(1)
  first <- ad_field_permutation_test(d$size, d$batch, d$field, n_perm = 99, seed = 4)
  expect_equal(stats::runif(1), before)
  expect_equal(ad_field_permutation_test(d$size, d$batch, d$field, n_perm = 99, seed = 4)$p_value, first$p_value)
})

test_that("with clustered particles the particle test rejects far too often, the field test does not", {
  # 60 pairs of batches from ONE population, clustered by field
  res <- t(vapply(1:60, function(i) {
    d <- clustered_batches(100 + i)
    c(field = ad_field_permutation_test(d$size, d$batch, d$field, n_perm = 99, seed = i)$p_value,
      particle = ad_ksample_test(d$size, d$batch, n_perm = 99, seed = i)$p_value)
  }, numeric(2)))
  field_rate <- mean(res[, "field"] <= 0.05)
  particle_rate <- mean(res[, "particle"] <= 0.05)
  expect_lt(field_rate, 0.15)          # nominal 5 %
  expect_gt(particle_rate, 0.30)       # pseudoreplication
})

test_that("a real difference is still found by the field test", {
  d <- clustered_batches(9, shift = 1)
  expect_lt(ad_field_permutation_test(d$size, d$batch, d$field, n_perm = 199)$p_value, 0.01)
  m <- permanova_field_test(ilr_transform(d, c("Ti", "Fe", "Cr"))$ilr, d$batch, d$field, n_perm = 199)
  expect_lt(m$p_value, 0.01)
})

test_that("the MANOVA with fields permuted has the particle test's F and the nominal error rate", {
  d <- clustered_batches(2)
  coords <- ilr_transform(d, c("Ti", "Fe", "Cr"))$ilr
  f <- permanova_field_test(coords, d$batch, d$field, n_perm = 99)
  p <- permanova_test(coords, d$batch, n_perm = 99)
  expect_equal(c(f$f, f$r2), c(p$f, p$r2))
  expect_equal(f$df, p$df)
  expect_equal(f$n_fields, c(A = 20L, B = 20L))
  rates <- t(vapply(1:40, function(i) {
    d <- clustered_batches(300 + i)
    coords <- ilr_transform(d, c("Ti", "Fe", "Cr"))$ilr
    c(field = permanova_field_test(coords, d$batch, d$field, n_perm = 99, seed = i)$p_value,
      particle = permanova_test(coords, d$batch, n_perm = 99, seed = i)$p_value)
  }, numeric(2)))
  expect_lt(mean(rates[, "field"] <= 0.05), 0.15)
  expect_gt(mean(rates[, "particle"] <= 0.05), 0.30)
})

test_that("bad input is refused, equal values give p = 1", {
  expect_error(ad_field_permutation_test(1:4, c("a", "a", "b", "b"), 1:3), "One SEM field ID is needed per value")
  expect_error(permanova_field_test(matrix(1:8, 4), c("a", "a", "b", "b"), 1:3), "One SEM field ID is needed per row")
  r <- ad_field_permutation_test(rep(3, 10), rep(c("a", "b"), 5), 1:10, n_perm = 9)
  expect_equal(c(r$statistic, r$p_value), c(0, 1))
})

test_that("Batch Comparison permutes fields by default, particles when asked or without a field column", {
  b <- make_batches()
  st <- list(n_perm = 99, n_boot = 100)
  by_field <- run_batch_comparison(b, BATCH_COLS, st)
  expect_equal(by_field$permutation_unit, "SEM fields")
  expect_equal(by_field$size$overall$n_fields, c(A = 20L, B = 20L, C = 20L))
  expect_match(by_field$notes$notes, "permute whole SEM fields between the batches", fixed = TRUE, all = FALSE)
  tests <- batch_result_tables(by_field)$`Overall tests`$test
  expect_match(tests, "Anderson-Darling, permutation of SEM fields", fixed = TRUE, all = FALSE)
  expect_match(tests, "pseudo-F, permutation of SEM fields", fixed = TRUE, all = FALSE)
  # the real differences of batch C are still found; A and B are alike
  expect_lt(by_field$size$pairs$p_value[by_field$size$pairs$batch_b == "C"][1], 0.05)
  expect_gt(by_field$size$pairs$p_value[1], 0.05)

  by_particle <- run_batch_comparison(b, BATCH_COLS, c(st, permute = "particle"))
  expect_equal(by_particle$permutation_unit, "particles")
  expect_match(by_particle$notes$notes, "these p-values are too small. Choose \"whole SEM fields\"", fixed = TRUE, all = FALSE)
  # the statistic itself does not depend on the unit
  expect_equal(by_field$size$overall$statistic, by_particle$size$overall$statistic, tolerance = 1e-4)
  expect_equal(by_field$composition$overall$f, by_particle$composition$overall$f)

  no_field <- run_batch_comparison(b, BATCH_COLS[names(BATCH_COLS) != "field"], st)
  expect_equal(no_field$permutation_unit, "particles")
  expect_match(no_field$notes$notes, "Choose a SEM field ID column to permute whole fields instead", fixed = TRUE, all = FALSE)
})

test_that("few fields are warned about", {
  b <- make_batches()
  for (n in names(b)) b[[n]]$Field <- pmin(b[[n]]$Field, 4)       # four fields per batch
  r <- run_batch_comparison(b, BATCH_COLS, list(n_perm = 99, n_boot = 100))
  expect_match(r$notes$warnings, "Few SEM fields hold particles in: A (4), B (4), C (4)", fixed = TRUE, all = FALSE)
})
