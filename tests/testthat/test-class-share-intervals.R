# Tests for R/class_share_intervals.R: share of each class with an interval.

test_that("counts and shares are right and sorted by count", {
  s <- class_share_intervals(c(rep("TiN", 6), rep("oxide", 3), "sulfide", NA))
  expect_equal(s$class, c("TiN", "oxide", "sulfide"))
  expect_equal(s$n, c(6, 3, 1))
  expect_equal(s$share_pct, c(60, 30, 10))
  expect_true(all(s$lower_pct < s$share_pct & s$share_pct < s$upper_pct))
  expect_equal(unique(s$method), "Wilson (independent particles)")
})

test_that("without fields the interval is the Wilson score interval", {
  s <- class_share_intervals(c(rep("a", 30), rep("b", 70)))
  wilson <- stats::prop.test(30, 100, correct = FALSE)$conf.int
  expect_equal(c(s$lower_pct[s$class == "a"], s$upper_pct[s$class == "a"]), 100 * as.numeric(wilson), tolerance = 1e-6)
  # a class that makes up everything keeps an interval inside 0..100
  all_one <- class_share_intervals(rep("a", 20))
  expect_equal(all_one$upper_pct, 100)
  expect_lt(all_one$lower_pct, 100)
})

test_that("with fields the interval comes from resampling fields and brackets the share", {
  withr::with_seed(5, {
    field <- sample(1:60, 600, replace = TRUE)
    class <- sample(c("a", "b", "c"), 600, replace = TRUE, prob = c(0.6, 0.3, 0.1))
  })
  s <- class_share_intervals(class, field, n_boot = 500)
  expect_equal(unique(s$method), "bootstrap over fields")
  expect_true(all(s$lower_pct < s$share_pct & s$share_pct < s$upper_pct))
  expect_equal(sum(s$share_pct), 100)
  # independent particles: about as wide as the Wilson interval
  wilson <- class_share_intervals(class)
  width <- function(d) (d$upper_pct - d$lower_pct)[d$class == "a"]
  expect_lt(abs(width(s) / width(wilson) - 1), 0.35)
})

test_that("a class that sits in one field gets a far wider interval than Wilson gives", {
  # 40 fields of 10 particles 'a'; one field holds 100 particles 'b' (a cluster)
  field <- c(rep(1:40, each = 10), rep(41, 100))
  class <- c(rep("a", 400), rep("b", 100))
  boot <- class_share_intervals(class, field, n_boot = 1000)
  wilson <- class_share_intervals(class)
  b <- function(d) d[d$class == "b", ]
  expect_equal(b(boot)$share_pct, 20)
  expect_gt(b(boot)$upper_pct - b(boot)$lower_pct, 3 * (b(wilson)$upper_pct - b(wilson)$lower_pct))
  # the cluster may be missing from a resample altogether
  expect_equal(b(boot)$lower_pct, 0)
})

test_that("the bootstrap is reproducible and leaves the caller's random stream alone", {
  field <- rep(1:20, each = 5)
  class <- rep(c("a", "b", "a", "a", "b"), 20)
  class[1:10] <- "b"
  set.seed(1); before <- stats::runif(1)
  set.seed(1)
  first <- class_share_intervals(class, field, n_boot = 300)
  expect_equal(stats::runif(1), before)
  expect_equal(class_share_intervals(class, field, n_boot = 300), first)
})

test_that("missing classes or fields are left out, and one field falls back to Wilson", {
  s <- class_share_intervals(c("a", "a", NA, "b", "b"), c(1, 2, 3, NA, 4))
  expect_equal(sum(s$n), 3)
  one_field <- class_share_intervals(c("a", "a", "b"), c(7, 7, 7))
  expect_equal(unique(one_field$method), "Wilson (independent particles)")
})

test_that("no particles gives an empty table with all columns", {
  s <- class_share_intervals(character(0))
  expect_equal(nrow(s), 0)
  expect_equal(names(s), c("class", "n", "share_pct", "lower_pct", "upper_pct", "method"))
  expect_equal(nrow(class_share_intervals(c(NA, NA))), 0)
})

test_that("inclusion classes get shares with an interval, by fields when the file has them", {
  d <- incl_data(incl_row(list(Al2O3 = 70), steel = 30), incl_row(list(Al2O3 = 70), steel = 30),
                 incl_row(list(MgO = 20, Al2O3 = 50), steel = 30), incl_row(list(CaO = 30, Al2O3 = 40), steel = 30))
  r <- analyze_inclusion_preset(d, "cao-al2o3-mgo")
  plain <- inclusion_class_shares(r, d, n_boot = 200)
  expect_equal(sum(plain$n), sum(r$keep))
  expect_equal(sum(plain$share_pct), 100)
  expect_equal(unique(plain$method), "Wilson (independent particles)")
  d$Field <- c(1, 2, 3, 4)
  by_field <- inclusion_class_shares(r, d, n_boot = 200)
  expect_equal(unique(by_field$method), "bootstrap over fields")
  expect_equal(by_field$n, plain$n)
})
