# Plot Builder: a continuous column must not become the grouping X axis of a
# violin, box or bar chart (one group per value froze the app for minutes).

make_builder_data <- function(n = 200) {
  set.seed(1)
  data.frame(
    source_file = rep(c("A", "B"), length.out = n),
    Class = rep(c("oxide", "sulfide", "nitride", "other"), length.out = n),
    Field = rep(1:10, length.out = n),
    ECD = stats::rlnorm(n, 1, 0.5),
    all_na = NA_real_,
    stringsAsFactors = FALSE
  )
}

test_that("builder_group_columns() keeps text and few-valued numeric columns, drops continuous ones", {
  d <- make_builder_data()
  cols <- builder_group_columns(d)
  expect_true(all(c("source_file", "Class", "Field") %in% cols))
  expect_false("ECD" %in% cols)
  expect_false("all_na" %in% cols)
  # text columns come before numeric ones
  expect_lt(match("Class", cols), match("Field", cols))
})

test_that("builder_group_columns() respects max_groups, also for text columns", {
  d <- make_builder_data()
  d$id <- paste0("p", seq_len(nrow(d)))
  expect_false("id" %in% builder_group_columns(d))
  expect_false("Field" %in% builder_group_columns(d, max_groups = 5))
  expect_true("Field" %in% builder_group_columns(d, max_groups = 10))
  expect_identical(builder_group_columns(data.frame()), character(0))
})

test_that("build_custom_plot() refuses a continuous X on violin, box and bar", {
  d <- make_builder_data()
  for (type in c("violin", "box")) {
    expect_error(build_custom_plot(d, type, x = "ECD", y = "ECD"), "different values")
  }
  expect_error(build_custom_plot(d, "bar", x = "ECD"), "limit 50")
})

test_that("build_custom_plot() still draws grouped charts and continuous charts", {
  d <- make_builder_data()
  expect_s3_class(build_custom_plot(d, "violin", x = "source_file", y = "ECD"), "ggplot")
  expect_s3_class(build_custom_plot(d, "box", x = "Field", y = "ECD"), "ggplot")
  expect_s3_class(build_custom_plot(d, "bar", x = "Class"), "ggplot")
  # histogram and scatter take a continuous X as before
  expect_s3_class(build_custom_plot(d, "hist", x = "ECD"), "ggplot")
  expect_s3_class(build_custom_plot(d, "scatter", x = "ECD", y = "Field"), "ggplot")
  # a larger limit can be passed on purpose
  expect_s3_class(build_custom_plot(d, "bar", x = "ECD", max_groups = 1000), "ggplot")
})

test_that("the refusal is fast on many particles (no plot is built)", {
  d <- make_builder_data(20000)
  t <- system.time(expect_error(build_custom_plot(d, "violin", x = "ECD", y = "ECD")))[["elapsed"]]
  expect_lt(t, 2)
})
