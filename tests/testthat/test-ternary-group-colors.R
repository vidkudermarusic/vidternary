test_that("group colours depend on the group names, not on order or repeats", {
  a <- group_colors_by_name(c("B", "A", "C", "A"))
  b <- group_colors_by_name(c("C", "C", "B", "A"))
  expect_equal(a, b)
  expect_equal(names(a), c("A", "B", "C"))
  expect_equal(unname(a), TERNARY_GROUP_PALETTE[1:3])
  expect_equal(names(group_colors_by_name(c("F10", "F2", "F1"))), c("F1", "F2", "F10"))
  expect_length(group_colors_by_name(character(0)), 0)
  expect_equal(names(group_colors_by_name(c("A", NA, "B"))), c("A", "B"))
})

test_that("more groups than palette colours still get distinct colours", {
  cols <- group_colors_by_name(paste0("G", 1:20))
  expect_length(cols, 20)
  expect_equal(length(unique(cols)), 20)
  expect_equal(length(unique(TERNARY_GROUP_PALETTE)), length(TERNARY_GROUP_PALETTE))
})

test_that("the palette avoids the pale colours of the old Set3 default", {
  luminance <- colSums(grDevices::col2rgb(TERNARY_GROUP_PALETTE) / 255 * c(0.299, 0.587, 0.114))
  expect_true(all(luminance < 0.75))
})

test_that("compute_point_styling gives a group the same colour whatever the row order or selection", {
  style <- function(g, selected) {
    n <- length(g)
    tp <- data.frame(A = rep(0.3, n), B = rep(0.3, n), C = rep(0.4, n))
    compute_point_styling(tp, data.frame(G = g), NULL, "point_size", list(col = "G"), "blue",
                          FALSE, 1, TRUE, selected)
  }
  g1 <- c("A", "B", "C", "A", "B")
  g2 <- c("C", "B", "A", "C")
  s1 <- style(g1, c("A", "B", "C"))
  s2 <- style(g2, c("A", "B", "C"))
  expect_equal(unname(s1$pointCol[match("A", g1)]), unname(s2$pointCol[match("A", g2)]))
  expect_equal(unname(s1$pointCol[match("C", g1)]), unname(s2$pointCol[match("C", g2)]))

  # Selecting only some groups leaves their colours unchanged.
  s3 <- style(g1, c("B", "C"))
  expect_equal(unname(s3$pointCol[1]), unname(s1$pointCol[2]))
  expect_equal(unname(s3$group_colors), unname(s1$pointCol[c(2, 3)]))
})
