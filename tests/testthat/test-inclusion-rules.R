# Classification rules, nearest-phase classes and their boundary lines.

lib <- load_inclusion_presets()
lab3 <- c("CaO", "Al2O3", "MgO")
pts <- function(...) {
  m <- rbind(...)
  data.frame(A = m[, 1], B = m[, 2], C = m[, 3])
}

test_that("conditions are parsed from a fixed grammar", {
  cs <- parse_rule_conditions("MgO>=0.30; CaO/(CaO+Al2O3) < 0.15", lab3)
  expect_equal(cs[[1]], list(kind = "threshold", k = 3L, op = ">=", value = 0.3))
  expect_equal(cs[[2]], list(kind = "ratio", i = 1L, j = 2L, op = "<", value = 0.15))
})

test_that("unreadable or unsafe rule text is rejected with a clear message", {
  expect_error(parse_rule_conditions("Foo<0.1", lab3), "'Foo' is not a corner")
  expect_error(parse_rule_conditions("MgO ~ 0.3", lab3), "Cannot read the condition")
  expect_error(parse_rule_conditions("CaO/(MgO+Al2O3)<0.3", lab3), "must have the form X/\\(X\\+Y\\)")
  expect_error(parse_rule_conditions("CaO/(CaO+CaO)<0.3", lab3), "two different corners")
  expect_error(parse_rule_conditions("system('x')>0", lab3), "Cannot read the condition")
  expect_error(parse_rule_conditions("MgO>0.1;;", lab3), "Empty rule condition")
})

test_that("conditions are evaluated on the fractions; 0/0 counts as not satisfied", {
  d <- pts(c(0.2, 0.6, 0.2), c(0, 0, 1), c(NA, 0.5, 0.5))
  thr <- parse_rule_conditions("MgO>=0.30", lab3)[[1]]
  rat <- parse_rule_conditions("CaO/(CaO+Al2O3)>=0.2", lab3)[[1]]
  expect_equal(eval_rule_condition(thr, d), c(FALSE, TRUE, TRUE))
  expect_equal(eval_rule_condition(rat, d), c(TRUE, FALSE, FALSE))
})

test_that("in the CaO-Al2O3-MgO preset every reference phase lands in its own class", {
  p <- get_inclusion_preset("cao-al2o3-mgo", lib)
  r <- p$references
  cls <- setNames(classify_inclusions(r, p), r$phase)
  expect_equal(cls[["C3A"]], "C3A")
  expect_equal(cls[["C12A7"]], "C12A7")
  expect_equal(cls[["CA"]], "CA")
  expect_equal(cls[["CA2"]], "CA2")
  expect_equal(cls[["CA6"]], "CA6")
  expect_equal(cls[["Spinel"]], "Spinel-type")
  expect_equal(cls[["Al2O3 (alumina)"]], "Alumina")
})

test_that("CaO-Al2O3-MgO rules put other compositions where the rule text says", {
  p <- get_inclusion_preset("cao-al2o3-mgo", lib)
  d <- pts(c(0, 0.5, 0.5), c(0.2, 0.4, 0.4), c(0, 0.98, 0.02), c(0.5, 0.05, 0.45), c(NA, NA, NA))
  expect_equal(classify_inclusions(d, p),
               c("MgO-rich", "MgO-rich Ca-aluminate", "Alumina", "Al2O3-poor (CaO/MgO-rich)", NA))
})

test_that("the class boundaries sit halfway between neighbouring compounds", {
  p <- get_inclusion_preset("cao-al2o3-mgo", lib)
  y <- function(ratio) pts(c(ratio, 1 - ratio, 0))
  above <- function(v) classify_inclusions(y(v + 1e-6), p)
  below <- function(v) classify_inclusions(y(v - 1e-6), p)
  ph <- p$references
  ratios <- setNames(ph$A / (ph$A + ph$B), ph$phase)[c("CA6", "CA2", "CA", "C12A7", "C3A")]
  mids <- (ratios[-5] + ratios[-1]) / 2
  expect_equal(unname(mids), c(0.15, 0.2855, 0.42, 0.554), tolerance = 2e-3)
  expect_equal(c(below(0.15), above(0.15)), c("CA6", "CA2"))
  expect_equal(c(below(0.2855), above(0.2855)), c("CA2", "CA"))
  expect_equal(c(below(0.42), above(0.42)), c("CA", "C12A7"))
  expect_equal(c(below(0.554), above(0.554)), c("C12A7", "C3A"))
})

test_that("the MnS-CaS-oxides rules separate sulfides by their Ca share and oxide-dominated particles", {
  p <- get_inclusion_preset("mns-cas-oxides", lib)
  d <- pts(c(1, 0, 0), c(0, 1, 0), c(0.5, 0.5, 0), c(0.2, 0.2, 0.6), c(0.9, 0.1, 0))
  expect_equal(classify_inclusions(d, p),
               c("MnS-rich sulfide", "CaS-rich sulfide", "(Mn;Ca)S mixed sulfide", "Oxide-dominated", "MnS-rich sulfide"))
})

test_that("in every nearest-phase preset each reference phase is closest to itself", {
  for (id in lib$presets$id[lib$presets$classification == "nearest"]) {
    p <- get_inclusion_preset(id, lib)
    expect_equal(classify_inclusions(p$references, p), p$references$phase, label = id)
  }
})

test_that("points with NA coordinates get no class", {
  p <- get_inclusion_preset("mno-sio2-al2o3", lib)
  expect_equal(classify_inclusions(pts(c(NA, NA, NA), c(1, 0, 0)), p), c(NA, "MnO (manganosite)"))
})

test_that("rule presets draw one line per distinct condition, in the right place", {
  segs <- preset_boundary_segments(get_inclusion_preset("cao-al2o3-mgo", lib))
  expect_length(segs, 9)  # 1 Al2O3 threshold + 5 CaO/(CaO+Al2O3) + 2 MgO/(MgO+Al2O3) + 1 MgO threshold
  for (s in segs) {
    expect_equal(rowSums(s), c(1, 1), tolerance = 1e-12)
    expect_true(all(s >= -1e-12))
  }
  has_end <- function(target) any(vapply(segs, function(s) any(apply(s, 1, function(r) all(abs(r - target) < 1e-9))), logical(1)))
  expect_true(has_end(c(0.15, 0.85, 0)) && has_end(c(0, 0, 1)))        # CaO/(CaO+Al2O3) = 0.15 runs to the MgO corner
  expect_true(has_end(c(0.9, 0.1, 0)) && has_end(c(0, 0.1, 0.9)))      # Al2O3 = 0.10 is parallel to the CaO-MgO edge
})

test_that("nearest-phase boundaries are the perpendicular bisectors between phases", {
  two <- data.frame(A = c(1, 0), B = c(0, 1), C = c(0, 0))
  s <- nearest_boundary_segments(two)
  expect_length(s, 1)
  ends <- s[[1]][order(s[[1]][, 3]), ]
  expect_equal(unname(ends), rbind(c(0.5, 0.5, 0), c(0, 0, 1)), tolerance = 1e-9)
  three <- data.frame(A = c(1, 0, 0), B = c(0, 1, 0), C = c(0, 0, 1))
  expect_length(nearest_boundary_segments(three), 3)
  expect_length(nearest_boundary_segments(two[1, ]), 0)
})

test_that("class colours: rule presets use the rules file, others get distinct colours, Other is grey", {
  p <- get_inclusion_preset("cao-al2o3-mgo", lib)
  cols <- inclusion_class_colors(p)
  expect_equal(cols[["Alumina"]], "#0072B2")
  expect_equal(cols[["Other"]], "#999999")
  q <- get_inclusion_preset("mno-sio2-al2o3", lib)
  qc <- inclusion_class_colors(q)
  expect_false(anyDuplicated(qc) > 0)
  expect_equal(names(inclusion_class_colors(q, c("Spessartine", "Other", "Nope"))), c("Spessartine", "Other"))
})
