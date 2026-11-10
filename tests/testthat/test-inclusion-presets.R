# Preset library and reference phases.

lib <- load_inclusion_presets()

test_that("the library loads and lists all presets", {
  tab <- list_inclusion_presets(lib)
  expect_equal(nrow(tab), 11)
  expect_true(all(c("cao-al2o3-mgo", "mno-sio2-al2o3", "mgo-sio2-al2o3", "ti-al-n") %in% tab$id))
  expect_equal(sum(tab$kind == "element"), 4)
})

test_that("every reference point of every preset is a valid composition", {
  for (id in lib$presets$id) {
    r <- get_inclusion_preset(id, lib)$references
    expect_gt(nrow(r), 0, label = id)
    expect_equal(rowSums(r[, c("A", "B", "C")]), rep(1, nrow(r)), tolerance = 1e-12, label = id)
    expect_true(all(as.matrix(r[, c("A", "B", "C")]) >= 0), label = id)
  }
})

test_that("reference compositions equal the stoichiometric values (mass basis)", {
  ref <- function(id, phase) {
    r <- get_inclusion_preset(id, lib)$references
    unlist(r[r$phase == phase, c("A", "B", "C")])
  }
  expect_equal(unname(ref("cao-al2o3-mgo", "Spinel")), c(0, 0.7167, 0.2833), tolerance = 1e-3)
  expect_equal(unname(ref("cao-al2o3-mgo", "C12A7")), c(0.485, 0.515, 0), tolerance = 1e-3)
  expect_equal(unname(ref("cao-al2o3-mgo", "C3A")), c(0.623, 0.377, 0), tolerance = 1e-3)
  expect_equal(unname(ref("cao-al2o3-mgo", "CA6")), c(0.084, 0.916, 0), tolerance = 1e-3)
  expect_equal(unname(ref("mno-sio2-al2o3", "Spessartine")), c(0.430, 0.364, 0.206), tolerance = 1e-3)
  expect_equal(unname(ref("mgo-sio2-al2o3", "Cordierite")), c(0.138, 0.514, 0.349), tolerance = 1e-3)
  expect_equal(unname(ref("cao-sio2-al2o3", "Anorthite")), c(0.202, 0.432, 0.366), tolerance = 1e-3)
})

test_that("mole basis changes reference points; element presets use element fractions", {
  m <- get_inclusion_preset("mgo-sio2-al2o3", lib, basis = "mole")$references
  expect_equal(unlist(m[m$phase == "Spinel", c("A", "B", "C")]), c(A = 0.5, B = 0, C = 0.5))
  expect_equal(unlist(m[m$phase == "Enstatite", c("A", "B", "C")]), c(A = 0.5, B = 0.5, C = 0))
  e <- get_inclusion_preset("mg-al-ca", lib)$references
  w <- inclusion_atomic_weights()
  expect_equal(unname(unlist(e[e$phase == "Spinel", c("A", "B", "C")])),
               c(w[["Mg"]], 2 * w[["Al"]], 0) / (w[["Mg"]] + 2 * w[["Al"]]))
})

test_that("group corners work: oxide phases collapse onto the oxides corner", {
  r <- get_inclusion_preset("mns-cas-oxides", lib)$references
  expect_equal(unlist(r[r$phase == "MnS", c("A", "B", "C")]), c(A = 1, B = 0, C = 0))
  expect_equal(unlist(r[r$phase == "Spinel", c("A", "B", "C")]), c(A = 0, B = 0, C = 1))
})

test_that("phases that do not fit a diagram are left out", {
  r <- get_inclusion_preset("ca-al-s", lib)$references
  expect_false(any(c("Spinel", "MnS", "TiN") %in% r$phase))
  expect_true(all(c("CaS", "CA", "C12A7") %in% r$phase))
})

test_that("a rules preset cannot be switched to a basis its thresholds are not written in", {
  expect_error(get_inclusion_preset("cao-al2o3-mgo", lib, basis = "mole"), "written in mass fractions")
  expect_no_error(get_inclusion_preset("mno-sio2-al2o3", lib, basis = "mole"))
})

test_that("unknown ids are reported with the available ones", {
  expect_error(get_inclusion_preset("nope", lib), "Unknown preset 'nope'.*cao-al2o3-mgo")
})

test_that("every phase and preset cites sources that exist", {
  cited <- unique(unlist(strsplit(c(lib$presets$sources, lib$phases$sources[!is.na(lib$phases$sources)]), ";")))
  expect_true(all(cited %in% lib$sources$id), info = paste(setdiff(cited, lib$sources$id), collapse = ","))
  expect_true(all(startsWith(lib$sources$url, "https://")))
})

test_that("a library with an unreadable rule or unknown compound is rejected when loaded", {
  copy_lib <- function() {
    d <- tempfile("presets"); dir.create(d)
    file.copy(list.files(inclusion_preset_dir(), full.names = TRUE), d)
    d
  }
  d1 <- copy_lib()
  r <- readLines(file.path(d1, "rules.csv")); r <- sub("Al2O3<0.10", "Foo<0.10", r, fixed = TRUE)
  writeLines(r, file.path(d1, "rules.csv"))
  expect_error(load_inclusion_presets(d1), "'Foo' is not a corner")

  d2 <- copy_lib()
  p <- readLines(file.path(d2, "presets.csv")); p <- sub("cao-al2o3-cas,CaO-Al2O3-CaS,compound,CaO,Al2O3,CaS", "cao-al2o3-cas,CaO-Al2O3-CaS,compound,CaO,Al2O3,FeS", p, fixed = TRUE)
  writeLines(p, file.path(d2, "presets.csv"))
  expect_error(load_inclusion_presets(d2), "Unknown compound")
})
