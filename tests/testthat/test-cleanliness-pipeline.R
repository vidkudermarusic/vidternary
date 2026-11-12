# Tests for R/cleanliness_pipeline.R: the whole analysis on the synthetic
# particle list of helper-cleanliness.R, whose right answer is known.

clean_result <- function(settings = list(), data = make_cleanliness_data(), cols = CLEAN_COLS, ...) {
  run_cleanliness_analysis(data, cols, utils::modifyList(list(n_boot = 300), settings), ...)
}

test_that("the families of the synthetic particles are recognised", {
  r <- clean_result()
  # compared by name: the order of table() depends on the locale
  n <- table(r$particles$family)
  # the 22 specks of 0.35 um are below the electron range (0.99 um): no family
  expect_setequal(names(n), c("aluminate", "silicate", "sulfide", "Ti-rich", "too small to classify"))
  expect_equal(as.integer(n[c("aluminate", "silicate", "sulfide", "Ti-rich", "too small to classify")]), c(3, 1, 1, 5, 22))
  expect_equal(r$n_valid, 32)
})

test_that("the inspected area is taken from the coordinates and cut into six rating fields", {
  r <- clean_result()
  expect_equal(r$inspected_area_mm2, 2)
  expect_equal(nrow(r$grid$fields), 6)
  expect_equal(sum(r$grid$fields$complete), 2)
  expect_equal(r$bbox, c(0, 2000, 0, 1000))
})

test_that("the chart rating is the one built into the data", {
  s <- clean_result()$rating_summary
  got <- stats::setNames(s$worst_index, s$series)
  expect_equal(got[["B fine"]], 0.5)
  expect_equal(got[["A fine"]], 0.5)
  expect_equal(got[["D fine"]], 1)
  expect_equal(got[["C thick"]], 0.5)
  expect_equal(got[["DS"]], 0.5)
  expect_equal(unname(got[c("A thick", "B thick", "C fine", "D thick")]), rep(0, 4))

  value <- stats::setNames(s$worst_value, s$series)
  expect_equal(value[["B fine"]], 64)
  expect_equal(value[["A fine"]], 50)
  expect_equal(value[["D fine"]], 4)
  expect_equal(value[["C thick"]], 30)
  expect_equal(value[["DS"]], 15, tolerance = 1e-8)

  field <- stats::setNames(s$worst_field, s$series)
  expect_equal(unname(field[c("B fine", "A fine", "D fine", "C thick", "DS")]), c(1, 1, 1, 2, 6))
  # method B: one field of six at 0.5
  expect_equal(s$mean_index[s$series == "A fine"], 0.5 / 6)
  expect_equal(s$mean_index[s$series == "D fine"], 1 / 6)
})

test_that("the stringer is one object, and the specks are not rated", {
  r <- clean_result()
  expect_equal(sum(r$objects$is_stringer), 1)
  stringer <- r$objects[r$objects$is_stringer, ]
  expect_equal(c(stringer$family, stringer$type), c("aluminate", "B"))
  expect_equal(stringer$n_members, 3)
  expect_equal(sum(is.na(r$particles$object)), 22)
  expect_equal(nrow(r$objects), 8)   # stringer + sulfide + silicate + 4 D + 1 DS
  expect_equal(sort(unique(stats::na.omit(r$particles$type))), c("A", "B", "C", "D", "DS"))
})

test_that("the rating per family splits the same objects by chemistry", {
  f <- clean_result()$rating_by_family
  key <- paste(f$family, f$series)
  expect_setequal(key, c("aluminate B fine", "sulfide A fine", "silicate C thick", "Ti-rich D fine", "Ti-rich DS"))
  expect_equal(f$worst_index[key == "Ti-rich D fine"], 1)
  expect_equal(sum(f$n_objects), 8)
})

test_that("indices count every particle, rated or not", {
  r <- clean_result()
  ind <- r$indices
  expect_equal(ind$n[ind$group == "All"], 32)
  expect_equal(ind$per_mm2[ind$group == "All"], 16)
  expect_equal(ind$n[ind$group == "Ti-rich"], 5)
  # area of the 15 um particle plus four of 5 um, over 2 mm2
  expect_equal(ind$area_ppm[ind$group == "Ti-rich"], (pi / 4 * 225 + 4 * pi / 4 * 25) / 2)
  expect_false(anyNA(ind$area_ppm_lower))
  expect_equal(sum(r$size_classes$n[r$size_classes$group == "All"]), 32)
  expect_equal(r$stereology$n[r$stereology$group == "All"], 32)
  expect_equal(r$stereology$volume_ppm, ind$area_ppm)
  expect_equal(nrow(r$saltykov), 10)
})

test_that("the mass balance uses the area fractions per family", {
  r <- clean_result(list(measured_ppm = c(S = 100)))
  mb <- r$mass_balance
  expect_setequal(mb$phases$family, c("aluminate", "silicate", "sulfide", "Ti-rich"))
  expect_equal(mb$phases$volume_ppm[mb$phases$family == "sulfide"], pi / 4 * 50 * 3 / 2)
  expect_equal(mb$elements$measured_ppm[mb$elements$element == "S"], 100)
  expect_equal(attr(mb, "skipped"), "too small to classify")
})

test_that("the notes warn about the small area and the many specks", {
  n <- clean_result()$notes
  expect_true(any(grepl("inspected area is 2.00 mm2", n$warnings, fixed = TRUE)))
  expect_true(any(grepl("69 % of the particles are smaller than the electron range of the beam (0.99 um in Fe at 15 kV)", n$warnings, fixed = TRUE)))
  expect_true(any(grepl("given no chemical family ('too small to classify')", n$warnings, fixed = TRUE)))
  expect_true(any(grepl("22 of 32 particles", n$notes, fixed = TRUE)))
  expect_false(any(grepl("no clear chemical family", n$notes, fixed = TRUE)))
  expect_false(any(grepl("No particle-direction column", n$notes)))
})

test_that("a filter does not shrink the inspected area or the number of fields", {
  d <- make_cleanliness_data()
  big <- d[d[[CLEAN_COLS$area]] > 1, ]
  r <- clean_result(data = big, all_data = d)
  expect_equal(r$inspected_area_mm2, 2)
  expect_equal(nrow(r$grid$fields), 6)
  expect_equal(nrow(r$particles), 10)
  expect_equal(r$rating_summary$worst_index, clean_result()$rating_summary$worst_index)
  expect_false(any(grepl("smaller than the electron range", r$notes$warnings)))
  # without the unfiltered data the area would come out too small
  expect_lt(clean_result(data = big)$inspected_area_mm2, 2)
})

test_that("an entered area is used, and a warning says when it disagrees with the coordinates", {
  r <- clean_result(list(inspected_area_mm2 = 1.9))
  expect_equal(r$inspected_area_mm2, 1.9)
  expect_equal(r$indices$per_mm2[1], 32 / 1.9)
  expect_false(any(grepl("differs from the area spanned", r$notes$warnings)))
  off <- clean_result(list(inspected_area_mm2 = 10))
  expect_true(any(grepl("differs from the area spanned", off$notes$warnings)))
})

test_that("coordinates in um give the same result as in mm", {
  d <- make_cleanliness_data()
  d[[CLEAN_COLS$x]] <- d[[CLEAN_COLS$x]] * 1000
  d[[CLEAN_COLS$y]] <- d[[CLEAN_COLS$y]] * 1000
  r <- clean_result(list(coord_unit = "um"), data = d)
  expect_equal(r$inspected_area_mm2, 2)
  expect_equal(r$rating_summary$worst_index, clean_result()$rating_summary$worst_index)
})

test_that("a class column can replace the EDS classification", {
  d <- make_cleanliness_data()
  d$MyClass <- ifelse(d[[CLEAN_COLS$breadth]] > 1, "oxide", NA)
  r <- clean_result(data = d, cols = c(CLEAN_COLS, list(family = "MyClass")))
  n <- table(r$particles$family)
  expect_equal(as.integer(n[c("oxide", "unclassified")]), c(10, 22))
  # no sulfide family any more: the long sulfide is rated as type C
  expect_equal(r$rating_summary$worst_index[r$rating_summary$series == "A fine"], 0)
  expect_true(all(r$mass_balance$elements$bound_ppm == 0) || nrow(r$mass_balance$elements) == 0)
})

test_that("without a direction column particles are taken as aligned, and a note says so", {
  cols <- CLEAN_COLS[setdiff(names(CLEAN_COLS), "direction")]
  r <- clean_result(cols = cols)
  expect_true(any(grepl("No particle-direction column", r$notes$notes)))
  expect_equal(r$rating_summary$worst_index, clean_result()$rating_summary$worst_index)
})

test_that("turning the working direction by 90 degrees breaks up the stringer", {
  r <- clean_result(list(axis_angle = 90))
  expect_equal(sum(r$objects$is_stringer), 0)
})

test_that("ASTM E45 settings are picked up from the standard", {
  r <- clean_result(list(standard = "ASTM E45"))
  expect_equal(r$standard$name, "ASTM E45")
  expect_equal(c(r$settings$max_offset, r$settings$aspect_limit), c(15, 2))
  expect_true(any(grepl("ASTM E45 asks for about 160 mm2", r$notes$warnings, fixed = TRUE)))
})

test_that("rows without valid numbers are left out and counted", {
  d <- make_cleanliness_data()
  d[[CLEAN_COLS$area]][3] <- NA
  d[[CLEAN_COLS$length]][4] <- 0
  r <- clean_result(data = d)
  expect_equal(r$n_valid, 30)
  expect_true(any(grepl("2 rows without a valid", r$notes$notes, fixed = TRUE)))
})

test_that("missing columns and unusable data give a clear error", {
  d <- make_cleanliness_data()
  expect_error(clean_result(cols = CLEAN_COLS[c("area", "length", "x", "y")]), "Choose the breadth column")
  expect_error(clean_result(cols = utils::modifyList(CLEAN_COLS, list(x = "nope"))), "Choose the x column")
  d[[CLEAN_COLS$area]] <- NA_real_
  expect_error(clean_result(data = d), "No particle has a valid")
})

test_that("the export tables cover every part of the result", {
  tabs <- cleanliness_result_tables(clean_result())
  expect_true(all(c("Rating", "Rating by family", "Field ratings", "Objects", "Indices", "Size classes",
                    "Stereology", "Saltykov", "Mass balance elements", "Particles", "Notes", "Settings") %in% names(tabs)))
  expect_true(all(vapply(tabs, is.data.frame, logical(1))))
  expect_true(all(nchar(names(tabs)) <= 31))        # Excel's limit for a sheet name
  expect_false(any(vapply(tabs$`Size classes`, is.factor, logical(1))))
  expect_equal(tabs$Settings$value[tabs$Settings$setting == "standard"], "ISO 4967")
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(tabs, path)
  expect_gt(file.info(path)$size, 0)
})

test_that("the plots are built from a result", {
  r <- clean_result()
  expect_s3_class(create_stringer_map(r), "ggplot")
  expect_s3_class(create_direction_rose(r$particles$direction, r$particles$length), "ggplot")
  expect_s3_class(create_size_class_plot(r$size_classes), "ggplot")
  # the plots must also render, not only be built
  path <- tempfile(fileext = ".png")
  ggplot2::ggsave(path, create_stringer_map(r), width = 6, height = 4, dpi = 60)
  expect_gt(file.info(path)$size, 0)
  ggplot2::ggsave(path, create_direction_rose(c(0, 10, 95, 170), NULL, axis_angle = 0), width = 4, height = 4, dpi = 60)
  expect_gt(file.info(path)$size, 0)
})
