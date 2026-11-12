# Tests for R/batch_pipeline.R, R/batch_evs.R, R/batch_plots.R and
# R/batch_report.R on the synthetic batches of helper-batch.R: A and B come
# from one population, C has half the particles, larger ones and more Ti.

batch_result <- function(settings = list(), batches = make_batches(), cols = BATCH_COLS, ...) {
  run_batch_comparison(batches, cols, utils::modifyList(list(n_perm = 199, n_boot = 300), settings), ...)
}

test_that("the summary has one row per batch with the built-in numbers", {
  s <- batch_result()$summary
  expect_equal(s$batch, c("A", "B", "C"))
  expect_equal(s$n, c(200, 200, 100))
  expect_equal(s$inspected_area_mm2, c(2, 2, 2))
  expect_equal(s$per_mm2, c(100, 100, 50))
  expect_true(all(s$per_mm2_lower < s$per_mm2 & s$per_mm2 < s$per_mm2_upper))
  expect_true(all(s$area_ppm_lower < s$area_ppm & s$area_ppm < s$area_ppm_upper))
  expect_equal(s$n_fields, c(20, 20, 20))
  expect_gt(s$median_ecd_um[3], 1.3 * s$median_ecd_um[1])
  # randomly scattered particles: index of dispersion near 1
  expect_true(all(s$field_dispersion > 0.4 & s$field_dispersion < 2))
})

test_that("the batch built to differ is found in every comparison, the twin batch in none", {
  r <- batch_result()
  pair <- function(tab, a, b) tab[tab$batch_a == a & tab$batch_b == b, ]
  expect_equal(pair(r$density$pairs, "A", "C")$ratio, 2)
  expect_lt(pair(r$density$pairs, "A", "C")$p_adjusted, 0.001)
  expect_equal(pair(r$density$pairs, "A", "B")$ratio, 1)
  expect_lt(pair(r$size$pairs, "A", "C")$p_adjusted, 0.05)
  expect_gt(pair(r$size$pairs, "A", "B")$p_adjusted, 0.05)
  expect_lt(pair(r$composition$pairs, "B", "C")$p_adjusted, 0.05)
  expect_gt(pair(r$composition$pairs, "A", "B")$p_adjusted, 0.05)
  expect_lt(r$field_counts$p_value, 0.01)
  expect_equal(nrow(r$particles), 500)
  expect_equal(r$parts, BATCH_COLS$parts)
})

test_that("each batch gets an extreme-value estimate", {
  e <- batch_result(list(fields_per_area = 2, return_period = 50))$evs
  expect_equal(e$batch, c("A", "B", "C"))
  expect_equal(e$n_control_areas, c(10, 10, 10))
  expect_true(all(is.na(e$problem)))
  expect_true(all(e$ci_lower < e$return_level & e$return_level < e$ci_upper))
  expect_true(all(e$max_upper > e$ci_upper))
  expect_gt(e$return_level[3], e$return_level[1])
  # the same numbers as the EVS functions give directly
  a <- make_batches()$A
  a$ca <- assign_control_areas(a$Field, 2)$control_area
  fit <- fit_evs_gumbel(compute_block_maxima(a, BATCH_COLS$area, "ca")$sqrt_area_max)
  expect_equal(e$return_level[1], predict_evs_max(fit, 50, n_boot = 300)$predicted)
})

test_that("a batch whose extreme-value fit fails is kept with the reason", {
  e <- batch_result(list(fields_per_area = 10))$evs      # 2 control areas per batch
  expect_true(all(is.na(e$return_level)))
  expect_match(e$problem[1], "fewer than 3 control areas")
  n <- batch_result(list(fields_per_area = 10))$notes
  expect_true(any(grepl("No extreme-value estimate for: A (fewer than 3", n$warnings, fixed = TRUE)))
})

test_that("without field IDs the field-based parts are left out and a note says so", {
  cols <- BATCH_COLS[setdiff(names(BATCH_COLS), "field")]
  r <- batch_result(cols = cols)
  expect_null(r$evs)
  expect_null(r$field_counts)
  expect_true(all(is.na(r$summary$area_ppm_lower)))
  expect_true(any(grepl("No SEM field ID column", r$notes$notes)))
  expect_equal(r$summary$per_mm2, c(100, 100, 50))
})

test_that("without chosen elements the composition is not compared", {
  r <- batch_result(cols = BATCH_COLS[setdiff(names(BATCH_COLS), "parts")])
  expect_null(r$composition)
  tabs <- batch_result_tables(r)
  expect_equal(tabs$`Composition pairs`$note, "not computed")
  expect_equal(nrow(tabs$`Overall tests`), 3)
})

test_that("the inspected area can be entered, once for all or per batch", {
  cols <- BATCH_COLS[c("area", "field")]
  expect_equal(batch_result(list(inspected_area_mm2 = 4), cols = cols)$summary$per_mm2, c(50, 50, 25))
  per <- batch_result(list(inspected_area_mm2 = c(2, 4, 1)), cols = cols)$summary
  expect_equal(per$per_mm2, c(100, 50, 100))
  expect_error(batch_result(list(inspected_area_mm2 = c(2, 4)), cols = cols), "one per batch \\(3 batches, 2 areas given\\)")
  expect_error(batch_result(list(inspected_area_mm2 = c(2, 0, 1)), cols = cols), "greater than zero")
  expect_error(batch_result(cols = cols), "Enter the inspected area, or choose the X and Y")
})

test_that("a filter does not shrink the inspected area or the number of fields", {
  all_b <- make_batches()
  big <- lapply(all_b, function(d) d[d[[BATCH_COLS$area]] > 3, ])
  r <- batch_result(batches = big, all_batches = all_b)
  expect_equal(r$summary$inspected_area_mm2, c(2, 2, 2))
  expect_equal(r$summary$n_fields, c(20, 20, 20))
  expect_equal(r$summary$n, unname(vapply(big, nrow, integer(1))))
})

test_that("the notes point at clustering, overlaps, zeros and small batches", {
  b <- make_batches()
  b$A$Field[1:150] <- 7                                 # most particles of A in one field
  names(b$A)[names(b$A) == "Cr.(Wt%)"] <- names(b$B)[names(b$B) == "Cr.(Wt%)"] <- names(b$C)[names(b$C) == "Cr.(Wt%)"] <- "N.(Wt%)"
  b$B[["N.(Wt%)"]][1:60] <- 0
  b$C <- b$C[1:20, ]
  cols <- utils::modifyList(BATCH_COLS, list(parts = c("Ti.(Wt%)", "Fe.(Wt%)", "N.(Wt%)")))
  n <- batch_result(batches = b, cols = cols)$notes
  expect_true(any(grepl("vary more than random scatter allows in: A ", n$warnings, fixed = TRUE)))
  expect_true(any(grepl("overlapping EDS lines (Ti - N)", n$warnings, fixed = TRUE)))
  expect_true(any(grepl("readings are zero in: N.(Wt%)", n$warnings, fixed = TRUE)))
  expect_true(any(grepl("Fewer than 30 particles in: C", n$warnings, fixed = TRUE)))
  expect_true(any(grepl("adjusted for the number of pairs (Holm)", n$notes, fixed = TRUE)))
})

test_that("unusable input gives a clear error", {
  b <- make_batches()
  expect_error(batch_result(batches = b["A"]), "at least 2 files")
  expect_error(batch_result(cols = utils::modifyList(BATCH_COLS, list(area = "nope"))), "Choose the area column")
  expect_error(batch_result(cols = utils::modifyList(BATCH_COLS, list(parts = c("Ti.(Wt%)", "Zr.(Wt%)")))),
               "not present in every file: Zr")
  names(b) <- c("A", "A", "C")
  expect_error(batch_result(batches = b), "its own name")
  b <- make_batches()
  b$B[[BATCH_COLS$area]] <- NA_real_
  expect_error(batch_result(batches = b), "Batch 'B' has no particle with a valid area")
})

test_that("the export and display tables cover the result", {
  r <- batch_result()
  tabs <- batch_result_tables(r)
  expect_true(all(c("Summary", "Overall tests", "Density pairs", "Size quantiles", "Size pairs", "Composition pairs",
                    "Composition clr means", "Extreme values", "Notes", "Settings") %in% names(tabs)))
  expect_true(all(vapply(tabs, is.data.frame, logical(1))))
  expect_true(all(nchar(names(tabs)) <= 31))
  expect_equal(nrow(tabs$`Overall tests`), 5)
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(tabs, path)
  expect_gt(file.info(path)$size, 0)

  d <- batch_display_tables(r)
  expect_equal(d$summary$Batch, c("A", "B", "C"))
  expect_equal(d$overall$Test, c("chi-squared", "Kruskal-Wallis", "k-sample Anderson-Darling, permutation",
                                 "permutation MANOVA on ilr, pseudo-F", "distance to batch centroid, F"))
  expect_equal(d$density_pairs$`Density ratio`[2], "2.00")
  expect_match(d$overall$`Share of variation explained (R2)`[4], "%$")
  expect_equal(names(d$clr), c("Element", "A", "B", "C", "Zero readings (%)"))
  # a permutation p-value is never shown as zero
  expect_false(any(grepl("^0(\\.0+)?$", unlist(d[c("overall", "size_pairs", "composition_pairs")]))))
})

test_that("the plots are built and render", {
  r <- batch_result()
  plots <- list(create_batch_interval_chart(r$summary, "per_mm2", "per_mm2_lower", "per_mm2_upper", "Particles per mm2", "Density"),
                create_batch_interval_chart(r$summary, "area_ppm", NULL, NULL),
                create_size_tail_plot(r$particles$ecd, r$particles$batch),
                create_clr_difference_plot(r$composition$clr_means))
  path <- tempfile(fileext = ".png")
  for (p in plots) {
    expect_s3_class(p, "ggplot")
    ggplot2::ggsave(path, p, width = 5, height = 4, dpi = 50)
    expect_gt(file.info(path)$size, 0)
  }
})

test_that("the script repeats the comparison and the report holds every section", {
  upload <- make_batch_upload()
  all_b <- lapply(upload$datapath, read_particle_table)
  names(all_b) <- c("A", "B", "C")
  filters <- stats::setNames(list("> 1"), BATCH_COLS$area)
  b <- lapply(all_b, apply_pre_filters, filters = filters)
  r <- batch_result(batches = b, all_batches = all_b)

  script <- batch_script(upload$datapath, BATCH_COLS, r$settings, filters)
  expect_silent(parse(text = script))
  env <- new.env()
  eval(parse(text = script[!grepl("^library\\(", script)]), envir = env)
  expect_equal(env$result$summary, r$summary)
  expect_equal(env$result$size$pairs, r$size$pairs)
  expect_equal(env$result$composition$pairs, r$composition$pairs)

  html <- batch_report(r, report_provenance(upload$name, upload$datapath, vapply(all_b, nrow, integer(1))), script, filters)
  for (heading in c("Data and settings", "What to keep in mind", "The batches at a glance", "Amount of inclusions",
                    "Size distribution", "Composition", "Largest inclusion", "Methods", "R script that repeats")) {
    expect_match(html, paste0("<h2>", heading), fixed = TRUE)
  }
  expect_match(html, "A.xlsx", fixed = TRUE)
  expect_match(html, unname(tools::md5sum(upload$datapath[1])), fixed = TRUE)
  expect_match(html, "filter: Area.(sq..um)", fixed = TRUE)
  expect_gte(lengths(regmatches(html, gregexpr("data:image/png;base64,", html, fixed = TRUE))), 5)
  expect_match(html, "run_batch_comparison(batches, cols, settings", fixed = TRUE)
})
