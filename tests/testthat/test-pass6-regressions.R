# Regression tests for pass 6 of the vidternary Structural Audit - one
# test_that() per finding fixed this pass, deliberately narrow rather than
# a general coverage push, matching test-pass5-regressions.R's own
# established convention.

test_that("apply_multivariate_filtering() with Isolation Forest keeps exactly the rows compute_isolation_forest() flagged, even with incomplete rows scattered through the target data (regression)", {
  # Previously, the row-reconstruction step shared one formula
  # (which(complete.cases(M_numeric))[keep_indices]) between Mahalanobis
  # and Isolation Forest. That formula is only correct when keep_indices
  # is scoped to complete-cases-only rows in complete-row order (true for
  # Mahalanobis) - Isolation Forest's own keep_indices is already a
  # full-length, original-row-order mask, so re-expanding it through
  # which(complete.cases(...)) risked misaligning results (phantom NA rows
  # or the wrong row's data) whenever the target had an incomplete row in
  # a selected column. A second, independent bug (iso_result$common_cols
  # reading a field compute_isolation_forest() never returns - see the
  # next test) happened to make that risk inert in the actual shipped
  # code by making M_numeric a 0-column matrix; fixing that bug on its own
  # would have made the misalignment live. This test locks down correct
  # behaviour regardless of that interaction, by comparing against a
  # ground-truth mask computed independently.
  set.seed(7)
  n <- 12
  M <- data.frame(
    id = 1:n,
    Al = c(20, rnorm(n - 2, 2, 0.3), 25),
    Si = c(1, rnorm(n - 2, 5, 0.5), 1)
  )
  M$Al[5] <- NA # an incomplete row positioned BEFORE a to-be-excluded outlier

  ground_truth <- compute_isolation_forest(
    M[, c("Al", "Si")], M[, c("Al", "Si")],
    selected_columns = c("Al", "Si"),
    keep_outliers = FALSE, ntrees = 100, contamination = 0.10, seed = 1
  )

  result <- apply_multivariate_filtering(
    M = M, use_mahalanobis = FALSE, use_isolation_forest = TRUE,
    selected_columns = c("Al", "Si"), mahalanobis_reference = "self",
    reference_data = NULL, preview = TRUE,
    keep_outliers_isolation = FALSE, keep_outliers_mahalanobis = FALSE,
    lambda = 1, omega = 0, custom_mdthresh = NULL, mdthresh_mode = "auto",
    isolation_ntrees = 100, isolation_contamination = 0.10, isolation_sample_size = NULL
  )

  expect_false(anyNA(result$M$id))
  expect_false(any(duplicated(result$M$id)))
  expect_equal(sort(result$M$id), sort(M$id[ground_truth$kept_mask]))
})

test_that("apply_multivariate_filtering() with Mahalanobis is unaffected by the Isolation Forest row-selection fix (regression)", {
  set.seed(7)
  n <- 12
  M <- data.frame(
    id = 1:n,
    Al = c(20, rnorm(n - 2, 2, 0.3), 25),
    Si = c(1, rnorm(n - 2, 5, 0.5), 1)
  )
  M$Al[5] <- NA

  result <- apply_multivariate_filtering(
    M = M, use_mahalanobis = TRUE, use_isolation_forest = FALSE,
    selected_columns = c("Al", "Si"), mahalanobis_reference = "self",
    reference_data = NULL, preview = TRUE,
    keep_outliers_isolation = FALSE, keep_outliers_mahalanobis = FALSE,
    lambda = 1, omega = 0, custom_mdthresh = NULL, mdthresh_mode = "auto",
    isolation_ntrees = 100, isolation_contamination = 0.10, isolation_sample_size = NULL
  )

  expect_false(anyNA(result$M$id))
  expect_false(any(duplicated(result$M$id)))
  # The incomplete row (id 5) can never be scored by Mahalanobis either, so
  # it must never appear in the filtered result.
  expect_false(5 %in% result$M$id)
})

test_that("apply_multivariate_filtering() reports the real columns used for Isolation Forest, not a blank string (regression)", {
  # iso_result$common_cols was always NULL - compute_isolation_forest()
  # returns its selected columns as `columns_used`, a different name only
  # compute_mahalanobis_distance() uses `common_cols` for - so every
  # "Columns used: ..." line (debug log, Analysis Report, on-plot notes)
  # printed blank for every Isolation Forest run. Confirmed directly via
  # capture.output() before fixing, not just reasoned about.
  set.seed(3)
  M <- data.frame(id = 1:20, Al = rnorm(20, 2, 0.3), Si = rnorm(20, 5, 0.5))

  out <- capture.output({
    apply_multivariate_filtering(
      M = M, use_mahalanobis = FALSE, use_isolation_forest = TRUE,
      selected_columns = c("Al", "Si"), mahalanobis_reference = "self",
      reference_data = NULL, preview = FALSE,
      keep_outliers_isolation = FALSE, keep_outliers_mahalanobis = FALSE,
      lambda = 1, omega = 0, custom_mdthresh = NULL, mdthresh_mode = "auto",
      isolation_ntrees = 100, isolation_contamination = 0.10, isolation_sample_size = NULL
    )
  })

  cols_line <- grep("^Columns used:", out, value = TRUE)
  expect_length(cols_line, 1)
  expect_match(cols_line, "Al")
  expect_match(cols_line, "Si")
})

test_that("Multiple Ternary Creator's Optional Parameter 1/2 selects are single-select in the UI", {
  # Previously multiple = TRUE on both selectizeInputs, which let the
  # per-column filter UI silently collapse to one filter misapplied to
  # every selected column, and let a multi-column Optional Parameter 2
  # silently color by only its first column while the legend/title claimed
  # otherwise (vidternary Structural Audit Sec.03). Optional Parameter 1/2
  # are a styling dimension, not a summed composition axis like Elements
  # A/B/C, and were never meant to support more than one column - confirmed
  # directly by the user, matching the main Ternary Plots tab's own
  # single-select optional_param1_1/_2.
  html <- as.character(create_multiple_ternary_tab("multiple_ternary"))
  expect_false(grepl('id="multiple_ternary-multiple_optional_param1"[^>]*multiple', html))
  expect_false(grepl('id="multiple_ternary-multiple_optional_param2"[^>]*multiple', html))
})

test_that("apply_element_and_parameter_filters() rejects more than one column for Optional Parameter 1/2 with a clear message (regression)", {
  # This function is exported and directly callable outside the UI, so the
  # single-select UI restriction above isn't sufficient on its own - a
  # direct caller passing 2+ columns must still fail clearly rather than
  # silently collapse to one filter misapplied to every column.
  M <- data.frame(A = 1:10, B = 1:10, C = 1:10, X = stats::rnorm(10), Y = stats::rnorm(10))
  base_args <- list(
    M = M,
    element_A = list(col = "A", filter = NULL), element_B = list(col = "B", filter = NULL),
    element_C = list(col = "C", filter = NULL),
    individual_filters_A = NULL, individual_filters_B = NULL, individual_filters_C = NULL,
    preview = TRUE
  )

  expect_error(
    do.call(apply_element_and_parameter_filters, c(base_args, list(
      optional_param1 = list(col = c("X", "Y"), filter = "> 0"), optional_param2 = NULL
    ))),
    "more than one column"
  )
  expect_error(
    do.call(apply_element_and_parameter_filters, c(base_args, list(
      optional_param1 = NULL, optional_param2 = list(col = c("X", "Y"), filter = NULL)
    ))),
    "more than one column"
  )

  # Regression: a single column for each still works.
  result <- do.call(apply_element_and_parameter_filters, c(base_args, list(
    optional_param1 = list(col = "X", filter = "> -100"), optional_param2 = list(col = "Y", filter = NULL)
  )))
  expect_equal(nrow(result$M), 10)
})

test_that("compute_point_styling() rejects more than one column for Optional Parameter 2 with a clear message (regression)", {
  ternary_points1 <- data.frame(x = stats::rnorm(10), y = stats::rnorm(10))
  matrika <- data.frame(X = stats::rnorm(10), Y = stats::rnorm(10))
  base_args <- list(
    ternary_points1 = ternary_points1, matrika = matrika,
    optional_param1 = NULL, optional_param1_representation = "point_size",
    color_palette = "blue", use_manual_point_size = FALSE, manual_point_size = 1,
    is_categorical_group = FALSE, selected_groups = NULL
  )

  expect_error(
    do.call(compute_point_styling, c(base_args, list(optional_param2 = list(col = c("X", "Y"), filter = NULL)))),
    "more than one column"
  )

  # Regression: a single column still colors correctly.
  result <- do.call(compute_point_styling, c(base_args, list(optional_param2 = list(col = "X", filter = NULL))))
  expect_length(result$pointCol, 10)
  expect_false(anyNA(result$pointCol))
})

test_that("Multiple Ternary Creator: a real Create & Save round-trip with single-column Optional Parameter 1 (filtered) and Optional Parameter 2 (color) still works end to end (regression)", {
  rv <- shiny::reactiveValues()
  show_message <- function(message, type = "info") invisible(NULL)
  log_operation <- function(...) invisible(NULL)
  server <- function(input, output, session) {
    shiny::moduleServer("multiple_ternary", function(input, output, session) {
      register_ternary_plots_batch_handlers(input, output, session, rv, show_message, log_operation)
    })
  }

  set.seed(11)
  d <- data.frame(Al = abs(stats::rnorm(15, 10, 2)) + 1, Si = abs(stats::rnorm(15, 10, 2)) + 1,
                   Mn = abs(stats::rnorm(15, 10, 2)) + 1, ECD = abs(stats::rnorm(15, 5, 1)))
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  upload <- data.frame(name = "one.xlsx", size = file.info(path)$size, type = "",
                        datapath = path, stringsAsFactors = FALSE)

  shiny::testServer(server, {
    session$setInputs(`multiple_ternary-multiple_xlsx_files` = upload)
    session$setInputs(`multiple_ternary-multiple_element_A` = "Al",
                       `multiple_ternary-multiple_element_B` = "Si",
                       `multiple_ternary-multiple_element_C` = "Mn")
    session$setInputs(`multiple_ternary-multiple_optional_param1` = "ECD")
    session$setInputs(`multiple_ternary-multiple_filter_op1_ECD` = "> 0")
    session$setInputs(`multiple_ternary-multiple_optional_param2` = "ECD")

    saved <- output[["multiple_ternary-create_save_multiple_ternary"]]
    expect_true(file.exists(saved))
  })
})

# ---- Optional Param 2's numeric color legend now matches its real
# quantile-binned colors, not a fresh evenly-spaced min-to-max relabeling ----
# ternary_plot_save.R / ternary_plot_preview.R (byte-identical code in both)
# each recomputed their own 5-swatch legend independently of
# compute_point_styling()'s actual quantile-based binning - a fresh
# seq(min, max, length.out = 6) for the labels, and a fresh always-length-5
# color ramp for the swatches - instead of reusing param2_breaks/
# param2_colors, the exact objects compute_point_styling() already computed
# and used to color the real points. On the right-skewed data this app
# filters the two binnings diverge substantially, so a swatch's printed
# range didn't correspond to the values that actually received that color;
# in the fully-degenerate case (too little variation for 5 unique breaks)
# it also silently kept showing 5 swatches for what was really 1 color.
# Fixed by reusing param2_breaks/param2_colors directly in both files.

test_that("the save/preview legend for a numeric Optional Param 2 uses the SAME quantile-based colors and breaks compute_point_styling() actually used, on skewed data", {
  set.seed(11)
  n <- 40
  d <- data.frame(
    Fe = runif(n, 20, 60), Al = runif(n, 5, 20), Ti = runif(n, 5, 20),
    # Strongly right-skewed: almost all values under 2, two large outliers.
    # A naive min-to-max split would spread its 5 bins across 0-95;  the
    # real quantile bins cluster near where the data actually sits.
    ECD = c(runif(n - 2, 0.1, 2), 80, 95)
  )
  xlsx_path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, xlsx_path)
  out_dir <- tempfile("out_"); dir.create(out_dir)

  pd <- prepare_ternary_plot_data(
    xlsx_file = xlsx_path, working_dir = tempdir(), output_dir = out_dir,
    element_A = list(col = "Fe"), element_B = list(col = "Al"), element_C = list(col = "Ti"),
    optional_param1 = NULL,
    optional_param2 = list(col = "ECD", filter = NULL),
    color_palette = "blue", xlsx_display_name = "test.xlsx",
    preview = TRUE, use_mahalanobis = FALSE, reference_data = NULL,
    optional_param1_representation = "point_size", output_format = "png",
    use_isolation_forest = FALSE, use_iqr_filter = FALSE, use_zscore_filter = FALSE, use_mad_filter = FALSE,
    stat_filter_log10 = FALSE,
    lambda = 1, omega = 0, keep_outliers_mahalanobis = FALSE, keep_outliers_isolation = FALSE,
    keep_outliers_iqr = FALSE, keep_outliers_zscore = FALSE, keep_outliers_mad = FALSE,
    individual_filters_A = NULL, individual_filters_B = NULL, individual_filters_C = NULL,
    custom_mdthresh = NULL, mdthresh_mode = "auto", mahalanobis_reference = "self",
    selected_columns = NULL, include_plot_notes = FALSE, use_manual_point_size = FALSE,
    manual_point_size = NULL, selected_groups = NULL, is_categorical_group = FALSE
  )

  expected_labels <- paste0(round(pd$param2_breaks[-length(pd$param2_breaks)], 3), " - ", round(pd$param2_breaks[-1], 3))
  naive_range <- range(d$ECD)
  naive_breaks <- seq(naive_range[1], naive_range[2], length.out = 6)
  naive_labels <- paste0(round(naive_breaks[1:5], 3), " - ", round(naive_breaks[2:6], 3))
  # The fixture must actually be skewed enough to distinguish the two
  # schemes, or this test wouldn't be exercising anything.
  expect_false(identical(expected_labels, naive_labels))

  captured <- list()
  fake_legend <- function(...) captured[[length(captured) + 1]] <<- list(...)
  # legend() is @importFrom'd into this package's own namespace (see
  # R/vidternary-package.R) - matching the exact gotcha already documented
  # for mtext() in test-plot-notes-formatting.R: mocking it under
  # .package = "graphics" would silently fail to intercept anything.
  testthat::local_mocked_bindings(legend = fake_legend, .package = "vidternary")

  f <- tempfile(fileext = ".png"); grDevices::png(f)
  render_ternary_plot_preview(pd)
  grDevices::dev.off()

  expect_true(length(captured) >= 1)
  call_args <- captured[[length(captured)]]
  expect_equal(call_args$legend, expected_labels)
  expect_false(identical(call_args$legend, naive_labels))
  expect_identical(call_args$col, pd$param2_colors)
  expect_equal(length(call_args$legend), length(pd$param2_colors))
})

test_that("the User Guide vignette and the Multiple Ternary Creator's own help text no longer describe the removed global Directory Settings / Cache Management picker (regression)", {
  # A repo-layout/content check, not a code-behavior check, so it only makes
  # sense against the source tree (devtools::test()) - skip under R CMD
  # check, which runs against the installed package where these raw source
  # files aren't present in the same layout, matching the established
  # pattern in test-modular-structure.R's "Package structure is complete".
  pkg_root <- file.path("..", "..")
  testthat::skip_if_not(dir.exists(file.path(pkg_root, "R")),
                        "source tree not present (running against an installed package)")

  vignette_path <- file.path(pkg_root, "vignettes", "user-guide.Rmd")
  ui_path <- file.path(pkg_root, "R", "ui_multiple_ternary_tab.R")
  testthat::skip_if_not(file.exists(vignette_path) && file.exists(ui_path),
                        "vignette or UI source file not found")

  vignette_text <- paste(readLines(vignette_path, warn = FALSE), collapse = "\n")
  ui_text <- paste(readLines(ui_path, warn = FALSE), collapse = "\n")

  # The stale claims this fix removed: a shared Working/Output Directory
  # panel, a Cache Management control, per-tab saves writing into a
  # "configured/OutputDir" location rather than through a real download,
  # and (Hexagonal Ternary Diagram) a two-step Slovenian-labeled
  # Generate-then-Save workflow that no longer exists.
  expect_false(grepl("Cache Management", vignette_text, fixed = TRUE))
  expect_false(grepl("Directory Settings and Cache", vignette_text, fixed = TRUE))
  expect_false(grepl("configured Output Directory", vignette_text, fixed = TRUE))
  expect_false(grepl("Save to File", vignette_text, fixed = TRUE))
  expect_false(grepl("OutputDir", vignette_text, fixed = TRUE))
  expect_false(grepl("Ustvari heksagonalni", vignette_text, fixed = TRUE))
  expect_false(grepl("Shrani v izhodno", vignette_text, fixed = TRUE))
  expect_false(grepl("configured output directory", ui_text, fixed = TRUE))
})

test_that("generate_distinct_colors() no longer prints RColorBrewer's benign 'minimal value for n is 3' warning at any floor-of-3 boundary (regression)", {
  # brewer.pal()'s undocumented floor silently over-returns for a
  # requested n of 1 or 2 (and the same via the "how many more are
  # needed" remainder at n_groups = 13/14/25/26) - generate_distinct_colors()
  # already truncates the result to the correct count afterward (a
  # pre-existing, confirmed-correct behavior, not a bug), so the warning
  # was real but permanently noise. Fixed by wrapping just the calls that
  # can receive a count < 3 in suppressWarnings(), inside the function
  # itself rather than at every call site.
  for (n in c(1, 2, 13, 14, 25, 26)) {
    expect_silent(generate_distinct_colors(n))
  }
  # Unaffected boundaries (a count that never hits the floor) still work
  # and still return exactly n colors.
  for (n in c(3, 12, 24, 32, 40)) {
    expect_length(generate_distinct_colors(n), n)
  }
})

test_that("repo-hygiene bundle: no dead viridisLite install.packages() fallback, no dangling FILES.md entry, no dead input$xlsx_display_name read (regression)", {
  pkg_root <- file.path("..", "..")
  testthat::skip_if_not(dir.exists(file.path(pkg_root, "R")),
                        "source tree not present (running against an installed package)")

  # (1) viridisLite is a hard Imports: dependency (see DESCRIPTION) - a
  # requireNamespace()-gated install.packages() fallback around it is dead
  # code that would still have real side effects (a network call, a
  # non-interactive hang) if it were ever somehow reached.
  for (f in c("ternary_plot_preview.R", "ternary_plot_data_prep.R", "ternary_plot_save.R")) {
    path <- file.path(pkg_root, "R", f)
    testthat::skip_if_not(file.exists(path), paste(f, "not found"))
    text <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_false(grepl('install.packages("viridisLite")', text, fixed = TRUE),
                 info = f)
  }

  # (2) FILES.md previously documented R/server_directory_management.R,
  # a file removed when the global directory picker was replaced by
  # per-save downloads (see §03's "picker removed" entry) - the doc entry
  # should never outlive the file it describes.
  files_md_path <- file.path(pkg_root, "FILES.md")
  testthat::skip_if_not(file.exists(files_md_path), "FILES.md not found")
  files_md_text <- paste(readLines(files_md_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("server_directory_management", files_md_text, fixed = TRUE))

  # (5) input$xlsx_display_name is not a real UI input anywhere in the
  # app - extract_ternary_params() reading it was always-NULL dead code,
  # since every real caller overwrites the field immediately afterward
  # anyway (server_ternary_plots.R, server_ternary_plots_batch.R).
  helpers_path <- file.path(pkg_root, "R", "helpers_filters.R")
  testthat::skip_if_not(file.exists(helpers_path), "helpers_filters.R not found")
  helpers_text <- paste(readLines(helpers_path, warn = FALSE), collapse = "\n")
  expect_false(grepl("input$xlsx_display_name", helpers_text, fixed = TRUE))
})

# ---- Post-publication: point-size divide-by-zero + spurious pointType
# reinitialize warning, found on a fresh re-check of the ternary
# coordinate/point-styling/rendering pipeline requested after pass 6 closed ----

test_that("compute_point_styling() under Point Size representation maps an all-zero Optional Param 1 column to MIN_POINT_SIZE, not NaN (regression)", {
  # Optional Param 1 is a non-negative physical measurement (wt%, ECD,
  # area, etc. - confirmed with the user, so a signed/negative column was
  # ruled out as a real scenario). The one real edge case left is every
  # selected value being exactly 0: pointSize <- param1_values * (maxSize -
  # minPointSize) / max(param1_values, na.rm = TRUE) + minPointSize then
  # divides by 0, producing NaN for every point - which the function's own
  # end-of-function safety check silently caught and replaced the WHOLE
  # pointSize vector with a uniform default, with only a generic "Point
  # size vector has issues. Reinitializing." message and no indication why.
  # Fixed with a direct max <= 0 guard that maps straight to minPointSize
  # (matching the formula's own "0 -> minPointSize" assumption) instead of
  # dividing by zero and relying on the safety net to paper over it.
  d <- data.frame(opt1 = c(0, 0, 0, 0, 0))
  pts <- data.frame(A = c(0.3, 0.3, 0.4, 0.2, 0.5), B = c(0.3, 0.3, 0.3, 0.4, 0.3), C = c(0.4, 0.4, 0.3, 0.4, 0.2))

  console_output <- capture.output(
    result <- compute_point_styling(
      ternary_points1 = pts, matrika = d,
      optional_param1 = list(col = "opt1"), optional_param1_representation = "point_size",
      optional_param2 = NULL, color_palette = "blue",
      use_manual_point_size = FALSE, manual_point_size = NULL,
      is_categorical_group = FALSE, selected_groups = NULL
    )
  )

  expect_false(anyNA(result$pointSize))
  expect_true(all(result$pointSize == 0.1))
  expect_false(any(grepl("has issues\\. Reinitializing", console_output)))
})

test_that("compute_point_styling() under Point Size representation no longer prints a spurious pointType 'Reinitializing' warning on a normal render (regression)", {
  # pointType <- 16 was assigned as a bare scalar (length 1) inside this
  # branch instead of a correctly-shaped rep(16, n_points) - the exact same
  # "reset to a bare scalar, silently caught and reshaped by the safety
  # net" bug class pass 5 already found and fixed for the sibling
  # "neither optional_param1 nor param2 set" branches (see this file's own
  # sibling test in test-pass5-regressions.R), just missed here. Harmless
  # to the final pointType values (always 16 either way), but printed
  # "Point type vector has issues. Reinitializing." on every single
  # Point-Size-representation render, not just an edge case.
  d <- data.frame(opt1 = c(5, 10, 15, 20, 25))
  pts <- data.frame(A = c(0.3, 0.3, 0.4, 0.2, 0.5), B = c(0.3, 0.3, 0.3, 0.4, 0.3), C = c(0.4, 0.4, 0.3, 0.4, 0.2))

  console_output <- capture.output(
    result <- compute_point_styling(
      ternary_points1 = pts, matrika = d,
      optional_param1 = list(col = "opt1"), optional_param1_representation = "point_size",
      optional_param2 = NULL, color_palette = "blue",
      use_manual_point_size = FALSE, manual_point_size = NULL,
      is_categorical_group = FALSE, selected_groups = NULL
    )
  )

  expect_length(result$pointType, 5)
  expect_true(all(result$pointType == 16))
  expect_false(any(grepl("has issues\\. Reinitializing", console_output)))
})
