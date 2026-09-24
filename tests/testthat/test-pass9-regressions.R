# Regression tests for pass 9 of the vidternary Structural Audit - one
# test_that() per finding fixed this pass, matching test-pass5..8-regressions.R.
library(shiny)

# prepare_ternary_plot_data() with sensible defaults, overridden per test.
pass9_pd <- function(d, ...) {
  xlsx_path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, xlsx_path)
  defaults <- list(
    xlsx_file = xlsx_path, working_dir = getwd(), output_dir = tempfile(),
    element_A = list(col = "Al"), element_B = list(col = "Si"), element_C = list(col = "Mn"),
    optional_param1 = NULL, optional_param2 = NULL, color_palette = "blue",
    xlsx_display_name = NULL, preview = TRUE,
    use_mahalanobis = FALSE, reference_data = NULL,
    optional_param1_representation = "point_size", output_format = "png",
    use_isolation_forest = FALSE, isolation_ntrees = 100, isolation_contamination = 0.10,
    use_iqr_filter = FALSE, use_zscore_filter = FALSE, use_mad_filter = FALSE,
    stat_filter_log10 = FALSE, lambda = 1, omega = 0,
    keep_outliers_mahalanobis = FALSE, keep_outliers_isolation = FALSE,
    keep_outliers_iqr = FALSE, keep_outliers_zscore = FALSE, keep_outliers_mad = FALSE,
    individual_filters_A = NULL, individual_filters_B = NULL, individual_filters_C = NULL,
    custom_mdthresh = NULL, mdthresh_mode = "auto", mahalanobis_reference = "self",
    selected_columns = c("Al", "Si"),
    include_plot_notes = TRUE, use_manual_point_size = FALSE, manual_point_size = 1.0,
    selected_groups = NULL, is_categorical_group = FALSE
  )
  do.call(prepare_ternary_plot_data, utils::modifyList(defaults, list(...)))
}

pass9_data <- function(n = 40, seed = 1) {
  set.seed(seed)
  data.frame(Al = abs(rnorm(n, 5, 1)), Si = abs(rnorm(n, 5, 1)), Mn = abs(rnorm(n, 5, 1)))
}

# ---- Item 2: a skipped/failed multivariate filter must not be labelled "(filtered)" ----

test_that("a Mahalanobis filter skipped for lack of a reference dataset is labelled NOT APPLIED, with a warning (regression)", {
  expect_warning(
    pd <- pass9_pd(pass9_data(), use_mahalanobis = TRUE, mahalanobis_reference = "dataset2"),
    "NOT applied"
  )
  expect_equal(pd$mv_status, "skipped_no_reference")
  expect_null(pd$mahal_result)
  expect_match(pd$plot_title, "Mahalanobis \\(NOT APPLIED\\)")
  expect_false(grepl("(filtered)", pd$plot_title, fixed = TRUE))
  notes <- strsplit(pd$col3_text, "\n")[[1]]
  expect_true(any(grepl("NOT APPLIED - plotted data is unfiltered", notes, fixed = TRUE)))
  expect_true(any(grepl("^  Reason: no reference dataset available", notes)))
  # Unfiltered: every valid row is still plotted.
  expect_equal(nrow(pd$ternary_points1), 40)
})

test_that("a Mahalanobis filter that errors is labelled NOT APPLIED and drops any partial result (regression)", {
  # A selected column absent from the data makes compute_mahalanobis_distance() stop().
  expect_warning(
    pd <- pass9_pd(pass9_data(), use_mahalanobis = TRUE, selected_columns = c("Al", "NoSuchColumn")),
    "NOT applied"
  )
  expect_equal(pd$mv_status, "failed")
  expect_null(pd$mahal_result)
  expect_match(pd$plot_title, "Mahalanobis \\(NOT APPLIED\\)")
})

test_that("a Mahalanobis filter that actually ran is still labelled (filtered), with no warning", {
  expect_no_warning(pd <- pass9_pd(pass9_data(), use_mahalanobis = TRUE))
  expect_equal(pd$mv_status, "applied")
  expect_match(pd$plot_title, "Mahalanobis(filtered)", fixed = TRUE)
  expect_false(any(grepl("NOT APPLIED", strsplit(pd$col3_text, "\n")[[1]])))
})

test_that("build_ternary_plot_title() defaults to the old labelling when mv_status isn't supplied", {
  t <- build_ternary_plot_title(
    list(col = "Al"), list(col = "Si"), list(col = "Mn"), NULL, "point_size", NULL,
    TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE,
    "x", NULL, NULL, function(p) paste(p, collapse = "\n"))
  expect_match(t$plot_title, "Mahalanobis(filtered)", fixed = TRUE)
})

# ---- Item 3: Mahalanobis threshold calibrated on the analysed data (Vode et al. 2022) ----

test_that("MDmean/stdMD are the mean/SD of data1's distances against the reference (Vode et al. 2022), not the reference's own", {
  set.seed(5)
  ref <- data.frame(a = rnorm(30), b = rnorm(30))
  tgt <- data.frame(a = c(rnorm(45), rnorm(5, 8)), b = c(rnorm(45), rnorm(5, 8)))
  res <- compute_mahalanobis_distance(tgt, ref, selected_columns = c("a", "b"))
  d_tgt <- mahalanobis(tgt, colMeans(ref), cov(ref))
  expect_equal(res$MDmean, mean(d_tgt), tolerance = 1e-10)
  expect_equal(res$stdMD, sd(d_tgt), tolerance = 1e-10)
  expect_equal(res$MDthresh, mean(d_tgt) + sqrt(100 / 101) * sd(d_tgt), tolerance = 1e-10)
})

# ---- Item 6: EVS / Spatial / Point Pattern take a single file ----

test_that("create_file_selection_column(multiple = FALSE) builds a single-file picker", {
  multi <- as.character(create_file_selection_column("f", multiple = TRUE))
  single <- as.character(create_file_selection_column("f", multiple = FALSE))
  expect_match(multi, "multiple", fixed = TRUE)
  expect_false(grepl("multiple=\"multiple\"", single, fixed = TRUE))
  expect_match(single, "Select Excel File", fixed = TRUE)
})

pass9_upload <- function(n_files) {
  do.call(rbind, lapply(seq_len(n_files), function(i) {
    p <- tempfile(fileext = ".xlsx")
    openxlsx::write.xlsx(data.frame(area = c(1, 2, 3), field = c("A", "B", "C")), p)
    data.frame(name = paste0("specimen", i, ".xlsx"), size = file.info(p)$size, type = "",
               datapath = p, stringsAsFactors = FALSE)
  }))
}

test_that("make_combined_upload_reactive(allow_multiple = FALSE) rejects more than one file, and still accepts one", {
  server <- function(input, output, session) {
    single_only <- make_combined_upload_reactive(input, "files", allow_multiple = FALSE)
    multi_ok <- make_combined_upload_reactive(input, "files")
    output$single <- renderText(nrow(single_only()))
    output$multi <- renderText(nrow(multi_ok()))
  }
  testServer(server, {
    session$setInputs(files = pass9_upload(2))
    expect_error(output$single, "one file")
    expect_equal(output$multi, "6")
    session$setInputs(files = pass9_upload(1))
    expect_equal(output$single, "3")
  })
})

test_that("the EVS tab refuses a two-file upload instead of merging same-named fields across specimens", {
  server <- function(input, output, session) {
    shiny::moduleServer("evs", function(input, output, session) {
      create_server_evs(input, output, session, shiny::reactiveValues(),
                        function(...) invisible(NULL), function(...) invisible(NULL))
    })
  }
  testServer(server, {
    session$setInputs(`evs-evs_files` = pass9_upload(2))
    session$setInputs(`evs-evs_area_col` = "area", `evs-evs_group_col` = "field")
    session$setInputs(`evs-evs_fit` = 1)
    expect_error(output[["evs-evs_status"]], "one file")
  })
})

# ---- Item 7: Ripley-Rasson window estimate in both spatial tabs ----

test_that("clark_evans_test() uses the Ripley-Rasson window for both window types", {
  set.seed(3)
  x <- runif(50); y <- runif(50)
  rect <- clark_evans_test(x, y, n_sim = 19, window = "rectangle")
  hull <- clark_evans_test(x, y, n_sim = 19, window = "convex_hull")
  expect_equal(rect$area, spatstat.geom::area.owin(spatstat.geom::ripras(x, y, shape = "rectangle")))
  expect_equal(hull$area, spatstat.geom::area.owin(spatstat.geom::ripras(x, y, shape = "convex")))
  # Enlarged relative to the tight windows it replaces.
  expect_gt(rect$area, diff(range(x)) * diff(range(y)))
  expect_gt(hull$area, spatstat.geom::area.owin(spatstat.geom::convexhull.xy(x, y)))
})

test_that("build_point_pattern()'s convex-hull window is the Ripley-Rasson enlarged hull", {
  set.seed(4)
  x <- runif(40, 0, 10); y <- runif(40, 0, 10)
  pp <- build_point_pattern(x, y, window = "convex_hull")
  a <- spatstat.geom::area.owin(spatstat.geom::Window(pp))
  expect_equal(a, spatstat.geom::area.owin(spatstat.geom::ripras(x, y, shape = "convex")))
  expect_gt(a, spatstat.geom::area.owin(spatstat.geom::convexhull.xy(x, y)))
})

# ---- Item 8: decimal commas are rejected, never reinterpreted ----

test_that("a decimal comma in a Ternary element filter is rejected, not read as a 10x larger threshold", {
  M <- data.frame(Al = c(0.5, 1.2, 2, 8, 20), Si = 1:5, Mn = 1:5)
  expect_error(
    apply_element_and_parameter_filters(M, list(col = "Al"), list(col = "Si"), list(col = "Mn"),
                                        list(Al = "> 1,5"), NULL, NULL, NULL, NULL, preview = TRUE),
    "decimal separator"
  )
  kept <- apply_element_and_parameter_filters(M, list(col = "Al"), list(col = "Si"), list(col = "Mn"),
                                              list(Al = "> 1.5"), NULL, NULL, NULL, NULL, preview = TRUE)$M
  expect_equal(kept$Al, c(2, 8, 20))
})

test_that("a decimal comma in a pre-analysis filter is rejected with the same clear message", {
  expect_error(apply_pre_filters(data.frame(Area = 1:3), list(Area = "> 1,5")), "decimal separator")
})

# ---- Item 9: point size scales by area and the legend matches the points ----

test_that("param1_point_size() makes symbol area (size above the floor, squared) proportional to the value", {
  s <- param1_point_size(c(0, 25, 50, 100), max_value = 100, min_size = 0.1, max_size = 2.5)
  expect_equal(s[1], 0.1)
  expect_equal(s[4], 2.5)
  scaled <- (s - 0.1) / (2.5 - 0.1)
  expect_equal(scaled^2, c(0, 0.25, 0.5, 1), tolerance = 1e-12)
  # Negative values clamp to the floor instead of producing NaN.
  expect_equal(param1_point_size(-5, 100, 0.1, 2.5), 0.1)
})

test_that("plotted point sizes come from param1_point_size(), and the size legend draws the same sizes", {
  d <- pass9_data(30)
  d$ECD <- seq(1, 30)
  pd <- pass9_pd(d, optional_param1 = list(col = "ECD", filter = NULL), include_plot_notes = FALSE)
  expect_equal(pd$pointSize, param1_point_size(pd$param1_values, max(pd$param1_values), 0.1, 2.5))

  png(tempfile(fileext = ".png"))
  plot.new()
  drawn <- draw_param1_size_legend(pd$param1_values, "ECD", 0.1, 2.5)
  dev.off()
  expect_equal(drawn$sizes, param1_point_size(drawn$values, max(pd$param1_values), 0.1, 2.5))
  expect_equal(drawn$values[1], max(pd$param1_values))
  # The legend's smallest entry is the data minimum, drawn at the size a
  # point with that value actually gets - not at the floor size.
  expect_equal(tail(drawn$values, 1), min(pd$param1_values))
  expect_equal(tail(drawn$sizes, 1), pd$pointSize[which.min(pd$param1_values)])
})

test_that("the preview renders with a size legend without needing PlotTools", {
  d <- pass9_data(30)
  d$ECD <- seq(1, 30)
  pd <- pass9_pd(d, optional_param1 = list(col = "ECD", filter = NULL))
  f <- tempfile(fileext = ".png")
  png(f, width = 800, height = 900)
  expect_no_error(render_ternary_plot_preview(pd))
  dev.off()
  expect_gt(file.info(f)$size, 0)
})

# ---- Item 10: missing values never create phantom rows ----

test_that("the pre-analysis filter drops rows with a missing value instead of adding all-NA rows", {
  d <- data.frame(Area = c(1, NA, 5, 10), id = 1:4)
  expect_equal(apply_pre_filters(d, list(Area = "> 2"))$id, c(3L, 4L))
  expect_equal(apply_pre_filters(d, list(Area = "!= 5"))$id, c(1L, 4L))
  expect_false(anyNA(apply_filter(d, "Area", "<= 10")$id))
})

test_that("Ternary element filters drop rows with a missing value instead of adding all-NA rows", {
  M <- data.frame(Al = c(1, NA, 5, 10), Si = 1:4, Mn = 1:4, id = 1:4)
  single <- apply_element_and_parameter_filters(M, list(col = "Al"), list(col = "Si"), list(col = "Mn"),
                                                list(Al = "> 2"), NULL, NULL, NULL, NULL, preview = TRUE)$M
  expect_equal(single$id, c(3L, 4L))
  multi <- apply_element_and_parameter_filters(M, list(col = c("Al", "Si"), filter = "> 0"),
                                               list(col = "Mn"), list(col = "id"),
                                               NULL, NULL, NULL, NULL, NULL, preview = TRUE)$M
  expect_false(anyNA(multi$id))
  opt <- apply_element_and_parameter_filters(M, list(col = "Si"), list(col = "Mn"), list(col = "id"),
                                             NULL, NULL, NULL, list(col = "Al", filter = "< 8"), NULL,
                                             preview = TRUE)$M
  expect_equal(opt$id, c(1L, 3L))
})
