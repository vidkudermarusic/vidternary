# Regression guards for pass 5's findings (see the vidternary Structural
# Audit's §08 recommendation: "aim new test coverage at what this pass
# proved, not a general push"). Each test below reproduces the exact
# scenario that used to crash or misbehave, against the now-fixed code -
# scoped deliberately narrow (one test per finding, not broad coverage
# expansion) so each one stands as a permanent guard against that specific
# regression reappearing silently.
#
# library(shiny) is required here for the testServer()-based tests (see
# test-server-evs.R's own header comment for the full explanation: this
# package attaches shiny at app-launch time via dependencies.R's
# initialize_packages(), not via NAMESPACE import(), so it isn't already on
# the search path during `devtools::test()`).
library(shiny)

make_upload <- function(df) {
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(df, path)
  data.frame(name = basename(path), size = file.info(path)$size, type = "",
             datapath = path, stringsAsFactors = FALSE)
}

# ---- Crash: R >= 4.3's &&-on-a-vector hard error, multi-column optional param ----
# (ternary_plot_data_prep.R:optional_param1$col %in% names(matrika) inside
# &&, and helpers_filters.R's input$multiple_optional_param1 != "" inside
# && - both fixed by wrapping in all()/replacing with !any(...== "")).

test_that("prepare_ternary_plot_data() no longer crashes on a multi-column optional parameter", {
  d <- data.frame(Al = runif(20, 1, 20), Si = runif(20, 1, 20), Mn = runif(20, 1, 20),
                   opt1 = runif(20, 0, 30), opt2 = runif(20, 0, 30))
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, tmp_xlsx)
  out_dir <- tempfile("outdir"); dir.create(out_dir)

  expect_no_error(
    result <- prepare_ternary_plot_data(
      xlsx_file = tmp_xlsx, working_dir = tempdir(), output_dir = out_dir,
      element_A = list(col = "Al"), element_B = list(col = "Si"), element_C = list(col = "Mn"),
      optional_param1 = list(col = c("opt1", "opt2"), filter = NULL),  # 2 columns - the crash trigger
      optional_param2 = NULL, color_palette = NULL, xlsx_display_name = "test.xlsx",
      preview = TRUE, use_mahalanobis = FALSE, reference_data = NULL,
      optional_param1_representation = "point_size", output_format = "png",
      use_isolation_forest = FALSE, use_iqr_filter = FALSE, use_zscore_filter = FALSE, use_mad_filter = FALSE,
      lambda = 1, omega = 0, keep_outliers_mahalanobis = FALSE, keep_outliers_isolation = FALSE,
      keep_outliers_iqr = FALSE, keep_outliers_zscore = FALSE, keep_outliers_mad = FALSE,
      individual_filters_A = NULL, individual_filters_B = NULL, individual_filters_C = NULL,
      custom_mdthresh = NULL, mdthresh_mode = "auto", mahalanobis_reference = NULL,
      selected_columns = NULL, include_plot_notes = FALSE, use_manual_point_size = FALSE,
      manual_point_size = NULL, selected_groups = NULL, is_categorical_group = FALSE
    )
  )
  expect_setequal(result$optional_columns, c("opt1", "opt2"))
})

test_that("extract_ternary_params(multiple_mode = TRUE) no longer crashes on a multi-column optional parameter", {
  fake_input <- list(
    multiple_element_A = "Al", multiple_element_B = "Si", multiple_element_C = "Mn",
    multiple_optional_param1 = c("opt1", "opt2"),  # 2 columns - the crash trigger
    multiple_optional_param1_representation = "point_size",
    multiple_optional_param2 = NULL, multiple_color_palette = NULL
  )
  rv <- list(xlsx_file1 = "dummy.xlsx")
  expect_no_error(
    params <- extract_ternary_params(fake_input, rv, dataset_num = 1, preview = TRUE,
                                      multiple_mode = TRUE)
  )
  expect_setequal(params$optional_param1$col, c("opt1", "opt2"))
})

# ---- Latent crash: same &&-on-a-vector class, found on a fresh audit
# recheck of the theme above rather than by tracing a real user report -
# extract_ternary_params()'s "Additional safety check for categorical
# detection" block (helpers_filters.R). Dormant on the app's current call
# graph (the only multiple_mode = TRUE caller always passes a temp_rv with
# no df<n> set, so !is.null(data) short-circuits false before the vector
# comparison), but the test below supplies a populated rv$df1 directly -
# exactly what a future refactor of the batch handler could do - to prove
# the fix holds even once that short-circuit no longer protects it, not
# just on today's one reachable path.

test_that("extract_ternary_params(multiple_mode = TRUE) no longer crashes on a multi-column optional_param2 even if rv$df1 is populated", {
  d <- data.frame(Al = runif(10, 1, 20), Si = runif(10, 1, 20), Mn = runif(10, 1, 20),
                   opt2a = runif(10, 0, 30), opt2b = runif(10, 0, 30))
  fake_input <- list(
    multiple_element_A = "Al", multiple_element_B = "Si", multiple_element_C = "Mn",
    multiple_optional_param1 = NULL,
    multiple_optional_param2 = c("opt2a", "opt2b"),  # 2 columns - the crash trigger
    multiple_color_palette = NULL
  )
  rv <- list(xlsx_file1 = "dummy.xlsx", df1 = d)  # populated, unlike the app's real batch path today
  expect_no_error(
    params <- extract_ternary_params(fake_input, rv, dataset_num = 1, preview = TRUE,
                                      multiple_mode = TRUE)
  )
  expect_setequal(params$optional_param2$col, c("opt2a", "opt2b"))
  # The categorical-detection safety check is correctly skipped for a
  # multi-column selection (it was never a coherent check for that case -
  # see the comment at the guard itself), not silently miscomputed.
  expect_false(params$is_categorical_group)
})

test_that("extract_ternary_params()'s categorical-detection safety check still upgrades is_categorical_group for a genuine single-column categorical optional_param2", {
  d <- data.frame(Al = runif(10, 1, 20), Si = runif(10, 1, 20), Mn = runif(10, 1, 20),
                   Category = sample(c("Oxide", "Sulfide"), 10, replace = TRUE), stringsAsFactors = FALSE)
  fake_input <- list(
    multiple_element_A = "Al", multiple_element_B = "Si", multiple_element_C = "Mn",
    multiple_optional_param1 = NULL,
    multiple_optional_param2 = "Category",  # single column - safety check should still run
    multiple_color_palette = NULL
  )
  rv <- list(xlsx_file1 = "dummy.xlsx", df1 = d)
  params <- extract_ternary_params(fake_input, rv, dataset_num = 1, preview = TRUE,
                                    multiple_mode = TRUE)
  expect_true(params$is_categorical_group)
})

# ---- Crash: cleared numericInput reports NA_real_, not NULL ----
# (validate_mahalanobis_inputs() - if(NA) is a hard error rather than the
# intended stop()-based validation message).

test_that("validate_mahalanobis_inputs() rejects NA/NaN lambda/omega/custom_mdthresh cleanly instead of crashing", {
  expect_error(validate_mahalanobis_inputs(NA_real_, 0, NULL, "auto", NULL), "non-negative")
  expect_error(validate_mahalanobis_inputs(1, NaN, NULL, "auto", NULL), "non-negative")
  expect_error(validate_mahalanobis_inputs(1, 0, NA_real_, "manual", NULL), "positive")
  # regression: normal valid inputs still pass
  expect_true(validate_mahalanobis_inputs(1, 0, NULL, "auto", NULL))
})

# ---- Crash: an Inf value anywhere in the selected columns ----
# (validate_multivariate_data()'s zero-variance check: var() returns NaN
# for a column containing Inf, NaN == 0 is NA, and any(zero_var_cols)
# feeding a bare NA into if() is a hard error on R >= 4.3).

test_that("validate_multivariate_data() no longer crashes on an Inf value in the data", {
  set.seed(1)
  n <- 20
  data_with_inf <- data.frame(Al = c(rnorm(n - 1, 10, 2), Inf), Si = rnorm(n, 20, 3), Mn = rnorm(n, 5, 1))
  data_ref <- data.frame(Al = rnorm(n, 10, 2), Si = rnorm(n, 20, 3), Mn = rnorm(n, 5, 1))

  expect_warning(
    result <- validate_multivariate_data(data_with_inf, data_ref, selected_columns = c("Al", "Si", "Mn"),
                                          method = "Test", min_obs_ratio = 2),
    "Zero variance"
  )
  expect_true("Al" %in% result$zero_var_cols)
})

# ---- Crash: outlier_95/outlier_99 lacked outlier_indices' own NA-coercion ----
# (compute_mahalanobis_distance() - masked until the Inf-in-data crash
# above was fixed, since that used to crash first).

test_that("compute_mahalanobis_distance()'s outlier_95/outlier_99 stay real numbers, not NA, with Inf in the data", {
  set.seed(2)
  d1_inf <- data.frame(a = c(rnorm(19), Inf), b = rnorm(20))
  d2_ref <- data.frame(a = rnorm(30), b = rnorm(30))
  result <- suppressWarnings(compute_mahalanobis_distance(d1_inf, d2_ref, selected_columns = c("a", "b")))
  expect_false(is.na(result$outlier_95))
  expect_false(is.na(result$outlier_99))
})

# ---- Crash: a "Filter by value" selection that excludes every row ----
# (build_custom_plot(type = "bar") - table() on a length-0 plain vector
# returns a dimnames-less "table of extent 0", breaking the subsequent
# names(d2) <- c(x, ...) assignment in all 4 percent x color branches).

test_that("build_custom_plot(type = 'bar') no longer crashes when a value filter removes every row", {
  d <- data.frame(cat = c("A", "B", "A", "C"), grp = c("x", "y", "x", "y"), stringsAsFactors = FALSE)
  for (has_color in c(FALSE, TRUE)) {
    for (percent in c(FALSE, TRUE)) {
      expect_no_error(
        build_custom_plot(d, type = "bar", x = "cat",
                           color_by = if (has_color) "grp" else "none",
                           percent = percent, bar_values = "ZZZ_not_a_real_value")
      )
    }
  }
})

# ---- Crash: NA/NaN threshold/multiplier params (same class as the Mahalanobis one above) ----
# (statistical_filters.R's three apply_*_filter() functions).

test_that("apply_iqr_filter()/apply_zscore_filter()/apply_mad_filter() reject NA/NaN thresholds cleanly instead of crashing", {
  d <- data.frame(x = c(1, 2, 3, 4, 5, 100))
  expect_error(apply_iqr_filter(d, "x", multiplier = NA_real_), "non-negative")
  expect_error(apply_zscore_filter(d, "x", threshold = NaN), "non-negative")
  expect_error(apply_mad_filter(d, "x", threshold = NA_real_), "non-negative")
  # regression: normal calls still filter correctly
  expect_lt(nrow(apply_iqr_filter(d, "x")), nrow(d))
})

# ---- Crash / silent misbehavior: compute_isolation_forest()'s 3 statistics-layer gaps ----
# (non-numeric selected column silently dropped instead of erroring;
# contamination completely unvalidated - out-of-range raised a raw
# quantile() error, NA silently produced zero flagged outliers; the
# target dataset's own minimum-row guard was missing, reaching predict()
# with 0 rows and a raw isotree error).

test_that("compute_isolation_forest() validates non-numeric columns, contamination, and target row count instead of crashing or silently misbehaving", {
  set.seed(1)
  ref <- data.frame(a = rnorm(30), b = rnorm(30), label = sample(letters[1:3], 30, replace = TRUE), stringsAsFactors = FALSE)
  target <- data.frame(a = rnorm(20), b = rnorm(20), label = sample(letters[1:3], 20, replace = TRUE), stringsAsFactors = FALSE)

  # (1) non-numeric column now errors by name instead of being silently dropped
  err <- expect_error(compute_isolation_forest(target, ref, selected_columns = c("a", "b", "label")))
  expect_match(conditionMessage(err), "label")

  # (2) contamination validated - out-of-range and NA both rejected with one clear message
  expect_error(compute_isolation_forest(target[, c("a", "b")], ref[, c("a", "b")], selected_columns = c("a", "b"), contamination = 1.5),
               "contamination")
  expect_error(compute_isolation_forest(target[, c("a", "b")], ref[, c("a", "b")], selected_columns = c("a", "b"), contamination = NA_real_),
               "contamination")

  # (3) a target with zero complete rows gets a friendly message, not the raw isotree/predict error
  target_allna <- data.frame(a = rep(NA_real_, 10), b = rnorm(10))
  err2 <- expect_error(compute_isolation_forest(target_allna, ref[, c("a", "b")], selected_columns = c("a", "b")))
  expect_false(grepl("newdata", conditionMessage(err2)))

  # regression: a normal, fully-valid call still works end-to-end
  expect_no_error(compute_isolation_forest(target[, c("a", "b")], ref[, c("a", "b")], selected_columns = c("a", "b")))
})

# ---- Filter-parser consolidation: leniency and error-format consistency ----
# (ternary_plot_data_prep.R's optional_param1/optional_param2 filtering
# used to have its own inline copy-pasted parser instead of going through
# the shared parse_filter_condition()/apply_filter() - a malformed value
# with a stray unit hard-errored here but was silently cleaned up on every
# other filter path, and an unrecognized operator was silently ignored
# instead of erroring).

test_that("optional_param1's filter now matches every other filter path's leniency and error behavior", {
  d <- data.frame(Al = runif(20, 1, 20), Si = runif(20, 1, 20), Mn = runif(20, 1, 20), opt = runif(20, 0, 30))
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, tmp_xlsx)
  out_dir <- tempfile("outdir"); dir.create(out_dir)

  base_args <- list(
    xlsx_file = tmp_xlsx, working_dir = tempdir(), output_dir = out_dir,
    element_A = list(col = "Al"), element_B = list(col = "Si"), element_C = list(col = "Mn"),
    optional_param1 = list(col = "opt", filter = "> 10"), optional_param2 = NULL,
    color_palette = NULL, xlsx_display_name = "test.xlsx", preview = TRUE,
    use_mahalanobis = FALSE, reference_data = NULL, optional_param1_representation = "point_size",
    output_format = "png", use_isolation_forest = FALSE, use_iqr_filter = FALSE,
    use_zscore_filter = FALSE, use_mad_filter = FALSE, lambda = 1, omega = 0,
    keep_outliers_mahalanobis = FALSE, keep_outliers_isolation = FALSE, keep_outliers_iqr = FALSE,
    keep_outliers_zscore = FALSE, keep_outliers_mad = FALSE, individual_filters_A = NULL,
    individual_filters_B = NULL, individual_filters_C = NULL, custom_mdthresh = NULL,
    mdthresh_mode = "auto", mahalanobis_reference = NULL, selected_columns = NULL,
    include_plot_notes = FALSE, use_manual_point_size = FALSE, manual_point_size = NULL,
    selected_groups = NULL, is_categorical_group = FALSE
  )
  clean <- do.call(prepare_ternary_plot_data, base_args)

  # a stray "%" character now recovers identically to the clean filter, instead of hard-erroring.
  # as.numeric()'s own strip-and-retry attempt on the raw "10%" (before the
  # fallback regex strip kicks in) emits a base R "NAs introduced by
  # coercion" warning - expected, and already covered by parse_filter_
  # condition()'s own dedicated test elsewhere; suppressed here just to
  # keep this test focused on the leniency behavior itself.
  args_lenient <- base_args
  args_lenient$optional_param1 <- list(col = "opt", filter = "> 10%")
  lenient <- suppressWarnings(do.call(prepare_ternary_plot_data, args_lenient))
  expect_equal(nrow(lenient$M), nrow(clean$M))

  # an unrecognized operator now errors clearly instead of silently applying no filter
  args_bad_op <- base_args
  args_bad_op$optional_param1 <- list(col = "opt", filter = "<>10")
  expect_error(do.call(prepare_ternary_plot_data, args_bad_op), "Invalid filter format")
})

# ---- Negative Optional Param 1 values under Point Size representation ----
# (unclipped size-mapping formula produced invisible or oversized points
# with no warning).

test_that("negative Optional Param 1 values no longer produce invisible/oversized points under Point Size representation", {
  d <- data.frame(Al = runif(15, 1, 20), Si = runif(15, 1, 20), Mn = runif(15, 1, 20),
                   signed_col = c(-5, -2, -1, 0.5, 1, 3, 5, 8, 10, 12, -8, -3, 2, 4, 6))
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, tmp_xlsx)
  out_dir <- tempfile("outdir"); dir.create(out_dir)

  args <- list(
    xlsx_file = tmp_xlsx, working_dir = tempdir(), output_dir = out_dir,
    element_A = list(col = "Al"), element_B = list(col = "Si"), element_C = list(col = "Mn"),
    optional_param1 = list(col = "signed_col", filter = NULL), optional_param2 = NULL,
    color_palette = NULL, xlsx_display_name = "test.xlsx", preview = TRUE,
    use_mahalanobis = FALSE, reference_data = NULL, optional_param1_representation = "point_size",
    output_format = "png", use_isolation_forest = FALSE, use_iqr_filter = FALSE,
    use_zscore_filter = FALSE, use_mad_filter = FALSE, lambda = 1, omega = 0,
    keep_outliers_mahalanobis = FALSE, keep_outliers_isolation = FALSE, keep_outliers_iqr = FALSE,
    keep_outliers_zscore = FALSE, keep_outliers_mad = FALSE, individual_filters_A = NULL,
    individual_filters_B = NULL, individual_filters_C = NULL, custom_mdthresh = NULL,
    mdthresh_mode = "auto", mahalanobis_reference = NULL, selected_columns = NULL,
    include_plot_notes = FALSE, use_manual_point_size = FALSE, manual_point_size = NULL,
    selected_groups = NULL, is_categorical_group = FALSE
  )
  expect_warning(result <- do.call(prepare_ternary_plot_data, args), "negative or out-of-range")
  expect_true(all(result$pointSize >= 0.1 - 1e-9))
  expect_true(all(result$pointSize <= 2.5 + 1e-9))
})

# ---- "Select All" sent the label-side value instead of the value-side one ----
# (server_ternary_plots_groups.R - checkboxGroupInput's name=label/value=
# value convention means the choices' names() are the labels, not what's
# actually submitted).

make_ternary_plots_server <- function() {
  rv <- shiny::reactiveValues()
  show_message <- function(message, type = "info") invisible(NULL)
  log_operation <- function(...) invisible(NULL)
  function(input, output, session) {
    shiny::moduleServer("ternary_plots", function(input, output, session) {
      create_server_file_handlers(input, output, session, rv, show_message, log_operation)
      create_server_ternary_plots(input, output, session, rv, show_message, log_operation)
    })
  }
}

test_that("'Select All' sends the real checkbox VALUES (not the label-side group names)", {
  d <- data.frame(val = runif(20), group = rep(c("Oxide", "Sulfide"), 10), stringsAsFactors = FALSE)
  testServer(make_ternary_plots_server(), {
    session$setInputs(`ternary_plots-xlsx_file1` = make_upload(d))
    session$setInputs(`ternary_plots-optional_param2_1` = "group")

    captured <- list()
    orig <- session$sendInputMessage
    session$sendInputMessage <- function(inputId, message) { captured[[inputId]] <<- message; orig(inputId, message) }

    session$setInputs(`ternary_plots-select_all_groups_1` = 1)

    sent <- captured[["selected_groups_1"]]$value
    expect_true(!is.null(sent))
    # The real submitted value is "Name (N samples)", not the bare group name
    expect_true(any(grepl("^Oxide \\(\\d+ samples\\)$", sent)))
    expect_true(any(grepl("^Sulfide \\(\\d+ samples\\)$", sent)))
  })
})

# ---- The validate()-message fix ported from EVS to its two structurally identical siblings ----
# (a plain tryCatch(..., error = function(e) NULL) swallows shiny::validate()'s
# specific message the same way it swallows req()'s empty one; EVS's own
# fix is already covered by test-server-evs.R, so only its two ports are
# guarded here).

make_spatial_server <- function() {
  rv <- shiny::reactiveValues()
  show_message <- function(message, type = "info") invisible(NULL)
  log_operation <- function(...) invisible(NULL)
  function(input, output, session) {
    shiny::moduleServer("spatial", function(input, output, session) {
      create_server_spatial(input, output, session, rv, show_message, log_operation)
    })
  }
}

test_that("spatial_status surfaces the specific validate() message instead of the generic placeholder", {
  d <- data.frame(x = runif(20), y = runif(20))
  testServer(make_spatial_server(), {
    session$setInputs(`spatial-spatial_files` = make_upload(d))
    session$setInputs(`spatial-spatial_x_col` = "not_a_real_column")
    session$setInputs(`spatial-spatial_analyze` = 1)
    err <- expect_error(output[["spatial-spatial_status"]])
    expect_match(conditionMessage(err), "Select a valid X coordinate column", fixed = TRUE)
  })
})

make_coda_server <- function() {
  rv <- shiny::reactiveValues()
  show_message <- function(message, type = "info") invisible(NULL)
  log_operation <- function(...) invisible(NULL)
  function(input, output, session) {
    shiny::moduleServer("coda", function(input, output, session) {
      create_server_coda(input, output, session, rv, show_message, log_operation)
    })
  }
}

test_that("coda_status surfaces the specific validate() message instead of the generic placeholder", {
  d <- data.frame(A = runif(20), B = runif(20))  # only 2 parts - below the required minimum of 3
  testServer(make_coda_server(), {
    session$setInputs(`coda-coda_files` = make_upload(d))
    session$setInputs(`coda-coda_parts` = c("A", "B"))
    session$setInputs(`coda-coda_run` = 1)
    err <- expect_error(output[["coda-coda_status"]])
    expect_match(conditionMessage(err), "Select at least 3", fixed = TRUE)
  })
})

# NOTE: no server-level (moduleServer("coda", ...)) regression test for the
# Inf fix below - confirmed empirically that it can't be exercised through
# a real file upload in this app: openxlsx::write.xlsx() has no way to
# store a literal Infinity in a numeric cell, so it writes Inf as a blank
# cell, which read.xlsx() reads back as NA, not Inf - the value never
# survives the round-trip to reach .coda_replace_zeros() at all (it's
# filtered out earlier by result()'s own complete.cases() check, as an
# ordinary missing value, before the fixed code ever runs). The fix still
# matters for this file's exported functions used any other way (direct
# calls, a future non-Excel data source), so it's kept and covered at the
# pure-logic level in test-compositional-data-analysis.R instead.

# ---- The comprehensive-panel reactivity fix ported to the single-method Mahalanobis panel ----
# (server_data_comparison_multivariate.R's output$mahalanobis_output used
# to read input$comparison_mv_lambda/etc. directly inside renderPrint(),
# so it silently recomputed on every keystroke rather than only on a
# button click).

make_data_comparison_server <- function() {
  rv <- shiny::reactiveValues()
  show_message <- function(message, type = "info") invisible(NULL)
  log_operation <- function(...) invisible(NULL)
  function(input, output, session) {
    shiny::moduleServer("data_comparison", function(input, output, session) {
      create_server_data_comparison(input, output, session, rv, show_message, log_operation)
    })
  }
}

test_that("mahalanobis_output no longer silently recomputes on every lambda/omega tweak", {
  set.seed(1)
  d <- data.frame(a = rnorm(30), b = rnorm(30), c = rnorm(30))
  testServer(make_data_comparison_server(), {
    session$setInputs(`data_comparison-comparison_files` = make_upload(d))
    session$setInputs(`data_comparison-comparison_mv_target` = tools::file_path_sans_ext(basename(input[["data_comparison-comparison_files"]]$name)))
    session$setInputs(`data_comparison-comparison_mv_reference` = "__self__")
    session$setInputs(`data_comparison-comparison_mv_columns` = c("a", "b", "c"))
    session$setInputs(`data_comparison-comparison_mv_lambda` = 1)
    session$setInputs(`data_comparison-mahalanobis_analysis` = 1)
    report1 <- output[["data_comparison-mahalanobis_output"]]

    # changing lambda WITHOUT clicking again must leave the report unchanged
    session$setInputs(`data_comparison-comparison_mv_lambda` = 5)
    report2 <- output[["data_comparison-mahalanobis_output"]]
    expect_identical(report1, report2)

    # clicking again with the new lambda must actually update it
    session$setInputs(`data_comparison-mahalanobis_analysis` = 2)
    report3 <- output[["data_comparison-mahalanobis_output"]]
    expect_false(identical(report1, report3))
  })
})

# ---- run_app()/create_app() reachable from a normal install ----
# (neither had roxygen docs or an @export tag, so neither had NAMESPACE
# presence - the README's own documented Quick Start,
# `library(vidternary); run_app()`, failed outside a devtools::load_all()'d
# dev environment).

test_that("run_app() and create_app() are exported (reachable from a normal install)", {
  ns <- asNamespace("vidternary")
  expect_true(exists("run_app", envir = ns, inherits = FALSE))
  expect_true(exists("create_app", envir = ns, inherits = FALSE))
  expect_true("run_app" %in% getNamespaceExports(ns))
  expect_true("create_app" %in% getNamespaceExports(ns))
})

# ---- Crash: a malformed Dataset 1/2 upload crashed the observer a second
# time, right after already showing the friendly error ----
# (server_file_handlers.R's upload handlers used to re-read the just-
# uploaded file a second time, unconditionally and unguarded by any
# tryCatch, purely to populate the multivariate column selector - a
# malformed/corrupted .xlsx failed the first, tryCatch'd read (showing a
# friendly "Error loading Dataset 1: ..." message), then immediately hit
# this same read again and failed a second time, this time uncaught,
# propagating out of the observer. Fixed by reusing the data frame already
# loaded inside the tryCatch instead of re-reading the file.)

make_malformed_upload <- function(name = "bad.xlsx") {
  path <- tempfile(fileext = ".xlsx")
  writeLines("this is not a real xlsx file, just plain text", path)
  data.frame(name = name, size = file.info(path)$size, type = "",
             datapath = path, stringsAsFactors = FALSE)
}

make_file_handlers_server <- function() {
  rv <- shiny::reactiveValues()
  messages <- list()
  show_message <- function(message, type = "info") messages[[length(messages) + 1]] <<- list(message = message, type = type)
  log_operation <- function(...) invisible(NULL)
  list(
    app = function(input, output, session) {
      shiny::moduleServer("ternary_plots", function(input, output, session) {
        create_server_file_handlers(input, output, session, rv, show_message, log_operation)
      })
    },
    rv = rv,
    get_messages = function() messages
  )
}

test_that("a malformed Dataset 1 upload shows the friendly error only - no second, uncaught crash", {
  server <- make_file_handlers_server()
  upload <- make_malformed_upload()
  expect_no_error(
    suppressWarnings(testServer(server$app, {
      session$setInputs(`ternary_plots-xlsx_file1` = upload)
    }))
  )
  messages <- server$get_messages()
  expect_true(any(sapply(messages, function(m) m$type == "error" && grepl("^Error loading Dataset 1:", m$message))))
})

test_that("a malformed Dataset 2 upload shows the friendly error only - no second, uncaught crash", {
  server <- make_file_handlers_server()
  upload <- make_malformed_upload()
  expect_no_error(
    suppressWarnings(testServer(server$app, {
      session$setInputs(`ternary_plots-xlsx_file2` = upload)
    }))
  )
  messages <- server$get_messages()
  expect_true(any(sapply(messages, function(m) m$type == "error" && grepl("^Error loading Dataset 2:", m$message))))
})

test_that("a genuine Dataset 1 upload still populates rv$df1 and the multivariate column selector correctly", {
  server <- make_file_handlers_server()
  d <- data.frame(Al = runif(10, 1, 20), Si = runif(10, 1, 20), Mn = runif(10, 1, 20),
                   Label = sample(c("a", "b"), 10, replace = TRUE), stringsAsFactors = FALSE)
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  upload <- data.frame(name = "good.xlsx", size = file.info(path)$size, type = "",
                        datapath = path, stringsAsFactors = FALSE)
  testServer(server$app, {
    orig <- session$sendInputMessage
    sent <- list()
    session$sendInputMessage <- function(inputId, message) { sent[[inputId]] <<- message; orig(inputId, message) }
    session$setInputs(`ternary_plots-xlsx_file1` = upload)

    expect_equal(nrow(server$rv$df1), 10)
    mv_choices <- regmatches(sent[["multivariate_columns"]]$options,
                              gregexpr('(?<=value=")[^"]+', sent[["multivariate_columns"]]$options, perl = TRUE))[[1]]
    # Only the numeric columns - the character "Label" column is excluded.
    expect_setequal(mv_choices, c("Al", "Si", "Mn"))
  })
})

# ---- Crash: compute_ternary_coordinates() on an overlapping/duplicate
# Element A/B/C selection ----
# Nothing in the UI (ui_ternary_plots_tab.R: three independent
# selectInputs) prevents picking the same column for two or more of
# Elements A/B/C. Two distinct, previously-uncaught raw indexing crashes
# followed, depending on exactly how much overlap there was:
#   - needed_columns collapsing to length 1 (every selected column
#     identical, e.g. A=B=C all the same single column) made
#     `M[, needed_columns]` (no drop = FALSE) silently return a plain
#     vector instead of a data frame -> "incorrect number of dimensions"
#     on the very next line.
#   - any partial duplicate in all_selected_elements (e.g. A and B sharing
#     one column while each also having other, distinct columns) made
#     `matrika[, all_selected_elements] <- ...` (an assignment, not a
#     read - reads tolerate duplicate names fine) throw "duplicate
#     subscripts for columns" instead.
# Both are now replaced by one clear, deliberate validation instead of
# either raw crash: no two of Elements A/B/C may select the exact same
# COMPLETE set of columns (checked via setequal(), order-independent) -
# collapsing every point onto a single edge or point is never a
# meaningful ternary diagram. A genuine PARTIAL overlap between two
# elements - explicitly confirmed as intentional, real usage (e.g. A:
# Fe+O, B: Al+O, C: Ti, where O legitimately contributes to more than one
# vertex in real oxide chemistry) - is not blocked and must still compute
# correctly.

test_that("selecting the identical single column for all three of A/B/C is rejected with a clear message, not a raw crash", {
  d <- data.frame(O = runif(10, 1, 20))
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, tmp_xlsx)

  err <- tryCatch(
    general_ternary_plot(
      xlsx_file = tmp_xlsx, working_dir = tempdir(), output_dir = NULL,
      element_A = list(col = "O"), element_B = list(col = "O"), element_C = list(col = "O"),
      preview = TRUE
    ),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "must each use a different set of columns", fixed = TRUE)
  expect_no_match(conditionMessage(err), "incorrect number of dimensions")
})

test_that("selecting the identical column for just two of A/B/C (the third different) is also rejected clearly", {
  err <- tryCatch(
    compute_ternary_coordinates(
      M = data.frame(A_col = 1:5, C_col = 5:1),
      all_selected_elements = c("A_col", "A_col", "C_col"),
      element_A = list(col = "A_col"), element_B = list(col = "A_col"), element_C = list(col = "C_col"),
      optional_param1 = NULL, optional_param2 = NULL, use_mahalanobis = FALSE, reference_data = NULL
    ),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "must each use a different set of columns", fixed = TRUE)
})

test_that("a genuine PARTIAL overlap between elements (e.g. A: Fe+O, B: Al+O, C: Ti) still computes correctly, not blocked", {
  d <- data.frame(Fe = c(10, 20, 30), O = c(5, 6, 7), Al = c(2, 3, 4), Ti = c(1, 1, 1))
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, tmp_xlsx)
  out_dir <- tempfile("outdir"); dir.create(out_dir)

  expect_no_error(
    result <- prepare_ternary_plot_data(
      xlsx_file = tmp_xlsx, working_dir = tempdir(), output_dir = out_dir,
      element_A = list(col = c("Fe", "O")), element_B = list(col = c("Al", "O")), element_C = list(col = "Ti"),
      optional_param1 = NULL, optional_param2 = NULL, color_palette = NULL, xlsx_display_name = "test.xlsx",
      preview = TRUE, use_mahalanobis = FALSE, reference_data = NULL,
      optional_param1_representation = "point_size", output_format = "png",
      use_isolation_forest = FALSE, use_iqr_filter = FALSE, use_zscore_filter = FALSE, use_mad_filter = FALSE,
      lambda = 1, omega = 0, keep_outliers_mahalanobis = FALSE, keep_outliers_isolation = FALSE,
      keep_outliers_iqr = FALSE, keep_outliers_zscore = FALSE, keep_outliers_mad = FALSE,
      individual_filters_A = NULL, individual_filters_B = NULL, individual_filters_C = NULL,
      custom_mdthresh = NULL, mdthresh_mode = "auto", mahalanobis_reference = NULL,
      selected_columns = NULL, include_plot_notes = FALSE, use_manual_point_size = FALSE,
      manual_point_size = NULL, selected_groups = NULL, is_categorical_group = FALSE
    )
  )
  expect_equal(nrow(result$ternary_points1), 3)
  # Row 1: A = Fe+O = 15, B = Al+O = 7, C = Ti = 1, total = 23.
  expect_equal(result$ternary_points1$A[1], 15 / 23)
  expect_equal(result$ternary_points1$B[1], 7 / 23)
  expect_equal(result$ternary_points1$C[1], 1 / 23)
})

test_that("three fully distinct single-column elements are unaffected (regression)", {
  d <- data.frame(A_col = c(10, 20, 30), B_col = c(1, 2, 3), C_col = c(5, 6, 7))
  result <- compute_ternary_coordinates(
    M = d, all_selected_elements = c("A_col", "B_col", "C_col"),
    element_A = list(col = "A_col"), element_B = list(col = "B_col"), element_C = list(col = "C_col"),
    optional_param1 = NULL, optional_param2 = NULL, use_mahalanobis = FALSE, reference_data = NULL
  )
  expect_equal(nrow(result$ternary_points1), 3)
})

# ---- Crash: compute_point_styling() with a categorical color column and
# no groups selected yet ----
# The moment a user picks a categorical column for Optional Param 2,
# rv$is_categorical_group_1/_2 flips to TRUE immediately
# (server_ternary_plots_groups.R's detection observer) - but
# rv$group_selections_1/_2 (selected_groups here) stays NULL/empty until
# the user actually checks a box in the group checklist that appears below
# it. Generating a plot in that gap - upload, pick A/B/C, pick a
# categorical color column, hit Save without first checking any group -
# used to fall through to the NUMERIC color-legend branch (is_categorical_
# group required a non-empty selected_groups too), which calls quantile()
# on the categorical column's raw character values: an immediate,
# uncaught "non-numeric argument to binary operator". Confirmed reachable
# through the real reactive server (testServer(), not just this function
# in isolation) before fixing.

extreme_value_categorical_test_data <- function() {
  set.seed(1)
  data.frame(Fe = runif(30, 10, 20), Cr = runif(30, 5, 15), Ni = runif(30, 1, 10),
             Label = paste0("id_", 1:30))
}

test_that("a categorical color column with no groups selected yet no longer crashes - shows all points instead", {
  d <- extreme_value_categorical_test_data()
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, tmp_xlsx)
  out_dir <- tempfile("outdir"); dir.create(out_dir)

  expect_no_error(
    result <- prepare_ternary_plot_data(
      xlsx_file = tmp_xlsx, working_dir = tempdir(), output_dir = out_dir,
      element_A = list(col = "Fe"), element_B = list(col = "Cr"), element_C = list(col = "Ni"),
      optional_param1 = NULL, optional_param2 = list(col = "Label", filter = NULL),
      color_palette = "blue", xlsx_display_name = "test.xlsx",
      preview = TRUE, use_mahalanobis = FALSE, reference_data = NULL,
      optional_param1_representation = "point_size", output_format = "png",
      use_isolation_forest = FALSE, use_iqr_filter = FALSE, use_zscore_filter = FALSE, use_mad_filter = FALSE,
      lambda = 1, omega = 0, keep_outliers_mahalanobis = FALSE, keep_outliers_isolation = FALSE,
      keep_outliers_iqr = FALSE, keep_outliers_zscore = FALSE, keep_outliers_mad = FALSE,
      individual_filters_A = NULL, individual_filters_B = NULL, individual_filters_C = NULL,
      custom_mdthresh = NULL, mdthresh_mode = "auto", mahalanobis_reference = NULL,
      selected_columns = NULL, include_plot_notes = FALSE, use_manual_point_size = FALSE,
      manual_point_size = NULL,
      selected_groups = NULL,  # the crash trigger: categorical, but nothing checked yet
      is_categorical_group = TRUE
    )
  )
  # Every row is kept (falls back to "show all groups"), not silently dropped.
  expect_equal(nrow(result$ternary_points1), 30)
})

test_that("explicitly selecting a real subset of categorical groups still filters correctly (regression)", {
  d <- extreme_value_categorical_test_data()
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, tmp_xlsx)
  out_dir <- tempfile("outdir"); dir.create(out_dir)

  result <- prepare_ternary_plot_data(
    xlsx_file = tmp_xlsx, working_dir = tempdir(), output_dir = out_dir,
    element_A = list(col = "Fe"), element_B = list(col = "Cr"), element_C = list(col = "Ni"),
    optional_param1 = NULL, optional_param2 = list(col = "Label", filter = NULL),
    color_palette = "blue", xlsx_display_name = "test.xlsx",
    preview = TRUE, use_mahalanobis = FALSE, reference_data = NULL,
    optional_param1_representation = "point_size", output_format = "png",
    use_isolation_forest = FALSE, use_iqr_filter = FALSE, use_zscore_filter = FALSE, use_mad_filter = FALSE,
    lambda = 1, omega = 0, keep_outliers_mahalanobis = FALSE, keep_outliers_isolation = FALSE,
    keep_outliers_iqr = FALSE, keep_outliers_zscore = FALSE, keep_outliers_mad = FALSE,
    individual_filters_A = NULL, individual_filters_B = NULL, individual_filters_C = NULL,
    custom_mdthresh = NULL, mdthresh_mode = "auto", mahalanobis_reference = NULL,
    selected_columns = NULL, include_plot_notes = FALSE, use_manual_point_size = FALSE,
    manual_point_size = NULL,
    selected_groups = c("id_1 (1 samples)", "id_2 (1 samples)"),
    is_categorical_group = TRUE
  )
  expect_equal(nrow(result$ternary_points1), 2)
})

# ---- Bug: spurious "vector has issues. Reinitializing." console noise on
# every plot that skips Optional Param 1/2 ----
# compute_point_styling() already sets pointSize/pointType/pointCol to
# correctly-sized rep(...) vectors up front - but the "neither manual size
# nor optional_param1" branch and the "no optional_param2" branch each
# used to overwrite them with bare length-1 scalars anyway (identical
# values, wrong shape), which the function's own end-of-function safety
# check then silently caught and reinitialized back to the exact same
# values - correctly shaped - printing "Point size/type/color vector has
# issues. Reinitializing." on every single render, even though nothing
# was ever actually wrong. Harmless to the final output (confirmed: no NAs
# either way) but noisy, and easy to mistake for a real problem while
# debugging - fixed by simply not overwriting the already-correct vectors.
test_that("plotting with neither Optional Param 1 nor 2 set produces correctly-shaped vectors with no reinitialize warning", {
  d <- data.frame(Fe = c(10, 20, 30), Cr = c(1, 2, 3), Ni = c(5, 6, 7))
  console_output <- capture.output(
    result <- compute_point_styling(
      ternary_points1 = data.frame(A = c(0.6, 0.7, 0.75), B = c(0.1, 0.1, 0.1), C = c(0.3, 0.2, 0.15)),
      matrika = d, optional_param1 = NULL, optional_param1_representation = "point_size",
      optional_param2 = NULL, color_palette = "blue", use_manual_point_size = FALSE,
      manual_point_size = 1, is_categorical_group = FALSE, selected_groups = NULL
    )
  )
  expect_false(any(grepl("has issues\\. Reinitializing", console_output)))
  expect_length(result$pointSize, 3)
  expect_length(result$pointType, 3)
  expect_length(result$pointCol, 3)
  expect_false(anyNA(result$pointSize))
  expect_false(anyNA(result$pointCol))
})

# ---- UX/robustness: character/factor Optional Param 2 columns are now
# capped at 50 distinct values before being treated as categorical, the
# same way every other non-numeric type already was ----
# server_ternary_plots_groups.R's two detection observers and
# helpers_filters.R's independent safety-check re-detection previously
# granted UNCONDITIONAL categorical status to any character/factor
# column, regardless of how many distinct values it had - is.character()/
# is.factor() were OR'd in ahead of the (!is.numeric() && count <= 50)
# check that every other non-numeric type already had to pass. A text
# column that happens to be a per-row identifier (e.g. a Sample_ID
# column) would get an unwieldy, unbounded checklist/legend. Fixed by
# relying on !is.numeric() alone (character and factor are never
# is.numeric() in R, so this already covers them) combined with the same
# existing 50-value cap, applied uniformly - plus a defensive guard in
# compute_point_styling()'s numeric-color branch, since
# prepare_ternary_plot_data()/general_ternary_plot() are directly
# callable (not gated behind the UI's own detection) and a non-numeric
# column reaching that branch would otherwise still crash on quantile().

evs_cardinality_test_data <- function(n) {
  set.seed(1)
  data.frame(Fe = runif(n, 10, 20), Cr = runif(n, 5, 15), Ni = runif(n, 1, 10),
             Label = paste0("id_", seq_len(n)))
}

test_that("a text color column at or under the 50-value cap still works as categorical (regression)", {
  d <- evs_cardinality_test_data(30)
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, tmp_xlsx)
  out_dir <- tempfile("outdir"); dir.create(out_dir)

  expect_no_error(
    result <- prepare_ternary_plot_data(
      xlsx_file = tmp_xlsx, working_dir = tempdir(), output_dir = out_dir,
      element_A = list(col = "Fe"), element_B = list(col = "Cr"), element_C = list(col = "Ni"),
      optional_param1 = NULL, optional_param2 = list(col = "Label", filter = NULL),
      color_palette = "blue", xlsx_display_name = "test.xlsx",
      preview = TRUE, use_mahalanobis = FALSE, reference_data = NULL,
      optional_param1_representation = "point_size", output_format = "png",
      use_isolation_forest = FALSE, use_iqr_filter = FALSE, use_zscore_filter = FALSE, use_mad_filter = FALSE,
      lambda = 1, omega = 0, keep_outliers_mahalanobis = FALSE, keep_outliers_isolation = FALSE,
      keep_outliers_iqr = FALSE, keep_outliers_zscore = FALSE, keep_outliers_mad = FALSE,
      individual_filters_A = NULL, individual_filters_B = NULL, individual_filters_C = NULL,
      custom_mdthresh = NULL, mdthresh_mode = "auto", mahalanobis_reference = NULL,
      selected_columns = NULL, include_plot_notes = FALSE, use_manual_point_size = FALSE,
      manual_point_size = NULL,
      selected_groups = NULL,
      is_categorical_group = TRUE  # matches what the (now-capped) detection observer would set for 30 <= 50
    )
  )
  expect_equal(nrow(result$ternary_points1), 30)
})

test_that("a text color column over the 50-value cap fails clearly instead of crashing on quantile()", {
  d <- evs_cardinality_test_data(60)
  tmp_xlsx <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, tmp_xlsx)
  out_dir <- tempfile("outdir"); dir.create(out_dir)

  err <- tryCatch(
    prepare_ternary_plot_data(
      xlsx_file = tmp_xlsx, working_dir = tempdir(), output_dir = out_dir,
      element_A = list(col = "Fe"), element_B = list(col = "Cr"), element_C = list(col = "Ni"),
      optional_param1 = NULL, optional_param2 = list(col = "Label", filter = NULL),
      color_palette = "blue", xlsx_display_name = "test.xlsx",
      preview = TRUE, use_mahalanobis = FALSE, reference_data = NULL,
      optional_param1_representation = "point_size", output_format = "png",
      use_isolation_forest = FALSE, use_iqr_filter = FALSE, use_zscore_filter = FALSE, use_mad_filter = FALSE,
      lambda = 1, omega = 0, keep_outliers_mahalanobis = FALSE, keep_outliers_isolation = FALSE,
      keep_outliers_iqr = FALSE, keep_outliers_zscore = FALSE, keep_outliers_mad = FALSE,
      individual_filters_A = NULL, individual_filters_B = NULL, individual_filters_C = NULL,
      custom_mdthresh = NULL, mdthresh_mode = "auto", mahalanobis_reference = NULL,
      selected_columns = NULL, include_plot_notes = FALSE, use_manual_point_size = FALSE,
      manual_point_size = NULL,
      selected_groups = NULL,
      is_categorical_group = FALSE  # matches what the (now-capped) detection observer would set for 60 > 50
    ),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "isn't being treated as a categorical color grouping", fixed = TRUE)
  expect_no_match(conditionMessage(err), "non-numeric argument to binary operator")
})

test_that("the categorical-detection cap treats character AND factor columns identically", {
  # !is.numeric() alone (the fix) must catch both; the previous
  # is.character()||is.factor()||... form happened to as well, so this
  # guards against a future edit narrowing it back to just one type.
  set.seed(2)
  base <- data.frame(Fe = runif(10, 10, 20), Cr = runif(10, 5, 15), Ni = runif(10, 1, 10))
  labels <- paste0("g", 1:10)

  for (col_type in c("character", "factor")) {
    d <- base
    d$Label <- if (col_type == "character") labels else factor(labels)
    tmp_xlsx <- tempfile(fileext = ".xlsx")
    openxlsx::write.xlsx(d, tmp_xlsx)
    out_dir <- tempfile("outdir"); dir.create(out_dir)

    result <- prepare_ternary_plot_data(
      xlsx_file = tmp_xlsx, working_dir = tempdir(), output_dir = out_dir,
      element_A = list(col = "Fe"), element_B = list(col = "Cr"), element_C = list(col = "Ni"),
      optional_param1 = NULL, optional_param2 = list(col = "Label", filter = NULL),
      color_palette = "blue", xlsx_display_name = "test.xlsx",
      preview = TRUE, use_mahalanobis = FALSE, reference_data = NULL,
      optional_param1_representation = "point_size", output_format = "png",
      use_isolation_forest = FALSE, use_iqr_filter = FALSE, use_zscore_filter = FALSE, use_mad_filter = FALSE,
      lambda = 1, omega = 0, keep_outliers_mahalanobis = FALSE, keep_outliers_isolation = FALSE,
      keep_outliers_iqr = FALSE, keep_outliers_zscore = FALSE, keep_outliers_mad = FALSE,
      individual_filters_A = NULL, individual_filters_B = NULL, individual_filters_C = NULL,
      custom_mdthresh = NULL, mdthresh_mode = "auto", mahalanobis_reference = NULL,
      selected_columns = NULL, include_plot_notes = FALSE, use_manual_point_size = FALSE,
      manual_point_size = NULL, selected_groups = NULL, is_categorical_group = TRUE
    )
    expect_equal(nrow(result$ternary_points1), 10, info = paste("column type:", col_type))
  }
})
