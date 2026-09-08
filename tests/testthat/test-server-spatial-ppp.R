# Tests for R/server_spatial_ppp.R - the "Point Pattern Analysis" tab's
# reactive wiring. Same structure/conventions as test-server-spatial.R:
# real testServer() against the real reactive server, a real xlsx upload,
# and the same blank-error-before-analyze regression check every other
# download-gated tab in this app has already needed.
#
# library(shiny) is required here (unlike the package's other test files):
# vidternary's NAMESPACE doesn't import shiny (dependencies are attached at
# app-launch time via dependencies.R's initialize_packages(), which only
# runs from R/app.R), and server_spatial_ppp.R's reactive/render bodies
# call shiny functions unqualified.
library(shiny)

make_spatial_ppp_server <- function() {
  rv <- shiny::reactiveValues()
  show_message <- function(message, type = "info") invisible(NULL)
  log_operation <- function(...) invisible(NULL)
  function(input, output, session) {
    shiny::moduleServer("spatial_ppp", function(input, output, session) {
      create_server_spatial_ppp(input, output, session, rv, show_message, log_operation)
    })
  }
}

# Writes a small xlsx with StageX/StageY coordinate columns (plus an
# optional Type mark column), returning the input$ppp_files-shaped data
# frame Shiny's fileInput produces - same helper style as
# test-server-spatial.R's make_spatial_upload().
make_ppp_upload <- function(n = 30, seed = 1, with_mark = FALSE) {
  set.seed(seed)
  d <- data.frame(StageX = stats::runif(n, 0, 1000), StageY = stats::runif(n, 0, 1000))
  if (with_mark) d$Type <- sample(c("A", "B"), n, replace = TRUE)
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  data.frame(name = basename(path), size = file.info(path)$size, type = "",
             datapath = path, stringsAsFactors = FALSE)
}

test_that("a normal, successful analysis reports n/area/mean-intensity/peak-intensity/nsim in the status text", {
  testServer(make_spatial_ppp_server(), {
    session$setInputs(`spatial_ppp-ppp_files` = make_ppp_upload(n = 40))
    session$setInputs(`spatial_ppp-ppp_x_col` = "StageX")
    session$setInputs(`spatial_ppp-ppp_y_col` = "StageY")
    session$setInputs(`spatial_ppp-ppp_nsim` = 19)
    session$setInputs(`spatial_ppp-ppp_analyze` = 1)

    status <- output[["spatial_ppp-ppp_status"]]
    expect_match(status, "n = 40 points")
    expect_match(status, "mean intensity =")
    expect_match(status, "peak \\(hotspot\\) intensity =")
    expect_match(status, "CSR envelope simulations = 19")
  })
})

# Writes an xlsx with points concentrated into 3 tight clumps within a much
# larger nominal coordinate range - mimicking a real inclusion banding/
# stringer pattern, and specifically designed to make the kernel intensity
# map's peak (a LOCAL density) meaningfully exceed the mean intensity (n /
# whole-window area) - the exact real-world scenario a user asked about
# directly ("why does the Kernel intensity estimate show colored values
# higher than my total number of points?").
make_clustered_ppp_upload <- function(n = 90, seed = 1) {
  set.seed(seed)
  centers <- data.frame(cx = c(0, 500, 1000), cy = c(0, 500, 0))
  per_clump <- n %/% 3
  d <- do.call(rbind, lapply(seq_len(3), function(i) {
    data.frame(StageX = stats::rnorm(per_clump, centers$cx[i], 2),
               StageY = stats::rnorm(per_clump, centers$cy[i], 2))
  }))
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  data.frame(name = basename(path), size = file.info(path)$size, type = "",
             datapath = path, stringsAsFactors = FALSE)
}

test_that("the summary table reports both mean and peak intensity, and peak is never less than the mean (a real mathematical invariant, not just a plausible-looking pair of numbers)", {
  # The kernel intensity surface integrates back to ~n over the whole
  # window (confirmed directly against spatstat before this fix - see the
  # vidternary Structural Audit), so its own maximum pixel value can never
  # be below the simple whole-window average (n / area) - true for ANY
  # point pattern, not just a clustered one. Checked here on a genuinely
  # clustered pattern specifically because that's the case where the two
  # numbers differ by a lot, matching the real scenario that prompted this
  # fix - a uniform pattern would make this same check trivially true
  # (peak ~= mean) and wouldn't actually exercise the interesting case.
  testServer(make_spatial_ppp_server(), {
    session$setInputs(`spatial_ppp-ppp_files` = make_clustered_ppp_upload())
    session$setInputs(`spatial_ppp-ppp_x_col` = "StageX")
    session$setInputs(`spatial_ppp-ppp_y_col` = "StageY")
    session$setInputs(`spatial_ppp-ppp_nsim` = 19)
    session$setInputs(`spatial_ppp-ppp_analyze` = 1)

    tbl <- output[["spatial_ppp-ppp_summary_table"]]
    expect_match(tbl, "Mean intensity")
    expect_match(tbl, "Peak kernel intensity")

    status <- output[["spatial_ppp-ppp_status"]]
    mean_val <- as.numeric(sub(".*mean intensity = ([0-9.eE+-]+).*", "\\1", status))
    peak_val <- as.numeric(sub(".*peak \\(hotspot\\) intensity = ([0-9.eE+-]+).*", "\\1", status))
    expect_true(is.finite(mean_val) && is.finite(peak_val))
    expect_gte(peak_val, mean_val)
    # And, matching this specific clustered configuration, genuinely and
    # substantially higher - not just a rounding-level difference - so
    # this test would have failed against a version of the code that
    # accidentally reported the same number twice for both metrics.
    expect_gt(peak_val, mean_val * 1.5)
  })
})

test_that("a cleared nsim field falls back to the documented default of 99, not a crash", {
  # Same NA_real_-from-a-cleared-numericInput hazard documented at length
  # elsewhere in this app (compute_isolation_forest()'s ntrees/
  # contamination, validate_mahalanobis_inputs()) - confirmed here too.
  testServer(make_spatial_ppp_server(), {
    session$setInputs(`spatial_ppp-ppp_files` = make_ppp_upload(n = 30))
    session$setInputs(`spatial_ppp-ppp_x_col` = "StageX")
    session$setInputs(`spatial_ppp-ppp_y_col` = "StageY")
    session$setInputs(`spatial_ppp-ppp_nsim` = NA_real_)
    session$setInputs(`spatial_ppp-ppp_analyze` = 1)

    status <- output[["spatial_ppp-ppp_status"]]
    expect_match(status, "CSR envelope simulations = 99")
  })
})

test_that("all 6 plot outputs and the summary table render without error after a real analysis", {
  testServer(make_spatial_ppp_server(), {
    session$setInputs(`spatial_ppp-ppp_files` = make_ppp_upload(n = 30, with_mark = TRUE))
    session$setInputs(`spatial_ppp-ppp_x_col` = "StageX")
    session$setInputs(`spatial_ppp-ppp_y_col` = "StageY")
    session$setInputs(`spatial_ppp-ppp_mark_col` = "Type")
    session$setInputs(`spatial_ppp-ppp_nsim` = 19)
    session$setInputs(`spatial_ppp-ppp_analyze` = 1)

    for (out in c("spatial_ppp-ppp_pattern_plot", "spatial_ppp-ppp_intensity_plot",
                   "spatial_ppp-ppp_k_plot", "spatial_ppp-ppp_l_plot",
                   "spatial_ppp-ppp_g_plot", "spatial_ppp-ppp_envelope_plot")) {
      expect_no_error(output[[out]])
    }
    tbl <- output[["spatial_ppp-ppp_summary_table"]]
    expect_match(tbl, "Points \\(n\\)")
    expect_match(tbl, "30")
  })
})

test_that("clicking any download before Analyze Point Pattern is ever clicked gives a clear message, not an uncaught blank error", {
  testServer(make_spatial_ppp_server(), {
    for (out in c("spatial_ppp-ppp_download_pattern", "spatial_ppp-ppp_download_intensity",
                   "spatial_ppp-ppp_download_k", "spatial_ppp-ppp_download_l",
                   "spatial_ppp-ppp_download_g", "spatial_ppp-ppp_download_envelope",
                   "spatial_ppp-ppp_download_data")) {
      res <- tryCatch({ output[[out]]; list(ok = TRUE) },
                       error = function(e) list(ok = FALSE, msg = conditionMessage(e)))
      expect_false(res$ok)
      expect_equal(res$msg, "Upload data, choose X/Y coordinate columns, and click \"Analyze Point Pattern\" before downloading.")
    }
  })
})

test_that("a normal, successful analysis still downloads real plots and a real combined-sheets xlsx", {
  testServer(make_spatial_ppp_server(), {
    session$setInputs(`spatial_ppp-ppp_files` = make_ppp_upload(n = 30))
    session$setInputs(`spatial_ppp-ppp_x_col` = "StageX")
    session$setInputs(`spatial_ppp-ppp_y_col` = "StageY")
    session$setInputs(`spatial_ppp-ppp_nsim` = 19)
    session$setInputs(`spatial_ppp-ppp_analyze` = 1)

    for (out in c("spatial_ppp-ppp_download_pattern", "spatial_ppp-ppp_download_intensity",
                   "spatial_ppp-ppp_download_k", "spatial_ppp-ppp_download_l",
                   "spatial_ppp-ppp_download_g", "spatial_ppp-ppp_download_envelope")) {
      path <- output[[out]]
      expect_true(file.exists(path))
      expect_gt(file.info(path)$size, 0)
    }

    data_path <- output[["spatial_ppp-ppp_download_data"]]
    expect_true(file.exists(data_path))
    sheets <- openxlsx::getSheetNames(data_path)
    expect_setequal(sheets, c("K_function", "L_function", "G_function", "CSR_envelope"))
  })
})

test_that("an invalid X/Y column selection surfaces a clear validation message, not a crash", {
  # shiny::validate()'s condition is designed to propagate OUT of
  # renderText() uncaught (Shiny's own reactive framework then renders its
  # message as a formatted validation notice) - output$ppp_status's own
  # tryCatch() only intercepts the OTHER shiny.silent.error case (req()'s,
  # always empty-message) and re-throws everything else via stop(e), same
  # convention already established and tested for output$evs_status - see
  # test-server-evs.R's own "surfaces the specific validate() message"
  # tests for the identical pattern this mirrors.
  testServer(make_spatial_ppp_server(), {
    session$setInputs(`spatial_ppp-ppp_files` = make_ppp_upload(n = 30))
    session$setInputs(`spatial_ppp-ppp_x_col` = "NoSuchColumn")
    session$setInputs(`spatial_ppp-ppp_y_col` = "StageY")
    session$setInputs(`spatial_ppp-ppp_analyze` = 1)

    err <- expect_error(output[["spatial_ppp-ppp_status"]])
    expect_match(conditionMessage(err), "Select a valid X coordinate column", fixed = TRUE)
  })
})

# ---- Pre-Analysis Data Filter (helpers_pre_analysis_filter.R) ----
# Same convention as the identical section in test-server-evs.R/
# test-server-spatial.R - the shared helper's pure logic is already covered
# in test-helpers-pre-analysis-filter.R; what matters here is that
# server_spatial_ppp.R actually wires it in - filtered_data() feeds
# result() (not the raw combined_data()), and the reported row counts are
# real.

# Writes an xlsx with an "Area" column: half the rows < 1, half > 1 - the
# user's own real-world example ("Area > 1" to drop inclusions too small
# to be worth analyzing) - lets a "> 1" filter cleanly halve the point
# count, a real, checkable change rather than a coincidence.
make_ppp_upload_with_area <- function(n = 40, seed = 1) {
  set.seed(seed)
  half <- n %/% 2
  d <- data.frame(
    StageX = stats::runif(n, 0, 1000), StageY = stats::runif(n, 0, 1000),
    Area = c(stats::runif(half, 0.01, 0.5), stats::runif(n - half, 2, 10))
  )
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  data.frame(name = basename(path), size = file.info(path)$size, type = "",
             datapath = path, stringsAsFactors = FALSE)
}

test_that("the pre-analysis filter genuinely changes which points reach the point pattern analysis", {
  testServer(make_spatial_ppp_server(), {
    session$setInputs(`spatial_ppp-ppp_files` = make_ppp_upload_with_area(n = 40))
    session$setInputs(`spatial_ppp-ppp_x_col` = "StageX")
    session$setInputs(`spatial_ppp-ppp_y_col` = "StageY")
    session$setInputs(`spatial_ppp-ppp_filter_cols` = "Area")
    session$setInputs(`spatial_ppp-ppp_filter_Area` = "> 1")
    session$setInputs(`spatial_ppp-ppp_nsim` = 19)
    session$setInputs(`spatial_ppp-ppp_analyze` = 1)

    status <- output[["spatial_ppp-ppp_status"]]
    expect_match(status, "n = 20 points")  # only the "large" half survives

    tbl <- output[["spatial_ppp-ppp_summary_table"]]
    expect_match(tbl, "Rows before pre-analysis filter")
    expect_match(tbl, "40")
    expect_match(tbl, "Rows after pre-analysis filter")
    expect_match(tbl, "20")
  })
})

test_that("a pre-analysis filter that removes every point surfaces a clear message instead of crashing downstream", {
  testServer(make_spatial_ppp_server(), {
    session$setInputs(`spatial_ppp-ppp_files` = make_ppp_upload_with_area(n = 40))
    session$setInputs(`spatial_ppp-ppp_x_col` = "StageX")
    session$setInputs(`spatial_ppp-ppp_y_col` = "StageY")
    session$setInputs(`spatial_ppp-ppp_filter_cols` = "Area")
    session$setInputs(`spatial_ppp-ppp_filter_Area` = "> 999999")
    session$setInputs(`spatial_ppp-ppp_nsim` = 19)
    session$setInputs(`spatial_ppp-ppp_analyze` = 1)

    err <- expect_error(output[["spatial_ppp-ppp_status"]])
    expect_match(conditionMessage(err), "No rows remain after applying the pre-analysis filter", fixed = TRUE)
  })
})

test_that("leaving the filter columns unselected behaves exactly as before this feature existed (no regression)", {
  testServer(make_spatial_ppp_server(), {
    session$setInputs(`spatial_ppp-ppp_files` = make_ppp_upload(n = 30))
    session$setInputs(`spatial_ppp-ppp_x_col` = "StageX")
    session$setInputs(`spatial_ppp-ppp_y_col` = "StageY")
    session$setInputs(`spatial_ppp-ppp_nsim` = 19)
    session$setInputs(`spatial_ppp-ppp_analyze` = 1)

    status <- output[["spatial_ppp-ppp_status"]]
    expect_match(status, "n = 30 points")
  })
})
