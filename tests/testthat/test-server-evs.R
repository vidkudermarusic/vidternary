# Tests for R/server_evs.R's Shiny wiring - in particular, that
# output$evs_status surfaces shiny::validate()'s specific message rather
# than a single generic placeholder for every failure (see server_evs.R's
# comment on output$evs_status for the full explanation).
#
# create_server_evs() is exercised through the real moduleServer("evs", ...)
# wrapper (matching how R/server_logic.R wires it up), so inputs/outputs
# below are read/written under the "evs-" namespace prefix that
# moduleServer() applies at the top-level session used by testServer().
#
# library(shiny) is required here (unlike the package's other test files):
# vidternary's NAMESPACE doesn't import shiny (this package attaches its
# dependencies at app-launch time via dependencies.R's
# initialize_packages(), not via NAMESPACE import()/importFrom()), and that
# only runs from R/app.R - never during `devtools::test()`. server_evs.R's
# reactive/render bodies call shiny functions (reactive(), renderText(),
# etc.) unqualified, so shiny must be attached to the search path for this
# file's tests to be able to invoke them at all, the same way it would be
# by the time the real app's server code runs.
library(shiny)

make_evs_server <- function() {
  rv <- shiny::reactiveValues()
  show_message <- function(message, type = "info") invisible(NULL)
  log_operation <- function(...) invisible(NULL)
  function(input, output, session) {
    shiny::moduleServer("evs", function(input, output, session) {
      create_server_evs(input, output, session, rv, show_message, log_operation)
    })
  }
}

# Writes a small xlsx with an `area` (numeric) and `field` (group) column,
# split into `n_groups` groups of `rows_per_group` rows each, and returns
# the `input$evs_files`-shaped data.frame Shiny's fileInput produces.
make_evs_upload <- function(n_groups = 8, rows_per_group = 6, group_means = NULL, seed = 1) {
  set.seed(seed)
  groups <- LETTERS[seq_len(n_groups)]
  if (is.null(group_means)) group_means <- rep(100, n_groups)
  d <- do.call(rbind, lapply(seq_along(groups), function(i) {
    data.frame(area = abs(stats::rnorm(rows_per_group, mean = group_means[i], sd = group_means[i] * 0.1)) + 1,
               field = groups[i])
  }))
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  data.frame(name = basename(path), size = file.info(path)$size, type = "",
             datapath = path, stringsAsFactors = FALSE)
}

test_that("evs_status shows the generic placeholder before the fit button is ever clicked", {
  testServer(make_evs_server(), {
    status <- output[["evs-evs_status"]]
    expect_equal(status, "Upload data, choose the area and grouping columns, and click \"Fit Extreme Value Model\".")
  })
})

test_that("evs_status falls back to the placeholder (not a blank message) if Fit is clicked before any data is ready", {
  # The Fit button has no server-side gating (ui_evs_tab.R's actionButton
  # is always clickable), so this is reachable in the real app: a user can
  # click it immediately, before uploading a file or choosing an area
  # column. That path fails inside fit_result() via req(), not
  # validate() - req() throws the same "shiny.silent.error" condition
  # class but with an EMPTY message (by design: nothing specific to say).
  # Letting that propagate unchanged would show a blank status instead of
  # the placeholder - this confirms it's caught and replaced instead.
  testServer(make_evs_server(), {
    session$setInputs(`evs-evs_fit` = 1)
    status <- output[["evs-evs_status"]]
    expect_equal(status, "Upload data, choose the area and grouping columns, and click \"Fit Extreme Value Model\".")
  })
})

test_that("evs_status surfaces the specific validate() message for an invalid area column", {
  testServer(make_evs_server(), {
    session$setInputs(`evs-evs_files` = make_evs_upload())
    session$setInputs(`evs-evs_area_col` = "not_a_real_column")
    session$setInputs(`evs-evs_group_col` = "field")
    session$setInputs(`evs-evs_use_manual_groups` = FALSE)
    session$setInputs(`evs-evs_fit` = 1)

    # fit_result()'s validate() error is no longer swallowed by evs_status's
    # own render function, so it propagates out when the output is read -
    # matching how Shiny's output-rendering machinery displays it for real
    # (as a distinct validation message, not the generic placeholder).
    err <- expect_error(output[["evs-evs_status"]])
    expect_match(conditionMessage(err), "Select a valid area column", fixed = TRUE)
  })
})

test_that("evs_status surfaces the specific validate() message for an out-of-range evs_n_groups", {
  testServer(make_evs_server(), {
    session$setInputs(`evs-evs_files` = make_evs_upload())
    session$setInputs(`evs-evs_area_col` = "area")
    session$setInputs(`evs-evs_use_manual_groups` = TRUE)
    session$setInputs(`evs-evs_n_groups` = 1000)
    session$setInputs(`evs-evs_fit` = 1)

    err <- expect_error(output[["evs-evs_status"]])
    expect_match(conditionMessage(err), "Number of groups must be between 3 and", fixed = TRUE)
  })
})

test_that("evs_status surfaces the specific validate() message for a missing group column", {
  testServer(make_evs_server(), {
    session$setInputs(`evs-evs_files` = make_evs_upload())
    session$setInputs(`evs-evs_area_col` = "area")
    session$setInputs(`evs-evs_use_manual_groups` = FALSE)
    session$setInputs(`evs-evs_group_col` = "not_a_real_column")
    session$setInputs(`evs-evs_fit` = 1)

    err <- expect_error(output[["evs-evs_status"]])
    expect_match(conditionMessage(err), "Select a field/group ID column", fixed = TRUE)
  })
})

test_that("evs_status still reports a normal successful fit correctly (no regression)", {
  testServer(make_evs_server(), {
    session$setInputs(`evs-evs_files` = make_evs_upload())
    session$setInputs(`evs-evs_area_col` = "area")
    session$setInputs(`evs-evs_group_col` = "field")
    session$setInputs(`evs-evs_use_manual_groups` = FALSE)
    session$setInputs(`evs-evs_fit` = 1)

    status <- output[["evs-evs_status"]]
    expect_match(status, "^Fit successful: n = 8 control areas")
    expect_match(status, "Goodness-of-fit: Anderson-Darling")
  })
})

test_that("evs_gof_warning stays silent (not an error) when the fit fails validation", {
  testServer(make_evs_server(), {
    session$setInputs(`evs-evs_files` = make_evs_upload())
    session$setInputs(`evs-evs_area_col` = "not_a_real_column")
    session$setInputs(`evs-evs_group_col` = "field")
    session$setInputs(`evs-evs_use_manual_groups` = FALSE)
    session$setInputs(`evs-evs_fit` = 1)

    # Unlike evs_status, evs_gof_warning is a supplementary banner that
    # should show nothing (not the validation error) when there's no
    # successful fit - see server_evs.R's comment on this output.
    expect_null(output[["evs-evs_gof_warning"]])
  })
})

test_that("evs_gof_warning renders the warning banner only when the GOF test rejects Gumbel", {
  testServer(make_evs_server(), {
    # A well-behaved single-population fit shouldn't reject Gumbel.
    session$setInputs(`evs-evs_files` = make_evs_upload())
    session$setInputs(`evs-evs_area_col` = "area")
    session$setInputs(`evs-evs_group_col` = "field")
    session$setInputs(`evs-evs_use_manual_groups` = FALSE)
    session$setInputs(`evs-evs_fit` = 1)
    expect_null(output[["evs-evs_gof_warning"]])
  })

  testServer(make_evs_server(), {
    # Two very different-scale populations of block maxima should reject a
    # single Gumbel fit and produce the warning banner.
    session$setInputs(`evs-evs_files` = make_evs_upload(
      n_groups = 10, group_means = c(rep(20, 5), rep(5000, 5)), seed = 3))
    session$setInputs(`evs-evs_area_col` = "area")
    session$setInputs(`evs-evs_group_col` = "field")
    session$setInputs(`evs-evs_use_manual_groups` = FALSE)
    session$setInputs(`evs-evs_fit` = 1)

    warning_ui <- output[["evs-evs_gof_warning"]]
    expect_false(is.null(warning_ui))
    expect_match(warning_ui$html, "rejects a single Gumbel distribution", fixed = TRUE)

    status <- output[["evs-evs_status"]]
    expect_match(status, "DEVIATE from a single Gumbel distribution", fixed = TRUE)
  })
})
