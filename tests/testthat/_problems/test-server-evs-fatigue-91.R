# Extracted from test-server-evs-fatigue.R:91

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vidternary", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(shiny)
fatigue_evs_server <- function() {
  rv <- shiny::reactiveValues()
  function(input, output, session) {
    shiny::moduleServer("evs", function(input, output, session) {
      create_server_evs(input, output, session, rv, function(...) invisible(NULL), function(...) invisible(NULL))
    })
  }
}
fatigue_evs_upload <- function(n_groups = 12, seed = 5) {
  set.seed(seed)
  d <- do.call(rbind, lapply(seq_len(n_groups), function(i) {
    data.frame(area = stats::rexp(8, rate = 1 / 20) + 1, field = i)
  }))
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  data.frame(name = basename(path), size = file.info(path)$size, type = "", datapath = path, stringsAsFactors = FALSE)
}
fit_evs <- function(session) {
  session$setInputs(`evs-evs_files` = fatigue_evs_upload(), `evs-evs_area_col` = "area", `evs-evs_group_col` = "field",
                    `evs-evs_return_period` = 100, `evs-evs_fit` = 1)
}

# test -------------------------------------------------------------------------
testServer(fatigue_evs_server(), {
    expect_equal(output[["evs-evs_volume_note"]], "")
    fit_evs(session)
    expect_equal(output[["evs-evs_volume_note"]], "")
    session$setInputs(`evs-evs_control_area_mm2` = 0.05, `evs-evs_volume_mm3` = 500)
    h <- mean(fit_result()$block_maxima$sqrt_area_max)
    note <- output[["evs-evs_volume_note"]]
    expect_match(note, sprintf("T = V / V0 = %.0f", 500 / (0.05 * h / 1000)), fixed = TRUE)
    expect_match(note, sprintf("h = mean block maximum = %.2f um", h), fixed = TRUE)
    session$setInputs(`evs-evs_volume_mm3` = 0)
    expect_equal(output[["evs-evs_volume_note"]], "")
  })
