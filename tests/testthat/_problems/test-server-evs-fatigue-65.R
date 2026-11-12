# Extracted from test-server-evs-fatigue.R:65

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
    fit_evs(session)
    pred <- predict_evs_max(fit_result(), 100)
    session$setInputs(`evs-evs_hv` = 300, `evs-evs_incl_location` = "internal", `evs-evs_stress_ratio` = -1)
    expect_match(output[["evs-evs_fatigue_table"]], sprintf("%.0f", murakami_fatigue_limit(pred$predicted, 300, "internal")), fixed = TRUE)
    session$setInputs(`evs-evs_stress_ratio` = 0)
    expect_match(output[["evs-evs_fatigue_table"]],
                 sprintf("%.0f", murakami_fatigue_limit(pred$predicted, 300, "internal", stress_ratio = 0)), fixed = TRUE)
    session$setInputs(`evs-evs_stress_ratio` = 1)
    err <- expect_error(output[["evs-evs_fatigue_table"]])
    expect_match(conditionMessage(err), "stress ratio R must be smaller than 1", fixed = TRUE)
  })
