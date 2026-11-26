# Extracted from test-server-evs-fatigue.R:54

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
    session$setInputs(`evs-evs_hv` = 300, `evs-evs_incl_location` = "surface", `evs-evs_stress_ratio` = -1)
    tab <- output[["evs-evs_fatigue_table"]]
    expect_match(tab, "Return level for T = 100 control areas", fixed = TRUE)
    expect_match(tab, "Fatigue limit, stress amplitude (MPa)", fixed = TRUE)
    expect_match(tab, "interval for the largest inclusion", fixed = TRUE)

    pred <- predict_evs_max(fit_result(), 100)
    expect_match(tab, sprintf("%.0f", murakami_fatigue_limit(pred$predicted, 300)), fixed = TRUE)
    expect_match(tab, sprintf("[%.0f, %.0f]", murakami_fatigue_limit(pred$upper, 300), murakami_fatigue_limit(pred$lower, 300)), fixed = TRUE)
    # inside the model's range: no warning box
    expect_null(output[["evs-evs_fatigue_notes"]])
  })
