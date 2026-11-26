# Tests for the fatigue-limit panel of the EVS tab (R/server_evs.R,
# R/fatigue_murakami.R). make_evs_server()/make_evs_upload() are defined in
# test-server-evs.R, which testthat runs before this file; they are
# repeated here so this file also runs on its own.
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

# The fit and prediction the tab works out for that upload (T = 100),
# computed here directly so the table can be checked against them.
expected_evs <- function() {
  d <- openxlsx::read.xlsx(fatigue_evs_upload()$datapath, sheet = 1)
  maxima <- compute_block_maxima(d, "area", "field")
  list(pred = predict_evs_max(fit_evs_gumbel(maxima$sqrt_area_max), 100), h = mean(maxima$sqrt_area_max))
}

fit_evs <- function(session) {
  session$setInputs(`evs-evs_files` = fatigue_evs_upload(), `evs-evs_area_col` = "area", `evs-evs_group_col` = "field",
                    `evs-evs_return_period` = 100, `evs-evs_fit` = 1)
}

test_that("the fatigue table stays empty before a fit and asks for the hardness after it", {
  testServer(fatigue_evs_server(), {
    # no fit yet: a silent (blank) stop, not an error message
    err <- tryCatch({ output[["evs-evs_fatigue_table"]]; NULL }, error = function(e) e)
    expect_s3_class(err, "shiny.silent.error")
    expect_equal(conditionMessage(err), "")

    fit_evs(session)
    err <- expect_error(output[["evs-evs_fatigue_table"]])
    expect_match(conditionMessage(err), "Enter the Vickers hardness", fixed = TRUE)
    expect_null(output[["evs-evs_fatigue_notes"]])
  })
})

test_that("with a hardness the table gives the fatigue limit of the return level and both intervals", {
  testServer(fatigue_evs_server(), {
    fit_evs(session)
    session$setInputs(`evs-evs_hv` = 300, `evs-evs_incl_location` = "surface", `evs-evs_stress_ratio` = -1)
    tab <- output[["evs-evs_fatigue_table"]]
    expect_match(tab, "Return level for T = 100 control areas", fixed = TRUE)
    expect_match(tab, "Fatigue limit, stress amplitude (MPa)", fixed = TRUE)
    expect_match(tab, "interval for the largest inclusion", fixed = TRUE)

    pred <- expected_evs()$pred
    expect_match(tab, sprintf("%.0f", murakami_fatigue_limit(pred$predicted, 300)), fixed = TRUE)
    expect_match(tab, sprintf("[%.0f, %.0f]", murakami_fatigue_limit(pred$upper, 300), murakami_fatigue_limit(pred$lower, 300)), fixed = TRUE)
    # inside the model's range: no warning box
    expect_null(output[["evs-evs_fatigue_notes"]])
  })
})

test_that("an internal inclusion gives a higher limit than one at the surface, and R = 0 a lower amplitude", {
  testServer(fatigue_evs_server(), {
    fit_evs(session)
    pred <- expected_evs()$pred
    session$setInputs(`evs-evs_hv` = 300, `evs-evs_incl_location` = "internal", `evs-evs_stress_ratio` = -1)
    expect_match(output[["evs-evs_fatigue_table"]], sprintf("%.0f", murakami_fatigue_limit(pred$predicted, 300, "internal")), fixed = TRUE)
    session$setInputs(`evs-evs_stress_ratio` = 0)
    expect_match(output[["evs-evs_fatigue_table"]],
                 sprintf("%.0f", murakami_fatigue_limit(pred$predicted, 300, "internal", stress_ratio = 0)), fixed = TRUE)
    session$setInputs(`evs-evs_stress_ratio` = 1)
    err <- expect_error(output[["evs-evs_fatigue_table"]])
    expect_match(conditionMessage(err), "stress ratio R must be smaller than 1", fixed = TRUE)
  })
})

test_that("a hardness outside the model's range shows a warning", {
  testServer(fatigue_evs_server(), {
    fit_evs(session)
    session$setInputs(`evs-evs_hv` = 900)
    expect_match(output[["evs-evs_fatigue_notes"]]$html, "HV = 900 is outside the range", fixed = TRUE)
  })
})

test_that("the return period for a stressed volume is worked out from the fit", {
  testServer(fatigue_evs_server(), {
    expect_equal(output[["evs-evs_volume_note"]], "")
    fit_evs(session)
    expect_equal(output[["evs-evs_volume_note"]], "")
    session$setInputs(`evs-evs_control_area_mm2` = 0.05, `evs-evs_volume_mm3` = 500)
    h <- expected_evs()$h
    note <- output[["evs-evs_volume_note"]]
    expect_match(note, sprintf("T = V / V0 = %.0f", 500 / (0.05 * h / 1000)), fixed = TRUE)
    expect_match(note, sprintf("h = mean block maximum = %.2f um", h), fixed = TRUE)
    session$setInputs(`evs-evs_volume_mm3` = 0)
    expect_equal(output[["evs-evs_volume_note"]], "")
  })
})

test_that("the fatigue panel is in the EVS tab", {
  ui <- as.character(create_evs_tab("evs"))
  for (id in c("evs-evs_hv", "evs-evs_incl_location", "evs-evs_stress_ratio", "evs-evs_fatigue_table",
               "evs-evs_control_area_mm2", "evs-evs_volume_mm3", "evs-evs_volume_note")) {
    expect_match(ui, id, fixed = TRUE)
  }
})
