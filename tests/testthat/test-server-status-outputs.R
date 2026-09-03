# Tests for R/server_status_outputs.R - a tiny app-shell module (not
# moduleServer()-wrapped, see that file's own header comment) that owns
# just the sidebar's project_status output.
library(shiny)

test_that("project_status renders its static placeholder text", {
  testServer(function(input, output, session) {
    rv <- shiny::reactiveValues()
    create_server_status_outputs(input, output, session, rv)
  }, {
    expect_equal(output[["project_status"]], "Project status: No project loaded")
  })
})
