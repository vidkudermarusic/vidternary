# Extracted from test-server-cleanliness.R:161

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vidternary", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(shiny)
make_cleanliness_server <- function(log = function(...) invisible(NULL)) {
  rv <- shiny::reactiveValues()
  show_message <- function(message, type = "info") invisible(NULL)
  function(input, output, session) {
    shiny::moduleServer("cleanliness", function(input, output, session) {
      create_server_cleanliness(input, output, session, rv, show_message, log)
    })
  }
}
set_cleanliness_inputs <- function(session, upload = make_cleanliness_upload(), ...) {
  values <- utils::modifyList(list(
    clean_files = upload,
    clean_area_col = CLEAN_COLS$area, clean_length_col = CLEAN_COLS$length, clean_breadth_col = CLEAN_COLS$breadth,
    clean_x_col = CLEAN_COLS$x, clean_y_col = CLEAN_COLS$y, clean_direction_col = CLEAN_COLS$direction,
    clean_field_col = CLEAN_COLS$field, clean_coord_unit = "mm", clean_family_source = "eds",
    clean_standard = "ISO 4967", clean_size_breaks = "1, 2, 5, 10, 20", clean_measured = ""
  ), list(...))
  names(values) <- paste0("cleanliness-", names(values))
  do.call(session$setInputs, values)
}
clean_placeholder <- "Upload a file, check the columns, and click \"Run Cleanliness Analysis\"."

# test -------------------------------------------------------------------------
d <- make_cleanliness_data()
d$MyClass <- ifelse(d[[CLEAN_COLS$breadth]] > 1, "oxide", "speck")
testServer(make_cleanliness_server(), {
    set_cleanliness_inputs(session, upload = make_cleanliness_upload(d),
                           clean_family_source = "column", clean_family_col = "MyClass")
    session$setInputs(`cleanliness-clean_run` = 1)
    indices <- output[["cleanliness-clean_indices_table"]]
    expect_match(indices, "oxide", fixed = TRUE)
    expect_match(indices, "speck", fixed = TRUE)
    expect_no_match(indices, "Ti-rich", fixed = TRUE)
  })
