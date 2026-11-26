# Extracted from test-server-inclusion.R:146

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vidternary", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(shiny)
incl_upload <- function(d, name = "specimen.xlsx") {
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  data.frame(name = name, size = file.info(path)$size, type = "", datapath = path, stringsAsFactors = FALSE)
}
incl_panel_data <- function() {
  d <- incl_data(
    incl_row(list(MgO = 28.331, Al2O3 = 71.669), steel = 40, carbon = 2),
    incl_row(list(Al2O3 = 100), steel = 40, carbon = 2),
    incl_row(list(CaO = 48.53, Al2O3 = 51.47), steel = 20),
    do.call(rbind, replicate(10, incl_row(steel = 100), simplify = FALSE))
  )
  d$Feature <- seq_len(nrow(d))
  d
}
make_incl_server <- function() {
  rv <- shiny::reactiveValues()
  function(input, output, session) {
    shiny::moduleServer("inclusion", function(input, output, session) {
      create_server_inclusion(input, output, session, rv, function(...) invisible(NULL), function(...) invisible(NULL))
    })
  }
}
set_incl <- function(session, data = incl_panel_data(), ...) {
  base <- list(incl_preset = "cao-al2o3-mgo", incl_matrix_element = "Fe",
               incl_matrix_mode = "matrix_and_alloys", incl_steel_source = "enter",
               incl_steel_composition = "Fe=70, Cr=20, Ni=10", incl_alloy_elements = c("Cr", "Ni"),
               incl_ignore_elements = "C", incl_max_matrix = 80, incl_min_residual = 5,
               incl_threshold = 50, incl_s_order = "ca_mn", incl_ti_as = "TiN")
  args <- utils::modifyList(base, list(...))
  if (!is.null(data)) args$incl_file <- if ("datapath" %in% names(data)) data else incl_upload(data)
  names(args) <- paste0("inclusion-", names(args))
  do.call(session$setInputs, args)
}
out <- function(name) paste0("inclusion-", name)

# test -------------------------------------------------------------------------
d <- incl_panel_data()
testServer(make_incl_server(), {
    set_incl(session, data = d)
    png_path <- output[[out("incl_download_plot")]]
    expect_true(file.exists(png_path))
    expect_equal(readBin(png_path, "raw", 4), as.raw(c(0x89, 0x50, 0x4e, 0x47)))
    xlsx_path <- output[[out("incl_download_table")]]
    expect_true(file.exists(xlsx_path))
    expect_equal(openxlsx::getSheetNames(xlsx_path), c("Particles", "Summary", "Settings", "Sources"))
    p <- openxlsx::read.xlsx(xlsx_path, sheet = "Particles")
    expect_equal(nrow(p), nrow(d))
    expect_equal(p$class[1:3], c("Spinel-type", "Alumina", "C12A7"))
  })
