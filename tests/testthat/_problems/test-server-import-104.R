# Extracted from test-server-import.R:104

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vidternary", path = "..")
attach(test_env, warn.conflicts = FALSE)

# prequel ----------------------------------------------------------------------
library(shiny)
make_import_server <- function(log = function(...) invisible(NULL)) {
  rv <- shiny::reactiveValues()
  function(input, output, session) {
    shiny::moduleServer("import", function(input, output, session) {
      create_server_import(input, output, session, rv, function(...) invisible(NULL), log)
    })
  }
}
foreign_lines <- c("Part No;Frame;Feature Area [um2];Dmax (um);Dmin (um);X_stage [mm];Y_stage [mm];Al At%;O At%;Ti At%",
                   "1;1;12,5;5,2;3,1;10,001;20,5;40,0;60,0;0",
                   "2;1;3,25;2,5;1,9;10,250;20,75;0;0;100",
                   "3;2;7;4;2;11,5;21;20,5;30,5;49")
import_upload <- function(lines = foreign_lines, names = "export.csv") {
  paths <- vapply(names, function(n) {
    path <- tempfile(fileext = paste0(".", tools::file_ext(n)))
    writeLines(lines, path)
    path
  }, character(1))
  data.frame(name = names, size = 1, type = "", datapath = unname(paths), stringsAsFactors = FALSE)
}
set_import_mapping <- function(session, upload = import_upload(), ...) {
  values <- utils::modifyList(list(
    import_files = upload,
    import_role_feature = "Part.No", import_role_field = "Frame", import_role_area = "Feature.Area.[um2]",
    import_role_ecd = "none", import_role_length = "Dmax.(um)", import_role_breadth = "Dmin.(um)",
    import_role_aspect = "none", import_role_direction = "none", import_role_perimeter = "none",
    import_role_x = "X_stage.[mm]", import_role_y = "Y_stage.[mm]",
    import_unit_area = "auto", import_unit_length = "auto", import_unit_breadth = "auto", import_unit_x = "auto", import_unit_y = "auto",
    import_elements = c("Al.At%", "O.At%", "Ti.At%"), import_basis = "at", import_keep_other = TRUE,
    import_profile_name = "Test SEM"
  ), list(...))
  names(values) <- paste0("import-", names(values))
  do.call(session$setInputs, values)
}
import_placeholder <- "Upload a file, check the column roles, and click \"Convert\"."

# test -------------------------------------------------------------------------
testServer(make_import_server(), {
    set_import_mapping(session, upload = import_upload(names = c("heat_1.csv", "heat_2.csv")))
    session$setInputs(`import-import_convert` = 1)
    expect_equal(output[["import-import_status"]], "heat_1.csv: 3 particles, 12 columns\nheat_2.csv: 3 particles, 12 columns")
    path <- output[["import-import_download"]]
    expect_match(path, "\\.zip$")
    expect_setequal(zip::zip_list(path)$filename, c("heat_1_standard.xlsx", "heat_2_standard.xlsx"))
  })
