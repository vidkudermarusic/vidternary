# Extracted from test-server-import.R:71

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
    set_import_mapping(session)
    session$setInputs(`import-import_convert` = 1)
    expect_equal(output[["import-import_status"]], "export.csv: 3 particles, 12 columns")
    preview <- output[["import-import_preview"]]
    for (col in c("Feature", "Field", "Area.(um2)", "ECD.(um)", "Length.(um)", "Aspect.Ratio", "X.(mm)", "Al.(Wt%)")) {
      expect_match(preview, col, fixed = TRUE)
    }
    notes <- output[["import-import_notes"]]$html
    expect_match(notes, "Atomic percent converted to weight percent over 3 elements", fixed = TRUE)
    expect_match(notes, "Equivalent circle diameter computed from the area", fixed = TRUE)

    path <- output[["import-import_download"]]
    expect_match(path, "\\.xlsx$")
    back <- openxlsx::read.xlsx(path)
    expect_equal(back[["Area.(um2)"]], c(12.5, 3.25, 7))
    expect_equal(back[["Al.(Wt%)"]][1], 52.93, tolerance = 1e-3)
    expect_equal(back[["X.(mm)"]], c(10.001, 10.25, 11.5))
  })
