# Tests for R/server_import.R: the Shiny wiring of the "Data Import" tab.
#
# library(shiny) is required here - see test-server-evs.R for why.
library(shiny)

make_import_server <- function(log = function(...) invisible(NULL)) {
  rv <- shiny::reactiveValues()
  function(input, output, session) {
    shiny::moduleServer("import", function(input, output, session) {
      create_server_import(input, output, session, rv, function(...) invisible(NULL), log)
    })
  }
}

# A European text export: semicolons, decimal commas, atomic percent.
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

# Sets the mapping the browser would hold after the automatic proposal
# (testServer does not apply updateSelectInput()).
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

test_that("an uploaded text file is described and previewed", {
  testServer(make_import_server(), {
    session$setInputs(`import-import_files` = import_upload())
    expect_equal(output[["import-import_file_info"]], "export.csv: 3 rows, 10 columns (text, delimiter semicolon, decimal comma)")
    expect_match(output[["import-import_raw_preview"]], "Feature.Area.[um2]", fixed = TRUE)
    expect_match(output[["import-import_profile_status"]], "Mapping proposed from the column names")
    expect_equal(output[["import-import_status"]], import_placeholder)
  })
})

test_that("a file that cannot be read is named with the reason", {
  testServer(make_import_server(), {
    bad <- import_upload(lines = c("no delimiter here", "1", "2"), names = "broken.csv")
    session$setInputs(`import-import_files` = rbind(import_upload(), bad))
    info <- output[["import-import_file_info"]]
    expect_match(info, "export.csv: 3 rows", fixed = TRUE)
    expect_match(info, "broken.csv: NOT READ - No column delimiter", fixed = TRUE)
  })
})

test_that("converting gives the standard table, its notes and a download", {
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
})

test_that("a unit chosen by hand overrides the column name, and unmapped columns can be dropped", {
  testServer(make_import_server(), {
    set_import_mapping(session, import_unit_length = "nm", import_keep_other = FALSE, import_role_field = "none")
    session$setInputs(`import-import_convert` = 1)
    expect_equal(output[["import-import_status"]], "export.csv: 3 particles, 11 columns")   # Frame is not mapped and not kept
    back <- openxlsx::read.xlsx(output[["import-import_download"]])
    expect_equal(back[["Length.(um)"]], c(5.2, 2.5, 4) / 1000)
    expect_false("Frame" %in% names(back))
  })
})

test_that("several files are converted with one mapping and downloaded as a zip", {
  testServer(make_import_server(), {
    set_import_mapping(session, upload = import_upload(names = c("heat_1.csv", "heat_2.csv")))
    session$setInputs(`import-import_convert` = 1)
    expect_equal(output[["import-import_status"]], "heat_1.csv: 3 particles, 12 columns\nheat_2.csv: 3 particles, 12 columns")
    path <- output[["import-import_download"]]
    expect_match(path, "\\.zip$")
    expect_setequal(zip::zip_list(path)$filename, c("heat_1_standard.xlsx", "heat_2_standard.xlsx"))
  })
})

test_that("a file that does not fit the mapping is reported and the others are still converted", {
  testServer(make_import_server(), {
    other <- import_upload(lines = c("Area;Length", "1,5;2", "3;4"), names = "other.csv")
    set_import_mapping(session, upload = rbind(import_upload(), other))
    session$setInputs(`import-import_convert` = 1)
    status <- output[["import-import_status"]]
    expect_match(status, "export.csv: 3 particles", fixed = TRUE)
    expect_match(status, "other.csv: NOT CONVERTED - The file does not match the profile 'Test SEM'", fixed = TRUE)
    expect_match(output[["import-import_download"]], "\\.xlsx$")       # one good table: a single xlsx
  })
})

test_that("converting with nothing mapped asks for a mapping", {
  testServer(make_import_server(), {
    none <- stats::setNames(as.list(rep("none", 11)), paste0("import_role_", particle_roles()$role))
    do.call(set_import_mapping, c(list(session), none, list(import_elements = character(0))))
    session$setInputs(`import-import_convert` = 1)
    err <- expect_error(output[["import-import_status"]])
    expect_match(conditionMessage(err), "Assign at least one column to a role", fixed = TRUE)
    expect_null(output[["import-import_notes"]])
  })
})

test_that("the mapping is saved as a profile that reproduces the conversion", {
  testServer(make_import_server(), {
    set_import_mapping(session)
    profile <- read_import_profile(output[["import-import_profile_save"]])
    expect_equal(profile$name, "Test SEM")
    expect_equal(profile$mapping$area, "Feature.Area.[um2]")
    expect_equal(profile$elements$element, c("Al", "O", "Ti"))
    expect_equal(profile$basis, "at")
    session$setInputs(`import-import_convert` = 1)
    direct <- apply_import_profile(read_particle_table(import_upload()$datapath, "export.csv"), profile)
    expect_equal(openxlsx::read.xlsx(output[["import-import_download"]])[["Ti.(Wt%)"]], direct[["Ti.(Wt%)"]])
  })
})

test_that("loading a profile reports what happened", {
  profile <- detect_import_profile(read_particle_table(import_upload()$datapath, "export.csv"), "Saved SEM")
  path <- tempfile(fileext = ".json")
  write_import_profile(profile, path)
  as_upload <- function(p) data.frame(name = basename(p), size = 1, type = "", datapath = p, stringsAsFactors = FALSE)

  testServer(make_import_server(), {
    session$setInputs(`import-import_profile_load` = as_upload(path))
    expect_equal(output[["import-import_profile_status"]], "Upload a data file first, then load the profile.")
  })
  testServer(make_import_server(), {
    session$setInputs(`import-import_files` = import_upload())
    session$setInputs(`import-import_profile_load` = as_upload(path))
    expect_equal(output[["import-import_profile_status"]], "Profile 'Saved SEM' loaded.")
  })
  testServer(make_import_server(), {
    session$setInputs(`import-import_files` = import_upload(lines = c("Area;Length", "1,5;2"), names = "other.csv"))
    session$setInputs(`import-import_profile_load` = as_upload(path))
    expect_match(output[["import-import_profile_status"]], "Profile 'Saved SEM' loaded, but the file lacks: Part.No", fixed = TRUE)
  })
  testServer(make_import_server(), {
    not_profile <- tempfile(fileext = ".json"); writeLines('{"kind": "tab settings"}', not_profile)
    session$setInputs(`import-import_files` = import_upload())
    session$setInputs(`import-import_profile_load` = as_upload(not_profile))
    expect_match(output[["import-import_profile_status"]], "Could not load the profile: This file is not a vidternary import profile", fixed = TRUE)
  })
})

test_that("downloads before anything is done give a clear message", {
  testServer(make_import_server(), {
    res <- tryCatch({ output[["import-import_download"]]; "ok" }, error = function(e) conditionMessage(e))
    expect_equal(res, "Upload a file and click \"Convert\" before downloading.")
    res <- tryCatch({ output[["import-import_profile_save"]]; "ok" }, error = function(e) conditionMessage(e))
    expect_equal(res, "Upload a data file before saving a profile.")
  })
})

test_that("the tab is part of the app and a conversion is logged", {
  ui <- as.character(create_main_ui())
  expect_match(ui, "Data Import", fixed = TRUE)
  expect_match(ui, "import-import_role_area", fixed = TRUE)
  expect_match(ui, "import-import_unit_x", fixed = TRUE)
  expect_no_match(ui, "import-import_unit_aspect", fixed = TRUE)      # a ratio has no unit
  expect_match(paste(deparse(create_server_logic), collapse = "\n"), "create_server_import", fixed = TRUE)

  record <- new.env()
  testServer(make_import_server(log = function(level, message, details = NULL) record$details <- details), {
    set_import_mapping(session)
    session$setInputs(`import-import_convert` = 1)
    output[["import-import_status"]]
    expect_equal(record$details, "1 of 1 file(s) converted with profile 'Test SEM'")
  })
})
