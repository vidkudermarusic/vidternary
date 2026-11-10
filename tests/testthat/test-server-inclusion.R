# The "Inclusion Presets" tab's server logic (R/server_inclusion.R),
# registered by server_logic.R inside moduleServer("inclusion").
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

# Sets every input the tab reads (the conditionalPanel ones exist in the real
# page even while hidden). `data` is uploaded as the tab's file (NULL: no file
# input is sent, so an earlier upload stays in place).
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

test_that("before a file is uploaded the tab says so instead of failing", {
  testServer(make_incl_server(), {
    set_incl(session, data = NULL)
    expect_error(output[[out("incl_summary_table")]], "Upload an XLSX file")
    expect_error(output[[out("incl_download_plot")]], "Could not generate this download: Upload an XLSX file")
    expect_null(output[[out("incl_message")]])
    expect_match(output[[out("incl_data_info")]]$html, "No file loaded yet")
  })
})

test_that("the uploaded file is summarised: particle count and the element columns found", {
  testServer(make_incl_server(), {
    set_incl(session)
    info <- output[[out("incl_data_info")]]$html
    expect_match(info, "13[[:space:]]+particles")
    expect_match(info, "Element columns found:")
    expect_match(info, "Fe")
  })
  testServer(make_incl_server(), {
    set_incl(session, data = data.frame(Area = 1:3))
    expect_match(output[[out("incl_data_info")]]$html, "No element wt% columns")
    expect_error(output[[out("incl_summary_table")]], "No element wt% columns")
  })
})

test_that("only one file (one specimen) can be used", {
  testServer(make_incl_server(), {
    two <- rbind(incl_upload(incl_panel_data(), "a.xlsx"), incl_upload(incl_panel_data(), "b.xlsx"))
    set_incl(session, data = two)
    expect_error(output[[out("incl_summary_table")]], "Upload one file")
  })
})

test_that("a preset is applied to the uploaded data and the class and status tables are filled", {
  testServer(make_incl_server(), {
    set_incl(session)
    cls <- output[[out("incl_summary_table")]]
    expect_match(cls, "Spinel-type"); expect_match(cls, "Alumina"); expect_match(cls, "C12A7")
    st <- output[[out("incl_status_table")]]
    expect_match(st, "plotted")
    expect_match(st, "no inclusion signal after matrix removal")
    expect_match(output[[out("incl_settings")]], "Preset: CaO-Al2O3-MgO \\(mass basis, rules classification\\)")
    expect_match(output[[out("incl_settings")]], "Fe removed \\(matrix and alloys; ignored: C\\)")
    expect_false(is.null(output[[out("incl_plot")]]))
  })
})

test_that("changing the preset changes the classes; a nearest-phase preset honours the basis", {
  testServer(make_incl_server(), {
    set_incl(session, incl_preset = "mgo-sio2-al2o3")
    session$setInputs(`inclusion-incl_basis` = "mole")
    expect_match(output[[out("incl_summary_table")]], "Spinel")
    expect_match(output[[out("incl_settings")]], "MgO-SiO2-Al2O3 \\(mole basis, nearest classification\\)")
  })
  testServer(make_incl_server(), {
    set_incl(session, incl_preset = "cao-al2o3-mgo")
    session$setInputs(`inclusion-incl_basis` = "mole")  # ignored: this preset's rules are in mass fractions
    expect_match(output[[out("incl_settings")]], "mass basis, rules classification")
  })
})

test_that("the steel composition can be estimated from the data instead of typed", {
  testServer(make_incl_server(), {
    set_incl(session, incl_steel_source = "estimate")
    expect_match(output[[out("incl_summary_table")]], "Spinel-type")
    expect_match(output[[out("incl_message")]]$html, "Steel ratios to Fe: </strong>[[:space:]]*Cr 0.286, Ni 0.143")
  })
})

test_that("bad input is reported with the reason", {
  testServer(make_incl_server(), {
    set_incl(session, incl_steel_composition = "Fe=70,5")
    expect_error(output[[out("incl_summary_table")]], "Cannot read '5'")
    set_incl(session, data = NULL, incl_steel_composition = "Fe=70, Cr=20, Ni=10", incl_threshold = 150)
    expect_error(output[[out("incl_summary_table")]], "between 0 and 100")
    set_incl(session, data = NULL, incl_threshold = 50, incl_steel_source = "estimate", incl_alloy_elements = character(0))
    expect_error(output[[out("incl_summary_table")]], "Choose the alloying elements")
    set_incl(session, data = NULL, incl_alloy_elements = c("Cr", "Ni"), incl_matrix_element = "Zn")
    expect_error(output[[out("incl_summary_table")]], "Matrix element Zn has no column")
  })
})

test_that("with no matrix removal and a high threshold nothing is plotted and the tab warns", {
  testServer(make_incl_server(), {
    set_incl(session, incl_matrix_element = "", incl_threshold = 99)
    expect_match(output[[out("incl_summary_table")]], "none plotted")
    expect_match(output[[out("incl_message")]]$html, "No particle is plotted")
  })
})

test_that("the two downloads produce a real PNG and a workbook with the results", {
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
})

test_that("the compound-only options are flagged by an output the UI reads", {
  testServer(make_incl_server(), {
    set_incl(session, incl_preset = "cao-al2o3-mgo")
    expect_equal(output[[out("incl_is_compound")]], "true")
    set_incl(session, data = NULL, incl_preset = "mg-al-ca")
    expect_equal(output[[out("incl_is_compound")]], "false")
  })
})

test_that("the server factory reports its module name", {
  rv <- shiny::reactiveValues()
  res <- NULL
  testServer(function(input, output, session) {
    res <<- create_server_inclusion(input, output, session, rv, function(...) NULL, function(...) NULL)
  }, expect_true(TRUE))
  expect_equal(res$module_name, "server_inclusion")
})

# ---- Placement: its own tab, and the first tab is left as it was ----

test_that("Inclusion Presets is its own tab, right after Ternary Plots, with a single-file upload", {
  html <- as.character(create_main_ui())
  tabs <- regmatches(html, gregexpr('data-value="[^"]+"', html))[[1]]
  expect_equal(tabs[1:2], c('data-value="Ternary Plots"', 'data-value="Inclusion Presets"'))
  tab <- as.character(create_inclusion_tab("inclusion"))
  expect_match(tab, 'id="inclusion-incl_file"')
  upload_tag <- regmatches(tab, regexpr('<input id="inclusion-incl_file"[^>]*>', tab))
  expect_false(grepl("multiple", upload_tag))
  expect_match(tab, "inclusion-incl_preset")
  expect_match(tab, "inclusion-incl_download_table")
})

test_that("the Ternary Plots tab carries no inclusion controls, and the app server registers the new tab", {
  ternary <- as.character(create_ternary_plots_tab("ternary_plots"))
  expect_false(grepl("incl_|Inclusion Diagram Presets", ternary))
  src <- paste(deparse(create_server_logic), collapse = "\n")
  expect_match(src, 'moduleServer\\("inclusion"')
  expect_match(src, "create_server_inclusion")
  expect_false(grepl("register_inclusion_preset_handlers", src))
})
