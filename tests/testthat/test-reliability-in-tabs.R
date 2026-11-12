# Tests for the EDS-reliability additions inside the tabs: size reliability
# and class shares in the Steel Cleanliness analysis, the reliability
# columns of the Data Import tab, and the overlap warning and class-share
# intervals of the Inclusion Presets tab.
library(shiny)

clean_run <- function(settings = list(), data = make_cleanliness_data(), cols = CLEAN_COLS, ...) {
  run_cleanliness_analysis(data, cols, utils::modifyList(list(n_boot = 300), settings), ...)
}

test_that("particles below the electron range get no EDS family, the others keep theirs", {
  r <- clean_run()
  p <- r$particles
  expect_equal(r$range_um, electron_range_um(15, "Fe"))
  expect_true(all(p$family[p$size_to_range < 1] == "too small to classify"))
  expect_false(any(p$family[p$size_to_range >= 1] == "too small to classify"))
  expect_equal(sum(p$family == "too small to classify"), 22)
  expect_equal(p$size_to_range, p$ecd / r$range_um)
})

test_that("the reliability table counts the classes and shows their matrix content", {
  rel <- clean_run()$reliability
  expect_equal(rel$reliability, c("matrix-dominated", "partly matrix", "reliable"))
  expect_equal(rel$n, c(22, 0, 10))
  # the specks are 97 % steel (68 % Fe), the inclusions 30 % steel (21 % Fe)
  expect_gt(rel$median_matrix_pct[1], 60)
  expect_lt(rel$median_matrix_pct[3], 25)
  expect_true(is.na(rel$median_matrix_pct[2]))
})

test_that("the size check can be switched off, moved with the voltage, and does not touch a class column", {
  off <- clean_run(list(min_size_to_range = 0))
  expect_false(any(off$particles$family == "too small to classify"))
  expect_equal(sum(off$particles$family == "unclassified"), 22)      # as before: too little inclusion signal

  # at 40 kV the range is 5.1 um: the 3.5 um aluminates and the 5 um Ti-rich
  # particles fall below it as well
  high <- clean_run(list(beam_kv = 40))
  expect_equal(high$range_um, electron_range_um(40, "Fe"))
  expect_equal(sum(high$particles$family == "too small to classify"), 22 + 3 + 4)
  expect_true(any(grepl("at 40 kV", c(high$notes$warnings, high$notes$notes), fixed = TRUE)))

  d <- make_cleanliness_data()
  d$MyClass <- "oxide"
  own <- clean_run(data = d, cols = c(CLEAN_COLS, list(family = "MyClass")))
  expect_true(all(own$particles$family == "oxide"))
  expect_equal(own$reliability$n, c(22, 0, 10))                       # still reported
})

test_that("the rating does not change with the size check", {
  expect_equal(clean_run(list(min_size_to_range = 0))$rating_summary, clean_run()$rating_summary)
})

test_that("family and type shares come with intervals over SEM fields", {
  r <- clean_run()
  f <- r$family_shares
  expect_setequal(f$class, c("Ti-rich", "aluminate", "sulfide", "silicate"))
  expect_equal(f$n[f$class == "Ti-rich"], 5)
  expect_equal(sum(f$share_pct), 100)
  expect_equal(unique(f$method), "bootstrap over fields")
  t <- r$type_shares
  # rated particles: 3 in the stringer (B), 4 D, 1 A, 1 C, 1 DS
  expect_equal(stats::setNames(t$n, t$class)[c("D", "B", "A", "C", "DS")], c(D = 4L, B = 3L, A = 1L, C = 1L, DS = 1L))
  # without a field column the interval is Wilson's
  no_field <- clean_run(cols = CLEAN_COLS[setdiff(names(CLEAN_COLS), "field")])
  expect_equal(unique(no_field$family_shares$method), "Wilson (independent particles)")
})

test_that("the new tables are exported and reported", {
  r <- clean_run()
  tabs <- cleanliness_result_tables(r)
  expect_true(all(c("Size reliability", "Family shares", "Type shares") %in% names(tabs)))
  expect_true(all(nchar(names(tabs)) <= 31))
  html <- cleanliness_report(r)
  expect_match(html, "<h2>Reliability of the EDS classification</h2>", fixed = TRUE)
  expect_match(html, "Electron range of the beam: 0.99 um in Fe at 15 kV", fixed = TRUE)
  expect_match(html, "bootstrap over fields", fixed = TRUE)
})

clean_server <- function() {
  rv <- shiny::reactiveValues()
  function(input, output, session) {
    shiny::moduleServer("cleanliness", function(input, output, session) {
      create_server_cleanliness(input, output, session, rv, function(...) invisible(NULL), function(...) invisible(NULL))
    })
  }
}
run_clean_tab <- function(session, ...) {
  values <- utils::modifyList(
    list(clean_files = make_cleanliness_upload(), clean_area_col = CLEAN_COLS$area, clean_length_col = CLEAN_COLS$length,
         clean_breadth_col = CLEAN_COLS$breadth, clean_x_col = CLEAN_COLS$x, clean_y_col = CLEAN_COLS$y,
         clean_direction_col = CLEAN_COLS$direction, clean_field_col = CLEAN_COLS$field, clean_coord_unit = "mm",
         clean_family_source = "eds", clean_standard = "ISO 4967", clean_size_breaks = "1, 2, 5, 10, 20",
         clean_measured = ""), list(...))
  names(values) <- paste0("cleanliness-", names(values))
  do.call(session$setInputs, values)
  session$setInputs(`cleanliness-clean_run` = 1)
}

test_that("the cleanliness tab shows the reliability and share tables and follows its settings", {
  testServer(clean_server(), {
    run_clean_tab(session)
    rel <- output[["cleanliness-clean_reliability_table"]]
    expect_match(rel, "matrix-dominated: smaller than the electron range", fixed = TRUE)
    expect_match(rel, "Median matrix element (wt%)", fixed = TRUE)
    expect_match(output[["cleanliness-clean_family_shares_table"]], "Chemical family", fixed = TRUE)
    expect_match(output[["cleanliness-clean_family_shares_table"]], "bootstrap over fields", fixed = TRUE)
    expect_match(output[["cleanliness-clean_type_shares_table"]], "95% interval (%)", fixed = TRUE)
    expect_match(output[["cleanliness-clean_indices_table"]], "too small to classify", fixed = TRUE)
  })
  testServer(clean_server(), {
    run_clean_tab(session, clean_skip_small = FALSE)
    expect_no_match(output[["cleanliness-clean_indices_table"]], "too small to classify", fixed = TRUE)
    expect_match(output[["cleanliness-clean_indices_table"]], "unclassified", fixed = TRUE)
  })
  testServer(clean_server(), {
    run_clean_tab(session, clean_beam_kv = 30, clean_range_matrix = "Fe")
    expect_match(output[["cleanliness-clean_notes"]]$html, "at 30 kV", fixed = TRUE)
  })
})

# ---- Data Import: reliability columns ----

import_server <- function() {
  rv <- shiny::reactiveValues()
  function(input, output, session) {
    shiny::moduleServer("import", function(input, output, session) {
      create_server_import(input, output, session, rv, function(...) invisible(NULL), function(...) invisible(NULL))
    })
  }
}
csv_upload <- function(lines) {
  path <- tempfile(fileext = ".csv")
  writeLines(lines, path)
  data.frame(name = "export.csv", size = 1, type = "", datapath = path, stringsAsFactors = FALSE)
}
set_import <- function(session, upload, ...) {
  none <- stats::setNames(as.list(rep("none", 11)), paste0("import_role_", particle_roles()$role))
  values <- utils::modifyList(c(none, list(import_files = upload, import_elements = character(0), import_basis = "wt",
                                           import_keep_other = FALSE)), list(...))
  names(values) <- paste0("import-", names(values))
  do.call(session$setInputs, values)
  session$setInputs(`import-import_convert` = 1)
}

test_that("the import tab adds the size-reliability columns and says with which range", {
  upload <- csv_upload(c("Diameter (um);Al (Wt%)", "0,5;10", "1,5;20", "5;60"))
  testServer(import_server(), {
    set_import(session, upload, import_role_ecd = "Diameter.(um)", import_elements = "Al.(Wt%)")
    back <- openxlsx::read.xlsx(output[["import-import_download"]])
    expect_equal(back$Size.to.range, c(0.5, 1.5, 5) / electron_range_um(15, "Fe"))
    expect_equal(back$Composition.reliability, c("matrix-dominated", "partly matrix", "reliable"))
    expect_match(output[["import-import_notes"]]$html, "Size-reliability columns added: electron range 0.99 um in Fe at 15 kV", fixed = TRUE)
  })
  testServer(import_server(), {
    set_import(session, upload, import_role_ecd = "Diameter.(um)", import_beam_kv = 20, import_range_matrix = "Ni")
    back <- openxlsx::read.xlsx(output[["import-import_download"]])
    expect_equal(back$Size.to.range, c(0.5, 1.5, 5) / electron_range_um(20, "Ni"))
  })
})

test_that("the columns are left out when switched off or when the table has no particle size", {
  upload <- csv_upload(c("Diameter (um);Al (Wt%)", "0,5;10", "5;60"))
  testServer(import_server(), {
    set_import(session, upload, import_role_ecd = "Diameter.(um)", import_add_reliability = FALSE)
    expect_false("Size.to.range" %in% names(openxlsx::read.xlsx(output[["import-import_download"]])))
  })
  testServer(import_server(), {
    set_import(session, upload, import_elements = "Al.(Wt%)")           # no size column mapped
    expect_equal(output[["import-import_status"]], "export.csv: 2 particles, 1 columns")
    expect_equal(names(openxlsx::read.xlsx(output[["import-import_download"]])), "Al.(Wt%)")
  })
})

# ---- Inclusion Presets: overlap warning and class shares ----

inclusion_server <- function() {
  rv <- shiny::reactiveValues()
  function(input, output, session) {
    shiny::moduleServer("inclusion", function(input, output, session) {
      create_server_inclusion(input, output, session, rv, function(...) invisible(NULL), function(...) invisible(NULL))
    })
  }
}
inclusion_upload <- function(with_field = TRUE) {
  d <- incl_data(incl_row(list(Al2O3 = 70), steel = 30), incl_row(list(Al2O3 = 70), steel = 30),
                 incl_row(list(MgO = 20, Al2O3 = 50), steel = 30), incl_row(list(CaO = 30, Al2O3 = 40), steel = 30),
                 incl_row(list(TiN = 70), steel = 30))
  if (with_field) d$Field <- c(1, 1, 2, 3, 4)
  path <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, path)
  data.frame(name = basename(path), size = 1, type = "", datapath = path, stringsAsFactors = FALSE)
}
set_inclusion <- function(session, upload, preset, ...) {
  values <- utils::modifyList(list(incl_file = upload, incl_preset = preset, incl_matrix_element = "", incl_threshold = 30,
                                   incl_max_matrix = 80, incl_min_residual = 5, incl_ignore_elements = "C",
                                   incl_ti_as = "TiN", incl_s_order = "ca_mn"), list(...))
  names(values) <- paste0("inclusion-", names(values))
  do.call(session$setInputs, values)
}

test_that("the classes of the inclusion tab come with an interval, and the note says which kind", {
  testServer(inclusion_server(), {
    set_inclusion(session, inclusion_upload(TRUE), "cao-al2o3-mgo")
    expect_match(output[["inclusion-incl_summary_table"]], "95% interval", fixed = TRUE)
    expect_match(output[["inclusion-incl_share_note"]], "bootstrap over SEM fields", fixed = TRUE)
    shares <- openxlsx::read.xlsx(output[["inclusion-incl_download_table"]], sheet = "Class shares")
    expect_equal(sum(shares$share_pct), 100)
    expect_equal(unique(shares$method), "bootstrap over fields")
  })
  testServer(inclusion_server(), {
    set_inclusion(session, inclusion_upload(FALSE), "cao-al2o3-mgo")
    expect_match(output[["inclusion-incl_share_note"]], "no SEM field column was found", fixed = TRUE)
  })
})

test_that("a preset that reads Ti and N together is flagged, unless N is ignored", {
  lib <- load_inclusion_presets()
  flagged <- Filter(function(id) !is.null(eds_overlap_message(inclusion_preset_elements(get_inclusion_preset(id, lib)))),
                    lib$presets$id)
  skip_if(length(flagged) == 0, "no preset of the library combines overlapping elements")
  id <- flagged[1]
  elements <- inclusion_preset_elements(get_inclusion_preset(id, lib))
  testServer(inclusion_server(), {
    set_inclusion(session, inclusion_upload(), id)
    expect_match(output[["inclusion-incl_message"]]$html, "EDS peak overlap", fixed = TRUE)
    # ignoring one element of every overlapping group clears the warning
    session$setInputs(`inclusion-incl_ignore_elements` = c("C", intersect(c("N", "O", "V", "Mo", "Pb", "Bi"), elements)))
    msg <- output[["inclusion-incl_message"]]
    expect_true(is.null(msg) || !grepl("EDS peak overlap", msg$html, fixed = TRUE))
  })
})

test_that("an oxide preset is not flagged", {
  testServer(inclusion_server(), {
    set_inclusion(session, inclusion_upload(), "cao-al2o3-mgo")
    msg <- output[["inclusion-incl_message"]]
    expect_true(is.null(msg) || !grepl("EDS peak overlap", msg$html, fixed = TRUE))
  })
})
