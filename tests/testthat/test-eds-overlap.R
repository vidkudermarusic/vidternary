# Tests for R/eds_overlap.R: overlapping EDS lines among chosen columns,
# and the warning each tab shows.
library(shiny)

test_that("element symbols are read from column names, other columns are skipped", {
  expect_equal(column_elements(c("Ti.(Wt%)", "N (Wt%)", "Fe", "Area", "Field", "Mean.grey", "Stage.X.(mm)")), c("Ti", "N", "Fe"))
  expect_equal(column_elements(c("Al (At%)", "Al.(Wt%)")), "Al")              # one element, named twice
  expect_equal(column_elements(list(c("Cr.(Wt%)", "Ni.(Wt%)"), "O.(Wt%)")), c("Cr", "Ni", "O"))
  # "Feature", "Shape", "Class" start with letters of a symbol but are not elements
  expect_length(column_elements(c("Feature", "Shape", "Class", "Rank", "Breadth.(um)")), 0)
})

test_that("nothing chosen gives no elements", {
  expect_length(column_elements(NULL), 0)
  expect_length(column_elements(c("", "none", NA)), 0)
  expect_length(column_elements(character(0)), 0)
})

test_that("each overlap group is found when two of its elements are chosen", {
  expect_equal(eds_overlaps(c("Ti", "N")), "Ti - N")
  expect_equal(eds_overlaps(c("O", "Cr")), "Cr - O")
  expect_equal(eds_overlaps(c("V", "Ti")), "Ti - V")
  expect_equal(eds_overlaps(c("Mo", "S")), "S - Mo")
  expect_equal(eds_overlaps(c("Bi", "Pb", "S")), "S - Pb - Bi")
  # Ti overlaps with both N and V
  expect_equal(eds_overlaps(c("Ti", "N", "V")), c("Ti - N", "Ti - V"))
  expect_length(eds_overlaps(c("Ti", "Fe", "Cr", "Ni", "Al", "Si")), 0)
  expect_length(eds_overlaps("Ti"), 0)
})

test_that("the message names the overlap and what the columns are for, or is NULL", {
  msg <- eds_overlap_message(c("Ti.(Wt%)", "Fe.(Wt%)", "N.(Wt%)"), "diagram 1")
  expect_match(msg, "The elements chosen for diagram 1 include overlapping EDS lines (Ti - N).", fixed = TRUE)
  expect_match(eds_overlap_message(c("Cr.(Wt%)", "O.(Wt%)", "S.(Wt%)", "Mo.(Wt%)")), "(Cr - O; S - Mo)", fixed = TRUE)
  expect_null(eds_overlap_message(c("Ti.(Wt%)", "Fe.(Wt%)", "C.(Wt%)")))
  expect_null(eds_overlap_message(NULL))
  # size or colour by a non-element column changes nothing
  expect_null(eds_overlap_message(c("Ti.(Wt%)", "Area.(um2)", "Field")))
})

test_that("the warning box appears only when there is an overlap", {
  expect_null(eds_overlap_ui(c("Ti.(Wt%)", "Fe.(Wt%)")))
  box <- as.character(eds_overlap_ui(c("Ti.(Wt%)", "N.(Wt%)"), "the hexagonal diagram"))
  expect_match(box, "EDS peak overlap", fixed = TRUE)
  expect_match(box, "the hexagonal diagram", fixed = TRUE)
})

test_that("a preset reads the cations of its corners, and S or N only when it uses them", {
  expect_setequal(inclusion_preset_elements(get_inclusion_preset("cao-al2o3-mgo")), c("Ca", "Al", "Mg"))
  # a sulfide corner brings sulfur in
  expect_true("S" %in% inclusion_preset_elements(get_inclusion_preset("cao-al2o3-cas")))
  expect_false("O" %in% inclusion_preset_elements(get_inclusion_preset("cao-al2o3-cas")))
  # ignored elements are left out
  expect_false("Mg" %in% inclusion_preset_elements(get_inclusion_preset("cao-al2o3-mgo"), ignore_elements = "Mg"))
})

test_that("every preset of the library can be checked, and element presets with Ti and N are flagged unless N is ignored", {
  lib <- load_inclusion_presets()
  for (id in lib$presets$id) {
    p <- get_inclusion_preset(id, lib)
    expect_type(inclusion_preset_elements(p), "character")
  }
  element_presets <- lib$presets$id[lib$presets$kind == "element"]
  with_ti_n <- Filter(function(id) all(c("Ti", "N") %in% inclusion_preset_elements(get_inclusion_preset(id, lib))), element_presets)
  for (id in with_ti_n) {
    p <- get_inclusion_preset(id, lib)
    expect_match(eds_overlap_message(inclusion_preset_elements(p), "this preset"), "Ti - N", fixed = TRUE)
    expect_null(eds_overlap_message(inclusion_preset_elements(p, ignore_elements = c("N", "V")), "this preset"))
  }
})

# ---- the warning in the tabs ----

module_server <- function(id, factory) {
  rv <- shiny::reactiveValues()
  function(input, output, session) {
    shiny::moduleServer(id, function(input, output, session) {
      factory(input, output, session, rv, function(...) invisible(NULL), function(...) invisible(NULL))
    })
  }
}

test_that("the ternary tab warns per diagram and for the outlier columns", {
  testServer(module_server("ternary_plots", create_server_ternary_plots), {
    session$setInputs(`ternary_plots-element_A1` = "Ti.(Wt%)", `ternary_plots-element_B1` = "Fe.(Wt%)",
                      `ternary_plots-element_C1` = "Al.(Wt%)")
    expect_null(output[["ternary_plots-overlap_warning_1"]])
    # the overlap may come from the colour column, not only from the axes
    session$setInputs(`ternary_plots-optional_param2_1` = "N.(Wt%)")
    expect_match(output[["ternary_plots-overlap_warning_1"]]$html, "diagram 1 (axes, size and colour)", fixed = TRUE)
    expect_match(output[["ternary_plots-overlap_warning_1"]]$html, "Ti - N", fixed = TRUE)
    expect_null(output[["ternary_plots-overlap_warning_2"]])

    session$setInputs(`ternary_plots-element_A2` = c("Cr.(Wt%)", "Ni.(Wt%)"), `ternary_plots-element_B2` = "O.(Wt%)")
    expect_match(output[["ternary_plots-overlap_warning_2"]]$html, "Cr - O", fixed = TRUE)

    session$setInputs(`ternary_plots-multivariate_columns` = c("S.(Wt%)", "Mo.(Wt%)", "Fe.(Wt%)"))
    expect_match(output[["ternary_plots-overlap_warning_mv"]]$html, "the outlier analysis", fixed = TRUE)
    expect_match(output[["ternary_plots-overlap_warning_mv"]]$html, "S - Mo", fixed = TRUE)
  })
})

test_that("the hexagonal tab warns for an overlap anywhere in the hexagon", {
  testServer(module_server("hex_ternary", create_server_hex_ternary), {
    session$setInputs(`hex_ternary-hex_element_1` = "Ti.(Wt%)", `hex_ternary-hex_element_2` = "Fe.(Wt%)",
                      `hex_ternary-hex_element_3` = "Al.(Wt%)")
    expect_null(output[["hex_ternary-hex_overlap_warning"]])
    # the two elements sit in different slots, far apart in the hexagon
    session$setInputs(`hex_ternary-hex_element_7` = "V.(Wt%)")
    expect_match(output[["hex_ternary-hex_overlap_warning"]]$html, "Ti - V", fixed = TRUE)
  })
})

test_that("the batch ternary, compositional and data-comparison tabs warn too", {
  testServer(module_server("multiple_ternary", register_ternary_plots_batch_handlers), {
    session$setInputs(`multiple_ternary-multiple_element_A` = "Ti.(Wt%)", `multiple_ternary-multiple_element_B` = "Fe.(Wt%)",
                      `multiple_ternary-multiple_element_C` = "Al.(Wt%)", `multiple_ternary-multiple_optional_param1` = "N.(Wt%)")
    expect_match(output[["multiple_ternary-multiple_overlap_warning"]]$html, "Ti - N", fixed = TRUE)
  })
  testServer(module_server("coda", create_server_coda), {
    session$setInputs(`coda-coda_parts` = c("Ti.(Wt%)", "Fe.(Wt%)", "Cr.(Wt%)"))
    expect_null(output[["coda-coda_overlap_warning"]])
    session$setInputs(`coda-coda_parts` = c("Ti.(Wt%)", "Fe.(Wt%)", "Cr.(Wt%)", "O.(Wt%)"))
    expect_match(output[["coda-coda_overlap_warning"]]$html, "Cr - O", fixed = TRUE)
  })
  testServer(module_server("data_comparison", register_data_comparison_multivariate_handlers), {
    session$setInputs(`data_comparison-comparison_mv_columns` = c("S.(Wt%)", "Pb.(Wt%)"))
    expect_match(output[["data_comparison-comparison_mv_overlap_warning"]]$html, "S - Pb", fixed = TRUE)
  })
})

test_that("every tab with element choices has the warning in its page", {
  ui <- as.character(create_main_ui())
  for (id in c("ternary_plots-overlap_warning_1", "ternary_plots-overlap_warning_2", "ternary_plots-overlap_warning_mv",
               "hex_ternary-hex_overlap_warning", "multiple_ternary-multiple_overlap_warning", "coda-coda_overlap_warning",
               "data_comparison-comparison_mv_overlap_warning")) {
    expect_match(ui, id, fixed = TRUE)
  }
})
