# Tests for R/example_data.R: the simulated files that ship with the package.
library(shiny)

test_that("three example files ship, the runs first", {
  files <- example_data_files()
  expect_equal(names(files), c("example_run_1.xlsx", "example_run_2.xlsx", "example_other_heat.xlsx"))
  expect_true(all(file.exists(files)))
})

test_that("an example file reads as a particle table every tab can use", {
  d <- example_data()
  expect_equal(d, example_data("example_run_1.xlsx"), ignore_attr = TRUE)
  expect_gt(nrow(d), 800)
  roles <- detect_column_roles(d)
  expect_equal(unname(roles[c("field", "area", "length", "breadth", "x", "y", "direction")]),
               c("Field", "Area.(sq..um)", "Length.(um)", "Breadth.(um)", "Stage.X.(mm)", "Stage.Y.(mm)", "Direction.(degrees)"))
  chem <- d[, grepl("Wt%", names(d), fixed = TRUE)]
  expect_equal(ncol(chem), 14)
  expect_equal(unname(rowSums(chem)), rep(100, nrow(d)), tolerance = 0.01)
  expect_true(all(d$Field %in% 1:200))
  expect_error(example_data("nothing"), "No example file 'nothing'. Available: example_run_1, example_run_2, example_other_heat")
})

test_that("the example data shows what it was built to show", {
  cols <- list(area = "Area.(sq..um)", length = "Length.(um)", breadth = "Breadth.(um)", x = "Stage.X.(mm)", y = "Stage.Y.(mm)",
               direction = "Direction.(degrees)", field = "Field")
  res <- run_cleanliness_analysis(example_data(), cols, list(n_boot = 200))
  expect_equal(res$inspected_area_mm2, 2, tolerance = 0.02)
  expect_gt(sum(res$objects$is_stringer & res$objects$rated), 0)                 # stringers
  expect_gt(sum(res$particles$family == "too small to classify"), 100)           # particles below the electron range
  expect_setequal(setdiff(unique(res$particles$family), c("too small to classify", "unclassified")),
                  c("aluminate", "silicate", "sulfide", "Ti-rich"))
  # clustered: counts per field vary more than random scatter
  expect_gt(field_count_dispersion(example_data()$Field, 200)$dispersion, 1.5)

  # the two runs agree, the other heat does not
  runs <- lapply(c(run_1 = "example_run_1", run_2 = "example_run_2", other = "example_other_heat"), example_data)
  plan_cols <- cols[c("area", "field", "x", "y")]
  expect_gt(plan_measurement(runs[1:2], plan_cols, list(n_sim = 50))$repeatability$density$p_value, 0.05)
  expect_lt(plan_measurement(runs, plan_cols, list(n_sim = 50))$repeatability$density$p_value, 0.001)
})

test_that("the example bar offers the files to the page and to download", {
  html <- as.character(example_data_ui())
  expect_match(html, "vidternaryLoadExample([&quot;example_run_1.xlsx&quot;,&quot;example_run_2.xlsx&quot;,&quot;example_other_heat.xlsx&quot;], &#39;example_data_status&#39;)", fixed = TRUE)
  expect_match(html, "Simulated particle files", fixed = TRUE)
  expect_true("vidternary-examples" %in% names(shiny::resourcePaths()))
  # the planning tab asks for the two runs only
  expect_match(as.character(create_planning_tab("planning")), "data-example-files=\"example_run_1.xlsx,example_run_2.xlsx\"", fixed = TRUE)

  testServer(function(input, output, session) register_example_data(output), {
    expect_setequal(zip::zip_list(output$example_data_download)$filename, names(example_data_files()))
  })
})
