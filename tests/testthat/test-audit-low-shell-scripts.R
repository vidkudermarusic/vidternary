# Three low findings of audit pass 7: the stale page shell, the
# reproduction gaps of the newest features (acceptance limits missing from
# the cleanliness script; no report or script for Measurement Planning), and
# the plot warning of a stringer map without rated objects.

# ---- Page shell ----

test_that("the page title names the app and its version, the old v6 heading is gone", {
  title <- app_title()
  expect_match(title, "^vidternary [0-9.]+ - SEM/EDS inclusion analysis$")
  expect_match(title, as.character(utils::packageVersion("vidternary")), fixed = TRUE)
  html <- as.character(create_main_ui())
  # the visible heading (the browser-tab title goes into the page head,
  # which Shiny adds when it serves the page)
  expect_match(html, paste0("<h2>", title, "</h2>"), fixed = TRUE)
  expect_false(grepl("Ternary Plot Generator with Advanced Filtering", html, fixed = TRUE))
  expect_false(grepl("Custom Ternary Builder v6", html, fixed = TRUE))
  expect_match(html, "steel cleanliness rating", fixed = TRUE)
  # the dead project_status output is gone from the server too
  expect_false(exists("create_server_status_outputs"))
})

# ---- Stringer map without rated objects ----

test_that("a stringer map without rated objects draws without a warning and says why it is empty", {
  d <- make_cleanliness_data()
  d <- d[d[["Breadth.(um)"]] < 1, , drop = FALSE]   # only the narrow matrix specks
  r <- run_cleanliness_analysis(d, CLEAN_COLS, list(n_boot = 50))
  expect_equal(sum(r$objects$rated), 0)
  g <- create_stringer_map(r)
  expect_no_warning(ggplot2::ggplot_build(g))
  expect_match(g$labels$caption, "No particle is wide enough to be rated", fixed = TRUE)
  expect_no_warning(ggplot2::ggsave(tempfile(fileext = ".png"), g, width = 5, height = 4, dpi = 50))
  # the report is written without a warning as well
  expect_no_warning(cleanliness_report(r))
})

test_that("a stringer map with rated objects is unchanged (colour by type, no caption)", {
  r <- run_cleanliness_analysis(make_cleanliness_data(), CLEAN_COLS, list(n_boot = 50))
  g <- create_stringer_map(r)
  expect_null(g$labels$caption)
  expect_true(any(vapply(g$layers, function(l) inherits(l$geom, "GeomSegment"), logical(1))))
  expect_no_warning(ggplot2::ggplot_build(g))
})

# ---- Cleanliness script with acceptance limits ----

test_that("the cleanliness script repeats the acceptance check with the limits entered", {
  upload <- make_cleanliness_upload()
  all_data <- read_particle_table(upload$datapath)
  r <- run_cleanliness_analysis(all_data, CLEAN_COLS, list(n_boot = 200), all_data = all_data)
  acc <- list(limits = acceptance_limits(per_mm2 = 10, area_ppm = 500, largest_um = 30, return_level_um = 40,
                                         ratings = parse_rating_limits("A=1, B=0.5")),
              fields_per_area = 1, return_period = 50)
  # what the tab computes (server_cleanliness.R)
  evs <- acceptance_evs(suppressWarnings(as.numeric(all_data[[CLEAN_COLS$area]])), all_data[[CLEAN_COLS$field]],
                        all_data[[CLEAN_COLS$field]], 1, 50)
  in_app <- assess_acceptance(r, acc$limits, evs)

  script <- cleanliness_script(upload$datapath, CLEAN_COLS, r$settings, acceptance = acc)
  expect_silent(parse(text = script))
  expect_true(any(grepl("^limits <- ", script)))
  expect_true(any(grepl("^evs <- acceptance_evs\\(", script)))
  env <- new.env()
  eval(parse(text = script[!grepl("^library\\(", script)]), envir = env)
  expect_equal(env$acceptance$table, in_app$table)
  expect_equal(env$acceptance$summary, in_app$summary)

  # without the extreme-value limit no fit is made
  acc2 <- list(limits = acceptance_limits(per_mm2 = 10), fields_per_area = 1, return_period = 100)
  s2 <- cleanliness_script("x.xlsx", CLEAN_COLS, r$settings, acceptance = acc2)
  expect_true("evs <- NULL" %in% s2)
  # no limits: no acceptance part
  for (a in list(NULL, list(limits = acceptance_limits(), fields_per_area = 1, return_period = 100))) {
    expect_false(any(grepl("assess_acceptance", cleanliness_script("x.xlsx", CLEAN_COLS, r$settings, acceptance = a))))
  }
})

# ---- Measurement Planning: report and script ----

PLAN_SCRIPT_COLS <- BATCH_COLS[c("area", "field", "x", "y")]

test_that("the planning script repeats the plan, filter included", {
  upload <- make_batch_upload(make_batches()[c("A", "B")])
  all_runs <- lapply(upload$datapath, read_particle_table)
  names(all_runs) <- batch_labels(upload$name)
  filters <- stats::setNames(list("> 1"), PLAN_SCRIPT_COLS$area)
  runs <- lapply(all_runs, apply_pre_filters, filters = filters)
  res <- plan_measurement(runs, PLAN_SCRIPT_COLS, list(n_sim = 50), all_runs = all_runs)

  script <- planning_script(upload$datapath, PLAN_SCRIPT_COLS, res$settings, filters, run_names = names(all_runs))
  expect_silent(parse(text = script))
  expect_match(script[1], "^# Measurement plan - written by vidternary ")
  env <- new.env()
  eval(parse(text = script[!grepl("^library\\(", script)]), envir = env)
  expect_equal(env$result$pilot, res$pilot)
  expect_equal(env$result$precision, res$precision)
  expect_equal(env$result$ratio, res$ratio)
  expect_equal(env$result$evs, res$evs)
  expect_equal(env$result$repeatability, res$repeatability)
  expect_equal(env$result$notes, res$notes)

  plain <- planning_script("x.xlsx", PLAN_SCRIPT_COLS, res$settings)
  expect_true("runs <- all_runs" %in% plain)
  expect_true(any(grepl('names(all_runs) <- "x"', plain, fixed = TRUE)))
})

test_that("the planning report holds every section and the script", {
  upload <- make_batch_upload(make_batches()[c("A", "B")])
  all_runs <- lapply(upload$datapath, read_particle_table)
  names(all_runs) <- batch_labels(upload$name)
  res <- plan_measurement(all_runs, PLAN_SCRIPT_COLS, list(n_sim = 50))
  script <- planning_script(upload$datapath, PLAN_SCRIPT_COLS, res$settings, run_names = names(all_runs))
  html <- planning_report(res, report_provenance(upload$name, upload$datapath, c(200, 200)), script)
  for (heading in c("Data and settings", "What to keep in mind", "The pilot measurement", "Number density",
                    "Largest inclusion (extreme value statistics)", "Repeated runs", "Methods",
                    "R script that repeats this plan")) {
    expect_match(html, paste0("<h2>", heading, "</h2>"), fixed = TRUE)
  }
  expect_match(html, "2 run(s), 400 particles", fixed = TRUE)
  expect_match(html, unname(tools::md5sum(upload$datapath[1])), fixed = TRUE)
  expect_match(html, "plan_measurement(runs, cols, settings", fixed = TRUE)
  expect_match(html, "data:image/png;base64,", fixed = TRUE)
  # one run, no field column: still a report, the missing parts say so
  one <- plan_measurement(all_runs[1], PLAN_SCRIPT_COLS[c("area", "x", "y")], list(n_sim = 50))
  html1 <- planning_report(one)
  expect_match(html1, "Upload two or more runs", fixed = TRUE)
  expect_match(html1, "Needs a SEM field ID column", fixed = TRUE)
})
