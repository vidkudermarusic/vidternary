# Tests for R/report_html.R, R/cleanliness_report.R and R/evs_report.R.

test_that("a report holds its sections, text, tables, code and embedded plots", {
  plot <- ggplot2::ggplot(data.frame(x = 1:3, y = c(2, 4, 3)), ggplot2::aes(x = x, y = y)) + ggplot2::geom_point()
  html <- build_html_report("My report", subtitle = "three particles", footer = "the end", sections = list(
    list(title = "First", content = list("A paragraph.", report_list(c("one", "two")), report_warning("Careful."))),
    list(title = "Second", content = list(data.frame(Name = c("a", "b"), Value = c(1.5, NA)), plot, report_code(c("x <- 1", "y <- 2"))))
  ))
  expect_length(html, 1)
  expect_match(html, "^<!DOCTYPE html>")
  expect_match(html, "<title>My report</title>", fixed = TRUE)
  expect_match(html, '<p class="subtitle">three particles</p>', fixed = TRUE)
  expect_match(html, "<h2>First</h2>", fixed = TRUE)
  expect_match(html, "<p>A paragraph.</p>", fixed = TRUE)
  expect_match(html, "<ul><li>one</li><li>two</li></ul>", fixed = TRUE)
  expect_match(html, '<p class="warning">Careful.</p>', fixed = TRUE)
  expect_match(html, "<th>Name</th><th>Value</th>", fixed = TRUE)
  expect_match(html, '<td>a</td><td class="num">1.5</td>', fixed = TRUE)
  expect_match(html, '<td>b</td><td class="num"></td>', fixed = TRUE)      # a missing number is an empty cell
  expect_match(html, '<img alt="plot" src="data:image/png;base64,iVBOR', fixed = TRUE)   # a PNG, embedded
  expect_match(html, "<pre>x &lt;- 1\ny &lt;- 2</pre>", fixed = TRUE)
  expect_match(html, "<footer>the end</footer>", fixed = TRUE)
  # nothing is loaded from elsewhere: the file stands on its own
  expect_no_match(html, "src=\"http", fixed = TRUE)
  expect_no_match(html, "<link", fixed = TRUE)
})

test_that("text from the data cannot break the page", {
  html <- build_html_report("A <b> & C", list(list(title = "T", content = list(
    "x < y & y > z", data.frame(`<col>` = "<script>alert(1)</script>", check.names = FALSE)))))
  expect_match(html, "<title>A &lt;b&gt; &amp; C</title>", fixed = TRUE)
  expect_match(html, "x &lt; y &amp; y &gt; z", fixed = TRUE)
  expect_match(html, "&lt;script&gt;alert(1)&lt;/script&gt;", fixed = TRUE)
  expect_no_match(html, "<script>", fixed = TRUE)
})

test_that("empty and missing content is left out, not printed as an empty box", {
  html <- build_html_report("R", list(list(title = "T", content = list(
    NULL, report_warning(character(0)), report_list(character(0)), character(0), data.frame(), "kept"))))
  expect_no_match(html, 'class="warning"', fixed = TRUE)
  expect_no_match(html, "<ul>", fixed = TRUE)
  expect_no_match(html, "<table>", fixed = TRUE)
  expect_match(html, "<p>kept</p>", fixed = TRUE)
  # a table without rows still shows its header
  expect_match(build_html_report("R", list(list(title = "T", content = list(data.frame(a = numeric(0)))))), "<th>a</th>", fixed = TRUE)
})

test_that("the provenance table identifies the very file", {
  path <- tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,2"), path)
  p <- report_provenance(c("mine.csv", "gone.csv"), c(path, tempfile()), c(1, NA))
  expect_equal(p$File, c("mine.csv", "gone.csv"))
  expect_equal(p$Rows, c(1L, NA))
  expect_equal(p$`MD5 checksum`[1], unname(tools::md5sum(path)))
  expect_equal(p$`Size (bytes)`[1], file.info(path)$size)
  expect_true(is.na(p$`MD5 checksum`[2]))
  none <- report_provenance("only_a_name.xlsx")
  expect_true(is.na(none$`MD5 checksum`))
})

test_that("the cleanliness script repeats the analysis, filter included", {
  upload <- make_cleanliness_upload()
  all_data <- read_particle_table(upload$datapath)
  filters <- stats::setNames(list("> 0.5"), CLEAN_COLS$area)
  data <- apply_pre_filters(all_data, filters)
  r <- run_cleanliness_analysis(data, CLEAN_COLS, list(n_boot = 200, measured_ppm = c(S = 100)), all_data = all_data)

  script <- cleanliness_script(upload$datapath, CLEAN_COLS, r$settings, filters)
  expect_silent(parse(text = script))
  expect_match(script[1], "^# Steel cleanliness analysis - written by vidternary ")
  env <- new.env()
  eval(parse(text = script[!grepl("^library\\(", script)]), envir = env)
  expect_equal(env$result$rating_summary, r$rating_summary)
  expect_equal(env$result$indices, r$indices)
  expect_equal(env$result$mass_balance, r$mass_balance)
  expect_equal(nrow(env$data), 10)

  # without a filter the script uses all rows
  plain <- cleanliness_script("x.xlsx", CLEAN_COLS, r$settings)
  expect_true("data <- all_data" %in% plain)
  expect_false(any(grepl("apply_pre_filters", plain)))
})

test_that("the cleanliness report holds every section and the script", {
  upload <- make_cleanliness_upload()
  r <- run_cleanliness_analysis(read_particle_table(upload$datapath), CLEAN_COLS, list(n_boot = 200))
  script <- cleanliness_script("particles.xlsx", CLEAN_COLS, r$settings)
  html <- cleanliness_report(r, report_provenance("particles.xlsx", upload$datapath, 32), script)
  for (heading in c("Data and settings", "What to keep in mind", "Chart rating (ISO 4967)", "Stringers",
                    "Number density and area fraction", "Stereology and mass balance", "Methods",
                    "R script that repeats this analysis")) {
    expect_match(html, paste0("<h2>", heading, "</h2>"), fixed = TRUE)
  }
  expect_match(html, "32 particles, 8 rated objects, inspected area 2.00 mm2", fixed = TRUE)
  expect_match(html, "The inspected area is 2.00 mm2", fixed = TRUE)
  expect_match(html, "64 um total length", fixed = TRUE)
  expect_match(html, unname(tools::md5sum(upload$datapath)), fixed = TRUE)
  expect_match(html, "run_cleanliness_analysis(data, cols, settings", fixed = TRUE)
  # map, rose diagram, size classes and Saltykov plot
  expect_equal(lengths(regmatches(html, gregexpr("data:image/png;base64,", html, fixed = TRUE))), 4)
  # the report can be written and is a real file
  path <- tempfile(fileext = ".html")
  writeLines(html, path, useBytes = TRUE)
  expect_gt(file.info(path)$size, 50000)
})

# The fit object as the EVS tab builds it.
evs_fit_for_report <- function(data, fields_per_area = 2) {
  areas <- assign_control_areas(data$Field, fields_per_area)
  data$.control_area <- areas$control_area
  maxima <- compute_block_maxima(data, BATCH_COLS$area, ".control_area")
  fit <- fit_evs_gumbel(maxima$sqrt_area_max)
  fit$block_maxima <- maxima
  fit$gof <- gumbel_goodness_of_fit(fit, n_sim = 99)
  fit$fields_per_area <- fields_per_area
  fit$n_fields <- areas$n_fields
  fit$n_control_areas <- areas$n_control_areas
  fit$area_notes <- evs_area_notes(areas, nrow(maxima), fields_per_area)
  fit
}

test_that("the EVS script repeats the fit, with and without merged fields and fatigue", {
  upload <- make_batch_upload(make_batches()["A"])
  data <- read_particle_table(upload$datapath)
  fit <- evs_fit_for_report(data, 2)
  pred <- predict_evs_max(fit, 50)

  script <- evs_script(upload$datapath, BATCH_COLS$area, "Field", 2, 50, hv = 200, location = "internal", stress_ratio = 0)
  expect_silent(parse(text = script))
  env <- new.env()
  grDevices::pdf(NULL)
  eval(parse(text = script[!grepl("^library\\(|^create_gumbel_plot", script)]), envir = env)
  grDevices::dev.off()
  expect_equal(env$prediction$predicted, pred$predicted)
  expect_equal(env$prediction$upper, pred$upper)
  expect_true(any(grepl('evs_fatigue_limit(prediction, hv = 200, location = "internal", stress_ratio = 0)', script, fixed = TRUE)))

  one <- evs_script("x.xlsx", "Area", "Field", 1, 100, filters = list(Area = "> 1"))
  expect_true("maxima <- compute_block_maxima(data, area_col, field_col)" %in% one)
  expect_true(any(grepl('apply_pre_filters(all_data, list(Area = "> 1"))', one, fixed = TRUE)))
  expect_false(any(grepl("evs_fatigue_limit", one)))       # no hardness, no fatigue line
  expect_false(any(grepl("evs_fatigue_limit", evs_script("x.xlsx", "Area", "Field", hv = NA))))
})

test_that("the EVS report holds the fit, the prediction and - when asked - the fatigue limit", {
  fit <- evs_fit_for_report(make_batches()$A, 2)
  pred <- predict_evs_max(fit, 50)
  plain <- evs_report(fit, pred)
  expect_match(plain, "<h2>Gumbel fit and prediction</h2>", fixed = TRUE)
  expect_match(plain, "<h2>Block maxima</h2>", fixed = TRUE)
  expect_match(plain, sprintf("%.2f", pred$predicted), fixed = TRUE)
  expect_match(plain, sprintf("10 control areas, return level %.2f um for T = 50", pred$predicted), fixed = TRUE)
  expect_match(plain, "about 63% of cases", fixed = TRUE)
  expect_no_match(plain, "Fatigue limit estimate", fixed = TRUE)
  expect_no_match(plain, "R script that repeats", fixed = TRUE)

  full <- evs_report(fit, pred, evs_fatigue_limit(pred, 200), report_provenance("a.xlsx"), evs_script("a.xlsx", "Area", "Field", 2, 50),
                     filters = list(Area = "> 1"))
  expect_match(full, "<h2>Fatigue limit estimate (Murakami)</h2>", fixed = TRUE)
  expect_match(full, sprintf("%.0f", murakami_fatigue_limit(pred$predicted, 200)), fixed = TRUE)
  expect_match(full, "<h2>R script that repeats this analysis</h2>", fixed = TRUE)
  expect_match(full, "<td>Area</td><td>&gt; 1</td>", fixed = TRUE)
})
