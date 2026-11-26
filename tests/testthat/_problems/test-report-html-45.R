# Extracted from test-report-html.R:45

# setup ------------------------------------------------------------------------
library(testthat)
test_env <- simulate_test_env(package = "vidternary", path = "..")
attach(test_env, warn.conflicts = FALSE)

# test -------------------------------------------------------------------------
html <- build_html_report("R", list(list(title = "T", content = list(
    NULL, report_warning(character(0)), report_list(character(0)), character(0), data.frame(), "kept"))))
expect_no_match(html, 'class="warning"', fixed = TRUE)
expect_no_match(html, "<ul>", fixed = TRUE)
expect_no_match(html, "<table>", fixed = TRUE)
expect_match(html, "<p>kept</p>", fixed = TRUE)
expect_match(build_html_report("R", list(list(title = "T", content = list(data.frame(a = numeric(0)))))), "<th>a</th>", fixed = TRUE)
