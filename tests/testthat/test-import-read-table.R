# Tests for R/import_read_table.R: Excel and delimited text files.
#
# library(shiny) is required for the last test - see test-server-evs.R for why.
library(shiny)

write_text <- function(lines, ext = ".csv", encoding = "UTF-8", bom = FALSE) {
  path <- tempfile(fileext = ext)
  con <- file(path, open = "wb")
  on.exit(close(con))
  if (bom) writeBin(as.raw(c(0xef, 0xbb, 0xbf)), con)
  text <- paste0(paste(lines, collapse = "\r\n"), "\r\n")
  writeBin(charToRaw(if (encoding == "latin1") iconv(text, "UTF-8", "latin1") else enc2utf8(text)), con)
  path
}

test_that("the delimiter and decimal mark are worked out from the first lines", {
  expect_equal(detect_text_format(c("a,b,c", "1.5,2,3", "4,5.25,6"))[c("sep", "dec")], list(sep = ",", dec = "."))
  expect_equal(detect_text_format(c("a;b;c", "1,5;2;3", "4;5,25;6"))[c("sep", "dec")], list(sep = ";", dec = ","))
  expect_equal(detect_text_format(c("a;b;c", "1.5;2;3", "4;5.25;6"))[c("sep", "dec")], list(sep = ";", dec = "."))
  expect_equal(detect_text_format(c("a\tb", "1,5\t2", "3,5\t4"))[c("sep", "dec")], list(sep = "\t", dec = ","))
  expect_equal(detect_text_format(c("a|b|c", "1|2|3"))$sep, "|")
  expect_equal(detect_text_format(c("a;b;c", "1;2;3"))$n_fields, 3)
})

test_that("decimal commas inside a semicolon file do not make the comma the delimiter", {
  lines <- c("Area;Length;Breadth", "12,5;5,2;3,1", "3,25;2,5;1,9", "7,5;4,5;2,5")
  expect_equal(detect_text_format(lines)$sep, ";")
})

test_that("a file without a delimiter and an empty file are reported", {
  expect_equal(detect_text_format(c("justonecolumn", "1", "2"))$n_fields, 1)
  expect_error(detect_text_format(character(0)), "empty")
  expect_error(read_particle_table(write_text(c("justonecolumn", "1", "2"))), "No column delimiter")
})

test_that("a comma-separated file is read with numbers as numbers", {
  d <- read_particle_table(write_text(c("Feature,Area (sq. um),Class", "1,1.5,oxide", "2,0.25,sulfide")))
  expect_equal(names(d), c("Feature", "Area.(sq..um)", "Class"))
  expect_equal(d[["Area.(sq..um)"]], c(1.5, 0.25))
  expect_equal(d$Class, c("oxide", "sulfide"))
  info <- attr(d, "import_info")
  expect_equal(info[c("format", "sep", "dec", "n_rows", "n_cols")],
               list(format = "delimited text", sep = ",", dec = ".", n_rows = 2L, n_cols = 3L))
})

test_that("a European export (semicolons, decimal commas) is read correctly", {
  d <- read_particle_table(write_text(c("Part No;Feature Area [um2];Dmax (um)", "1;12,5;5,2", "2;3,25;2,5")))
  expect_equal(names(d), c("Part.No", "Feature.Area.[um2]", "Dmax.(um)"))
  expect_equal(d[["Feature.Area.[um2]"]], c(12.5, 3.25))
  expect_true(is.numeric(d[["Dmax.(um)"]]))
  expect_equal(attr(d, "import_info")$dec, ",")
})

test_that("tab-delimited .txt and .tsv files are read", {
  lines <- c("Feature\tLength (um)", "1\t2.5", "2\t0.9")
  for (ext in c(".txt", ".tsv")) {
    d <- read_particle_table(write_text(lines, ext))
    expect_equal(d[["Length.(um)"]], c(2.5, 0.9), info = ext)
  }
})

test_that("a byte-order mark and a non-UTF-8 micro sign do not spoil the column names", {
  bom <- read_particle_table(write_text(c("Feature,Area", "1,2.5"), bom = TRUE))
  expect_equal(names(bom), c("Feature", "Area"))
  micro <- "\u00b5"
  for (enc in c("UTF-8", "latin1")) {
    d <- read_particle_table(write_text(c(paste0("Feature;Length (", micro, "m)"), "1;2,5"), encoding = enc))
    expect_equal(names(d)[2], paste0("Length.(", micro, "m)"), info = enc)
    expect_equal(d[[2]], 2.5, info = enc)
  }
})

test_that("a delimiter at the end of every line does not add an empty column", {
  d <- read_particle_table(write_text(c("a;b;", "1;2;", "3;4;")))
  expect_equal(names(d), c("a", "b"))
  expect_equal(d$b, c(2, 4))
})

test_that("empty cells and dashes are missing values, not text", {
  d <- read_particle_table(write_text(c("a,b", "1,", "2,-", "3,4.5")))
  expect_equal(d$b, c(NA, NA, 4.5))
})

test_that("the same table gives the same column names from xlsx and from csv", {
  d <- data.frame(`Area (sq. um)` = c(1.5, 2), `Stage X (mm)` = c(10, 11), `Al (Wt%)` = c(5, 0), check.names = FALSE)
  xlsx <- tempfile(fileext = ".xlsx")
  openxlsx::write.xlsx(d, xlsx)
  csv <- tempfile(fileext = ".csv")
  utils::write.csv(d, csv, row.names = FALSE)
  from_xlsx <- read_particle_table(xlsx)
  from_csv <- read_particle_table(csv)
  expect_equal(names(from_csv), names(from_xlsx))
  expect_equal(from_csv[[1]], from_xlsx[[1]])
  expect_equal(attr(from_xlsx, "import_info")$format, "Excel")
})

test_that("the extension is taken from the given name when the path has none", {
  path <- write_text(c("a,b", "1,2"), ext = "")
  expect_equal(names(read_particle_table(path, name = "export.csv")), c("a", "b"))
})

test_that("unreadable kinds of file are refused with a way out", {
  expect_error(read_particle_table(tempfile(fileext = ".xls")), "old-format .xls file")
  expect_error(read_particle_table(tempfile(fileext = ".pdf")), "not an Excel \\(.xlsx\\) or text")
})

test_that("uploads of text files reach the tabs through the shared upload reader", {
  path <- write_text(c("area;field", "12,5;A", "3,25;A", "7;B"))
  upload <- data.frame(name = "export.csv", size = 1, type = "", datapath = path, stringsAsFactors = FALSE)
  shiny::testServer(function(input, output, session) {
    data <- make_combined_upload_reactive(input, "files", allow_multiple = FALSE)
    output$n <- shiny::renderText(paste(nrow(data()), sum(data()$area)))
  }, {
    session$setInputs(files = upload)
    expect_equal(output$n, "3 22.75")
  })
})
