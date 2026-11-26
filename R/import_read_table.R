# ---- Import: read a particle table from Excel or a text file ----
# Automated SEM systems export particle lists as .xlsx or as delimited text
# (.csv, .txt, .tsv), and text exports from a European Windows use ";" with
# a decimal comma. read_particle_table() reads all of them into the same
# shape openxlsx gives (spaces in column names become dots), so every tab
# can work on the result without knowing where it came from.

# Text extensions read as delimited text.
.text_extensions <- c("csv", "txt", "tsv", "tab", "dat")
# Excel extensions read with openxlsx (.xlsm: macros are not run).
.excel_extensions <- c("xlsx", "xlsm")

#' File types the particle-data upload boxes offer
#'
#' Every tab that reads particle data offers exactly the extensions that
#' [read_particle_table()] can read, so the file dialogs cannot drift apart
#' from the reader (or from each other). An old `.xls` file is not offered;
#' the reader says to save it as `.xlsx` or `.csv`.
#'
#' @return A character vector such as `c(".xlsx", ".xlsm", ".csv", ...)`, for
#'   the `accept` argument of `shiny::fileInput()`.
#' @export
particle_file_accept <- function() {
  paste0(".", c(.excel_extensions, .text_extensions))
}

# Lines of a text file as UTF-8, whatever it was saved as.
.read_text_lines <- function(path, n = -1L) {
  lines <- readLines(path, n = n, warn = FALSE, encoding = "UTF-8")
  if (length(lines) && startsWith(lines[1], "\ufeff")) lines[1] <- substring(lines[1], 2)
  bad <- !validUTF8(lines)
  if (any(bad)) lines[bad] <- iconv(lines[bad], from = "latin1", to = "UTF-8")
  lines
}

#' Work out the delimiter and decimal mark of a delimited text file
#'
#' The delimiter is the candidate (tab, semicolon, comma, vertical bar)
#' that splits the first lines into the same number of fields, more than
#' one. The decimal mark is a comma when the delimiter is not a comma and
#' more fields look like `1,5` than like `1.5`.
#'
#' @param lines The first lines of the file (header included).
#' @return A list: `sep`, `dec`, `n_fields`.
#' @export
detect_text_format <- function(lines) {
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) == 0) stop("The file is empty.", call. = FALSE)
  lines <- utils::head(lines, 50)
  candidates <- c("\t", ";", ",", "|")
  score <- vapply(candidates, function(sep) {
    n <- lengths(strsplit(lines, sep, fixed = TRUE))
    # Share of lines agreeing with the most common field count, if above 1.
    common <- as.integer(names(which.max(table(n))))
    if (common < 2) 0 else mean(n == common) * 1000 + common
  }, numeric(1))
  if (all(score == 0)) return(list(sep = ",", dec = ".", n_fields = 1L))
  sep <- candidates[which.max(score)]
  fields <- unlist(strsplit(utils::tail(lines, -1), sep, fixed = TRUE))
  fields <- trimws(gsub('"', "", fields))
  dec <- "."
  if (sep != ",") {
    comma <- sum(grepl("^[-+]?[0-9]+,[0-9]+([eE][-+]?[0-9]+)?$", fields))
    point <- sum(grepl("^[-+]?[0-9]*\\.[0-9]+([eE][-+]?[0-9]+)?$", fields))
    if (comma > point) dec <- ","
  }
  list(sep = sep, dec = dec, n_fields = as.integer(names(which.max(table(lengths(strsplit(lines, sep, fixed = TRUE)))))))
}

#' Fill the blank cells of the element columns with 0
#'
#' An EDS export may leave the cell of an element that was not detected
#' empty instead of writing 0. The app treats such a blank as 0 everywhere,
#' and does so once, when a file is read ([read_particle_table()]), so that
#' every tab sees the same particles. Only columns named as weight or
#' atomic percent of an element (e.g. `Al.(Wt%)`, `Wt% Al`, `Fe at%`) are
#' filled - not a bare `Y` or `B`, which may be a coordinate or something
#' else. A column that is blank in every row (read as logical) becomes a
#' column of zeros.
#'
#' @param data A data frame.
#' @return `data` with the blanks of its element columns set to 0, and an
#'   attribute `blank_element_cells` (number of cells filled).
#' @export
fill_blank_element_cells <- function(data) {
  all_blank <- vapply(data, function(v) length(v) > 0 && all(is.na(v)), logical(1))
  probe <- data
  probe[all_blank] <- lapply(probe[all_blank], as.numeric)
  el <- detect_element_columns(probe)
  cols <- el$column[el$basis %in% c("wt", "at")]
  filled <- 0
  for (cn in cols) {
    v <- probe[[cn]]
    blank <- is.na(v)
    filled <- filled + sum(blank)
    v[blank] <- 0
    data[[cn]] <- v
  }
  attr(data, "blank_element_cells") <- filled
  data
}

#' Read a particle table from an Excel or delimited text file
#'
#' `.xlsx` and `.xlsm` files are read with `openxlsx` (first sheet by
#' default; macros are not run); `.csv`,
#' `.txt`, `.tsv`, `.tab` and `.dat` files as delimited text with the
#' delimiter and decimal mark worked out by [detect_text_format()]. Column
#' names are trimmed and their spaces replaced by dots, as `openxlsx` does,
#' so the same file gives the same column names whichever way it was saved.
#' Blank cells of the element columns are set to 0 (an element that was not
#' detected), see [fill_blank_element_cells()].
#'
#' @param path Path of the file.
#' @param name File name to take the extension from (an uploaded file's
#'   temporary path keeps the extension, but its original name is clearer in
#'   messages). Default: `path`.
#' @param sheet Sheet of an Excel file. Default 1.
#' @return A data frame, with an `import_info` attribute: `format`, `sep`,
#'   `dec`, `n_rows`, `n_cols`, `blank_element_cells`.
#' @export
read_particle_table <- function(path, name = path, sheet = 1) {
  ext <- tolower(tools::file_ext(name))
  if (!nzchar(ext)) ext <- tolower(tools::file_ext(path))
  if (ext %in% .excel_extensions) {
    d <- openxlsx::read.xlsx(path, sheet = sheet)
    info <- list(format = "Excel", sep = NA_character_, dec = NA_character_)
  } else if (ext %in% .text_extensions) {
    lines <- .read_text_lines(path)
    fmt <- detect_text_format(lines)
    if (fmt$n_fields < 2) stop("No column delimiter (tab, semicolon, comma) found in '", basename(name), "'.", call. = FALSE)
    d <- utils::read.table(text = lines, sep = fmt$sep, dec = fmt$dec, header = TRUE, quote = "\"",
                           check.names = FALSE, stringsAsFactors = FALSE, fill = TRUE, comment.char = "",
                           na.strings = c("", "NA", "N/A", "n/a", "-", "--"), strip.white = TRUE,
                           fileEncoding = "", encoding = "UTF-8")
    # A delimiter at the end of every line gives one empty, unnamed column.
    empty <- vapply(d, function(v) all(is.na(v)), logical(1)) & (is.na(names(d)) | !nzchar(names(d)) | grepl("^V[0-9]+$", names(d)))
    d <- d[, !empty, drop = FALSE]
    names(d) <- gsub("\\s+", ".", trimws(names(d)))
    info <- list(format = "delimited text", sep = fmt$sep, dec = fmt$dec)
  } else if (ext == "xls") {
    stop("'", basename(name), "' is an old-format .xls file, which cannot be read. Save it as .xlsx or .csv and upload that.", call. = FALSE)
  } else {
    stop("'", basename(name), "' is not an Excel (.xlsx, .xlsm) or text (.csv, .txt, .tsv, .tab, .dat) file.", call. = FALSE)
  }
  if (is.null(d) || ncol(d) == 0) stop("'", basename(name), "' holds no table.", call. = FALSE)
  d <- fill_blank_element_cells(d)
  filled <- attr(d, "blank_element_cells")
  attr(d, "blank_element_cells") <- NULL
  attr(d, "import_info") <- c(info, list(n_rows = nrow(d), n_cols = ncol(d), blank_element_cells = filled))
  d
}
