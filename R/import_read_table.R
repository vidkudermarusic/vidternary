# ---- Import: read a particle table from Excel or a text file ----
# Automated SEM systems export particle lists as .xlsx or as delimited text
# (.csv, .txt, .tsv), and text exports from a European Windows use ";" with
# a decimal comma. read_particle_table() reads all of them into the same
# shape openxlsx gives (spaces in column names become dots), so every tab
# can work on the result without knowing where it came from.

# Text extensions read as delimited text.
.text_extensions <- c("csv", "txt", "tsv", "tab", "dat")

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

#' Read a particle table from an Excel or delimited text file
#'
#' `.xlsx` files are read with `openxlsx` (first sheet by default); `.csv`,
#' `.txt`, `.tsv`, `.tab` and `.dat` files as delimited text with the
#' delimiter and decimal mark worked out by [detect_text_format()]. Column
#' names are trimmed and their spaces replaced by dots, as `openxlsx` does,
#' so the same file gives the same column names whichever way it was saved.
#'
#' @param path Path of the file.
#' @param name File name to take the extension from (an uploaded file's
#'   temporary path keeps the extension, but its original name is clearer in
#'   messages). Default: `path`.
#' @param sheet Sheet of an Excel file. Default 1.
#' @return A data frame, with an `import_info` attribute: `format`, `sep`,
#'   `dec`, `n_rows`, `n_cols`.
#' @export
read_particle_table <- function(path, name = path, sheet = 1) {
  ext <- tolower(tools::file_ext(name))
  if (!nzchar(ext)) ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xlsm")) {
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
    stop("'", basename(name), "' is not an Excel (.xlsx) or text (.csv, .txt, .tsv) file.", call. = FALSE)
  }
  if (is.null(d) || ncol(d) == 0) stop("'", basename(name), "' holds no table.", call. = FALSE)
  attr(d, "import_info") <- c(info, list(n_rows = nrow(d), n_cols = ncol(d)))
  d
}
