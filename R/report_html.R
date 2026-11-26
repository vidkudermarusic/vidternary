# ---- Reproducibility: a self-contained HTML report ----
# build_html_report() turns a list of sections (text, tables, plots, code)
# into one HTML file with the plots embedded, so a result can be archived
# or sent as a single file that opens in any browser. It writes the HTML
# itself and needs neither pandoc nor rmarkdown, which a Shiny server or a
# plain R installation may not have.

.html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' Mark report content as a warning, a bullet list or code
#'
#' Plain character vectors in a report section are paragraphs; these
#' helpers mark the other kinds of text.
#'
#' @param text Character vector.
#' @return `text` with a class that [build_html_report()] recognises.
#' @export
report_warning <- function(text) structure(as.character(text), class = "report_warning")

#' @rdname report_warning
#' @export
report_list <- function(text) structure(as.character(text), class = "report_list")

#' @rdname report_warning
#' @export
report_code <- function(text) structure(paste(as.character(text), collapse = "\n"), class = "report_code")

# One content item as HTML.
.report_item <- function(item, plot_width = 8, plot_height = 5, dpi = 110) {
  if (is.null(item) || (!is.data.frame(item) && !inherits(item, "ggplot") && length(item) == 0)) return("")
  if (inherits(item, "report_warning")) {
    return(paste0('<p class="warning">', .html_escape(item), "</p>", collapse = "\n"))
  }
  if (inherits(item, "report_list")) {
    if (length(item) == 0) return("")
    return(paste0("<ul>", paste0("<li>", .html_escape(item), "</li>", collapse = ""), "</ul>"))
  }
  if (inherits(item, "report_code")) return(paste0("<pre>", .html_escape(item), "</pre>"))
  if (inherits(item, "ggplot")) {
    path <- tempfile(fileext = ".png")
    on.exit(unlink(path), add = TRUE)
    ggplot2::ggsave(path, plot = item, width = plot_width, height = plot_height, dpi = dpi)
    data <- jsonlite::base64_enc(readBin(path, "raw", n = file.info(path)$size))
    return(paste0('<img alt="plot" src="data:image/png;base64,', gsub("\\s", "", data), '">'))
  }
  if (is.data.frame(item)) {
    if (ncol(item) == 0) return("")
    cells <- vapply(item, function(v) {
      as.character(if (is.numeric(v)) ifelse(is.na(v), "", format(signif(v, 5), trim = TRUE, scientific = FALSE, drop0trailing = TRUE))
                   else ifelse(is.na(v), "", .html_escape(v)))
    }, character(nrow(item)))
    cells <- matrix(cells, nrow = nrow(item))
    numeric_col <- vapply(item, is.numeric, logical(1))
    td <- function(row) paste0(ifelse(numeric_col, '<td class="num">', "<td>"), row, "</td>", collapse = "")
    body <- if (nrow(item)) paste0("<tr>", apply(cells, 1, td), "</tr>", collapse = "\n") else ""
    head <- paste0("<th>", .html_escape(names(item)), "</th>", collapse = "")
    return(paste0('<div class="table"><table><thead><tr>', head, "</tr></thead><tbody>\n", body, "\n</tbody></table></div>"))
  }
  paste0("<p>", .html_escape(item), "</p>", collapse = "\n")
}

.report_css <- "
body { font-family: Segoe UI, Helvetica, Arial, sans-serif; color: #1c2733; background: #ffffff;
  max-width: 1000px; margin: 0 auto; padding: 24px 16px 60px; line-height: 1.45; }
h1 { font-size: 26px; margin-bottom: 4px; color: #002147; }
h2 { font-size: 19px; margin-top: 34px; padding-bottom: 4px; border-bottom: 2px solid #357ABD; color: #002147; }
p.subtitle { color: #55657a; margin-top: 0; }
p.warning { background: #fff3cd; border: 1px solid #ffe08a; color: #6b5200; padding: 9px 12px; border-radius: 4px; }
div.table { overflow-x: auto; margin: 10px 0 16px; }
table { border-collapse: collapse; font-size: 13px; }
th { text-align: left; background: #eef3f9; border-bottom: 1px solid #b8c7d9; padding: 5px 10px; }
td { border-bottom: 1px solid #e3e9f0; padding: 4px 10px; vertical-align: top; }
td.num { text-align: right; font-variant-numeric: tabular-nums; white-space: nowrap; }
img { max-width: 100%; height: auto; margin: 8px 0; }
pre { background: #f4f6f9; border: 1px solid #dde4ec; padding: 12px; overflow-x: auto; font-size: 12.5px; }
footer { margin-top: 40px; color: #6b7a8c; font-size: 12px; border-top: 1px solid #dde4ec; padding-top: 8px; }
"

#' Build a self-contained HTML report
#'
#' @param title Report title.
#' @param sections List of sections, each a list with `title` and
#'   `content`: a list whose items are character vectors (paragraphs), data
#'   frames (tables), `ggplot` objects (embedded as PNG) or text marked with
#'   [report_warning()], [report_list()] or [report_code()].
#' @param subtitle Optional line under the title.
#' @param footer Optional footer text.
#' @return The HTML document as one string; write it with [writeLines()]
#'   (`useBytes = TRUE`).
#' @export
build_html_report <- function(title, sections, subtitle = NULL, footer = NULL) {
  body <- vapply(sections, function(s) {
    items <- vapply(s$content, .report_item, character(1))
    paste0("<section>\n<h2>", .html_escape(s$title), "</h2>\n", paste(items[nzchar(items)], collapse = "\n"), "\n</section>")
  }, character(1))
  enc2utf8(paste0(
    '<!DOCTYPE html>\n<html lang="en">\n<head>\n<meta charset="utf-8">\n',
    '<meta name="viewport" content="width=device-width, initial-scale=1">\n',
    "<title>", .html_escape(title), "</title>\n<style>", .report_css, "</style>\n</head>\n<body>\n",
    "<h1>", .html_escape(title), "</h1>\n",
    if (!is.null(subtitle)) paste0('<p class="subtitle">', .html_escape(subtitle), "</p>\n"),
    paste(body, collapse = "\n"),
    if (!is.null(footer)) paste0("\n<footer>", .html_escape(footer), "</footer>"),
    "\n</body>\n</html>\n"))
}

#' Where the data of a report came from
#'
#' @param names File names as the user knows them.
#' @param paths Paths the files can be read from (for size and checksum).
#' @param rows Number of rows read from each file.
#' @return A data frame: `File`, `Rows`, `Size (bytes)`, `MD5 checksum`.
#'   The checksum lets a reader confirm they hold the very same file.
#' @export
report_provenance <- function(names, paths = NULL, rows = NULL) {
  n <- length(names)
  exists_ <- if (is.null(paths)) rep(FALSE, n) else file.exists(paths)
  data.frame(
    File = names,
    Rows = if (is.null(rows)) rep(NA_integer_, n) else as.integer(rows),
    `Size (bytes)` = ifelse(exists_, file.info(paths %||% NA_character_)$size, NA_real_),
    `MD5 checksum` = ifelse(exists_, unname(tools::md5sum(paths %||% NA_character_)), NA_character_),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# The line every report ends with.
.report_footer <- function() {
  sprintf("Written by vidternary %s with %s on %s.", as.character(utils::packageVersion("vidternary")),
          R.version.string, format(Sys.time(), "%Y-%m-%d %H:%M"))
}

# A value as R code, on one line.
.r_literal <- function(x) paste(deparse(x, width.cutoff = 500L), collapse = " ")

# The opening lines every generated script shares.
.script_header <- function(what, tab) {
  c(sprintf("# %s - written by vidternary %s on %s", what, as.character(utils::packageVersion("vidternary")),
            format(Sys.time(), "%Y-%m-%d %H:%M")),
    sprintf("# Repeats the analysis of the \"%s\" tab without the app.", tab),
    "# Set the file path(s) below to where the data is on your computer.",
    "", "library(vidternary)", "")
}
