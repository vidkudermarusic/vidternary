# ---- Reproducibility: save and load the settings of a tab ----
# Every input of a tab (columns, numbers, choices, filters) can be saved as
# a small JSON file and loaded again later, so an analysis can be repeated
# exactly - by the same person next month, or by a reviewer. Shared by the
# tabs through settings_io_ui() and register_settings_io().
#
# Loading has one subtlety: some inputs only exist after another one has
# been set (a filter box appears once its column is chosen), and a value
# sent to an input that is not on the page yet is lost. Whether an input is
# on the page cannot be told from the server, so after loading, every value
# that has not arrived is sent again a few times, a moment apart; what
# still has not arrived then is reported.

#' The current settings of a tab as a plain list
#'
#' Keeps the inputs that hold a setting (text, numbers, logicals, of any
#' length) and leaves out uploads, buttons and anything else that is not a
#' plain value. An input without a value (a selection box with nothing
#' selected) is kept as an empty selection, so loading clears it again.
#'
#' @param values Named list of input values, e.g.
#'   `shiny::reactiveValuesToList(input)`.
#' @param exclude Regular expressions of input ids to leave out.
#' @return A named list, sorted by name.
#' @export
collect_settings <- function(values, exclude = character(0)) {
  keep <- vapply(names(values), function(id) {
    v <- values[[id]]
    is_plain <- is.null(v) || (is.null(dim(v)) && (is.character(v) || is.numeric(v) || is.logical(v)) &&
      !inherits(v, "shinyActionButtonValue"))
    is_plain && !any(vapply(exclude, grepl, logical(1), x = id))
  }, logical(1))
  out <- values[keep]
  out <- lapply(out, function(v) {
    if (is.null(v)) return(character(0))
    attributes(v) <- NULL
    v
  })
  out[order(names(out))]
}

#' Write and read a settings file
#'
#' @param settings From [collect_settings()].
#' @param tab Id of the tab the settings belong to.
#' @param path File to write or read (JSON).
#' @return `write_settings_file()` returns `path` invisibly;
#'   `read_settings_file()` returns a list: `tab`, `saved`, `version`,
#'   `settings`.
#' @export
write_settings_file <- function(settings, tab, path) {
  out <- list(app = "vidternary", kind = "tab settings", tab = tab,
              version = as.character(utils::packageVersion("vidternary")),
              saved = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
              # Vectors stay arrays, so a one-column selection reads back as a selection.
              settings = lapply(settings, function(v) if (length(v) == 1) jsonlite::unbox(v) else v))
  jsonlite::write_json(out, path, pretty = TRUE, null = "null", na = "null", digits = NA)
  invisible(path)
}

#' @rdname write_settings_file
#' @export
read_settings_file <- function(path, tab = NULL) {
  raw <- tryCatch(jsonlite::fromJSON(path, simplifyVector = TRUE),
                  error = function(e) stop("The settings file is not valid JSON.", call. = FALSE))
  if (!identical(raw$kind, "tab settings")) stop("This file is not a vidternary settings file.", call. = FALSE)
  if (!is.null(tab) && !identical(raw$tab, tab)) {
    stop(sprintf("These settings were saved from the '%s' tab and cannot be loaded into '%s'.", raw$tab, tab), call. = FALSE)
  }
  settings <- raw$settings
  if (is.null(settings)) settings <- list()
  # An empty selection is written as [] and read back as list(): keep it
  # empty. A value written as null (an empty number box) is left alone.
  settings <- settings[!vapply(settings, is.null, logical(1))]
  settings <- lapply(settings, function(v) if (is.list(v) && length(v) == 0) character(0) else v)
  list(tab = raw$tab, saved = raw$saved, version = raw$version, settings = settings)
}

#' Save / load buttons for the settings of a tab
#'
#' @param ns The tab's namespacing function.
#' @param prefix Short, tab-unique input-id prefix (e.g. `"evs"`).
#' @return A `shiny::div()`.
#' @export
settings_io_ui <- function(ns, prefix) {
  div(style = "border: 1px solid #6c757d; padding: 10px 15px; border-radius: 5px; margin: 10px 0; background-color: #f8f9fa;",
    h4("Settings of this tab"),
    fluidRow(
      column(4, downloadButton(ns(paste0(prefix, "_settings_save")), "Save settings (JSON)")),
      column(8, fileInput(ns(paste0(prefix, "_settings_load")), "Load settings:", accept = ".json", width = "100%"))
    ),
    helpText("Load the data file first, then the settings: column choices can only be restored once the file's columns are known."),
    textOutput(ns(paste0(prefix, "_settings_status")))
  )
}

#' Wire the save / load settings buttons of a tab
#'
#' @param input,output,session The tab's Shiny objects.
#' @param tab Id of the tab (written into the file and checked on load).
#' @param prefix Same prefix passed to [settings_io_ui()].
#' @param exclude Regular expressions of input ids that are not settings.
#' @param send Function `(id, value)` that sets an input in the browser;
#'   the default uses `session$sendInputMessage()`. Replaceable for tests.
#' @param retries How often values that have not arrived are sent again.
#' @param retry_ms Milliseconds between the attempts.
#' @return A list with `pending` (reactive: values that have not arrived
#'   yet) and `unsettled` (reactive: ids of the values given up on).
#' @export
register_settings_io <- function(input, output, session, tab, prefix, exclude = character(0), send = NULL,
                                 retries = 3, retry_ms = 700) {
  if (is.null(send)) send <- function(id, value) session$sendInputMessage(id, list(value = value))
  own <- paste0("^", prefix, "_settings_")
  exclude <- c(exclude, own, "_files?$", "_run$", "_fit$", "_analyze$", "_download", "_convert$")
  pending <- reactiveVal(list())
  attempts <- reactiveVal(0)
  unsettled <- reactiveVal(character(0))
  status <- reactiveVal("")
  # A number box holds 8 where the file says 8.0: compare what is shown.
  arrived <- function(id, value) identical(as.character(input[[id]]), as.character(value))

  output[[paste0(prefix, "_settings_save")]] <- downloadHandler(
    filename = function() paste0(tab, "_settings_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"),
    content = function(file) write_settings_file(collect_settings(reactiveValuesToList(input), exclude), tab, file)
  )

  observeEvent(input[[paste0(prefix, "_settings_load")]], {
    upload <- input[[paste0(prefix, "_settings_load")]]
    loaded <- tryCatch(read_settings_file(upload$datapath[1], tab), error = function(e) conditionMessage(e))
    if (is.character(loaded)) {
      status(paste("Could not load the settings:", loaded))
      return()
    }
    settings <- loaded$settings
    settings <- settings[!vapply(names(settings), function(id) any(vapply(exclude, grepl, logical(1), x = id)), logical(1))]
    for (id in names(settings)) send(id, settings[[id]])
    unsettled(character(0))
    attempts(0)
    pending(settings)
    status(sprintf("Loaded %d settings saved on %s.", length(settings), loaded$saved %||% "an unknown date"))
  })

  # Checks a moment later which values have arrived, and sends the others
  # again; gives up after `retries` attempts.
  observe({
    waiting <- pending()
    if (length(waiting) == 0) return()
    n <- isolate(attempts())
    still <- waiting[!vapply(names(waiting), function(id) isolate(arrived(id, waiting[[id]])), logical(1))]
    if (length(still) == 0 || n >= retries) {
      unsettled(names(still))
      pending(list())
      return()
    }
    if (n > 0) for (id in names(still)) send(id, still[[id]])
    attempts(n + 1)
    invalidateLater(retry_ms, session)
  })

  output[[paste0(prefix, "_settings_status")]] <- renderText({
    msg <- status()
    left <- unsettled()
    if (nzchar(msg) && length(left) > 0) {
      paste0(msg, sprintf(" %d could not be set, because the input is not shown or does not offer that value: %s.",
                          length(left), paste(left, collapse = ", ")))
    } else msg
  })
  list(pending = pending, unsettled = unsettled)
}
