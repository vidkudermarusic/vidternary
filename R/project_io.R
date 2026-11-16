# ---- Reproducibility: one project file for all tabs ----
# "Save project" writes the settings of every tab into one JSON file -
# the same values the per-tab "Save settings" buttons write, keyed by tab -
# together with the names of the data files each tab had loaded. "Load
# project" sets them again.
#
# A browser does not let a page choose files for the user, so the data
# files themselves cannot be restored: after loading a project, the status
# line lists which file each tab is waiting for. Column choices can only be
# set once a tab's file is there, so whenever a file is uploaded the
# settings still waiting are sent again - load the project first and then
# the files, or the other way round.
#
# Works on the app's top-level input, where the inputs of a tab carry the
# tab's id as a prefix ("evs-evs_return_period").

# Inputs that are not settings: table and plot interaction state.
.project_exclude <- c("_rows_(current|all|selected)$", "_row_last_clicked$", "_cells?_(clicked|selected)$",
                      "_columns_selected$", "_search(_columns)?$", "_state$", "_(click|dblclick|hover|brush)$")

# Tab of a top-level input id ("evs-evs_col" -> "evs"); "" for the app shell.
.input_tab <- function(id) ifelse(grepl("-", id, fixed = TRUE), sub("-.*$", "", id), "")

#' Names of the files every upload box holds
#'
#' @param values Named list of input values, e.g.
#'   `shiny::reactiveValuesToList(input)`.
#' @param exclude Regular expression of upload boxes to leave out (the
#'   settings and project loaders themselves).
#' @return Named list: input id -> character vector of file names.
#' @export
uploaded_file_names <- function(values, exclude = "_settings_load$|_profile_load$") {
  is_upload <- vapply(values, function(v) is.data.frame(v) && all(c("name", "datapath") %in% names(v)), logical(1))
  out <- lapply(values[is_upload], function(v) as.character(v$name))
  out <- out[!grepl(exclude, names(out))]
  out[order(names(out))]
}

#' Status line after loading a project
#'
#' @param loaded The project as read by [read_settings_file()].
#' @param unsettled Ids of the settings that could not be set, or `NULL`
#'   while values are still being sent.
#' @param files_now Files loaded at the moment ([uploaded_file_names()]).
#' @return One string.
#' @export
describe_project_load <- function(loaded, unsettled = NULL, files_now = list()) {
  n <- length(loaded$settings)
  head <- sprintf("Project saved on %s: %d settings in %d tabs.", loaded$saved %||% "an unknown date", n,
                  length(setdiff(unique(.input_tab(names(loaded$settings))), "")))
  if (is.null(unsettled)) return(paste(head, "Setting the inputs ..."))
  if (length(unsettled) == 0) return(paste(head, "All set."))

  wanted <- loaded$extra$files
  if (is.null(wanted)) wanted <- list()
  per_tab <- table(.input_tab(unsettled))
  parts <- vapply(names(per_tab), function(tab) {
    boxes <- names(wanted)[.input_tab(names(wanted)) == tab]
    missing <- boxes[!vapply(boxes, function(b) setequal(unlist(wanted[[b]]), files_now[[b]] %||% character(0)), logical(1))]
    files <- unique(unlist(wanted[missing]))
    sprintf("%s (%d%s)", if (nzchar(tab)) tab else "page", per_tab[[tab]],
            if (length(files)) paste0("; load ", paste(files, collapse = ", ")) else "")
  }, character(1))
  sprintf("%s %d set. Waiting in: %s. Settings that depend on a data file are filled in as soon as that file is loaded in its tab.",
          head, n - length(unsettled), paste(parts, collapse = "; "))
}

#' Save / load buttons for the whole project
#'
#' @return A `shiny::div()` for the top of the page.
#' @export
project_io_ui <- function() {
  div(style = "border: 1px solid #6c757d; padding: 8px 15px; border-radius: 5px; margin: 0 0 12px 0; background-color: #f8f9fa;",
    fluidRow(
      column(2, strong("Project"), br(), downloadButton("project_settings_save", "Save project (JSON)")),
      column(4, fileInput("project_settings_load", "Load project:", accept = ".json", width = "100%")),
      column(6, helpText("One file with the settings of every tab and the names of the data files used. The data files themselves are not stored: load them in their tabs, before or after the project."),
             textOutput("project_settings_status"))
    )
  )
}

#' Wire the project save / load buttons
#'
#' Call from the app's top-level server function (not inside a module).
#'
#' @param input,output,session The app's top-level Shiny objects.
#' @param send,retries,retry_ms Passed to [register_settings_io()].
#' @return What [register_settings_io()] returns.
#' @export
register_project_io <- function(input, output, session, send = NULL, retries = 3, retry_ms = 700) {
  files_now <- reactive(uploaded_file_names(reactiveValuesToList(input), "_settings_load$|_profile_load$"))
  io <- register_settings_io(
    input, output, session, tab = "project", prefix = "project", exclude = .project_exclude, send = send,
    retries = retries, retry_ms = retry_ms, kind = "project",
    extra = function() list(files = isolate(files_now())),
    describe = function(loaded, unsettled) describe_project_load(loaded, unsettled, isolate(files_now())))

  # A newly uploaded data file: send the settings that were still waiting.
  seen <- NULL
  observe({
    now <- files_now()
    if (identical(now, seen)) return()
    seen <<- now
    loaded <- isolate(io$loaded())
    left <- isolate(io$unsettled())
    if (is.null(loaded) || length(left) == 0) return()
    io$apply(loaded$settings[intersect(left, names(loaded$settings))])
  })
  io
}
