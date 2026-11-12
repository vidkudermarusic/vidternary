# ---- Server: "Data Import" tab ----
# Wires R/import_*.R into the app: read the uploaded files, propose a
# column mapping, let the user correct it or load a profile, convert, and
# offer the standard tables for download.
#
# NOTE: shiny::validate()/need() must stay fully qualified here - see
# server_spatial.R header comment for why.

# The profile as currently set in the tab's inputs.
import_profile_from_inputs <- function(input, data) {
  roles <- particle_roles()
  pick <- function(id) {
    v <- input[[id]]
    if (is.null(v) || !nzchar(v) || identical(v, "none")) NULL else v
  }
  mapping <- lapply(stats::setNames(roles$role, roles$role), function(r) pick(paste0("import_role_", r)))
  mapping <- mapping[!vapply(mapping, is.null, logical(1))]
  units <- lapply(stats::setNames(names(mapping), names(mapping)), function(r) {
    u <- input[[paste0("import_unit_", r)]]
    if (is.null(u) || identical(u, "auto")) NULL else u
  })
  cols <- input$import_elements
  detected <- detect_element_columns(data)
  symbol <- function(cn) {
    hit <- detected$element[match(cn, detected$column)]
    if (!is.na(hit)) return(hit)
    regmatches(cn, regexpr("[A-Z][a-z]?", cn))
  }
  elements <- data.frame(element = if (length(cols)) vapply(cols, symbol, character(1)) else character(0),
                         column = cols %||% character(0), basis = rep(input$import_basis %||% "wt", length(cols)),
                         stringsAsFactors = FALSE, row.names = NULL)
  name <- input$import_profile_name
  list(name = if (is.null(name) || !nzchar(trimws(name))) "Unnamed profile" else trimws(name),
       mapping = mapping, units = units[!vapply(units, is.null, logical(1))], elements = elements,
       basis = input$import_basis %||% "wt")
}

#' Wire up the Data Import tab's server logic
#'
#' @param input The Shiny `input` object.
#' @param output The Shiny `output` object.
#' @param session The Shiny session object.
#' @param rv The app's shared `reactiveValues` object.
#' @param show_message Function to show a user-facing status message.
#' @param log_operation Function to record a structured log entry.
#' @return A list with `module_name`.
#' @export
create_server_import <- function(input, output, session, rv, show_message, log_operation) {

  # Every uploaded file as read, or the reason it could not be read.
  raw_tables <- reactive({
    req(input$import_files)
    files <- input$import_files
    lapply(seq_len(nrow(files)), function(i) {
      tryCatch(read_particle_table(files$datapath[i], name = files$name[i]), error = function(e) conditionMessage(e))
    })
  })
  first_table <- reactive({
    tables <- raw_tables()
    ok <- Filter(is.data.frame, tables)
    shiny::validate(shiny::need(length(ok) > 0, paste("No file could be read:", paste(unlist(Filter(is.character, tables)), collapse = " "))))
    ok[[1]]
  })

  # Sets the mapping inputs from a profile.
  show_profile <- function(profile, data) {
    roles <- particle_roles()
    cols <- names(data)
    for (r in roles$role) {
      col <- profile$mapping[[r]]
      updateSelectInput(session, paste0("import_role_", r), choices = c("None" = "none", cols),
                        selected = if (is.null(col) || is.na(col) || !col %in% cols) "none" else col)
      if (roles$kind[roles$role == r] %in% c("length", "area", "position")) {
        unit <- profile$units[[r]]
        updateSelectInput(session, paste0("import_unit_", r), selected = if (is.null(unit) || is.na(unit)) "auto" else unit)
      }
    }
    numeric_cols <- cols[vapply(data, is.numeric, logical(1))]
    updateSelectizeInput(session, "import_elements", choices = numeric_cols,
                         selected = intersect(profile$elements$column, numeric_cols))
    updateRadioButtons(session, "import_basis", selected = profile$basis %||% "wt")
  }

  profile_status <- reactiveVal("")
  observeEvent(first_table(), {
    show_profile(detect_import_profile(first_table()), first_table())
    profile_status("Mapping proposed from the column names - check it before converting.")
  })

  observeEvent(input$import_profile_load, {
    data <- tryCatch(first_table(), error = function(e) NULL)
    if (is.null(data)) {
      profile_status("Upload a data file first, then load the profile.")
      return()
    }
    profile <- tryCatch(read_import_profile(input$import_profile_load$datapath[1]), error = function(e) conditionMessage(e))
    if (is.character(profile)) {
      profile_status(paste("Could not load the profile:", profile))
      return()
    }
    needed <- c(unlist(profile$mapping), profile$elements$column)
    missing_cols <- setdiff(needed[!is.na(needed)], names(data))
    show_profile(profile, data)
    updateTextInput(session, "import_profile_name", value = profile$name)
    profile_status(if (length(missing_cols)) {
      sprintf("Profile '%s' loaded, but the file lacks: %s. Those roles were set to None.", profile$name, paste(missing_cols, collapse = ", "))
    } else sprintf("Profile '%s' loaded.", profile$name))
  })
  output$import_profile_status <- renderText(profile_status())

  output$import_profile_save <- downloadHandler(
    filename = function() paste0("import_profile_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"),
    content = function(file) {
      data <- safe_reactive_result(function() first_table(), "Upload a data file before saving a profile.")
      write_import_profile(import_profile_from_inputs(input, data), file)
    }
  )

  output$import_file_info <- renderText({
    req(input$import_files)
    tables <- raw_tables()
    paste(vapply(seq_along(tables), function(i) {
      t <- tables[[i]]
      name <- input$import_files$name[i]
      if (is.character(t)) return(sprintf("%s: NOT READ - %s", name, t))
      info <- attr(t, "import_info")
      detail <- if (identical(info$format, "Excel")) "Excel, sheet 1" else {
        sprintf("text, delimiter %s, decimal %s", switch(info$sep, "\t" = "tab", ";" = "semicolon", "," = "comma", info$sep),
                if (info$dec == ",") "comma" else "point")
      }
      sprintf("%s: %d rows, %d columns (%s)", name, nrow(t), ncol(t), detail)
    }, character(1)), collapse = "\n")
  })
  output$import_raw_preview <- renderTable(utils::head(first_table(), 6), digits = 4)

  converted <- eventReactive(input$import_convert, {
    tables <- raw_tables()
    files <- input$import_files
    profile <- import_profile_from_inputs(input, first_table())
    shiny::validate(shiny::need(length(profile$mapping) + nrow(profile$elements) > 0,
                                 "Assign at least one column to a role before converting."))
    keep <- !identical(input$import_keep_other, FALSE)
    out <- lapply(seq_along(tables), function(i) {
      if (is.character(tables[[i]])) return(tables[[i]])
      tryCatch(apply_import_profile(tables[[i]], profile, keep_other = keep), error = function(e) conditionMessage(e))
    })
    names(out) <- files$name
    ok <- vapply(out, is.data.frame, logical(1))
    shiny::validate(shiny::need(any(ok), paste("No file could be converted:", paste(unlist(out[!ok]), collapse = " "))))
    log_operation("INFO", "Data import", sprintf("%d of %d file(s) converted with profile '%s'", sum(ok), length(out), profile$name))
    list(tables = out, ok = ok, profile = profile)
  })

  placeholder <- "Upload a file, check the column roles, and click \"Convert\"."
  output$import_status <- renderText({
    render_gated_status(input, "import_convert", placeholder, function() {
      res <- converted()
      paste(vapply(names(res$tables), function(name) {
        t <- res$tables[[name]]
        if (is.character(t)) sprintf("%s: NOT CONVERTED - %s", name, t)
        else sprintf("%s: %d particles, %d columns", name, nrow(t), ncol(t))
      }, character(1)), collapse = "\n")
    })
  })
  output$import_notes <- renderUI({
    res <- tryCatch(converted(), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    notes <- attr(res$tables[[which(res$ok)[1]]], "standardize_notes")
    if (length(notes) == 0) return(NULL)
    tags$ul(style = "font-size: 12px; margin: 6px 0;", lapply(notes, tags$li))
  })
  output$import_preview <- renderTable({
    res <- converted()
    utils::head(res$tables[[which(res$ok)[1]]], 6)
  }, digits = 4)

  # One file: an xlsx. Several: a zip of xlsx files.
  output$import_download <- downloadHandler(
    filename = function() {
      res <- tryCatch(converted(), error = function(e) NULL)
      stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      if (!is.null(res) && sum(res$ok) > 1) paste0("standard_tables_", stamp, ".zip")
      else paste0("standard_table_", stamp, ".xlsx")
    },
    content = function(file) {
      res <- safe_reactive_result(function() converted(), "Upload a file and click \"Convert\" before downloading.")
      good <- res$tables[res$ok]
      if (length(good) == 1) {
        writexl::write_xlsx(good[[1]], file)
      } else {
        dir <- tempfile("standard_tables_")
        dir.create(dir)
        on.exit(unlink(dir, recursive = TRUE), add = TRUE)
        paths <- vapply(names(good), function(name) {
          path <- file.path(dir, paste0(tools::file_path_sans_ext(basename(name)), "_standard.xlsx"))
          writexl::write_xlsx(good[[name]], path)
          path
        }, character(1))
        zip::zipr(file, paths)
      }
    }
  )

  return(list(
    module_name = "server_import"
  ))
}
