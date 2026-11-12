# ---- UI: "Data Import" tab ----
# Turns the particle export of any automated SEM system into the app's
# standard table: reads Excel or delimited text, maps the columns to their
# roles, converts units and atomic percent. See R/import_*.R for the
# functions and R/server_import.R for the Shiny wiring.

# One row of the mapping table: the column of a role, and its unit.
import_role_row <- function(ns, role, label, kind) {
  with_unit <- kind %in% c("length", "area", "position")
  fluidRow(
    column(7, selectInput(ns(paste0("import_role_", role)), paste0(label, ":"), choices = c("None" = "none"))),
    column(5, if (with_unit) selectInput(ns(paste0("import_unit_", role)), "Unit:",
      choices = c("As named in the column" = "auto", "nm" = "nm", "um" = "um", "mm" = "mm"), selected = "auto"))
  )
}

#' Build the "Data Import" tab's UI
#'
#' @param id Module namespace id - must match the id passed to
#'   `moduleServer()` for this tab in `server_logic.R`.
#' @return A `shiny::tabPanel()`.
#' @export
create_import_tab <- function(id) {
  ns <- NS(id)
  roles <- particle_roles()
  half <- ceiling(nrow(roles) / 2)
  role_rows <- lapply(seq_len(nrow(roles)), function(i) import_role_row(ns, roles$role[i], roles$label[i], roles$kind[i]))
  tabPanel("Data Import",
    fluidRow(
      column(12,
        h3("Data Import: Any Particle Export into the Standard Table"),
        helpText("Reads the particle list of an automated SEM system, whatever its column names, units or file format, and writes it in the form the other tabs recognise by themselves."),

        info_box(" How it works",
          tags$ul(
            tags$li(strong("Files: "), "Excel (.xlsx) and delimited text (.csv, .txt, .tsv). The delimiter and a decimal comma are detected."),
            tags$li(strong("Roles: "), "each column the analyses need (area, length, position ...) is matched to a column of the file from its name. Check the proposal and correct it - only the Oxford AZtec Feature export has been checked against real files; for other systems the proposal is an educated guess."),
            tags$li(strong("Units: "), "lengths become um, the area um2, positions mm. The unit is read from the column name; set it by hand when the name does not say."),
            tags$li(strong("Chemistry: "), "element columns are renamed to 'El.(Wt%)'. Atomic percent is converted to weight percent."),
            tags$li(strong("Reliability: "), "optionally two columns say whether each particle is large enough for its EDS analysis to describe it, from the electron range of the beam in the matrix."),
            tags$li(strong("Profile: "), "save the mapping as a profile and load it for the next file of the same instrument. Several files can be converted in one go with one mapping.")
          )
        ),

        fluidRow(
          column(6,
            h4("Files"),
            fileInput(ns("import_files"), "Select file(s) to import", multiple = TRUE, accept = c(".xlsx", ".csv", ".txt", ".tsv", ".tab", ".dat")),
            verbatimTextOutput(ns("import_file_info"))
          ),
          column(6,
            h4("Profile"),
            textInput(ns("import_profile_name"), "Profile name:", value = "", placeholder = "e.g. instrument and software"),
            fileInput(ns("import_profile_load"), "Load a saved profile:", accept = ".json"),
            downloadButton(ns("import_profile_save"), "Save this mapping as a profile (JSON)"),
            textOutput(ns("import_profile_status"))
          )
        ),

        h4("First rows of the file as read"),
        div(style = "overflow-x: auto;", tableOutput(ns("import_raw_preview"))),

        h4("Column roles"),
        fluidRow(column(6, role_rows[seq_len(half)]), column(6, role_rows[-seq_len(half)])),

        fluidRow(
          column(8,
            h4("Chemistry"),
            selectizeInput(ns("import_elements"), "Element columns:", choices = NULL, multiple = TRUE,
                           options = list(placeholder = "Columns that hold element contents"))
          ),
          column(4,
            radioButtons(ns("import_basis"), "The element columns are in:", choices = c("Weight percent" = "wt", "Atomic percent" = "at"), selected = "wt"),
            checkboxInput(ns("import_keep_other"), "Keep the columns that are not mapped", value = TRUE),
            checkboxInput(ns("import_add_reliability"), "Add size-reliability columns", value = TRUE),
            conditionalPanel("input.import_add_reliability", ns = ns,
              fluidRow(
                column(6, numericInput(ns("import_beam_kv"), "Voltage (kV):", value = 15, min = 1, max = 40, step = 1)),
                column(6, selectInput(ns("import_range_matrix"), "Matrix:", choices = c("Fe", "Ni", "Co", "Cu", "Ti", "Al"), selected = "Fe"))
              ),
              helpText("Adds 'Size.to.range' (particle diameter divided by the electron range of the beam; filter with > 1) and 'Composition.reliability'."))
          )
        ),

        centered_action_button_row(ns("import_convert"), "Convert", margin_top = "10px"),

        fluidRow(
          column(12,
            verbatimTextOutput(ns("import_status")),
            uiOutput(ns("import_notes")),
            downloadButton(ns("import_download"), "Download the standard table(s)"),
            h4("First rows of the standard table"),
            div(style = "overflow-x: auto;", tableOutput(ns("import_preview")))
          )
        )
      )
    )
  )
}
