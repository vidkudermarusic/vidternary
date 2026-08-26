# ---- UI: "Plot Builder" tab ----
# A single generic chart builder (pick plot type, X/Y, color/group, log
# scales) driven entirely by the loaded data's own column names - this app
# has no fixed schema, unlike the "Multiple Plot Types" tab's 6 hardcoded
# sub-tabs. Includes user-saved presets (a preset is just a snapshot of the
# builder_* inputs) since no fixed column names can be assumed for
# hardcoded presets. See R/server_plot_builder.R for the wiring and
# R/plotting_utils_builder.R for the rendering logic.

#' Build the "Plot Builder" tab's UI
#'
#' @return A `shiny::tabPanel()`.
#' @export
create_plot_builder_tab <- function() {
  tabPanel("Plot Builder",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h4("Data"),
        fileInput("builder_files", "Select Excel File(s)", multiple = TRUE, accept = c(".xlsx", ".xls")),
        helpText("Each file's Sheet 1 is read and combined. When multiple files are selected, rows are tagged with a 'source_file' column."),
        uiOutput("builder_dataset_selector_ui"),
        tags$hr(),

        h4("Chart"),
        selectInput("builder_type", "Plot Type", choices = c(
          "Violin" = "violin", "Box plot" = "box", "Bar (counts)" = "bar",
          "Histogram" = "hist", "Scatter" = "scatter", "Rose diagram" = "rose"
        )),
        uiOutput("builder_axis_selectors"),
        selectInput("builder_color_by", "Color / group by", choices = c("None" = "none")),
        checkboxInput("builder_log_x", "Log-scale X axis", value = FALSE),
        checkboxInput("builder_log_y", "Log-scale Y axis", value = FALSE),
        downloadButton("builder_download", "Download plot (PNG)"),
        tags$hr(),

        h4("Presets"),
        helpText("Save the current chart configuration under a name and reload it later."),
        textInput("builder_preset_name", "Preset name"),
        actionButton("builder_save_preset", "Save current as preset", class = "btn-sm"),
        br(), br(),
        selectInput("builder_preset_select", "Saved presets", choices = NULL),
        actionButton("builder_load_preset", "Load preset", class = "btn-sm btn-primary"),
        actionButton("builder_delete_preset", "Delete preset", class = "btn-sm btn-danger")
      ),
      mainPanel(
        width = 8,
        plotOutput("builder_plot", height = "600px")
      )
    )
  )
}
