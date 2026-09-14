
# ---- Shiny UI Components Module ----
# create_main_ui() is the page shell: global styling/JS, header, and the
# main tabset. There is no shared save-location picker - every tab's own
# Save/Export button prompts for where to save right when it's clicked,
# via the browser's native Save dialog (a plain downloadButton/
# downloadHandler), the same pattern used across every tab. Each tab's
# own UI tree is built by a dedicated function in a
# sibling module, split out for size:
#   ui_ternary_plots_tab.R       - create_ternary_plots_tab()
#   ui_data_comparison_tab.R     - create_data_comparison_tab()
#   ui_multiple_ternary_tab.R    - create_multiple_ternary_tab()
#   ui_hex_ternary_tab.R         - create_hex_ternary_tab()
#   ui_plot_builder_tab.R        - create_plot_builder_tab()
#   ui_evs_tab.R                 - create_evs_tab()
#   ui_spatial_tab.R              - create_spatial_tab()
#   ui_spatial_ppp_tab.R          - create_spatial_ppp_tab()
#   ui_coda_tab.R                 - create_coda_tab()
#   ui_analysis_log_tab.R        - create_analysis_log_tab()

# Short inline citation for a method/formula used in one of the "How it
# works" info boxes - a link to the DOI when one exists, or plain
# (non-linked) text for sources without one (books, book chapters, and
# standards bodies that don't expose a resolvable DOI).
cite_link <- function(label, doi_url = NULL) {
  if (is.null(doi_url)) {
    tags$span(style = "color: #0c5460; font-style: italic;", label)
  } else {
    tags$a(href = doi_url, label, target = "_blank",
           style = "color: #0c5460; font-style: italic; text-decoration: underline;")
  }
}

# Main UI function
#' Build the full app UI: page shell, header, and tabset
#'
#' Assembles the page's global styling/JS, header, and all 10 tabs (via
#' each tab's own `create_*_tab()`). No shared save-location picker - see
#' this file's own header comment for why.
#'
#' @return A `shiny::fluidPage()`.
#' @export
create_main_ui <- function() {
  fluidPage(
    titlePanel("Ternary Plot Generator with Advanced Filtering"),

    # Add error handling and user feedback
    tags$head(
      tags$style(HTML("
        .error-message {
          color: #d32f2f;
          background-color: #ffebee;
          padding: 10px;
          border-radius: 4px;
          margin: 10px 0;
          border-left: 4px solid #d32f2f;
        }
        .success-message {
          color: #388e3c;
          background-color: #e8f5e8;
          padding: 10px;
          border-radius: 4px;
          margin: 10px 0;
          border-left: 4px solid #388e3c;
        }
        .warning-message {
          color: #f57c00;
          background-color: #fff3e0;
          padding: 10px;
          border-radius: 4px;
          margin: 10px 0;
          border-left: 4px solid #f57c00;
        }
        .info-box {
          background-color: #e3f2fd;
          border: 1px solid #2196f3;
          border-radius: 4px;
          padding: 15px;
          margin: 10px 0;
        }
      ")),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('showMessage', function(data) {
          var messageDiv = document.createElement('div');
          messageDiv.className = data.type + '-message';
          messageDiv.textContent = data.message;

          // Insert at the top of the page
          document.body.insertBefore(messageDiv, document.body.firstChild);

          // Remove after 5 seconds
          setTimeout(function() {
            if (messageDiv.parentNode) {
              messageDiv.parentNode.removeChild(messageDiv);
            }
          }, 5000);
        });
      "))
    ),

    fluidRow(
      column(12,
        h2("Custom Ternary Builder v6 - Individual Element Filtering", style = "margin-bottom: 20px;")
      )
    ),

    hr(),

    # Main Tabset Panel
    tabsetPanel(
      create_ternary_plots_tab("ternary_plots"),
      create_data_comparison_tab("data_comparison"),
      create_multiple_ternary_tab("multiple_ternary"),
      create_hex_ternary_tab("hex_ternary"),
      create_plot_builder_tab("plot_builder"),
      create_evs_tab("evs"),
      create_spatial_tab("spatial"),
      create_spatial_ppp_tab("spatial_ppp"),
      create_coda_tab("coda"),
      create_analysis_log_tab("analysis_log")
    ),  # Close main tabsetPanel

    tags$hr(),
    tags$footer(
      HTML("Vid Kuder Marusic - <a href='mailto:vid.kudermarusic@gmail.com'>vid.kudermarusic@gmail.com</a>"),
      align = "center",
      style = "color: #888; background-color: #f9f9f9; padding: 10px 0; font-size: 0.95em;"
    )
  )
}

#' Build a styled "how it works" / purpose info callout
#'
#' Shared markup for the teal info box repeated across several tabs (e.g.
#' `create_coda_tab()`, `create_evs_tab()`) to explain a method or note
#' something important.
#'
#' @param title Heading text shown at the top of the box.
#' @param ... Further UI content (e.g. `tags$ul()`, `tags$p()`, nested
#'   `div()`s) shown below the heading.
#' @return A `shiny::div()`.
#' @export
info_box <- function(title, ...) {
  div(style = "border: 1px solid #17a2b8; padding: 15px; border-radius: 5px; margin: 10px 0; background-color: #d1ecf1;",
    h5(title, style = "margin-top: 0; color: #0c5460;"),
    ...
  )
}

#' Build the "File Selection" column content for a tab
#'
#' Returns a `tagList()`, not wrapped in `column()` - callers wrap it in
#' their own `column(6, ...)` alongside a different second column. The
#' caller is responsible for namespacing `file_input_id` (i.e. it is
#' already called as `create_file_selection_column(ns("coda_files"))`,
#' so this function does not apply its own namespace).
#'
#' @param file_input_id Already-namespaced input id for the `fileInput()`.
#' @param label Label shown above the file picker.
#' @return A `shiny::tagList()`.
#' @export
create_file_selection_column <- function(file_input_id, label = "Select Excel File(s)") {
  tagList(
    h4("File Selection"),
    fileInput(file_input_id, label, multiple = TRUE, accept = c(".xlsx", ".xls")),
    helpText("Each file's Sheet 1 is read and combined.")
  )
}

#' Build a centered primary action button row
#'
#' The caller is responsible for namespacing `button_id`.
#'
#' @param button_id Already-namespaced input id for the `actionButton()`.
#' @param label Button label.
#' @param margin_top CSS `margin-top` for the row (default `"10px"`).
#' @return A `shiny::fluidRow()`.
#' @export
centered_action_button_row <- function(button_id, label, margin_top = "10px") {
  fluidRow(
    column(12, style = paste0("text-align: center; margin-top: ", margin_top, ";"),
      actionButton(button_id, label, class = "btn-primary btn-lg", style = "font-size: 18px;")
    )
  )
}

#' Build X/Y coordinate column selectors
#'
#' Returns only the X/Y pair - tabs that need a third selector (e.g. a
#' grouping or size column) append it separately, since its id/label
#' differ per tab. The caller is responsible for namespacing `x_id` and
#' `y_id`.
#'
#' @param x_id Already-namespaced input id for the X `selectInput()`.
#' @param y_id Already-namespaced input id for the Y `selectInput()`.
#' @param x_label Label for the X selector.
#' @param y_label Label for the Y selector.
#' @return A `shiny::tagList()`.
#' @export
create_xy_coordinate_selectors <- function(x_id, y_id, x_label = "X coordinate column:", y_label = "Y coordinate column:") {
  tagList(
    selectInput(x_id, x_label, choices = NULL),
    selectInput(y_id, y_label, choices = NULL)
  )
}
