
# ---- Shiny UI Components Module ----
# create_main_ui() is the page shell: global styling/JS, header, the main
# tabset, and the always-visible Directory Settings / Cache Management
# sections. Each tab's own UI tree is built by a dedicated function in a
# sibling module, split out for size:
#   ui_ternary_plots_tab.R       - create_ternary_plots_tab()
#   ui_data_comparison_tab.R     - create_data_comparison_tab()
#   ui_multiple_ternary_tab.R    - create_multiple_ternary_tab()
#   ui_hex_ternary_tab.R         - create_hex_ternary_tab()
#   ui_plot_builder_tab.R        - create_plot_builder_tab()
#   ui_evs_tab.R                 - create_evs_tab()
#   ui_spatial_tab.R              - create_spatial_tab()
#   ui_coda_tab.R                 - create_coda_tab()
#   ui_analysis_log_tab.R        - create_analysis_log_tab()

# Short inline citation for a method/formula used in one of the "How it
# works" info boxes - a link to the DOI when one exists, or plain
# (non-linked) text for sources without one (books, book chapters, and
# standards bodies that don't expose a resolvable DOI). Citations were
# verified against CrossRef/publisher records before being added here -
# see the citation review discussed with the user.
cite_link <- function(label, doi_url = NULL) {
  if (is.null(doi_url)) {
    tags$span(style = "color: #0c5460; font-style: italic;", label)
  } else {
    tags$a(href = doi_url, label, target = "_blank",
           style = "color: #0c5460; font-style: italic; text-decoration: underline;")
  }
}

# Main UI function
#' Build the full app UI: page shell, header, tabset, and shared settings
#'
#' Assembles the page's global styling/JS, header, all 9 tabs (via each
#' tab's own `create_*_tab()`), and the always-visible Directory Settings /
#' Cache Management sections.
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
        div(
          style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 20px;",
          h2("Custom Ternary Builder v6 - Individual Element Filtering"),
          actionButton("help_button", "?",
            style = "background-color: #007bff; color: white; border: none; border-radius: 50%; height: 30px; font-weight: bold; font-size: 16px;",
            title = "Help")
        )
      )
    ),

    hr(),

    # Main Tabset Panel
    tabsetPanel(
      create_ternary_plots_tab(),
      create_data_comparison_tab("data_comparison"),
      create_multiple_ternary_tab(),
      create_hex_ternary_tab("hex_ternary"),
      create_plot_builder_tab("plot_builder"),
      create_evs_tab("evs"),
      create_spatial_tab("spatial"),
      create_coda_tab("coda"),
      create_analysis_log_tab("analysis_log")
    ),  # Close main tabsetPanel

    # Directory Settings Section
    fluidRow(
      column(12,
        h3("Directory Settings"),
        fluidRow(
          column(6,
            h4("Working Directory"),
            shinyDirButton("working_dir", "Choose Working Directory", "Select Working Directory"),
            verbatimTextOutput("working_dir_text")
          ),
          column(6,
            h4("Output Directory"),
            shinyDirButton("output_dir", "Choose Output Directory", "Select Output Directory"),
            verbatimTextOutput("output_dir_text")
          )
        )
      )
    ),

    # Cache Management Section
    fluidRow(
      column(12,
        h3("Cache Management"),
        fluidRow(
          column(4,
            actionButton("clear_cache", "Clear All Cache", class = "btn-warning"),
            helpText("Remove all cached data")
          ),
          column(4,
            actionButton("clear_expired_cache", "Clear Expired Cache", class = "btn-info"),
            helpText("Remove only expired cache entries")
          ),
          column(4,
            verbatimTextOutput("cache_stats"),
            helpText("Current cache status"),
            actionButton("refresh_cache_stats", "Refresh Stats", class = "btn-sm btn-info")
          )
        ),
        fluidRow(
          column(12,
            checkboxInput("debug_mode", "Enable Debug Mode", value = FALSE),
            helpText("Enable detailed debug output in console. Controls verbose logging for multivariate analysis, caching, and performance monitoring.")
          )
        )
      )
    ),



    tags$hr(),
    tags$footer(
      HTML("© 2025 Vid Kuder Marušič — <a href='mailto:vid.kudermarusic@gmail.com'>vid.kudermarusic@gmail.com</a>"),
      align = "center",
      style = "color: #888; background-color: #f9f9f9; padding: 10px 0; font-size: 0.95em;"
    )
  )
}
