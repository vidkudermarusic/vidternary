
# ---- Shiny UI Components Module ----
# create_main_ui() is the page shell: global styling/JS, header, the main
# tabset, and the always-visible Directory Settings / Cache Management
# sections. Each tab's own UI tree is built by a dedicated function in a
# sibling module, split out for size:
#   ui_ternary_plots_tab.R       - create_ternary_plots_tab()
#   ui_data_comparison_tab.R     - create_data_comparison_tab()
#   ui_multiple_plot_types_tab.R - create_multiple_plot_types_tab()
#   ui_multiple_ternary_tab.R    - create_multiple_ternary_tab()
#   ui_data_export_tab.R         - create_data_export_tab()
#   ui_analysis_log_tab.R        - create_analysis_log_tab()

# Main UI function
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
      create_data_comparison_tab(),
      create_multiple_plot_types_tab(),
      create_multiple_ternary_tab(),
      create_data_export_tab(),
      create_analysis_log_tab()
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
