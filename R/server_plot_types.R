# ---- Server Plot Types Module ----
# This module contains the shared column-choice observers for the Multiple
# Plot Types tab. Each chart type's own create/save/render/color/filename
# handlers live in a sibling module, split out for size:
#   server_plot_types_scatter.R
#   server_plot_types_histogram.R
#   server_plot_types_boxplot.R
#   server_plot_types_violin.R
#   server_plot_types_connected.R
#   server_plot_types_stacked.R

create_server_plot_types <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {

  # ---- Column Choice Updates ----

  # Update column choices when datasets are loaded
  observe({
    if (!is.null(rv$df1)) {
      choices1 <- names(rv$df1)

      # Update column selectors for dataset1
      updateSelectizeInput(session, "scatter_x", choices = choices1, selected = NULL)
      updateSelectizeInput(session, "scatter_y", choices = choices1, selected = NULL)
      updateSelectizeInput(session, "scatter_color", choices = choices1, selected = NULL)
      updateSelectizeInput(session, "scatter_size", choices = choices1, selected = NULL)
      updateSelectizeInput(session, "hist_column", choices = choices1, selected = NULL)
      updateSelectizeInput(session, "corr_columns", choices = choices1, selected = NULL)
      updateSelectizeInput(session, "box_y", choices = choices1, selected = NULL)
      updateSelectizeInput(session, "box_x", choices = choices1, selected = NULL)
      updateSelectizeInput(session, "density_column", choices = choices1, selected = NULL)
      updateSelectizeInput(session, "density_group", choices = choices1, selected = NULL)
    }

    if (!is.null(rv$df2)) {
      choices2 <- names(rv$df2)

      # Update column selectors for dataset2
      updateSelectizeInput(session, "scatter_x", choices = choices2, selected = NULL)
      updateSelectizeInput(session, "scatter_y", choices = choices2, selected = NULL)
      updateSelectizeInput(session, "scatter_color", choices = choices2, selected = NULL)
      updateSelectizeInput(session, "scatter_size", choices = choices2, selected = NULL)
      updateSelectizeInput(session, "hist_column", choices = choices2, selected = NULL)
      updateSelectizeInput(session, "corr_columns", choices = choices2, selected = NULL)
      updateSelectizeInput(session, "box_y", choices = choices2, selected = NULL)
      updateSelectizeInput(session, "box_x", choices = choices2, selected = NULL)
      updateSelectizeInput(session, "density_column", choices = choices2, selected = NULL)
      updateSelectizeInput(session, "density_group", choices = choices2, selected = NULL)
    }
  })

  # Update column choices for individual plot types based on dataset selection
  observe({
    # Scatter plot column updates
    if (!is.null(input$scatter_dataset)) {
      if (input$scatter_dataset == "dataset1" && !is.null(rv$df1)) {
        choices <- names(rv$df1)
        updateSelectizeInput(session, "scatter_columns", choices = choices, selected = NULL)
        updateSelectizeInput(session, "scatter_x_col", choices = choices, selected = NULL)
        updateSelectizeInput(session, "scatter_y_col", choices = choices, selected = NULL)
      } else if (input$scatter_dataset == "dataset2" && !is.null(rv$df2)) {
        choices <- names(rv$df2)
        updateSelectizeInput(session, "scatter_columns", choices = choices, selected = NULL)
        updateSelectizeInput(session, "scatter_x_col", choices = choices, selected = NULL)
        updateSelectizeInput(session, "scatter_y_col", choices = choices, selected = NULL)
      } else if (input$scatter_dataset == "both" && !is.null(rv$df1) && !is.null(rv$df2)) {
        # Use common columns for both datasets
        choices <- intersect(names(rv$df1), names(rv$df2))
        updateSelectizeInput(session, "scatter_columns", choices = choices, selected = NULL)
        updateSelectizeInput(session, "scatter_x_col", choices = choices, selected = NULL)
        updateSelectizeInput(session, "scatter_y_col", choices = choices, selected = NULL)
      }
    }

    # Histogram column updates
    if (!is.null(input$histogram_dataset)) {
      if (input$histogram_dataset == "dataset1" && !is.null(rv$df1)) {
        choices <- names(rv$df1)
        updateSelectizeInput(session, "histogram_columns", choices = choices, selected = NULL)
      } else if (input$histogram_dataset == "dataset2" && !is.null(rv$df2)) {
        choices <- names(rv$df2)
        updateSelectizeInput(session, "histogram_columns", choices = choices, selected = NULL)
      } else if (input$histogram_dataset == "both" && !is.null(rv$df1) && !is.null(rv$df2)) {
        choices <- intersect(names(rv$df1), names(rv$df2))
        updateSelectizeInput(session, "histogram_columns", choices = choices, selected = NULL)
      }
    }

    # Box plot column updates
    if (!is.null(input$boxplot_dataset)) {
      if (input$boxplot_dataset == "dataset1" && !is.null(rv$df1)) {
        choices <- names(rv$df1)
        updateSelectizeInput(session, "boxplot_columns", choices = choices, selected = NULL)
      } else if (input$boxplot_dataset == "dataset2" && !is.null(rv$df2)) {
        choices <- names(rv$df2)
        updateSelectizeInput(session, "boxplot_columns", choices = choices, selected = NULL)
      } else if (input$boxplot_dataset == "both" && !is.null(rv$df1) && !is.null(rv$df2)) {
        choices <- intersect(names(rv$df1), names(rv$df2))
        updateSelectizeInput(session, "boxplot_columns", choices = choices, selected = NULL)
      }
    }

    # Violin plot column updates
    if (!is.null(input$violin_dataset)) {
      if (input$violin_dataset == "dataset1" && !is.null(rv$df1)) {
        choices <- names(rv$df1)
        updateSelectizeInput(session, "violin_columns", choices = choices, selected = NULL)
        updateSelectizeInput(session, "violin_group_column", choices = choices, selected = NULL)
      } else if (input$violin_dataset == "dataset2" && !is.null(rv$df2)) {
        choices <- names(rv$df2)
        updateSelectizeInput(session, "violin_columns", choices = choices, selected = NULL)
        updateSelectizeInput(session, "violin_group_column", choices = choices, selected = NULL)
      } else if (input$violin_dataset == "both" && !is.null(rv$df1) && !is.null(rv$df2)) {
        choices <- intersect(names(rv$df1), names(rv$df2))
        updateSelectizeInput(session, "violin_columns", choices = choices, selected = NULL)
        updateSelectizeInput(session, "violin_group_column", choices = choices, selected = NULL)
      }
    }

    # Connected scatter plot column updates
    if (!is.null(input$connected_dataset)) {
      if (input$connected_dataset == "dataset1" && !is.null(rv$df1)) {
        choices <- names(rv$df1)
        updateSelectizeInput(session, "connected_columns", choices = choices, selected = NULL)
        updateSelectizeInput(session, "connected_x_column", choices = choices, selected = NULL)
        updateSelectizeInput(session, "connected_group_column", choices = choices, selected = NULL)
      } else if (input$connected_dataset == "dataset2" && !is.null(rv$df2)) {
        choices <- names(rv$df2)
        updateSelectizeInput(session, "connected_columns", choices = choices, selected = NULL)
        updateSelectizeInput(session, "connected_x_column", choices = choices, selected = NULL)
        updateSelectizeInput(session, "connected_group_column", choices = choices, selected = NULL)
      } else if (input$connected_dataset == "both" && !is.null(rv$df1) && !is.null(rv$df2)) {
        choices <- intersect(names(rv$df1), names(rv$df2))
        updateSelectizeInput(session, "connected_columns", choices = choices, selected = NULL)
        updateSelectizeInput(session, "connected_x_column", choices = choices, selected = NULL)
        updateSelectizeInput(session, "connected_group_column", choices = choices, selected = NULL)
      }
    }

    # Stacked bar chart column updates
    if (!is.null(input$stacked_dataset)) {
      if (input$stacked_dataset == "dataset1" && !is.null(rv$df1)) {
        choices <- names(rv$df1)
        updateSelectizeInput(session, "stacked_columns", choices = choices, selected = NULL)
        updateSelectizeInput(session, "stacked_x_column", choices = choices, selected = NULL)
      } else if (input$stacked_dataset == "dataset2" && !is.null(rv$df2)) {
        choices <- names(rv$df2)
        updateSelectizeInput(session, "stacked_columns", choices = choices, selected = NULL)
        updateSelectizeInput(session, "stacked_x_column", choices = choices, selected = NULL)
      } else if (input$stacked_dataset == "both" && !is.null(rv$df1) && !is.null(rv$df2)) {
        choices <- intersect(names(rv$df1), names(rv$df2))
        updateSelectizeInput(session, "stacked_columns", choices = choices, selected = NULL)
        updateSelectizeInput(session, "stacked_x_column", choices = choices, selected = NULL)
      }
    }
  })


  # ---- Per-chart-type handlers (sibling modules) ----
  register_plot_types_scatter_handlers(input, output, session, rv, show_message, log_operation, directory_management)
  register_plot_types_histogram_handlers(input, output, session, rv, show_message, log_operation, directory_management)
  register_plot_types_boxplot_handlers(input, output, session, rv, show_message, log_operation, directory_management)
  register_plot_types_violin_handlers(input, output, session, rv, show_message, log_operation, directory_management)
  register_plot_types_connected_handlers(input, output, session, rv, show_message, log_operation, directory_management)
  register_plot_types_stacked_handlers(input, output, session, rv, show_message, log_operation, directory_management)

  # Return the module functions
  return(list(
    # This module doesn't need to return specific functions since it's all observeEvent blocks
    # But we can return a status indicator
    module_name = "server_plot_types",
    functions_available = c("scatter_plots", "histograms", "box_plots", "violin_plots", "connected_scatter", "stacked_bar_charts", "multifile_comparison")
  ))
}
