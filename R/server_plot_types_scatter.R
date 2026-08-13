# ---- Server Plot Types Module: Scatter Plots ----
# Split out of server_plot_types.R: everything specific to the Scatter Plots
# sub-tab (multi-file column observer, create/save handlers, render output,
# color inputs, filename suggestion).

register_plot_types_scatter_handlers <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {

  # Update column choices for multi-file scatter plots
  observeEvent(input$scatter_multifile_files, {
    req(input$scatter_multifile_files)
    tryCatch({
      # Read the first file to get column names
      first_file <- input$scatter_multifile_files$datapath[1]
      df <- read_file_by_type(first_file)

      # Update column choices
      updateSelectizeInput(session, "scatter_multifile_column", choices = names(df), selected = NULL)

      log_operation("SUCCESS", "Updated column choices for multi-file scatter",
                   paste("File:", basename(first_file), "Columns:", length(names(df))))

    }, error = function(e) {
      show_message(paste("Error reading file for column selection:", e$message), "error")
      log_operation("ERROR", "Failed to read file for column selection", e$message)
    })
  })

  # Scatter Plot
  observeEvent(input$create_scatter, {
    tryCatch({
      if (input$scatter_dataset == "multifile") {
        req(input$scatter_multifile_files, input$scatter_multifile_column)

        # Multi-file comparison
        files <- input$scatter_multifile_files$datapath
        file_names <- input$scatter_multifile_files$name
        column_name <- input$scatter_multifile_column

        # Read data from all files
        all_data <- list()
        for (i in seq_along(files)) {
          file_path <- files[i]
          file_name <- file_names[i]

          df <- read_file_by_type(file_path)

          if (column_name %in% names(df)) {
            # Add file identifier
            df$file_source <- file_name
            all_data[[file_name]] <- df
          }
        }

        # Create multi-file scatter plot
        rv$scatter_plot <- create_multifile_scatter_plot(
          all_data,
          column_name,
          normalize = input$scatter_multifile_normalize
        )

        show_message(paste("Multi-file scatter plot created successfully! Processed", length(all_data), "files"), "success")

      } else {
        # Regular scatter plot
        req(input$scatter_columns)

        # Get dataset
        if (input$scatter_dataset == "dataset1") {
          data <- rv$df1
        } else if (input$scatter_dataset == "dataset2") {
          data <- rv$df2
        } else if (input$scatter_dataset == "both") {
          data <- list(rv$df1, rv$df2)
        } else {
          data <- rv$df1  # Default fallback
        }

        # Get colors
        colors <- sapply(seq_along(input$scatter_columns), function(i) {
          input[[paste0("scatter_color_", i)]]
        })

        # Create scatter plot
        rv$scatter_plot <- create_scatter_plots(
          data,
          input$scatter_columns,
          colors,
          input$scatter_x_col,
          input$scatter_y_col,
          input$scatter_point_size,
          input$scatter_dataset,
          add_trendline = input$scatter_add_trendline,
          add_smooth = input$scatter_add_smooth,
          log_x = input$scatter_log_x,
          log_y = input$scatter_log_y
        )

        show_message("Scatter plot created successfully!", "success")
      }
    }, error = function(e) {
      show_message(paste("Error creating scatter plot:", e$message), "error")
    })
  })

  output$scatter_plot_output <- renderPlot({
    req(rv$scatter_plot)
    print(rv$scatter_plot)
  })

  # Save Scatter Plot
  observeEvent(input$save_scatter, {
    req(rv$scatter_plot, input$scatter_filename, input$scatter_output_format)
    tryCatch({
      filename <- paste0(input$scatter_filename, ".", input$scatter_output_format)

      if (!is.null(directory_management)) {
        output_dir <- directory_management$get_output_directory()
        filepath <- file.path(output_dir, filename)
      } else {
        filepath <- filename
      }

      ggsave(filepath, rv$scatter_plot, width = 10, height = 8, dpi = 300)
      show_message(paste("Scatter plot saved as:", filename), "success")
    }, error = function(e) {
      show_message(paste("Error saving scatter plot:", e$message), "error")
    })
  })

  # Dynamic color inputs for scatter plots
  output$scatter_color_inputs <- renderUI({
    if (is.null(input$scatter_columns) || length(input$scatter_columns) == 0) {
      return(p("Select columns first"))
    }

    color_inputs <- lapply(seq_along(input$scatter_columns), function(i) {
      col_name <- input$scatter_columns[i]
      colourInput(paste0("scatter_color_", i),
                 label = paste("Color for", col_name),
                 value = rainbow(length(input$scatter_columns))[i])
    })

    do.call(tagList, color_inputs)
  })

  output$scatter_filename_suggestion <- renderText({
    if (is.null(rv$scatter_plot)) {
      "Create a scatter plot first"
    } else {
      paste("Suggested filename:", paste0("scatter_", paste(input$scatter_columns, collapse = "_"), "_",
                                        format(Sys.time(), "%Y%m%d_%H%M%S")))
    }
  })
}
