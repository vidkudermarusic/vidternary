# ---- Server Plot Types Module: Connected Scatter ----
# Split out of server_plot_types.R: everything specific to the Connected
# Scatter sub-tab (multi-file column observer, create/save handlers, render
# output, color inputs, filename suggestion).

register_plot_types_connected_handlers <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {

  # Update column choices for multi-file connected plots
  observeEvent(input$connected_multifile_files, {
    req(input$connected_multifile_files)
    tryCatch({
      # Read the first file to get column names
      first_file <- input$connected_multifile_files$datapath[1]
      df <- read_file_by_type(first_file)

      # Update column choices
      updateSelectizeInput(session, "connected_multifile_column", choices = names(df), selected = NULL)

      log_operation("SUCCESS", "Updated column choices for multi-file connected",
                   paste("File:", basename(first_file), "Columns:", length(names(df))))

    }, error = function(e) {
      show_message(paste("Error reading file for column selection:", e$message), "error")
      log_operation("ERROR", "Failed to read file for column selection", e$message)
    })
  })

  # Connected Scatter Plot
  observeEvent(input$create_connected, {
    tryCatch({
      if (input$connected_dataset == "multifile") {
        req(input$connected_multifile_files, input$connected_multifile_column)

        # Multi-file comparison
        files <- input$connected_multifile_files$datapath
        file_names <- input$connected_multifile_files$name
        column_name <- input$connected_multifile_column

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

        # Create multi-file connected scatter plot
        rv$connected_plot <- create_multifile_connected_plot(
          all_data,
          column_name,
          normalize = input$connected_multifile_normalize,
          show_points = input$connected_points,
          show_lines = input$connected_lines,
          smooth_lines = input$connected_smooth,
          line_width = input$connected_line_width,
          point_size = input$connected_point_size
        )

        show_message(paste("Multi-file connected scatter plot created successfully! Processed", length(all_data), "files"), "success")

      } else {
        # Regular connected scatter plot
        req(input$connected_columns, input$connected_x_column)

        # Get dataset
        if (input$connected_dataset == "dataset1") {
          data <- rv$df1
        } else if (input$connected_dataset == "dataset2") {
          data <- rv$df2
        } else if (input$connected_dataset == "both") {
          data <- list(rv$df1, rv$df2)
        } else {
          data <- rv$df1  # Default fallback
        }

        # Create connected scatter plot
        rv$connected_plot <- create_connected_scatter_plots(
          data,
          input$connected_columns,
          input$connected_x_column,
          group_column = input$connected_group_column,
          show_points = input$connected_points,
          show_lines = input$connected_lines,
          smooth_lines = input$connected_smooth,
          line_width = input$connected_line_width,
          point_size = input$connected_point_size,
          dataset_mode = input$connected_dataset
        )

        show_message("Connected scatter plot created successfully!", "success")
      }
    }, error = function(e) {
      show_message(paste("Error creating connected scatter plot:", e$message), "error")
    })
  })

  output$connected_plot_output <- renderPlot({
    req(rv$connected_plot)
    print(rv$connected_plot)
  })

  # Save Connected Scatter Plot
  observeEvent(input$save_connected, {
    req(rv$connected_plot, input$connected_filename, input$connected_output_format)
    tryCatch({
      filename <- paste0(input$connected_filename, ".", input$connected_output_format)

      if (!is.null(directory_management)) {
        output_dir <- directory_management$get_output_directory()
        filepath <- file.path(output_dir, filename)
      } else {
        filepath <- filename
      }

      ggsave(filepath, rv$connected_plot, width = 10, height = 8, dpi = 300)
      show_message(paste("Connected scatter plot saved as:", filename), "success")
    }, error = function(e) {
      show_message(paste("Error saving connected scatter plot:", e$message), "error")
    })
  })

  # Dynamic color inputs for connected scatter plots
  output$connected_color_inputs <- renderUI({
    if (is.null(input$connected_columns) || length(input$connected_columns) == 0) {
      return(p("Select columns first"))
    }

    color_inputs <- lapply(seq_along(input$connected_columns), function(i) {
      col_name <- input$connected_columns[i]
      colourInput(paste0("connected_color_", i),
                 label = paste("Color for", col_name),
                 value = rainbow(length(input$connected_columns))[i])
    })

    do.call(tagList, color_inputs)
  })

  output$connected_filename_suggestion <- renderText({
    if (is.null(rv$connected_plot)) {
      "Create a connected scatter plot first"
    } else {
      paste("Suggested filename:", paste0("connected_", paste(input$connected_columns, collapse = "_"), "_",
                                        format(Sys.time(), "%Y%m%d_%H%M%S")))
    }
  })
}
