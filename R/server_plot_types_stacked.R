# ---- Server Plot Types Module: Stacked Bar Charts ----
# Split out of server_plot_types.R: everything specific to the Stacked Bar
# Charts sub-tab (multi-file column observer, create/save handlers, render
# output, color inputs, filename suggestion).

register_plot_types_stacked_handlers <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {

  # Update column choices for multi-file stacked plots
  observeEvent(input$stacked_multifile_files, {
    req(input$stacked_multifile_files)
    tryCatch({
      # Read the first file to get column names
      first_file <- input$stacked_multifile_files$datapath[1]
      df <- read_file_by_type(first_file)

      # Update column choices
      updateSelectizeInput(session, "stacked_multifile_column", choices = names(df), selected = NULL)

      log_operation("SUCCESS", "Updated column choices for multi-file stacked",
                   paste("File:", basename(first_file), "Columns:", length(names(df))))

    }, error = function(e) {
      show_message(paste("Error reading file for column selection:", e$message), "error")
      log_operation("ERROR", "Failed to read file for column selection", e$message)
    })
  })

  # Stacked Bar Chart
  observeEvent(input$create_stacked, {
    tryCatch({
      if (input$stacked_dataset == "multifile") {
        req(input$stacked_multifile_files, input$stacked_multifile_column)

        # Multi-file comparison
        files <- input$stacked_multifile_files$datapath
        file_names <- input$stacked_multifile_files$name
        column_name <- input$stacked_multifile_column

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

        # Create multi-file stacked bar chart
        rv$stacked_plot <- create_multifile_stacked_plot(
          all_data,
          column_name,
          normalize = input$stacked_multifile_normalize,
          percentage = input$stacked_percentage,
          horizontal = input$stacked_horizontal,
          show_labels = input$stacked_labels,
          alpha = input$stacked_alpha
        )

        show_message(paste("Multi-file stacked bar chart created successfully! Processed", length(all_data), "files"), "success")

      } else {
        # Regular stacked bar chart
        req(input$stacked_columns, input$stacked_x_column)

        # Get dataset
        if (input$stacked_dataset == "dataset1") {
          data <- rv$df1
        } else if (input$stacked_dataset == "dataset2") {
          data <- rv$df2
        } else if (input$stacked_dataset == "both") {
          data <- list(rv$df1, rv$df2)
        } else {
          data <- rv$df1  # Default fallback
        }

        # Create stacked bar chart
        rv$stacked_plot <- create_stacked_bar_charts(
          data,
          input$stacked_columns,
          input$stacked_x_column,
          percentage = input$stacked_percentage,
          horizontal = input$stacked_horizontal,
          show_labels = input$stacked_labels,
          dataset_mode = input$stacked_dataset
        )

        show_message("Stacked bar chart created successfully!", "success")
      }
    }, error = function(e) {
      show_message(paste("Error creating stacked bar chart:", e$message), "error")
    })
  })

  output$stacked_plot_output <- renderPlot({
    req(rv$stacked_plot)
    print(rv$stacked_plot)
  })

  # Save Stacked Bar Chart
  observeEvent(input$save_stacked, {
    req(rv$stacked_plot, input$stacked_filename, input$stacked_output_format)
    tryCatch({
      filename <- paste0(input$stacked_filename, ".", input$stacked_output_format)

      if (!is.null(directory_management)) {
        output_dir <- directory_management$get_output_directory()
        filepath <- file.path(output_dir, filename)
      } else {
        filepath <- filename
      }

      ggsave(filepath, rv$stacked_plot, width = 10, height = 8, dpi = 300)
      show_message(paste("Stacked bar chart saved as:", filename), "success")
    }, error = function(e) {
      show_message(paste("Error saving stacked bar chart:", e$message), "error")
    })
  })

  # Dynamic color inputs for stacked bar charts
  output$stacked_color_inputs <- renderUI({
    if (is.null(input$stacked_columns) || length(input$stacked_columns) == 0) {
      return(p("Select columns first"))
    }

    color_inputs <- lapply(seq_along(input$stacked_columns), function(i) {
      col_name <- input$stacked_columns[i]
      colourInput(paste0("stacked_color_", i),
                 label = paste("Color for", col_name),
                 value = rainbow(length(input$stacked_columns))[i])
    })

    do.call(tagList, color_inputs)
  })

  output$stacked_filename_suggestion <- renderText({
    if (is.null(rv$stacked_plot)) {
      "Create a stacked bar chart first"
    } else {
      paste("Suggested filename:", paste0("stacked_", paste(input$stacked_columns, collapse = "_"), "_",
                                        format(Sys.time(), "%Y%m%d_%H%M%S")))
    }
  })
}
