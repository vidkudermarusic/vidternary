# ---- Server Plot Types Module: Histograms ----
# Split out of server_plot_types.R: everything specific to the Histograms
# sub-tab (multi-file column observer, create/save handlers, render output,
# color inputs, filename suggestion).

register_plot_types_histogram_handlers <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {

  # Update column choices for multi-file histogram plots
  observeEvent(input$histogram_multifile_files, {
    req(input$histogram_multifile_files)
    tryCatch({
      # Read the first file to get column names
      first_file <- input$histogram_multifile_files$datapath[1]
      df <- read_file_by_type(first_file)

      # Update column choices
      updateSelectizeInput(session, "histogram_multifile_column", choices = names(df), selected = NULL)

      log_operation("SUCCESS", "Updated column choices for multi-file histogram",
                   paste("File:", basename(first_file), "Columns:", length(names(df))))

    }, error = function(e) {
      show_message(paste("Error reading file for column selection:", e$message), "error")
      log_operation("ERROR", "Failed to read file for column selection", e$message)
    })
  })

  # Histogram
  observeEvent(input$create_histogram, {
    tryCatch({
      if (input$histogram_dataset == "multifile") {
        req(input$histogram_multifile_files, input$histogram_multifile_column)

        # Multi-file comparison
        files <- input$histogram_multifile_files$datapath
        file_names <- input$histogram_multifile_files$name
        column_name <- input$histogram_multifile_column

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

        # Create multi-file histogram
        rv$histogram_plot <- create_multifile_histogram_plot(
          all_data,
          column_name,
          normalize = input$histogram_multifile_normalize,
          breaks = input$histogram_bins,
          alpha = input$histogram_alpha,
          data_type = if (input$histogram_density) "density" else "frequency"
        )

        show_message(paste("Multi-file histogram created successfully! Processed", length(all_data), "files"), "success")

      } else {
        # Regular histogram
        req(input$histogram_columns)

        # Get dataset
        if (input$histogram_dataset == "dataset1") {
          data <- rv$df1
        } else if (input$histogram_dataset == "dataset2") {
          data <- rv$df2
        } else if (input$histogram_dataset == "both") {
          data <- list(rv$df1, rv$df2)
        } else {
          data <- rv$df1  # Default fallback
        }

            # Create histogram
            rv$histogram_plot <- create_histograms(
              data,
              input$histogram_columns,
              breaks = input$histogram_bins,
              alpha = input$histogram_alpha,
              data_type = if (input$histogram_density) "density" else "frequency",
              dataset_mode = input$histogram_dataset,
              log_x = input$histogram_log_x,
              log_y = input$histogram_log_y
            )

        show_message("Histogram created successfully!", "success")
      }
    }, error = function(e) {
      show_message(paste("Error creating histogram:", e$message), "error")
    })
  })

  output$histogram_plot_output <- renderPlot({
    req(rv$histogram_plot)
    print(rv$histogram_plot)
  })

  # Save Histogram Plot
  observeEvent(input$save_histogram, {
    req(rv$histogram_plot, input$histogram_filename, input$histogram_output_format)
    tryCatch({
      filename <- paste0(input$histogram_filename, ".", input$histogram_output_format)

      if (!is.null(directory_management)) {
        output_dir <- directory_management$get_output_directory()
        filepath <- file.path(output_dir, filename)
      } else {
        filepath <- filename
      }

      ggsave(filepath, rv$histogram_plot, width = 10, height = 8, dpi = 300)
      show_message(paste("Histogram saved as:", filename), "success")
    }, error = function(e) {
      show_message(paste("Error saving histogram:", e$message), "error")
    })
  })

  # Dynamic color inputs for histograms
  output$histogram_color_inputs <- renderUI({
    if (is.null(input$histogram_columns) || length(input$histogram_columns) == 0) {
      return(p("Select columns first"))
    }

    color_inputs <- lapply(seq_along(input$histogram_columns), function(i) {
      col_name <- input$histogram_columns[i]
      colourInput(paste0("histogram_color_", i),
                 label = paste("Color for", col_name),
                 value = rainbow(length(input$histogram_columns))[i])
    })

    do.call(tagList, color_inputs)
  })

  output$histogram_filename_suggestion <- renderText({
    if (is.null(rv$histogram_plot)) {
      "Create a histogram first"
    } else {
      paste("Suggested filename:", paste0("histogram_", paste(input$histogram_columns, collapse = "_"), "_",
                                        format(Sys.time(), "%Y%m%d_%H%M%S")))
    }
  })
}
