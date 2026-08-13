# ---- Server Plot Types Module: Box Plots ----
# Split out of server_plot_types.R: everything specific to the Box Plots
# sub-tab (multi-file column observer, create/save handlers, render output,
# color inputs, filename suggestion).

register_plot_types_boxplot_handlers <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {

  # Update column choices for multi-file boxplot plots
  observeEvent(input$boxplot_multifile_files, {
    req(input$boxplot_multifile_files)
    tryCatch({
      # Read the first file to get column names
      first_file <- input$boxplot_multifile_files$datapath[1]
      df <- read_file_by_type(first_file)

      # Update column choices
      updateSelectizeInput(session, "boxplot_multifile_column", choices = names(df), selected = NULL)

      log_operation("SUCCESS", "Updated column choices for multi-file boxplot",
                   paste("File:", basename(first_file), "Columns:", length(names(df))))

    }, error = function(e) {
      show_message(paste("Error reading file for column selection:", e$message), "error")
      log_operation("ERROR", "Failed to read file for column selection", e$message)
    })
  })

  # Box Plot
  observeEvent(input$create_boxplot, {
    tryCatch({
      if (input$boxplot_dataset == "multifile") {
        req(input$boxplot_multifile_files, input$boxplot_multifile_column)

        # Multi-file comparison
        files <- input$boxplot_multifile_files$datapath
        file_names <- input$boxplot_multifile_files$name
        column_name <- input$boxplot_multifile_column

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

        # Create multi-file box plot
        rv$boxplot_plot <- create_multifile_boxplot_plot(
          all_data,
          column_name,
          normalize = input$boxplot_multifile_normalize,
          horizontal = input$boxplot_horizontal,
          notch = input$boxplot_notch,
          show_outliers = input$boxplot_outliers,
          violin_overlay = input$boxplot_violin,
          alpha = input$boxplot_alpha
        )

        show_message(paste("Multi-file box plot created successfully! Processed", length(all_data), "files"), "success")

      } else {
        # Regular box plot
        req(input$boxplot_columns)

        # Get dataset
        if (input$boxplot_dataset == "dataset1") {
          data <- rv$df1
        } else if (input$boxplot_dataset == "dataset2") {
          data <- rv$df2
        } else if (input$boxplot_dataset == "both") {
          data <- list(rv$df1, rv$df2)
        } else {
          data <- rv$df1  # Default fallback
        }

        # Create box plot
        rv$boxplot_plot <- create_box_plots(
          data,
          input$boxplot_columns,
          horizontal = input$boxplot_horizontal,
          notch = input$boxplot_notch,
          show_outliers = input$boxplot_outliers,
          dataset_mode = input$boxplot_dataset,
          violin_overlay = input$boxplot_violin
        )

        show_message("Box plot created successfully!", "success")
      }
    }, error = function(e) {
      show_message(paste("Error creating box plot:", e$message), "error")
    })
  })

  output$boxplot_plot_output <- renderPlot({
    req(rv$boxplot_plot)
    print(rv$boxplot_plot)
  })

  # Save Box Plot
  observeEvent(input$save_boxplot, {
    req(rv$boxplot_plot, input$boxplot_filename, input$boxplot_output_format)
    tryCatch({
      filename <- paste0(input$boxplot_filename, ".", input$boxplot_output_format)

      if (!is.null(directory_management)) {
        output_dir <- directory_management$get_output_directory()
        filepath <- file.path(output_dir, filename)
      } else {
        filepath <- filename
      }

      ggsave(filepath, rv$boxplot_plot, width = 10, height = 8, dpi = 300)
      show_message(paste("Box plot saved as:", filename), "success")
    }, error = function(e) {
      show_message(paste("Error saving box plot:", e$message), "error")
    })
  })

  # Dynamic color inputs for box plots
  output$boxplot_color_inputs <- renderUI({
    if (is.null(input$boxplot_columns) || length(input$boxplot_columns) == 0) {
      return(p("Select columns first"))
    }

    color_inputs <- lapply(seq_along(input$boxplot_columns), function(i) {
      col_name <- input$boxplot_columns[i]
      colourInput(paste0("boxplot_color_", i),
                 label = paste("Color for", col_name),
                 value = rainbow(length(input$boxplot_columns))[i])
    })

    do.call(tagList, color_inputs)
  })

  output$boxplot_filename_suggestion <- renderText({
    if (is.null(rv$boxplot_plot)) {
      "Create a box plot first"
    } else {
      paste("Suggested filename:", paste0("boxplot_", paste(input$boxplot_columns, collapse = "_"), "_",
                                        format(Sys.time(), "%Y%m%d_%H%M%S")))
    }
  })
}
