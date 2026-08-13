# ---- Server Plot Types Module: Violin Plots ----
# Split out of server_plot_types.R: everything specific to the Violin Plots
# sub-tab (multi-file column observer, create/save handlers, render output,
# color inputs, filename suggestion).

register_plot_types_violin_handlers <- function(input, output, session, rv, show_message, log_operation, directory_management = NULL) {

  # Update column choices for multi-file violin plots
  observeEvent(input$violin_multifile_files, {
    req(input$violin_multifile_files)
    tryCatch({
      # Read the first file to get column names
      first_file <- input$violin_multifile_files$datapath[1]
      df <- read_file_by_type(first_file)

      # Update column choices
      updateSelectizeInput(session, "violin_multifile_column", choices = names(df), selected = NULL)

      log_operation("SUCCESS", "Updated column choices for multi-file violin",
                   paste("File:", basename(first_file), "Columns:", length(names(df))))

    }, error = function(e) {
      show_message(paste("Error reading file for column selection:", e$message), "error")
      log_operation("ERROR", "Failed to read file for column selection", e$message)
    })
  })

  # Violin Plot
  observeEvent(input$create_violin, {
    tryCatch({
      if (input$violin_dataset == "multifile") {
        req(input$violin_multifile_files, input$violin_multifile_column)

        # Multi-file comparison
        files <- input$violin_multifile_files$datapath
        file_names <- input$violin_multifile_files$name
        column_name <- input$violin_multifile_column

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

        # Create multi-file violin plot
        rv$violin_plot <- create_multifile_violin_plot(
          all_data,
          column_name,
          normalize = input$violin_multifile_normalize,
          fill_violins = input$violin_fill,
          add_boxplot = input$violin_boxplot,
          show_points = input$violin_points,
          scale_method = input$violin_scale,
          alpha = input$violin_alpha
        )

        show_message(paste("Multi-file violin plot created successfully! Processed", length(all_data), "files"), "success")

      } else {
        # Regular violin plot
        req(input$violin_columns)

        # Get dataset
        if (input$violin_dataset == "dataset1") {
          data <- rv$df1
        } else if (input$violin_dataset == "dataset2") {
          data <- rv$df2
        } else if (input$violin_dataset == "both") {
          data <- list(rv$df1, rv$df2)
        } else {
          data <- rv$df1  # Default fallback
        }

        # Create violin plot
        rv$violin_plot <- create_violin_plots(
          data,
          input$violin_columns,
          group_column = input$violin_group_column,
          fill_violins = input$violin_fill,
          add_boxplot = input$violin_boxplot,
          show_points = input$violin_points,
          scale_method = input$violin_scale,
          dataset_mode = input$violin_dataset
        )

        show_message("Violin plot created successfully!", "success")
      }
    }, error = function(e) {
      show_message(paste("Error creating violin plot:", e$message), "error")
    })
  })

  output$violin_plot_output <- renderPlot({
    req(rv$violin_plot)
    print(rv$violin_plot)
  })

  # Save Violin Plot
  observeEvent(input$save_violin, {
    req(rv$violin_plot, input$violin_filename, input$violin_output_format)
    tryCatch({
      filename <- paste0(input$violin_filename, ".", input$violin_output_format)

      if (!is.null(directory_management)) {
        output_dir <- directory_management$get_output_directory()
        filepath <- file.path(output_dir, filename)
      } else {
        filepath <- filename
      }

      ggsave(filepath, rv$violin_plot, width = 10, height = 8, dpi = 300)
      show_message(paste("Violin plot saved as:", filename), "success")
    }, error = function(e) {
      show_message(paste("Error saving violin plot:", e$message), "error")
    })
  })

  # Dynamic color inputs for violin plots
  output$violin_color_inputs <- renderUI({
    if (is.null(input$violin_columns) || length(input$violin_columns) == 0) {
      return(p("Select columns first"))
    }

    color_inputs <- lapply(seq_along(input$violin_columns), function(i) {
      col_name <- input$violin_columns[i]
      colourInput(paste0("violin_color_", i),
                 label = paste("Color for", col_name),
                 value = rainbow(length(input$violin_columns))[i])
    })

    do.call(tagList, color_inputs)
  })

  output$violin_filename_suggestion <- renderText({
    if (is.null(rv$violin_plot)) {
      "Create a violin plot first"
    } else {
      paste("Suggested filename:", paste0("violin_", paste(input$violin_columns, collapse = "_"), "_",
                                        format(Sys.time(), "%Y%m%d_%H%M%S")))
    }
  })
}
