# =============================================================================
# vidternary: Shiny Server Module - Plot Types
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Server-side logic for all plot type functionality including
#              scatter plots, histograms, box plots, and advanced statistical plots.
# 
# Key Functions:
#   - create_server_plot_types(): Main plot types server logic
#   - [Plot type functions for various statistical visualizations]
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - shiny, ggplot2, GGally, corrplot, plotly
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

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
  
  
  # ---- Multiple Plot Types ----
  
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
  
  output$boxplot_plot_output <- renderPlot({
    req(rv$boxplot_plot)
    print(rv$boxplot_plot)
  })
  
  output$violin_plot_output <- renderPlot({
    req(rv$violin_plot)
    print(rv$violin_plot)
  })
  
  output$connected_plot_output <- renderPlot({
    req(rv$connected_plot)
    print(rv$connected_plot)
  })
  
  output$stacked_plot_output <- renderPlot({
    req(rv$stacked_plot)
    print(rv$stacked_plot)
  })
  
  # ---- Save Plot Functionality ----
  
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
  
  # ---- Dynamic Color Inputs ----
  
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
  
  # ---- Plot Outputs ----
  
  # ---- Plot Filename Suggestions ----
  
  output$scatter_filename_suggestion <- renderText({
    if (is.null(rv$scatter_plot)) {
      "Create a scatter plot first"
    } else {
      paste("Suggested filename:", paste0("scatter_", paste(input$scatter_columns, collapse = "_"), "_", 
                                        format(Sys.time(), "%Y%m%d_%H%M%S")))
    }
  })
  
  output$histogram_filename_suggestion <- renderText({
    if (is.null(rv$histogram_plot)) {
      "Create a histogram first"
    } else {
      paste("Suggested filename:", paste0("histogram_", paste(input$histogram_columns, collapse = "_"), "_", 
                                        format(Sys.time(), "%Y%m%d_%H%M%S")))
    }
  })
  
  output$boxplot_filename_suggestion <- renderText({
    if (is.null(rv$boxplot_plot)) {
      "Create a box plot first"
    } else {
      paste("Suggested filename:", paste0("boxplot_", paste(input$boxplot_columns, collapse = "_"), "_", 
                                        format(Sys.time(), "%Y%m%d_%H%M%S")))
    }
  })
  
  output$violin_filename_suggestion <- renderText({
    if (is.null(rv$violin_plot)) {
      "Create a violin plot first"
    } else {
      paste("Suggested filename:", paste0("violin_", paste(input$violin_columns, collapse = "_"), "_", 
                                        format(Sys.time(), "%Y%m%d_%H%M%S")))
    }
  })
  
  output$connected_filename_suggestion <- renderText({
    if (is.null(rv$connected_plot)) {
      "Create a connected scatter plot first"
    } else {
      paste("Suggested filename:", paste0("connected_", paste(input$connected_columns, collapse = "_"), "_", 
                                        format(Sys.time(), "%Y%m%d_%H%M%S")))
    }
  })
  
  output$stacked_filename_suggestion <- renderText({
    if (is.null(rv$stacked_plot)) {
      "Create a stacked bar chart first"
    } else {
      paste("Suggested filename:", paste0("stacked_", paste(input$stacked_columns, collapse = "_"), "_", 
                                        format(Sys.time(), "%Y%m%d_%H%M%S")))
    }
  })
  
  # Return the module functions
  return(list(
    # This module doesn't need to return specific functions since it's all observeEvent blocks
    # But we can return a status indicator
    module_name = "server_plot_types",
    functions_available = c("scatter_plots", "histograms", "box_plots", "violin_plots", "connected_scatter", "stacked_bar_charts", "multifile_comparison")
  ))
}
