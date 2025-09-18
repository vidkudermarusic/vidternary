# =============================================================================
# vidternary: Shiny Server Module - Directory Management
# =============================================================================
# 
# Package:     vidternary
# Version:     1.0.0
# Author:      Vid Kuder Marušič <vidkm30@gmail.com>
# Maintainer:  Vid Kuder Marušič <vidkm30@gmail.com>
# License:     MIT + file LICENSE
# Repository:  https://github.com/vidkudermarusic/vidternary
# 
# Description: Server-side logic for directory management functionality including
#              working and output directory handling and validation.
# 
# Key Functions:
#   - create_server_directory_management(): Main directory management server logic
#   - [Directory management functions and validation]
# 
# Dependencies:
#   - R (>= 4.0.0)
#   - shiny, fs
# 
# Last Modified: 2025-09-07
# 
# =============================================================================

create_server_directory_management <- function(input, output, session, default_working_dir, default_output_dir) {
  
  # ---- Directory Management Functions ----
  
  # Enhanced directory handling with volume selection
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())
  shinyDirChoose(input, "working_dir", roots = volumes, session = session)
  shinyDirChoose(input, "output_dir", roots = volumes, session = session)
  
  # Working directory reactive
  working_dir <- reactive({
    wd <- shinyFiles::parseDirPath(volumes, input$working_dir)
    if (length(wd) == 0) default_working_dir else wd
  })
  
  # Output directory reactive
  output_dir <- reactive({
    od <- shinyFiles::parseDirPath(volumes, input$output_dir)
    if (length(od) == 0) default_output_dir else od
  })
  
  # Directory text outputs
  output$working_dir_text <- renderText({
    wd <- working_dir()
    if (length(wd) == 0) "" else wd
  })
  
  output$output_dir_text <- renderText({
    od <- output_dir()
    if (length(od) == 0) "" else od
  })
  
  # Return the directory functions for external use
  return(list(
    working_dir = working_dir,
    output_dir = output_dir,
    volumes = volumes
  ))
}
