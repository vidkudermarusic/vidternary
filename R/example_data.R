# ---- Example data: try the app without a measurement of one's own ----
# Three simulated particle files ship with the package (written by
# data-raw/make_example_data.R; see there for what is built into them):
# two runs of one specimen and one "other heat". They have the columns of
# an automated SEM/EDS export, but describe no real steel. They are Excel
# files because the older tabs (ternary plots, Plot Builder, Data
# Comparison) read Excel only.
#
# In the app, the button "Load example data into the open tab" puts them
# into the upload box of the tab that is showing, exactly as if the user
# had chosen the files: the page fetches them and hands them to the upload
# box, so no tab needs code of its own for it. A box that takes one file
# gets the first run; a box that takes several gets all three, unless the
# tab names the files it wants (`data-example-files` on a parent element).

#' The example files that ship with the package
#'
#' @return Named character vector: file name -> full path.
#' @export
example_data_files <- function() {
  dir <- system.file("extdata", "examples", package = "vidternary")
  files <- sort(list.files(dir, pattern = "\\.xlsx$", full.names = TRUE))
  # The two runs first, so a single-file tab gets a run.
  files <- files[order(!grepl("run", basename(files)), basename(files))]
  stats::setNames(files, basename(files))
}

#' Read one of the example files
#'
#' The example data is simulated: it has the columns and plausible numbers
#' of an automated SEM/EDS particle export, but describes no real steel.
#'
#' @param name File name (see [example_data_files()]), with or without
#'   `.xlsx`. Default: the first run.
#' @return A data frame, as [read_particle_table()] reads it.
#' @export
example_data <- function(name = "example_run_1") {
  files <- example_data_files()
  hit <- files[tools::file_path_sans_ext(names(files)) == tools::file_path_sans_ext(name)]
  if (length(hit) == 0) {
    stop("No example file '", name, "'. Available: ", paste(tools::file_path_sans_ext(names(files)), collapse = ", "), ".", call. = FALSE)
  }
  read_particle_table(hit[[1]])
}

# Browser side of "Load example data into the open tab".
.example_data_js <- "
window.vidternaryLoadExample = async function(files, statusId) {
  var say = function(text) { var el = document.getElementById(statusId); if (el) el.textContent = text; };
  var boxes = Array.prototype.filter.call(document.querySelectorAll('input[type=file]'), function(el) {
    var group = el.closest('.form-group') || el.parentElement;
    return group && group.offsetParent !== null && !/json/i.test(el.getAttribute('accept') || '');
  });
  if (boxes.length === 0) { say('The open tab has no upload box for data files.'); return; }
  var box = boxes[0];
  var named = box.closest('[data-example-files]');
  var wanted = named ? named.getAttribute('data-example-files').split(',') : (box.multiple ? files : [files[0]]);
  if (typeof DataTransfer === 'undefined') { say('This browser cannot do that - download the example files and upload them yourself.'); return; }
  try {
    var transfer = new DataTransfer();
    for (var i = 0; i < wanted.length; i++) {
      var response = await fetch('vidternary-examples/' + wanted[i]);
      if (!response.ok) throw new Error(wanted[i]);
      transfer.items.add(new File([await response.blob()], wanted[i], { type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet' }));
    }
    box.files = transfer.files;
    $(box).trigger('change');
    say('Loaded into the open tab: ' + wanted.join(', ') + ' (simulated data).');
  } catch (e) {
    say('Could not load the example files - download them and upload them yourself.');
  }
};
"

#' Buttons that load or download the example data
#'
#' @return A `shiny::div()` for the top of the page.
#' @export
example_data_ui <- function() {
  files <- example_data_files()
  dir <- dirname(files[1])
  if (length(files) && dir.exists(dir)) shiny::addResourcePath("vidternary-examples", dir)
  call <- sprintf("vidternaryLoadExample(%s, 'example_data_status')", jsonlite::toJSON(names(files)))
  div(style = "border: 1px solid #6c757d; padding: 8px 15px; border-radius: 5px; margin: 0 0 12px 0; background-color: #f8f9fa;",
    tags$script(HTML(.example_data_js)),
    fluidRow(
      column(6, strong("Example data"), br(),
             tags$button(type = "button", class = "btn btn-default", onclick = call, "Load example data into the open tab"),
             downloadButton("example_data_download", "Download the example files (zip)")),
      column(6, helpText("Simulated particle files with the columns of an SEM/EDS export, to try any tab without a measurement of your own: two runs of one specimen and one other heat. Not real steel."),
             tags$span(id = "example_data_status", style = "font-size: 13px;"))
    )
  )
}

#' Wire the example-data download
#'
#' Call from the app's top-level server function.
#'
#' @param output The app's top-level Shiny `output` object.
#' @return `NULL`, invisibly.
#' @export
register_example_data <- function(output) {
  output$example_data_download <- downloadHandler(
    filename = function() "vidternary_example_data.zip",
    content = function(file) {
      files <- example_data_files()
      zip::zip(file, files = names(files), root = dirname(files[1]))
    }
  )
  invisible(NULL)
}
