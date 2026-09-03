# File overview for the `vidternary` package

A short description of every file in the package, grouped by functional area. For a more detailed description of an individual function, see the comment at the top of each file.

## Entry point

| File | Description |
|---|---|
| `R/app.R` | Runs the whole Shiny application (`run_app()` and `create_app()`). |

## UI modules (`ui_*.R`)

| File | Description |
|---|---|
| `R/ui_components.R` | Whole-UI shell: title, global CSS/JS, the main tabset, and the always-visible Directory Settings section. Each tab's own UI is split into the files below. |
| `R/ui_ternary_plots_tab.R` | **Ternary Plots** tab – the main workflow: two-dataset upload, axis/element selection, per-element filters, analysis methods, output options, live preview. |
| `R/ui_data_comparison_tab.R` | **Data Comparison** tab – descriptive statistics, correlation analysis, and multivariate (Mahalanobis / Isolation Forest) comparison across an arbitrary set of uploaded files. |
| `R/ui_multiple_ternary_tab.R` | **Multiple Ternary Creator** tab – batch-generates one ternary plot per uploaded file from a shared element mapping and per-element filters. |
| `R/ui_hex_ternary_tab.R` | **Hexagonal Ternary Diagram** tab – composites 6 triangular ternary plots from one file into a single hexagonal image. |
| `R/ui_plot_builder_tab.R` | **Plot Builder** tab – generic chart builder (plot type, X/Y axis, colour/group, log scales) plus saved user presets. |
| `R/ui_evs_tab.R` | **Extreme Value Analysis** tab – Murakami / ASTM E2283 extreme value statistics for predicting inclusion size. |
| `R/ui_spatial_tab.R` | **Spatial Clustering** tab – Clark-Evans test of complete spatial randomness for inclusion positions (spacing / clustering). |
| `R/ui_coda_tab.R` | **Compositional Analysis** tab – CLR / ILR log-ratio transforms and PCA for compositional (Wt%) chemistry data. |
| `R/ui_analysis_log_tab.R` | **Analysis Log** tab – filterable / searchable activity log. |

## Server modules (`server_*.R`)

| File | Description |
|---|---|
| `R/server_logic.R` | Top-level server module – initializes the shared `reactiveValues` (`rv`) and wires every other server module into one reactive graph. |
| `R/server_ternary_plots.R` | Single-file ternary plot logic: parameter building, live preview, analysis-report text, the Save Plot 1/2/Both buttons. |
| `R/server_ternary_plots_batch.R` | Batch preview/save for **Multiple Ternary Creator** (split out of `server_ternary_plots.R`; now its own module/namespace). |
| `R/server_ternary_plots_groups.R` | Detects categorical data in "Optional Parameter 2" and manages the group-selection checklist UI. |
| `R/server_file_handlers.R` | Dataset 1/2 file upload/download and copy-settings, for the Ternary Plots tab. |
| `R/server_data_comparison.R` | Registers all **Data Comparison** tab handlers in the right order (upload first, then stats, multivariate, preview). |
| `R/server_data_comparison_upload.R` | Owns `rv$comparison_data` (one data frame per uploaded file) and every dataset / target / reference selector the other Data Comparison modules read from. |
| `R/server_data_comparison_stats.R` | Descriptive-statistics and correlation handlers – DT tables, stat cards, correlation heatmap, per-row mini-histograms. |
| `R/server_data_comparison_multivariate.R` | Mahalanobis distance and Isolation Forest buttons plus the combined comprehensive results panel. |
| `R/server_data_comparison_preview.R` | Missing-value / outlier summaries and a raw preview of the uploaded Excel files. |
| `R/server_hex_ternary.R` | **Hexagonal Ternary Diagram** tab logic (generate into preview / save to file). |
| `R/server_plot_builder.R` | **Plot Builder** tab logic – chart rendering plus save / load / delete of user presets. |
| `R/server_evs.R` | **Extreme Value Analysis** tab logic – Gumbel fit, probability plot, goodness-of-fit test. |
| `R/server_spatial.R` | **Spatial Clustering** tab logic – Clark-Evans test, scatter plot, nearest-neighbour distance histogram. |
| `R/server_coda.R` | **Compositional Analysis** tab logic – CLR / ILR transform, PCA, biplot. |
| `R/server_analysis_log.R` | Records and renders the activity log; saves / exports the log to file. |
| `R/server_directory_management.R` | Working-directory and output-directory selection. |
| `R/server_status_outputs.R` | App-shell status text and user feedback. |

## Statistics and analysis (no Shiny dependency)

| File | Description |
|---|---|
| `R/extreme_value_analysis.R` | Murakami / ASTM E2283 extreme value statistics: block maxima, Gumbel fit, prediction, goodness-of-fit test (parametric bootstrap). |
| `R/spatial_clustering_analysis.R` | Clark-Evans clustering / spatial-randomness test for inclusion positions, with Donnelly's rectangular-window edge correction. |
| `R/compositional_data_analysis.R` | CLR / ILR log-ratio transforms and PCA for compositional (Wt%) chemistry data. |
| `R/multivariate.R` | Mahalanobis distance, Isolation Forest, and input validation for multivariate analysis. |
| `R/statistical_filters.R` | IQR / Z-score / MAD filtering ((positive) outliers only). |
| `R/stats_display_utils.R` | Builds the DT tables, stat cards, and mini-histograms used to display statistics on the Data Comparison tab. |

## Ternary plotting

| File | Description |
|---|---|
| `R/ternary_plot.R` | `general_ternary_plot()` – a thin orchestrator that calls the modules below to prepare / draw / save. |
| `R/ternary_plot_data_prep.R` | Ternary plot data preparation: load the file, apply filters, compute ternary coordinates, build the title, work out point size / colour / shape. |
| `R/ternary_plot_preview.R` | Draws the ternary plot to the currently active graphics device (preview). |
| `R/ternary_plot_save.R` | Redraws and saves the ternary plot to an image file. |
| `R/hex_ternary_plot.R` | Composites 6 ternary plots into one hexagonal diagram. |

## Plotting utilities

| File | Description |
|---|---|
| `R/plotting_utils.R` | `corrplot` correlation heatmap for the Data Comparison tab – the only ggplot-independent plotting helper still wired into the app. |
| `R/plotting_utils_builder.R` | Generic chart builder for the **Plot Builder** tab (violin / box / bar / histogram / scatter / rose). |

## Helper functions

| File | Description |
|---|---|
| `R/helpers.R` | Core helpers: logging (`log_operation` / `debug_log`), column-name cleaning, console messages, multi-line plot-title layout. |
| `R/helpers_filters.R` | Collects filter values from Shiny `input` and applies them to a data frame. |

## Configuration and misc

| File | Description |
|---|---|
| `R/options.R` | App-wide constants – the min/max point-size bounds shared by every plot's point-size legend/scaling. |
| `R/dependencies.R` | Checks (does not install) the required R packages. |
| `R/file_management.R` | Output-directory creation, file naming, timestamp logic. |
| `R/plot_builder_presets.R` | Save / load user presets for the Plot Builder tab (`plot_builder_presets.json`). |

## Package structure (outside `R/`)

| Path | Description |
|---|---|
| `DESCRIPTION`, `NAMESPACE` | Standard R package metadata and the list of exported functions. |
| `README.md` | Main project description and usage instructions. |
| `man/` | Generated `.Rd` function help pages (roxygen2 output). |
| `vignettes/` | Long-form guides: `vidternary-intro.Rmd`, `user-guide.Rmd`, `statistical-appendix.Rmd`. |
| `tests/testthat/` | `testthat` tests: modular structure plus the EVS / spatial-clustering / compositional-data analysis modules. |
| `testdata/` | Sample Excel data (`test_data.xlsx`) with a README. |
| `inst/`, `data-raw/` | Installed extra files and the scripts used to prepare bundled data. |
| `legacy/` | Old / retired scripts kept for reference (`App6.0.1.R`, `ternary_plot_old.R`, ...) – not part of the active package. |
| `renv/` | `renv` R environment / dependency manager for the package. |
