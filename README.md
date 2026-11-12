# Vidternary: Modular Ternary Plot Analysis Tool

A comprehensive R package/Shiny app for creating ternary plots with advanced filtering and multivariate analysis capabilities. Built with a modular architecture for maintainability and extensibility.

## Application Tabs

- **Ternary Plots** — main dual-dataset (primary + reference) ternary plotting with element/optional-parameter selection, per-element filters, statistical filters (IQR, Z-score, MAD), and multivariate filters (Mahalanobis distance, Isolation Forest)
- **Data Comparison** — descriptive-statistics tables, correlation heatmaps, missing-value/outlier summaries, raw Excel previews, and Mahalanobis / Isolation Forest comparison across an arbitrary set of uploaded files (independent of the Ternary Plots uploads, not limited to two)
- **Multiple Ternary Creator** — batch-generates one ternary plot per uploaded file using a shared element mapping and per-element filters
- **Hexagonal Ternary Diagram** — composites 6 triangular ternary plots that share a central element/combination into one hexagonal image
- **Plot Builder** — a generic chart builder: pick plot type (violin/box/bar/histogram/scatter/rose), X/Y axis, color/group, and log scales from dropdowns populated from your data's own columns, with named presets you can save and reload
- **Extreme Value Analysis** — Murakami / ASTM E2283 extreme value statistics: fits a Gumbel probability plot to per-field maximum inclusion sizes and extrapolates the return level (and an interval for the largest inclusion) expected over a larger inspection area; with the steel's Vickers hardness it also gives a fatigue-limit estimate (Murakami's √area model)
- **Steel Cleanliness** — chart rating after ISO 4967 / ASTM E45 from automated SEM/EDS data (types A, B, C, D, DS per 0.5 mm² field; worst field and mean), stringer detection, particles per mm² and area fraction with confidence intervals, and stereology (Fullman, Schwartz-Saltykov, O/S/N bound in the inclusions)
- **Batch Comparison** — one file per batch (heat, specimen): number density and area fraction with intervals, size distributions (k-sample Anderson-Darling, permutation), chemistry (permutation MANOVA on log-ratio coordinates) and the extreme-value return level per batch, each with a test and a measure of how large the difference is
- **Data Import** — turns the particle export of any automated SEM system (.xlsx, .csv, .txt; decimal commas and atomic percent included) into the app's standard table; the column mapping can be saved as a profile
- **Spatial Clustering** — Clark-Evans nearest-neighbour test on inclusion X/Y positions: are they randomly scattered, clustered, or more evenly spread than chance predicts (asymptotic + Monte Carlo p-values)
- **Compositional Analysis** — CLR / ILR log-ratio transforms plus PCA for Wt% chemistry data, avoiding the closure-problem artifacts of running ordinary statistics on raw percentages
- **Analysis Log** — a running, filterable/searchable log of operations performed during the session

## Modular Architecture

The package is organized into logical modules, each handling specific functionality:

### Core
- **`R/dependencies.R`** — package dependency checking/loading
- **`R/options.R`** — app-wide point-size constants
- **`R/file_management.R`** — output-directory creation, file naming, and timestamp logic
- **`R/plot_builder_presets.R`** — load/save Plot Builder presets (`plot_builder_presets.json`)

### Helpers
- **`R/helpers.R`** — logging, debug output, column-name cleaning, plot-title/summary text
- **`R/helpers_filters.R`** — filter collection from Shiny `input` and application to a data frame

### Analysis
- **`R/multivariate.R`** — Mahalanobis distance, Isolation Forest
- **`R/statistical_filters.R`** — IQR / Z-score / MAD filtering (positive/high-value outliers only)
- **`R/extreme_value_analysis.R`** — Murakami / ASTM E2283 extreme value statistics (block maxima + Gumbel fit); pure stats behind the Extreme Value Analysis tab
- **`R/fatigue_murakami.R`** — fatigue limit from the largest inclusion (Murakami's √area model) and the return period that matches a stressed volume
- **`R/cleanliness_family.R`**, **`R/cleanliness_stringers.R`**, **`R/cleanliness_rating_tables.R`**, **`R/cleanliness_rating_objects.R`**, **`R/cleanliness_rating.R`**, **`R/cleanliness_indices.R`**, **`R/cleanliness_plots.R`**, **`R/cleanliness_pipeline.R`** — steel cleanliness: chemical family from EDS, stringers, the numbers of ISO 4967 / ASTM E45, typed objects, field rating, number density / area fraction, plots, and `run_cleanliness_analysis()` which runs them all
- **`R/stereology_saltykov.R`**, **`R/stereology_mass_balance.R`** — particles per volume (Fullman, Schwartz-Saltykov) and O/S/N bound in the inclusions
- **`R/batch_density.R`**, **`R/batch_size_distribution.R`**, **`R/batch_composition.R`**, **`R/batch_evs.R`**, **`R/batch_plots.R`**, **`R/batch_pipeline.R`**, **`R/batch_report.R`** — batch comparison: Poisson rate ratios and dispersion of counts per field, k-sample Anderson-Darling permutation test, permutation MANOVA on ilr coordinates, extreme-value estimate per batch, plots, `run_batch_comparison()`, report and script
- **`R/import_read_table.R`**, **`R/import_roles.R`**, **`R/import_standardize.R`**, **`R/import_profiles.R`** — data import: Excel / delimited-text reader, column-role and unit detection, conversion to the standard table (units, atomic to weight percent), saved profiles
- **`R/settings_io.R`**, **`R/report_html.R`**, **`R/cleanliness_report.R`**, **`R/evs_report.R`** — reproducibility: tab settings as JSON, self-contained HTML reports, scripts that repeat an analysis
- **`R/spatial_clustering_analysis.R`** — Clark-Evans nearest-neighbour CSR test with Donnelly edge correction; pure stats behind the Spatial Clustering tab
- **`R/compositional_data_analysis.R`** — CLR / ILR log-ratio transforms + PCA; pure stats behind the Compositional Analysis tab
- **`R/stats_display_utils.R`** — tidy, DT-ready descriptive/correlation stat tables for the Data Comparison tab

### Ternary Plotting
- **`R/ternary_plot.R`**, **`R/ternary_plot_data_prep.R`**, **`R/ternary_plot_preview.R`**, **`R/ternary_plot_save.R`** — core single-triangle ternary plot pipeline
- **`R/hex_ternary_plot.R`** — hexagonal joint ternary diagram compositing

### Plotting Utilities
- **`R/plotting_utils.R`** — the Data Comparison tab's `corrplot` correlation heatmap
- **`R/plotting_utils_builder.R`** — generic chart builder behind "Plot Builder"

### UI (Shiny)
- **`R/ui_components.R`** — page shell and main tabset
- **`R/ui_ternary_plots_tab.R`**, **`R/ui_data_comparison_tab.R`**, **`R/ui_multiple_ternary_tab.R`**, **`R/ui_hex_ternary_tab.R`**, **`R/ui_plot_builder_tab.R`**, **`R/ui_evs_tab.R`**, **`R/ui_cleanliness_tab.R`**, **`R/ui_batch_tab.R`**, **`R/ui_import_tab.R`**, **`R/ui_spatial_tab.R`**, **`R/ui_coda_tab.R`**, **`R/ui_analysis_log_tab.R`** — one `create_*_tab()` per application tab

### Server (Shiny Backend)
- **`R/server_logic.R`** — wires every tab's server module together
- **`R/server_ternary_plots.R`** (+ `_batch.R`, `_groups.R`) — single-file "Ternary Plots" handlers, the "Multiple Ternary Creator" batch handlers, and the categorical group-selection UI
- **`R/server_file_handlers.R`** — Dataset 1/2 upload and copy-settings for the Ternary Plots tab
- **`R/server_hex_ternary.R`** — Hexagonal Ternary Diagram handlers
- **`R/server_plot_builder.R`** — Plot Builder handlers
- **`R/server_evs.R`**, **`R/server_spatial.R`**, **`R/server_coda.R`** — Shiny wiring for the Extreme Value Analysis / Spatial Clustering / Compositional Analysis tabs (front-ends to the matching `*_analysis.R` modules)
- **`R/server_cleanliness.R`** (+ `_outputs.R`) — Steel Cleanliness tab: upload, column detection, the run, and its tables, plots and downloads
- **`R/server_batch.R`** (+ `_outputs.R`) — Batch Comparison tab
- **`R/server_import.R`** — Data Import tab
- **`R/server_data_comparison.R`** (+ `_upload.R`, `_stats.R`, `_multivariate.R`, `_preview.R`) — Data Comparison tab
- **`R/server_analysis_log.R`** — Analysis Log tab
- **`R/server_status_outputs.R`** — app-shell status text

### Application Entry
- **`R/app.R`** — `run_app()` / `create_app()`

## Quick Start

### Installation

```r
# Install vidternary from GitHub (its Imports: are pulled in automatically)
# install.packages("remotes")
remotes::install_github("vidkudermarusic/vidternary")

# Load and run the application
library(vidternary)
run_app()
```

### Local Development

```r
# Clone the repository, then from its root:
devtools::load_all(".")
run_app()
```

### Basic Usage

```r
# Run the full interactive Shiny application
run_app()

# Or create the app object for custom deployment
app <- create_app()
shiny::runApp(app)
```

## Features

### Ternary Plot Generation
- Support for multiple element columns per axis (summed)
- Automatic normalization and coordinate calculation
- Customizable color palettes and point styling
- Hexagonal joint diagrams for 7-element/combination overviews

### Advanced Filtering
- **Statistical methods**: IQR, Z-score, MAD (positive/high-value outliers only)
- **Multivariate methods**: Mahalanobis distance, Isolation Forest
- **Individual element filters**: apply different filters per element
- **Reference dataset support**: compare against a different dataset

### Plot Builder
- Pick plot type (violin/box/bar/histogram/scatter), X/Y axis, color/group, and log scales from your data's own columns — no fixed schema required
- Save the current configuration as a named preset and reload it later

### Reproducibility
- **Settings**: the analysis tabs save all their inputs as a JSON file and load them again
- **Report**: Steel Cleanliness, Extreme Value Analysis and Batch Comparison write a self-contained HTML report (tables, plots, settings, notes, file checksum)
- **Script**: the same tabs write an R script that repeats the analysis without the app

### Data Quality Assessment
- Missing-value and outlier summaries
- Data quality reporting

### Export Capabilities
- Multiple plot formats (PNG, PDF, JPEG, TIFF)
- Data export (Excel, CSV, RDS, JSON)

## Testing

```r
library(testthat)
devtools::test()
```

## Documentation

- **Vignette**: `vignettes/vidternary-intro.Rmd`
- **Function help**: `?function_name` once the package is loaded
- **Sample data**: `testdata/test_data.xlsx` (see `testdata/README.md`)

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes following the modular structure above
4. Add tests in `tests/testthat/`
5. Submit a pull request

## License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.

## Support

For issues and questions, open an issue on the [GitHub repository](https://github.com/vidkudermarusic/vidternary).
