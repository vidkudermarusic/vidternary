# Vidternary: Modular Ternary Plot Analysis Tool

A comprehensive R package/Shiny app for creating ternary plots with advanced filtering and multivariate analysis capabilities. Built with a modular architecture for maintainability and extensibility.

## 🖥️ Application Tabs

- **Ternary Plots** — main dual-dataset (primary + reference) ternary plotting with element/optional-parameter selection, statistical filters (IQR, Z-score, MAD), and multivariate filters (Mahalanobis distance, Isolation Forest)
- **Data Comparison** — missing-value/outlier summaries, raw Excel previews, and statistical comparison between the two datasets
- **Multiple Plot Types** — scatter, histogram, box, violin, connected-scatter, and stacked-bar charts across one or more files
- **Multiple Ternary Creator** — batch-generates one ternary plot per uploaded file using the same element mapping and filters
- **Hexagonal Ternary Diagram** — composites 6 triangular ternary plots that share a central element/combination into one hexagonal image
- **Plot Builder** — a generic chart builder: pick plot type, X/Y axis, color/group, and log scales from dropdowns populated from your data's own columns, with named presets you can save and reload
- **Analysis Log** — a running log of operations performed during the session

## 🏗️ Modular Architecture

The package is organized into logical modules, each handling specific functionality:

### Core
- **`R/dependencies.R`** — package dependency checking/loading
- **`R/config.R`** — configuration management (`ternary_config.json`)
- **`R/cache.R`**, **`R/cache_performance.R`** — in-memory result caching and performance monitoring
- **`R/options.R`** — app-wide options and constants
- **`R/file_management.R`** — file handling utilities

### Helpers
- **`R/helpers.R`**, **`R/helpers_filters.R`**, **`R/helpers_multivariate.R`**, **`R/helpers_reporting.R`**, **`R/helpers_validation.R`**

### Analysis
- **`R/multivariate.R`** — Mahalanobis distance, Isolation Forest
- **`R/statistical_filters.R`** — IQR / Z-score / MAD filtering (positive/high-value outliers only)
- **`R/comprehensive_analysis.R`**

### Ternary Plotting
- **`R/ternary_plot.R`**, **`R/ternary_plot_data_prep.R`**, **`R/ternary_plot_preview.R`**, **`R/ternary_plot_save.R`** — core single-triangle ternary plot pipeline
- **`R/hex_ternary_plot.R`** — hexagonal joint ternary diagram compositing

### Plotting Utilities
- **`R/plotting_utils.R`**, **`R/plotting_utils_compare.R`**, **`R/plotting_utils_multifile.R`** — chart builders behind "Multiple Plot Types"
- **`R/plotting_utils_builder.R`** — generic chart builder behind "Plot Builder"
- **`R/plot_builder_presets.R`** — load/save Plot Builder presets (`plot_builder_presets.json`)

### UI (Shiny)
- **`R/ui_components.R`** — page shell and main tabset
- **`R/ui_ternary_plots_tab.R`**, **`R/ui_data_comparison_tab.R`**, **`R/ui_multiple_plot_types_tab.R`**, **`R/ui_multiple_ternary_tab.R`**, **`R/ui_hex_ternary_tab.R`**, **`R/ui_plot_builder_tab.R`**, **`R/ui_data_export_tab.R`**, **`R/ui_analysis_log_tab.R`**

### Server (Shiny Backend)
- **`R/server_logic.R`** — wires every tab's server module together
- **`R/server_ternary_plots.R`** (+ `_batch.R`, `_groups.R`) — main and batch ternary plot handlers
- **`R/server_hex_ternary.R`** — hexagonal diagram handlers
- **`R/server_plot_types.R`** (+ `_scatter.R`, `_histogram.R`, `_boxplot.R`, `_violin.R`, `_connected.R`, `_stacked.R`) — per-chart-type handlers
- **`R/server_plot_builder.R`** — Plot Builder handlers
- **`R/server_filter_management.R`**, **`R/server_cache_management.R`**, **`R/server_file_handlers.R`**, **`R/server_directory_management.R`**
- **`R/server_export.R`** (+ `_data.R`, `_reports.R`)
- **`R/server_data_comparison.R`** (+ `_stats.R`, `_multivariate.R`, `_preview.R`)
- **`R/server_status_outputs.R`**, **`R/server_help_system.R`**, **`R/server_analysis_log.R`**, **`R/server_ui_coordination.R`**
- **`R/server_multiple_ternary.R`** — superseded by `server_ternary_plots_batch.R`; kept for reference but not wired up

### Application Entry
- **`R/app.R`** — `run_app()` / `create_app()`

## 🚀 Quick Start

### Installation

```r
# Install required packages first
install.packages(c("shiny", "openxlsx", "ggplot2", "DT", "corrplot", "GGally",
                    "Ternary", "PlotTools", "robustbase", "isotree",
                    "magick", "png", "rlang"))

# Install vidternary from GitHub
devtools::install_github("vidkudermarusic/vidternary")

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

## 📊 Features

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

### Data Quality Assessment
- Missing-value and outlier summaries
- Data quality reporting

### Export Capabilities
- Multiple plot formats (PNG, PDF, JPEG, TIFF)
- Data export (Excel, CSV, RDS, JSON)

## 🔧 Configuration

```r
config <- load_config()
set_config_value("plotting", "default_color_palette", "viridis")
save_config(config)
```

## 🧪 Testing

```r
library(testthat)
devtools::test()
```

## 📚 Documentation

- **Vignette**: `vignettes/vidternary-intro.Rmd`
- **Function help**: `?function_name` once the package is loaded
- **Sample data**: `testdata/test_data.xlsx` (see `testdata/README.md`)

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes following the modular structure above
4. Add tests in `tests/testthat/`
5. Submit a pull request

## 📄 License

This project is licensed under the MIT License — see the [LICENSE](LICENSE) file for details.

## 🆘 Support

For issues and questions, open an issue on the [GitHub repository](https://github.com/vidkudermarusic/vidternary).
