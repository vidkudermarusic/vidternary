
# Vidternary: Modular Ternary Plot Analysis Tool

> **⚠️ In Development**  
> Currently only the **Ternary plot** tab and **Multiple ternary plots** tab have full functionality. Other features are under development.

A comprehensive R package for creating ternary plots with advanced filtering and multivariate analysis capabilities. Built with a modular architecture for maintainability and extensibility. 

## 🏗️ Modular Architecture

The package is organized into logical modules, each handling specific functionality:

### Core Modules

- **`R/dependencies.R`** - Package dependency management and loading
- **`R/config.R`** - Configuration management and settings
- **`R/cache.R`** - Caching system for performance optimization
- **`R/helpers.R`** - Utility functions for data processing and validation
- **`R/options.R`** - App-wide options and constants
- **`R/file_management.R`** - File handling and management utilities

### Analysis Modules

- **`R/multivariate.R`** - Multivariate analysis (Mahalanobis distance, isolation forest)
- **`R/statistical_filters.R`** - Statistical filtering methods (IQR, Z-score, MAD)
- **`R/ternary_plot.R`** - Core ternary plot generation functionality
- **`R/comprehensive_analysis.R`** - Comprehensive data analysis functions

### Visualization Modules

- **`R/plotting_utils.R`** - Plotting utilities and formatting functions
- **`R/ui_components.R`** - Shiny UI components and layouts

### Server Modules (Shiny Backend)

- **`R/server_logic.R`** - Main Shiny server-side logic and reactive functionality
- **`R/server_ternary_plots.R`** - Ternary plot server logic
- **`R/server_multiple_ternary.R`** - Multiple ternary plots server logic
- **`R/server_plot_types.R`** - Plot types server logic
- **`R/server_filter_management.R`** - Filter management server logic
- **`R/server_cache_management.R`** - Cache management server logic
- **`R/server_file_handlers.R`** - File handling server logic
- **`R/server_export.R`** - Export functionality server logic
- **`R/server_directory_management.R`** - Directory management server logic
- **`R/server_data_comparison.R`** - Data comparison server logic
- **`R/server_status_outputs.R`** - Status and outputs server logic
- **`R/server_help_system.R`** - Help system server logic
- **`R/server_analysis_log.R`** - Analysis logging server logic
- **`R/server_ui_coordination.R`** - UI coordination server logic

### Application Module

- **`R/app.R`** - Main application file that brings everything together

## 🚀 Quick Start

### Installation

#### Option 1: Install from GitHub (Recommended)
```r
# Install required packages first
install.packages(c("shiny", "openxlsx", "ggplot2", "DT", "corrplot", "GGally", "Ternary", "PlotTools", "robustbase", "isotree"))

# Install vidternary from GitHub
devtools::install_github("VAŠ_USERNAME/vidternary")

# Load and run the application
library(vidternary)
run_app()
```

#### Option 2: Local Development
```r
# Clone or download the repository
# Set working directory to the package folder

# Install required packages
install.packages(c("shiny", "openxlsx", "ggplot2", "DT", "corrplot", "GGally", "Ternary", "PlotTools", "robustbase", "isotree"))

# Load the package
source("R/app.R")

# Run the application
run_app()
```

### Basic Usage

#### Interactive Application
```r
# Run the full interactive Shiny application
run_app()

# Or create app object for custom deployment
app <- create_app()
shiny::runApp(app)
```

#### Programmatic Usage
```r
# Load the package
library(vidternary)

# Initialize packages (loads all dependencies)
initialize_packages()

# Create a simple ternary plot
result <- general_ternary_plot(
  xlsx_file = "your_data.xlsx",
  element_A = list(col = "SiO2", filter = NULL),
  element_B = list(col = "Al2O3", filter = NULL),
  element_C = list(col = "Fe2O3", filter = NULL),
  output_dir = "output/",
  color_palette = "viridis"
)
```

## 📊 Features

### Ternary Plot Generation
- Support for multiple element columns
- Automatic normalization and coordinate calculation
- Customizable color palettes and point styling

### Advanced Filtering
- **Statistical Methods**: IQR, Z-score, MAD
- **Multivariate Methods**: Mahalanobis distance, isolation forest
- **Individual Element Filters**: Apply different filters to each element
- **Reference Dataset Support**: Compare against different datasets

### Data Quality Assessment
- Automatic data validation
- Missing value handling
- Outlier detection and management
- Data quality reporting

### Performance Optimization
- Intelligent caching system
- Progress tracking
- Memory-efficient data handling
- Debug logging capabilities

### Export Capabilities
- Multiple output formats (PNG, PDF, JPEG, TIFF)
- Data export (Excel, CSV, RDS, JSON)
- Comprehensive metadata inclusion

## 🔧 Configuration

The package uses a configuration system for customizable settings:

```r
# Load configuration
config <- load_config()

# Modify settings
set_config_value("plotting.default_color_palette", "viridis")
set_config_value("analysis.default_lambda", 1.5)

# Save configuration
save_config(config)
```

## 📈 Multivariate Analysis

### Mahalanobis Distance
```r
# Standard Mahalanobis distance
result <- compute_mahalanobis_distance(
  data = your_data,
  reference_data = reference_dataset,
  lambda = 1,
  omega = 0
)
```


### Isolation Forest
```r
# Isolation Forest outlier detection
result <- compute_isolation_forest(
  data = your_data,
  reference_data = reference_dataset,
  contamination = 0.1
)
```

## 🎨 Plotting Utilities

### Create Custom Plots
```r
# Ternary plot with custom styling
plot <- create_ternary_plot(
  ternary_points = your_points,
  color_values = your_colors,
  point_size = 3,
  alpha = 0.8,
  color_palette = "viridis"
)

# Apply consistent theme
plot <- apply_consistent_theme(plot, "minimal")

# Save plot
save_plot(plot, "my_plot", "output/", formats = c("png", "pdf"))
```

### Statistical Visualizations
```r
# Correlation matrix
cor_matrix <- create_correlation_plot(
  data = your_data,
  method = "pearson",
  type = "upper"
)

# Distribution plots
hist_plot <- create_histogram_density(
  data = your_data,
  column = "SiO2"
)
```

## 🧪 Testing

Run the test suite:

```r
# Load testthat
library(testthat)

# Run tests
test_dir("tests/")
```

## 📝 Development

### Adding New Modules

1. Create a new R file in the `R/` directory
2. Follow the naming convention: `module_name.R`
3. Add exports to `NAMESPACE`
4. Update `R/app.R` to source the new module
5. Add tests in `tests/testthat/`

### Module Structure Template

```r
# ---- Module Name ----
# Brief description of the module's purpose

# Main function
main_function <- function(param1, param2, ...) {
  # Function implementation
}

# Helper functions
helper_function <- function(x) {
  # Helper implementation
}

# Export functions
if (!exists("main_function")) {
  main_function <- main_function
}
```

## 🔍 Debugging

Enable debug mode for detailed logging:

```r
# Enable debug mode
options(ternary.debug = TRUE)

# Or through the UI
# Check "Enable Debug Mode" in the Settings tab
```

## 📚 Documentation

- **Vignettes**: `vignettes/vidternary-intro.Rmd`
- **Function Help**: Use `?function_name` for detailed documentation
- **Examples**: See `examples/` directory for usage examples

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes following the modular structure
4. Add tests for new functionality
5. Submit a pull request

## 📄 License

This project is licensed under the MIT License - see the LICENSE file for details.

## 🆘 Support

For issues and questions:
- Check the documentation and examples
- Review the debug logs
- Open an issue on the repository
- Contact the maintainers

## 🔄 Version History

- **v1.1.0** - Package cleanup and optimization (September 3, 2025)
- **v1.0.0** - Initial modular release with comprehensive functionality
- **v0.9.0** - Beta version with core modules
- **v0.8.0** - Alpha version with basic structure

---

## 📋 **Recent Updates (September 3, 2025)**

### **Package Cleanup Completed:**
- ✅ **Removed duplicate files:** `R/server_cache.R` (functionality moved to `server_cache_management.R`)
- ✅ **Cleaned temporary files:** `test_debug.R`, `test_functionality.R`, `Rplots.pdf`
- ✅ **Removed empty directories:** `test_directory/`
- ✅ **Total space freed:** ~531KB

### **Current Package Status:**
- **Total R files:** 28 (optimized from 29)
- **Package structure:** Clean and organized
- **All functionality:** Intact and improved
- **Documentation:** Comprehensive and up-to-date

---

**Note**: This package is designed to be modular and extensible. Each module can be used independently or as part of the complete application. The modular structure makes it easy to maintain, test, and extend the functionality.
