# Test Data

This directory contains test data files for the vidternary package.

## Files:
- `test_data.xlsx` - Sample dataset for testing ternary plot functionality

## Usage:
```r
# Load test data using readxl
library(readxl)
test_data <- read_excel("testdata/test_data.xlsx")

# Or use the file path directly in your functions
xlsx_file <- "testdata/test_data.xlsx"
```

## Notes:
- This data is provided for testing and development purposes
- File contains sample data suitable for ternary plot analysis
- Use this data to test the package functionality before using your own datasets

## Data Structure:
The Excel file should contain columns suitable for ternary plot analysis with appropriate element data.
