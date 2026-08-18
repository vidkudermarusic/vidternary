# Debug script to help identify plot saving issues
# Run this in your R console to enable debugging

# Enable debug mode
options(ternary.debug = TRUE)

cat("=== PLOT SAVING DEBUG SCRIPT ===\n")
cat("This script will help identify why plot saving is not working.\n\n")

cat("1. Enable debug mode: options(ternary.debug = TRUE)\n")
cat("2. Load your data files in the Shiny app\n")
cat("3. Select your elements (A, B, C) for both datasets\n")
cat("4. Try to save Plot 1 or Plot 2\n")
cat("5. Check the console output for debug messages\n\n")

cat("Common issues to check:\n")
cat("- Are the required elements (A, B, C) selected for both datasets?\n")
cat("- Are the file paths valid and accessible?\n")
cat("- Are there any error messages in the console?\n")
cat("- Is the output directory writable?\n\n")

cat("Debug messages to look for:\n")
cat("- 'DEBUG: Save Plot 1 button clicked'\n")
cat("- 'DEBUG: build_ternary_plot_params returned NULL'\n")
cat("- 'DEBUG: extract_ternary_params returning NULL'\n")
cat("- 'DEBUG: Invalid parameters for Plot 1'\n\n")

cat("If you see 'extract_ternary_params returning NULL', check:\n")
cat("- Missing required elements: element_A, element_B, element_C\n")
cat("- Invalid file path: xlsx_file is NULL or file doesn't exist\n\n")

cat("To test the fix:\n")
cat("1. Make sure you have uploaded files for both datasets\n")
cat("2. Select elements A, B, C for both datasets\n")
cat("3. Try saving Plot 1 or Plot 2\n")
cat("4. The debug messages should now show the file paths and element selections\n\n")

cat("If the issue persists, please share the debug output from the console.\n")
