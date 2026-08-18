# Comprehensive debugging script for plot saving issues
# This script will help identify exactly why plot saving is not working

cat("=== COMPREHENSIVE PLOT SAVING DEBUG SCRIPT ===\n\n")

cat("STEP 1: Enable comprehensive debugging\n")
cat("Run this in your R console:\n")
cat("options(ternary.debug = TRUE)\n\n")

cat("STEP 2: Test the Shiny app with these steps:\n")
cat("1. Load your Shiny app\n")
cat("2. Upload Excel files for both Dataset 1 and Dataset 2\n")
cat("3. Select elements A, B, C for both datasets\n")
cat("4. Try to save Plot 1, Plot 2, or Both Plots\n")
cat("5. Check the console output for debug messages\n\n")

cat("STEP 3: Look for these specific debug messages:\n")
cat("=== When you click Save Plot 1 ===\n")
cat("- '=== SAVE PLOT 1 BUTTON CLICKED ==='\n")
cat("- 'DEBUG: input$xlsx_file1 exists: TRUE/FALSE'\n")
cat("- 'DEBUG: input$element_A1 exists: TRUE/FALSE'\n")
cat("- 'DEBUG: File path: [path]'\n")
cat("- 'DEBUG: File exists: TRUE/FALSE'\n")
cat("- 'DEBUG: build_ternary_plot_params called for dataset 1'\n")
cat("- 'DEBUG: extract_ternary_params returned: NULL/valid parameters'\n\n")

cat("=== Common Issues to Check ===\n")
cat("1. File Upload Issues:\n")
cat("   - Are both Excel files uploaded successfully?\n")
cat("   - Do you see 'Dataset 1 loaded successfully!' and 'Dataset 2 loaded successfully!' messages?\n")
cat("   - Check if 'DEBUG: File exists: TRUE' appears\n\n")

cat("2. Element Selection Issues:\n")
cat("   - Are elements A, B, C selected for both datasets?\n")
cat("   - Check if 'DEBUG: input$element_A1 exists: TRUE' (and B1, C1, A2, B2, C2)\n")
cat("   - Are the element selections not NULL or empty?\n\n")

cat("3. Parameter Extraction Issues:\n")
cat("   - Look for 'DEBUG: extract_ternary_params returned: NULL'\n")
cat("   - If NULL, check for these specific error messages:\n")
cat("     * 'DEBUG: extract_ternary_params returning NULL - missing required elements'\n")
cat("     * 'DEBUG: extract_ternary_params returning NULL - invalid file path'\n\n")

cat("4. File Path Issues:\n")
cat("   - Check if 'DEBUG: xlsx_file: [path]' shows a valid path\n")
cat("   - Verify 'DEBUG: file.exists: TRUE' appears\n")
cat("   - Ensure the file path is not NULL\n\n")

cat("STEP 4: Common Solutions\n")
cat("If you see 'missing required elements':\n")
cat("- Make sure you have selected at least one element for A, B, and C\n")
cat("- Check that the element selections are not empty\n\n")

cat("If you see 'invalid file path':\n")
cat("- Try re-uploading the Excel files\n")
cat("- Check that the files are valid Excel (.xlsx) files\n")
cat("- Ensure the files are not corrupted\n\n")

cat("If you see 'extract_ternary_params returned: NULL':\n")
cat("- Check both the file upload and element selection issues above\n")
cat("- Look for any error messages in the console\n\n")

cat("STEP 5: Manual Test\n")
cat("Try this manual test to verify the fix:\n")
cat("1. options(ternary.debug = TRUE)\n")
cat("2. Upload a simple Excel file with numeric data\n")
cat("3. Select 3 different numeric columns for A, B, C\n")
cat("4. Click 'Save Plot 1'\n")
cat("5. Check console output for the debug messages above\n\n")

cat("STEP 6: Report Results\n")
cat("Please share the console output showing:\n")
cat("- The debug messages from when you click the save button\n")
cat- Any error messages that appear\n")
cat("- Whether the file upload and element selection debug messages show TRUE\n\n")

cat("This comprehensive debugging should identify exactly where the plot saving is failing.\n")
