import pandas as pd
import os

# Define the mapping of sheet names to output file names
sheet_to_filename = {
    "AFL": "WIM Raw Data - AFL.csv",
    "Ligue 1": "WIM Raw Data - Ligue 1.csv",
    "Prem League": "WIM Raw Data - Prem League.csv"
}

# Paths: raw data lives in Data/Raw Data
script_dir = os.path.dirname(os.path.abspath(__file__))
raw_data_dir = os.path.join(script_dir, "Data", "Raw Data")
excel_file = os.path.join(raw_data_dir, "WIM Raw Data (1).xlsx")

# Read the Excel file
print(f"Reading Excel file: {excel_file}")

# Get all sheet names
excel_data = pd.ExcelFile(excel_file)
sheet_names = excel_data.sheet_names

print(f"Found {len(sheet_names)} sheets: {sheet_names}")

# Process each sheet
for sheet_name in sheet_names:
    if sheet_name in sheet_to_filename:
        # Read the sheet
        df = pd.read_excel(excel_file, sheet_name=sheet_name)
        
        # Get the output filename (save CSVs to same Raw Data folder)
        output_filename = sheet_to_filename[sheet_name]
        output_path = os.path.join(raw_data_dir, output_filename)
        
        # Save as CSV
        df.to_csv(output_path, index=False, encoding='utf-8-sig')
        print(f"✓ Saved '{sheet_name}' sheet to '{output_filename}' ({len(df)} rows)")
    else:
        print(f"⚠ Warning: Sheet '{sheet_name}' not found in mapping. Skipping.")

print("\nConversion complete!")
