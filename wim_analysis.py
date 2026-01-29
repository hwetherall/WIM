# ==============================================================================
# WIM (Wetherall Imbalance Measure) - The Flight Pack
# Author: The Flying Economist (feat. Gemini) - Python port
# Date: Dec 2025
# ==============================================================================

import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')  # Use non-interactive backend for terminal compatibility
import matplotlib.pyplot as plt
import re
import os

# ==============================================================================
# 1. THE WIM CALCULATOR FUNCTION
# ==============================================================================

def calculate_wim(df: pd.DataFrame, league_name: str) -> pd.DataFrame:
    """
    Takes a raw dataframe, cleans it, and returns the WIM score per season.
    
    WIM = Mean Absolute Log-Ratio of Points For / Points Against
    A lower WIM indicates a more balanced league.
    """
    
    # A. Identify the Team column (it's usually 'Team' or 'Squad')
    team_col = None
    for col in df.columns:
        if re.search(r'Team|Squad', col, re.IGNORECASE):
            team_col = col
            break
    
    if team_col is None:
        print(f"Warning: Could not find team column for {league_name}")
        return None
    
    # B. Basic Cleaning - Remove header rows embedded in data
    # Filter out rows where team column contains "Team", "Squad", "Ladder", or is empty
    clean_df = df[~df[team_col].astype(str).str.contains(
        r'Team|Squad|Ladder|Rk|^#$', case=False, na=True, regex=True
    )].copy()
    clean_df = clean_df[clean_df[team_col].notna()]
    clean_df = clean_df[clean_df[team_col].astype(str).str.strip() != '']
    
    # C. Column Mapping (The "Rosetta Stone")
    # Find Points For: 'For', 'GF', 'PointsFor'
    for_col = None
    for col in clean_df.columns:
        if re.search(r'^For$|^GF$|^Points.?For', col, re.IGNORECASE):
            for_col = col
            break
    
    # Find Points Against: 'Agn', 'GA', 'PointsAgainst'
    agn_col = None
    for col in clean_df.columns:
        if re.search(r'^Agn$|^GA$|^Points.?Against', col, re.IGNORECASE):
            agn_col = col
            break
    
    # Find Year/Season column
    year_col = None
    for col in clean_df.columns:
        if re.search(r'Year|Season', col, re.IGNORECASE):
            year_col = col
            break
    
    # Check if we found the necessary columns
    if for_col is None or agn_col is None:
        print(f"Warning: Could not find scoring columns for {league_name}")
        print(f"  Available columns: {list(clean_df.columns)}")
        return None
    
    if year_col is None:
        print(f"Warning: Could not find year/season column for {league_name}")
        return None
    
    print(f"Processing {league_name}:")
    print(f"  Team column: {team_col}")
    print(f"  Points For column: {for_col}")
    print(f"  Points Against column: {agn_col}")
    print(f"  Year column: {year_col}")
    
    # D. Data Conversion
    # Remove commas and convert to numeric
    def clean_numeric(val):
        if pd.isna(val):
            return np.nan
        val_str = str(val).replace(',', '')
        try:
            return float(val_str)
        except ValueError:
            return np.nan
    
    clean_df['PF'] = clean_df[for_col].apply(clean_numeric)
    clean_df['PA'] = clean_df[agn_col].apply(clean_numeric)
    clean_df['Season'] = clean_df[year_col].apply(clean_numeric)
    
    # Remove rows with invalid data (0 or NaN values to avoid log errors)
    clean_df = clean_df[(clean_df['PF'] > 0) & (clean_df['PA'] > 0)]
    clean_df = clean_df[clean_df['Season'].notna()]
    
    # E. The Calculation
    # WIM = mean(abs(log(PF / PA))) per season
    wim_results = clean_df.groupby('Season').apply(
        lambda x: pd.Series({
            'WIM': np.mean(np.abs(np.log(x['PF'] / x['PA']))),
            'Teams': len(x),
            'League': league_name
        }),
        include_groups=False
    ).reset_index()
    
    # Convert Season to int for cleaner display
    wim_results['Season'] = wim_results['Season'].astype(int)
    
    print(f"  Found {len(wim_results)} seasons\n")
    
    return wim_results


# ==============================================================================
# 2. LOAD AND PROCESS DATA
# ==============================================================================

def load_csv_smart(file_path: str) -> pd.DataFrame:
    """
    Loads a CSV file, handling cases where the first row is a season header
    and the actual column names are in the second row.
    """
    # First, try reading normally
    df = pd.read_csv(file_path)
    
    # Check if we got proper column names (look for Team, Squad, Rk, etc.)
    has_team_col = any(re.search(r'Team|Squad', str(col), re.IGNORECASE) for col in df.columns)
    
    if not has_team_col:
        # The first row might be a season header - check the second row for actual headers
        # Read again, using the second row (index 1) as header and skip the first row
        df = pd.read_csv(file_path, header=1)
        
        # If still no team column, the file might need row 0 as header after all
        has_team_col = any(re.search(r'Team|Squad', str(col), re.IGNORECASE) for col in df.columns)
        
        if not has_team_col:
            # Fall back to original read
            df = pd.read_csv(file_path)
    
    return df


def main():
    # Define the files to process
    files = {
        "AFL": "WIM Raw Data - AFL.csv",
        "Ligue 1": "WIM Raw Data - Ligue 1.csv",
        "Premier League": "WIM Raw Data - Prem League.csv",
        "A-League": "WIM Raw Data - A-League.csv"
    }
    
    # Get the directory where this script is located
    script_dir = os.path.dirname(os.path.abspath(__file__))
    
    all_wim_data = []
    
    for league, filename in files.items():
        file_path = os.path.join(script_dir, filename)
        
        if os.path.exists(file_path):
            print(f"Loading {league}...")
            
            # Read CSV with smart header detection
            raw_data = load_csv_smart(file_path)
            
            # Calculate WIM
            league_wim = calculate_wim(raw_data, league)
            
            if league_wim is not None:
                all_wim_data.append(league_wim)
        else:
            print(f"Warning: File not found: {file_path}")
    
    # Combine all results
    if not all_wim_data:
        print("Error: No data was processed successfully.")
        return
    
    combined_df = pd.concat(all_wim_data, ignore_index=True)
    
    # ==========================================================================
    # 3. VISUALIZATION - The "Money Plot"
    # ==========================================================================
    
    plt.figure(figsize=(12, 8))
    
    # Define colors for each league
    colors = {
        'AFL': '#1f77b4',
        'Ligue 1': '#ff7f0e', 
        'Premier League': '#2ca02c'
    }
    
    for league in combined_df['League'].unique():
        league_data = combined_df[combined_df['League'] == league].sort_values('Season')
        plt.plot(
            league_data['Season'], 
            league_data['WIM'],
            marker='o',
            markersize=8,
            linewidth=2,
            label=league,
            color=colors.get(league, None)
        )
    
    plt.ylim(bottom=0)
    plt.xlabel('Season', fontsize=12)
    plt.ylabel('WIM Score (Mean Log-Ratio Deviation)', fontsize=12)
    plt.title('The Wetherall Imbalance Measure (WIM)', fontsize=16, fontweight='bold')
    plt.suptitle('Comparing Competitive Balance Across Leagues (Lower is Better)', fontsize=11, y=0.92)
    
    # Add caption as text at bottom
    plt.figtext(0.5, 0.01, '0 = Perfectly Balanced League', ha='center', fontsize=10, style='italic')
    
    plt.legend(loc='best', fontsize=10)
    plt.grid(True, alpha=0.3)
    plt.tight_layout(rect=[0, 0.03, 1, 0.95])
    
    # Save the plot
    plot_path = os.path.join(script_dir, 'WIM_Plot.png')
    plt.savefig(plot_path, dpi=300, bbox_inches='tight')
    print(f"\nPlot saved as '{plot_path}'")
    
    # ==========================================================================
    # 4. PRINT SUMMARY TABLE
    # ==========================================================================
    
    print("\n" + "=" * 50)
    print("WIM Results Summary")
    print("=" * 50)
    
    # Sort by League and Season for display
    summary = combined_df.sort_values(['League', 'Season'])
    print(summary.to_string(index=False))
    
    # Additional summary statistics
    print("\n" + "=" * 50)
    print("Average WIM by League (All Seasons)")
    print("=" * 50)
    avg_by_league = combined_df.groupby('League')['WIM'].agg(['mean', 'std', 'min', 'max'])
    avg_by_league.columns = ['Mean WIM', 'Std Dev', 'Min WIM', 'Max WIM']
    print(avg_by_league.to_string())
    
    return combined_df


if __name__ == "__main__":
    results = main()
