# ==============================================================================
# WIM (Wetherall Imbalance Measure) - Analysis Suite
# Includes: WIM, Noll-Scully, Win % Std Dev, HHI
# ==============================================================================

import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')  # Non-interactive backend
import matplotlib.pyplot as plt
import re
import os
import glob

# ==============================================================================
# 1. METRIC CALCULATOR CORE
# ==============================================================================

def calculate_metrics(df: pd.DataFrame, league_name: str) -> pd.DataFrame:
    """
    Calculates WIM, Noll-Scully, Win % Std Dev, and HHI per season.
    """
    
    # --- A. Column Identification (Regex) ---
    cols = df.columns.tolist()
    
    # 1. Team Name
    team_col = next((c for c in cols if re.search(r'^(Team|Squad|Club)$', c, re.IGNORECASE)), None)
    
    # 2. Points For / Against (for WIM)
    pf_col = next((c for c in cols if re.search(r'^(For|GF|Points.?For|PF)$', c, re.IGNORECASE)), None)
    pa_col = next((c for c in cols if re.search(r'^(Agn|GA|Points.?Against|PA)$', c, re.IGNORECASE)), None)
    
    # 3. Wins / Games Played (for Noll-Scully)
    # Note: Look for exact 'W' or 'Wins' to avoid matching 'Draws'
    w_col = next((c for c in cols if re.search(r'^(W|Wins|Won)$', c, re.IGNORECASE)), None)
    
    # Note: Look for 'P', 'MP' (Matches Played), 'G' (Games), 'GP', 'Matches'
    gp_col = next((c for c in cols if re.search(r'^(P|MP|Games|Played|G|GP|Matches)$', c, re.IGNORECASE)), None)
    
    # Loss/Tie columns (for calculating Games Played if missing)
    l_col = next((c for c in cols if re.search(r'^(L|Loss|Losses|Lost)$', c, re.IGNORECASE)), None)
    t_col = next((c for c in cols if re.search(r'^(T|D|Ties|Draws)$', c, re.IGNORECASE)), None)
    
    # 4. Season/Year
    year_col = next((c for c in cols if re.search(r'Year|Season', c, re.IGNORECASE)), None)

    # Validate critical WIM columns
    if not all([team_col, pf_col, pa_col, year_col]):
        print(f"Skipping {league_name}: Missing core WIM columns (Team, PF, PA, or Year).")
        print(f"Found: Team={team_col}, PF={pf_col}, PA={pa_col}, Year={year_col}")
        return None

    # --- B. Data Cleaning ---
    # Create a working copy
    work_df = df.copy()
    
    # Clean numeric columns function
    def clean_numeric(col_name):
        if col_name:
            # Convert to string, coerce errors, drop NaNs
            work_df[col_name] = pd.to_numeric(work_df[col_name], errors='coerce')

    clean_numeric(pf_col)
    clean_numeric(pa_col)
    clean_numeric(w_col)
    clean_numeric(gp_col)
    clean_numeric(l_col)
    clean_numeric(t_col)
    clean_numeric(year_col)

    # Drop rows where critical data is NaN (e.g. headers in the middle of CSV)
    work_df = work_df.dropna(subset=[pf_col, pa_col, year_col])
    
    # Calculate Games Played if missing (e.g. NFL)
    if not gp_col and w_col and l_col:
        print(f"  -> Calculating Games Played from Wins/Losses/Ties for {league_name}")
        # Fill NaNs with 0 for calculation
        w_vals = work_df[w_col].fillna(0)
        l_vals = work_df[l_col].fillna(0)
        t_vals = work_df[t_col].fillna(0) if t_col else 0
        
        work_df['Calculated_GP'] = w_vals + l_vals + t_vals
        gp_col = 'Calculated_GP'

    # Handle Zeros for Logs (WIM specific fix)
    # If Points For or Against is 0, bump to 1 to avoid -inf
    work_df[pf_col] = work_df[pf_col].replace(0, 1)
    work_df[pa_col] = work_df[pa_col].replace(0, 1)

    # --- C. Calculation Loop (Per Season) ---
    results = []
    
    for season, group in work_df.groupby(year_col):
        season_stats = {
            'League': league_name,
            'Season': int(season),
            'Teams': len(group)
        }
        
        # 1. Calculate WIM
        # Formula: Mean(Abs(Ln(PF/PA)))
        ratios = group[pf_col] / group[pa_col]
        wim = np.mean(np.abs(np.log(ratios)))
        season_stats['WIM'] = wim
        
        # 2. Calculate Noll-Scully & Win % SD
        # Requires Wins and Games Played columns
        if w_col and gp_col and not group[w_col].isnull().all():
            wins = group[w_col]
            games = group[gp_col]
            
            # Win Percentage
            # Handle division by zero if games=0
            # Ensure float division by specifying dtype=float for the output array
            win_pct = np.divide(wins, games, out=np.zeros_like(wins, dtype=float), where=games!=0)
            
            # Actual Std Dev (ASD)
            # Use ddof=0 for Population Std Dev (standard in Noll-Scully papers)
            asd = np.std(win_pct, ddof=0)
            season_stats['WinPct_SD'] = asd
            
            # Ideal Std Dev (ISD)
            # Formula: 0.5 / sqrt(Games)
            # We use the average games played in the season to handle slight variances
            avg_games = games.mean()
            if avg_games > 0:
                isd = 0.5 / np.sqrt(avg_games)
                noll_scully = asd / isd
                season_stats['Noll_Scully'] = noll_scully
            else:
                season_stats['Noll_Scully'] = None
                
            # HHI (Herfindahl-Hirschman Index) of Wins
            # Share of total wins
            total_wins = wins.sum()
            if total_wins > 0:
                win_shares = wins / total_wins
                hhi = np.sum(win_shares ** 2)
                season_stats['HHI'] = hhi
            else:
                season_stats['HHI'] = None
                
        else:
            # If Win/Games data is missing, fill NaNs
            season_stats['WinPct_SD'] = None
            season_stats['Noll_Scully'] = None
            season_stats['HHI'] = None
            
        results.append(season_stats)
        
    return pd.DataFrame(results)

# ==============================================================================
# 2. MAIN EXECUTION
# ==============================================================================

def load_csv_smart(file_path):
    """
    Loads a CSV, attempting to find the correct header row by looking for 'Team' or 'Squad'.
    """
    try:
        # First, try reading normally
        df = pd.read_csv(file_path)
        
        # Check if 'Team' or 'Squad' is in columns
        cols = [str(c).lower() for c in df.columns]
        if any(x in cols for x in ['team', 'squad', 'club']):
            return df
            
        # If not, try to find the header in the first few rows
        # Read first 10 rows as raw data
        preview = pd.read_csv(file_path, header=None, nrows=10)
        
        header_row_idx = None
        for idx, row in preview.iterrows():
            row_str = row.astype(str).str.lower().tolist()
            if any('team' in x or 'squad' in x for x in row_str):
                header_row_idx = idx
                break
        
        if header_row_idx is not None:
            # Reload with correct header
            # Note: header=header_row_idx means 0-based index of the row to use as header
            return pd.read_csv(file_path, header=header_row_idx)
            
        return df
    except Exception as e:
        print(f"Error in smart load: {e}")
        return pd.read_csv(file_path) # Fallback

def main():
    script_dir = os.path.dirname(os.path.abspath(__file__))
    csv_files = glob.glob(os.path.join(script_dir, "*.csv"))
    
    if not csv_files:
        print("No CSV files found in the script directory.")
        return

    all_data = []
    
    print("Starting Analysis... (WIM + Noll-Scully + Win% SD)")
    print("-" * 60)

    for file_path in csv_files:
        # Infer league name from filename (e.g., "AFL.csv" -> "AFL")
        filename = os.path.basename(file_path)
        
        # Clean up filename for display
        league_name = filename.replace('.csv', '').replace('WIM Raw Data (1).xlsx - ', '').strip()
        
        print(f"Processing: {league_name}")
        
        try:
            df = load_csv_smart(file_path)
            metric_df = calculate_metrics(df, league_name)
            
            if metric_df is not None:
                all_data.append(metric_df)
                print(f"  -> Processed {len(metric_df)} seasons.")
            else:
                print("  -> Failed to calculate metrics.")
                
        except Exception as e:
            print(f"  -> Error reading file: {e}")

    if not all_data:
        print("No valid data processed.")
        return

    # Combine all results
    final_df = pd.concat(all_data, ignore_index=True)
    
    # ==========================================================================
    # 3. REPORTING
    # ==========================================================================
    
    # Sort for cleaner viewing
    final_df = final_df.sort_values(['League', 'Season'])
    
    print("\n" + "=" * 80)
    print("FULL RESULTS TABLE")
    print("=" * 80)
    # Formatting helper for cleaner output
    pd.set_option('display.max_rows', None)
    pd.set_option('display.float_format', '{:.4f}'.format)
    
    # Reorder columns for logical reading
    cols_order = ['League', 'Season', 'Teams', 'WIM', 'Noll_Scully', 'WinPct_SD', 'HHI']
    print(final_df[cols_order].to_string(index=False))
    
    # Save results to CSV
    output_path = os.path.join(script_dir, "wim_full_results.csv")
    final_df.to_csv(output_path, index=False)
    print(f"\nResults saved to: {output_path}")

    # ==========================================================================
    # 4. CORRELATION CHECK (The "Hypothesis 3" Test)
    # ==========================================================================
    
    print("\n" + "=" * 80)
    print("HYPOTHESIS 3 TEST: CORRELATION MATRIX")
    print("=" * 80)
    
    # Calculate correlation between WIM and the Competitors
    # We drop NaNs in case some leagues didn't have Win/Games data
    corr_matrix = final_df[['WIM', 'Noll_Scully', 'WinPct_SD']].corr()
    print(corr_matrix)
    
    print("-" * 80)
    wim_ns_corr = corr_matrix.loc['WIM', 'Noll_Scully']
    print(f"Correlation (WIM vs Noll-Scully): {wim_ns_corr:.4f}")
    
    if wim_ns_corr > 0.95:
        print(">> WARNING: Very high correlation. WIM might be redundant.")
    elif 0.5 < wim_ns_corr < 0.90:
        print(">> SWEET SPOT: Strong positive correlation (validity), but distinct enough to add value.")
    else:
        print(">> INTERESTING: Low or Negative correlation. WIM is measuring something totally different.")

    # ==========================================================================
    # 5. VISUALIZATION (Comparison Plot)
    # ==========================================================================
    
    plt.figure(figsize=(12, 7))
    
    # Group by League and plot WIM averages
    # We can add Noll-Scully as a secondary axis or separate chart later
    # For now, let's stick to the 10-year WIM trend
    
    leagues = final_df['League'].unique()
    markers = ['o', 's', '^', 'D', 'v', '<', '>']
    
    for i, league in enumerate(leagues):
        subset = final_df[final_df['League'] == league]
        plt.plot(subset['Season'], subset['WIM'], 
                 label=league, 
                 marker=markers[i % len(markers)], 
                 linewidth=2)
    
    plt.title('WIM (Wetherall Imbalance Measure) - 10 Year Trend', fontsize=14)
    plt.ylabel('WIM Score (Higher = More Unbalanced)', fontsize=12)
    plt.xlabel('Season', fontsize=12)
    plt.axhline(0, color='black', linewidth=1)
    plt.legend()
    plt.grid(True, alpha=0.3)
    
    plot_path = os.path.join(script_dir, 'WIM_Chart.png')
    plt.savefig(plot_path, dpi=300)
    print(f"Chart saved to: {plot_path}")

if __name__ == "__main__":
    main()
