# ==============================================================================
# WIM Case Study: 2015-16 Premier League Season
# "The Leicester City Miracle" - 5000:1 Odds Champions
# WIM Z-Score: -1.02 (One Standard Deviation BELOW Mean = Unusually Balanced)
# ==============================================================================

import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import os

# ==============================================================================
# 1. DATA: 2015-16 Premier League Final Standings
# ==============================================================================

# Source: FBRef / Premier League Official
data_2016 = {
    'Rank': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20],
    'Team': [
        'Leicester City', 'Arsenal', 'Tottenham', 'Manchester City', 'Manchester Utd',
        'Southampton', 'West Ham', 'Liverpool', 'Stoke City', 'Chelsea',
        'Everton', 'Swansea City', 'Watford', 'West Brom', 'Crystal Palace',
        'Bournemouth', 'Sunderland', 'Newcastle Utd', 'Norwich City', 'Aston Villa'
    ],
    'MP': [38]*20,
    'W': [23, 20, 19, 19, 19, 18, 16, 16, 14, 12, 11, 12, 12, 10, 11, 11, 9, 9, 9, 3],
    'D': [12, 11, 13, 9, 9, 9, 14, 12, 9, 14, 14, 11, 9, 13, 9, 9, 12, 10, 7, 8],
    'L': [3, 7, 6, 10, 10, 11, 8, 10, 15, 12, 13, 15, 17, 15, 18, 18, 17, 19, 22, 27],
    'GF': [68, 65, 69, 71, 49, 59, 65, 63, 41, 59, 59, 42, 40, 34, 39, 45, 48, 44, 39, 27],
    'GA': [36, 36, 35, 41, 35, 41, 51, 50, 55, 53, 55, 52, 50, 48, 51, 67, 62, 65, 67, 76],
    'Pts': [81, 71, 70, 66, 66, 63, 62, 60, 51, 50, 47, 47, 45, 43, 42, 42, 39, 37, 34, 17]
}

df = pd.DataFrame(data_2016)

# ==============================================================================
# 2. CALCULATE WIM METRICS
# ==============================================================================

# Points Ratio (GF/GA)
df['Ratio'] = df['GF'] / df['GA']

# Log Ratio
df['Log_Ratio'] = np.log(df['Ratio'])

# Absolute Log Ratio (for WIM)
df['Abs_Log_Ratio'] = np.abs(df['Log_Ratio'])

# Win Percentage
df['Win_Pct'] = df['W'] / df['MP']

# Calculate WIM
wim = df['Abs_Log_Ratio'].mean()

# Calculate WIM-TB (Top 4 + Bottom 4)
sorted_df = df.sort_values('Ratio', ascending=False)
top_4 = sorted_df.head(4)
bottom_4 = sorted_df.tail(4)
wim_tb = pd.concat([top_4, bottom_4])['Abs_Log_Ratio'].mean()

# Noll-Scully
asd = df['Win_Pct'].std(ddof=0)  # Actual SD
isd = 0.5 / np.sqrt(38)  # Ideal SD
noll_scully = asd / isd

# Historical averages for context (from full analysis)
EPL_HISTORICAL = {
    'WIM_Mean': 0.4390,
    'WIM_Std': 0.0496,
    'WIM_TB_Mean': 0.7507,
    'WIM_TB_Std': 0.0962,
    'NS_Mean': 2.0311,
    'NS_Std': 0.2175
}

# Z-Scores
wim_z = (wim - EPL_HISTORICAL['WIM_Mean']) / EPL_HISTORICAL['WIM_Std']
wim_tb_z = (wim_tb - EPL_HISTORICAL['WIM_TB_Mean']) / EPL_HISTORICAL['WIM_TB_Std']
ns_z = (noll_scully - EPL_HISTORICAL['NS_Mean']) / EPL_HISTORICAL['NS_Std']

# ==============================================================================
# 3. OUTPUT DIRECTORY SETUP
# ==============================================================================

script_dir = os.path.dirname(os.path.abspath(__file__))

# ==============================================================================
# 4. CONSOLE OUTPUT
# ==============================================================================

print("=" * 80)
print("CASE STUDY: 2015-16 PREMIER LEAGUE SEASON")
print("'The Leicester City Miracle'")
print("=" * 80)

print("\n" + "-" * 80)
print("FINAL STANDINGS WITH WIM METRICS")
print("-" * 80)

display_cols = ['Rank', 'Team', 'Pts', 'GF', 'GA', 'Ratio', 'Log_Ratio', 'Abs_Log_Ratio']
pd.set_option('display.float_format', '{:.4f}'.format)
print(df[display_cols].to_string(index=False))

print("\n" + "-" * 80)
print("SEASON SUMMARY STATISTICS")
print("-" * 80)
print(f"Champion: Leicester City (81 pts) - 5000:1 Pre-Season Odds")
print(f"Points Gap (1st to 2nd): {df['Pts'].iloc[0] - df['Pts'].iloc[1]} points")
print(f"Points Gap (1st to 20th): {df['Pts'].iloc[0] - df['Pts'].iloc[-1]} points")

print("\n" + "-" * 80)
print("WIM ANALYSIS")
print("-" * 80)
print(f"WIM (Wetherall Imbalance Measure): {wim:.4f}")
print(f"  -> EPL Historical Mean: {EPL_HISTORICAL['WIM_Mean']:.4f}")
print(f"  -> Z-Score: {wim_z:.2f} standard deviations from mean")

print(f"\nWIM-TB (Top 4 + Bottom 4 Only): {wim_tb:.4f}")
print(f"  -> EPL Historical Mean: {EPL_HISTORICAL['WIM_TB_Mean']:.4f}")
print(f"  -> Z-Score: {wim_tb_z:.2f} standard deviations from mean")

print(f"\nNoll-Scully Ratio: {noll_scully:.4f}")
print(f"  -> EPL Historical Mean: {EPL_HISTORICAL['NS_Mean']:.4f}")
print(f"  -> Z-Score: {ns_z:.2f} standard deviations from mean")

print("\n" + "-" * 80)
print("KEY INSIGHT")
print("-" * 80)
print("""
The 2015-16 Premier League season had a WIM Z-Score of -1.02, meaning it was
ONE FULL STANDARD DEVIATION more balanced than the typical EPL season.

This quantifies what the football world witnessed: Leicester City's championship
wasn't just a fairy tale - the entire league was unusually competitive that year.

The traditional "Big 6" underperformed while mid-table teams overperformed,
creating a flatter distribution of goal differentials than any recent season.

WIM captures this phenomenon that pure standings cannot show: it wasn't just
about Leicester winning - it was about EVERYONE being closer together.
""")

# ==============================================================================
# 5. VISUALIZATION 1: Goal Ratio Distribution
# ==============================================================================

fig, axes = plt.subplots(2, 2, figsize=(14, 12))

# Plot 1: Points For vs Against
ax1 = axes[0, 0]
colors = ['#00ff00' if team == 'Leicester City' else '#0066cc' for team in df['Team']]
ax1.scatter(df['GF'], df['GA'], c=colors, s=100, alpha=0.7, edgecolors='black')
for i, team in enumerate(df['Team']):
    if team in ['Leicester City', 'Aston Villa', 'Manchester City', 'Arsenal']:
        ax1.annotate(team, (df['GF'].iloc[i], df['GA'].iloc[i]), 
                    fontsize=8, ha='left', va='bottom')
ax1.plot([20, 80], [20, 80], 'k--', alpha=0.3, label='Balance Line (GF=GA)')
ax1.set_xlabel('Goals For', fontsize=11)
ax1.set_ylabel('Goals Against', fontsize=11)
ax1.set_title('Goals For vs Goals Against\n(Green = Leicester City)', fontsize=12)
ax1.legend()
ax1.grid(True, alpha=0.3)

# Plot 2: Log Ratio Distribution
ax2 = axes[0, 1]
sorted_log = df.sort_values('Log_Ratio', ascending=False)
colors2 = ['#00ff00' if team == 'Leicester City' else '#0066cc' for team in sorted_log['Team']]
bars = ax2.barh(range(20), sorted_log['Log_Ratio'], color=colors2, edgecolor='black')
ax2.set_yticks(range(20))
ax2.set_yticklabels(sorted_log['Team'], fontsize=9)
ax2.axvline(0, color='black', linewidth=1)
ax2.set_xlabel('Log(GF/GA)', fontsize=11)
ax2.set_title('Log Goal Ratio by Team\n(Green = Leicester City)', fontsize=12)
ax2.grid(True, alpha=0.3, axis='x')

# Plot 3: Points Distribution
ax3 = axes[1, 0]
colors3 = ['#00ff00' if team == 'Leicester City' else '#cc0000' if team == 'Aston Villa' else '#0066cc' 
           for team in df['Team']]
ax3.bar(range(20), df['Pts'], color=colors3, edgecolor='black')
ax3.set_xticks(range(20))
ax3.set_xticklabels(df['Team'], rotation=45, ha='right', fontsize=8)
ax3.set_ylabel('Points', fontsize=11)
ax3.set_title('Final Points Distribution\n(Green=Leicester, Red=Aston Villa)', fontsize=12)
ax3.axhline(df['Pts'].mean(), color='orange', linestyle='--', label=f'Mean: {df["Pts"].mean():.1f}')
ax3.legend()
ax3.grid(True, alpha=0.3, axis='y')

# Plot 4: WIM Comparison
ax4 = axes[1, 1]
metrics = ['WIM', 'WIM-TB', 'Noll-Scully']
season_vals = [wim, wim_tb, noll_scully]
historical_vals = [EPL_HISTORICAL['WIM_Mean'], EPL_HISTORICAL['WIM_TB_Mean'], EPL_HISTORICAL['NS_Mean']]

x = np.arange(len(metrics))
width = 0.35
bars1 = ax4.bar(x - width/2, season_vals, width, label='2015-16 Season', color='#00ff00', edgecolor='black')
bars2 = ax4.bar(x + width/2, historical_vals, width, label='EPL Historical Avg', color='#0066cc', edgecolor='black')
ax4.set_ylabel('Metric Value', fontsize=11)
ax4.set_title('2015-16 vs Historical Averages\n(Lower WIM = More Balanced)', fontsize=12)
ax4.set_xticks(x)
ax4.set_xticklabels(metrics)
ax4.legend()
ax4.grid(True, alpha=0.3, axis='y')

# Add Z-score annotations
for i, (bar, z) in enumerate(zip(bars1, [wim_z, wim_tb_z, ns_z])):
    ax4.annotate(f'Z={z:.2f}', (bar.get_x() + bar.get_width()/2, bar.get_height()),
                ha='center', va='bottom', fontsize=9, fontweight='bold')

plt.tight_layout()
plot_path = os.path.join(script_dir, '2016_EPL_Leicester_Analysis.png')
plt.savefig(plot_path, dpi=300, bbox_inches='tight')
print(f"\nVisualization saved to: {plot_path}")

# ==============================================================================
# 6. SAVE DATA TO CSV
# ==============================================================================

csv_path = os.path.join(script_dir, '2016_EPL_Leicester_Data.csv')
df.to_csv(csv_path, index=False)
print(f"Data saved to: {csv_path}")

# Summary stats CSV
summary = pd.DataFrame({
    'Metric': ['WIM', 'WIM_TB', 'Noll_Scully', 'WIM_Z', 'WIM_TB_Z', 'NS_Z', 
               'Points_Gap_1st_2nd', 'Points_Gap_1st_20th', 'Champion_Points'],
    'Value': [wim, wim_tb, noll_scully, wim_z, wim_tb_z, ns_z,
              df['Pts'].iloc[0] - df['Pts'].iloc[1], 
              df['Pts'].iloc[0] - df['Pts'].iloc[-1],
              df['Pts'].iloc[0]]
})
summary_path = os.path.join(script_dir, '2016_EPL_Leicester_Summary.csv')
summary.to_csv(summary_path, index=False)
print(f"Summary saved to: {summary_path}")

print("\n" + "=" * 80)
print("ANALYSIS COMPLETE")
print("=" * 80)
