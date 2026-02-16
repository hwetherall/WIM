# ==============================================================================
# WIM Case Study: 2009-10 Premier League Season
# "The Ancelotti Machine" - Chelsea's Record-Breaking Attack
# WIM Z-Score: +1.76 (Historically IMBALANCED - The Most Extreme Season)
# ==============================================================================

import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import os

# ==============================================================================
# 1. DATA: 2009-10 Premier League Final Standings
# ==============================================================================

# Source: FBRef / Premier League Official
data_2010 = {
    'Rank': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20],
    'Team': [
        'Chelsea', 'Manchester Utd', 'Arsenal', 'Tottenham', 'Manchester City',
        'Aston Villa', 'Liverpool', 'Everton', 'Birmingham City', 'Blackburn',
        'Stoke City', 'Fulham', 'Sunderland', 'Bolton', 'Wolves',
        'Wigan Athletic', 'West Ham', 'Burnley', 'Hull City', 'Portsmouth'
    ],
    'MP': [38]*20,
    'W': [27, 27, 23, 21, 18, 17, 18, 16, 13, 13, 11, 12, 11, 10, 10, 9, 8, 8, 6, 7],
    'D': [5, 4, 6, 7, 13, 13, 9, 13, 11, 11, 14, 10, 11, 9, 8, 9, 11, 6, 12, 7],
    'L': [6, 7, 9, 10, 7, 8, 11, 9, 14, 14, 13, 16, 16, 19, 20, 20, 19, 24, 20, 24],
    'GF': [103, 86, 83, 67, 73, 52, 61, 60, 38, 41, 34, 39, 48, 42, 32, 37, 47, 42, 34, 34],
    'GA': [32, 28, 41, 41, 45, 39, 35, 49, 47, 55, 48, 46, 56, 67, 56, 79, 66, 82, 75, 66],
    'Pts': [86, 85, 75, 70, 67, 64, 63, 61, 50, 50, 47, 46, 44, 39, 38, 36, 35, 30, 30, 19] # Portsmouth -9 pts deduction
}

df = pd.DataFrame(data_2010)

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

# Historical averages for context (from full analysis - 25 seasons 2001-2025)
EPL_HISTORICAL = {
    'WIM_Mean': 0.4161,
    'WIM_Std': 0.0602,
    'WIM_TB_Mean': 0.7099,
    'WIM_TB_Std': 0.0962,
    'NS_Mean': 1.9324,
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
print("CASE STUDY: 2009-10 PREMIER LEAGUE SEASON")
print("'The Ancelotti Machine' - Chelsea's Record-Breaking Attack")
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
print(f"Champion: Chelsea (86 pts)")
print(f"  -> Goals For: 103 (First team to break 100 since 1963)")
print(f"  -> Goal Difference: +71")
print(f"Points Gap (1st to 2nd): {df['Pts'].iloc[0] - df['Pts'].iloc[1]} points (Very close!)")
print(f"Points Gap (1st to 20th): {df['Pts'].iloc[0] - df['Pts'].iloc[-1]} points (Massive!)")

# Chelsea specific stats
chelsea = df[df['Team'] == 'Chelsea'].iloc[0]
print(f"\nChelsea Goal Ratio: {chelsea['Ratio']:.4f}")
print(f"Chelsea Log Ratio: {chelsea['Log_Ratio']:.4f}")

# Wigan specific stats (The 8-0 loss)
wigan = df[df['Team'] == 'Wigan Athletic'].iloc[0]
print(f"\nWigan Goal Ratio: {wigan['Ratio']:.4f}")
print(f"Wigan Log Ratio: {wigan['Log_Ratio']:.4f}")

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
The 2009-10 season had a WIM Z-Score of +1.76, making it the MOST IMBALANCED
season in our entire 25-year dataset - even more than Man City's 100-point year!

Why? Because it wasn't just one team.
- Chelsea scored 103 goals (+71 GD)
- Man Utd scored 86 goals (+58 GD)
- Arsenal scored 83 goals (+42 GD)

Meanwhile, the bottom was phenomenally weak:
- Wigan conceded 79 goals (including an 8-0 loss to Chelsea)
- Burnley conceded 82 goals
- Hull conceded 75 goals

This season represents "Peak Polarization" in the Premier League, where the
gap between the Top 3 and the Bottom 3 was at its absolute widest.
""")

# ==============================================================================
# 5. VISUALIZATION: The Polarization Chart
# ==============================================================================

fig, axes = plt.subplots(2, 2, figsize=(14, 12))

# Plot 1: Points For vs Against
ax1 = axes[0, 0]
colors = ['#034694' if team == 'Chelsea' else '#DA291C' if team == 'Manchester Utd' else '#0066cc' for team in df['Team']]
ax1.scatter(df['GF'], df['GA'], c=colors, s=100, alpha=0.7, edgecolors='black')
for i, team in enumerate(df['Team']):
    if team in ['Chelsea', 'Manchester Utd', 'Wigan Athletic', 'Burnley']:
        ax1.annotate(team, (df['GF'].iloc[i], df['GA'].iloc[i]), 
                    fontsize=8, ha='left', va='bottom')
ax1.plot([20, 110], [20, 110], 'k--', alpha=0.3, label='Balance Line (GF=GA)')
ax1.set_xlabel('Goals For', fontsize=11)
ax1.set_ylabel('Goals Against', fontsize=11)
ax1.set_title('Goals For vs Goals Against\n(Blue=Chelsea, Red=Man Utd)', fontsize=12)
ax1.legend()
ax1.grid(True, alpha=0.3)

# Plot 2: Log Ratio Distribution
ax2 = axes[0, 1]
sorted_log = df.sort_values('Log_Ratio', ascending=False)
colors2 = ['#034694' if team == 'Chelsea' else '#0066cc' for team in sorted_log['Team']]
bars = ax2.barh(range(20), sorted_log['Log_Ratio'], color=colors2, edgecolor='black')
ax2.set_yticks(range(20))
ax2.set_yticklabels(sorted_log['Team'], fontsize=9)
ax2.axvline(0, color='black', linewidth=1)
ax2.set_xlabel('Log(GF/GA)', fontsize=11)
ax2.set_title('Log Goal Ratio by Team\n(Blue = Chelsea)', fontsize=12)
ax2.grid(True, alpha=0.3, axis='x')

# Plot 3: Points Distribution
ax3 = axes[1, 0]
colors3 = ['#034694' if team == 'Chelsea' else '#0066cc' for team in df['Team']]
ax3.bar(range(20), df['Pts'], color=colors3, edgecolor='black')
ax3.set_xticks(range(20))
ax3.set_xticklabels(df['Team'], rotation=45, ha='right', fontsize=8)
ax3.set_ylabel('Points', fontsize=11)
ax3.set_title('Final Points Distribution\n(Blue = Chelsea)', fontsize=12)
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
bars1 = ax4.bar(x - width/2, season_vals, width, label='2009-10 Season', color='#034694', edgecolor='black')
bars2 = ax4.bar(x + width/2, historical_vals, width, label='EPL Historical Avg', color='#0066cc', edgecolor='black')
ax4.set_ylabel('Metric Value', fontsize=11)
ax4.set_title('2009-10 vs Historical Averages\n(Higher WIM = More Imbalanced)', fontsize=12)
ax4.set_xticks(x)
ax4.set_xticklabels(metrics)
ax4.legend()
ax4.grid(True, alpha=0.3, axis='y')

# Add Z-score annotations
for i, (bar, z) in enumerate(zip(bars1, [wim_z, wim_tb_z, ns_z])):
    ax4.annotate(f'Z={z:.2f}', (bar.get_x() + bar.get_width()/2, bar.get_height()),
                ha='center', va='bottom', fontsize=9, fontweight='bold')

plt.tight_layout()
plot_path = os.path.join(script_dir, '2010_EPL_Chelsea_Analysis.png')
plt.savefig(plot_path, dpi=300, bbox_inches='tight')
print(f"\nVisualization saved to: {plot_path}")

# ==============================================================================
# 6. SAVE DATA TO CSV
# ==============================================================================

csv_path = os.path.join(script_dir, '2010_EPL_Chelsea_Data.csv')
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
summary_path = os.path.join(script_dir, '2010_EPL_Chelsea_Summary.csv')
summary.to_csv(summary_path, index=False)
print(f"Summary saved to: {summary_path}")

print("\n" + "=" * 80)
print("ANALYSIS COMPLETE")
print("=" * 80)
