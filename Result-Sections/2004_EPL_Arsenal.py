# ==============================================================================
# WIM Case Study: 2003-04 Premier League Season
# "The Invincibles" - Arsenal's Undefeated Season
# WIM Z-Score: -1.78 (Historically BALANCED)
# ==============================================================================

import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import os

# ==============================================================================
# 1. DATA: 2003-04 Premier League Final Standings
# ==============================================================================

# Source: FBRef / Premier League Official
data_2004 = {
    'Rank': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20],
    'Team': [
        'Arsenal', 'Chelsea', 'Manchester Utd', 'Liverpool', 'Newcastle Utd',
        'Aston Villa', 'Charlton Ath', 'Bolton', 'Fulham', 'Birmingham City',
        'Middlesbrough', 'Southampton', 'Portsmouth', 'Tottenham', 'Blackburn',
        'Manchester City', 'Everton', 'Leicester City', 'Leeds United', 'Wolves'
    ],
    'MP': [38]*20,
    'W': [26, 24, 23, 16, 13, 15, 14, 14, 14, 12, 13, 12, 12, 13, 12, 9, 9, 6, 8, 7],
    'D': [12, 7, 6, 12, 17, 11, 11, 11, 10, 14, 9, 11, 9, 6, 8, 14, 12, 15, 9, 12],
    'L': [0, 7, 9, 10, 8, 12, 13, 13, 14, 12, 16, 15, 17, 19, 18, 15, 17, 17, 21, 19],
    'GF': [73, 67, 64, 55, 52, 48, 51, 48, 52, 43, 44, 44, 47, 47, 51, 55, 45, 48, 40, 38],
    'GA': [26, 30, 35, 37, 40, 44, 51, 56, 46, 48, 52, 45, 54, 57, 59, 54, 57, 65, 79, 77],
    'Pts': [90, 79, 75, 60, 56, 56, 53, 53, 52, 50, 48, 47, 45, 45, 44, 41, 39, 33, 33, 33]
}

df = pd.DataFrame(data_2004)

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
print("CASE STUDY: 2003-04 PREMIER LEAGUE SEASON")
print("'The Invincibles' - Arsenal's Undefeated Season")
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
print(f"Champion: Arsenal (90 pts) - 0 Losses")
print(f"  -> Goals For: 73")
print(f"  -> Goal Difference: +47")
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
The 2003-04 'Invincibles' season had a WIM Z-Score of -1.78, making it the
MOST BALANCED season in our 25-year dataset.

This is counter-intuitive! We usually associate "Invincibles" with dominance.
However, WIM reveals that while Arsenal didn't lose, they didn't crush teams
by massive margins (GD +47 vs Man City's +79 in 2018).

Furthermore, the rest of the league was incredibly tight. The bottom teams
were competitive (Wolves 33 pts vs Aston Villa 17 pts in 2016).

WIM proves that Arsenal's achievement was navigating a highly competitive
field without losing, rather than simply overpowering weak opposition.
""")

# ==============================================================================
# 5. VISUALIZATION: The Balance of Power
# ==============================================================================

fig, axes = plt.subplots(2, 2, figsize=(14, 12))

# Plot 1: Points For vs Against
ax1 = axes[0, 0]
colors = ['#EF0107' if team == 'Arsenal' else '#0066cc' for team in df['Team']]
ax1.scatter(df['GF'], df['GA'], c=colors, s=100, alpha=0.7, edgecolors='black')
for i, team in enumerate(df['Team']):
    if team in ['Arsenal', 'Chelsea', 'Leeds United', 'Wolves']:
        ax1.annotate(team, (df['GF'].iloc[i], df['GA'].iloc[i]), 
                    fontsize=8, ha='left', va='bottom')
ax1.plot([20, 80], [20, 80], 'k--', alpha=0.3, label='Balance Line (GF=GA)')
ax1.set_xlabel('Goals For', fontsize=11)
ax1.set_ylabel('Goals Against', fontsize=11)
ax1.set_title('Goals For vs Goals Against\n(Red = Arsenal)', fontsize=12)
ax1.legend()
ax1.grid(True, alpha=0.3)

# Plot 2: Log Ratio Distribution
ax2 = axes[0, 1]
sorted_log = df.sort_values('Log_Ratio', ascending=False)
colors2 = ['#EF0107' if team == 'Arsenal' else '#0066cc' for team in sorted_log['Team']]
bars = ax2.barh(range(20), sorted_log['Log_Ratio'], color=colors2, edgecolor='black')
ax2.set_yticks(range(20))
ax2.set_yticklabels(sorted_log['Team'], fontsize=9)
ax2.axvline(0, color='black', linewidth=1)
ax2.set_xlabel('Log(GF/GA)', fontsize=11)
ax2.set_title('Log Goal Ratio by Team\n(Red = Arsenal)', fontsize=12)
ax2.grid(True, alpha=0.3, axis='x')

# Plot 3: Points Distribution
ax3 = axes[1, 0]
colors3 = ['#EF0107' if team == 'Arsenal' else '#0066cc' for team in df['Team']]
ax3.bar(range(20), df['Pts'], color=colors3, edgecolor='black')
ax3.set_xticks(range(20))
ax3.set_xticklabels(df['Team'], rotation=45, ha='right', fontsize=8)
ax3.set_ylabel('Points', fontsize=11)
ax3.set_title('Final Points Distribution\n(Red = Arsenal)', fontsize=12)
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
bars1 = ax4.bar(x - width/2, season_vals, width, label='2003-04 Season', color='#EF0107', edgecolor='black')
bars2 = ax4.bar(x + width/2, historical_vals, width, label='EPL Historical Avg', color='#0066cc', edgecolor='black')
ax4.set_ylabel('Metric Value', fontsize=11)
ax4.set_title('2003-04 vs Historical Averages\n(Lower WIM = More Balanced)', fontsize=12)
ax4.set_xticks(x)
ax4.set_xticklabels(metrics)
ax4.legend()
ax4.grid(True, alpha=0.3, axis='y')

# Add Z-score annotations
for i, (bar, z) in enumerate(zip(bars1, [wim_z, wim_tb_z, ns_z])):
    ax4.annotate(f'Z={z:.2f}', (bar.get_x() + bar.get_width()/2, bar.get_height()),
                ha='center', va='bottom', fontsize=9, fontweight='bold')

plt.tight_layout()
plot_path = os.path.join(script_dir, '2004_EPL_Arsenal_Analysis.png')
plt.savefig(plot_path, dpi=300, bbox_inches='tight')
print(f"\nVisualization saved to: {plot_path}")

# ==============================================================================
# 6. SAVE DATA TO CSV
# ==============================================================================

csv_path = os.path.join(script_dir, '2004_EPL_Arsenal_Data.csv')
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
summary_path = os.path.join(script_dir, '2004_EPL_Arsenal_Summary.csv')
summary.to_csv(summary_path, index=False)
print(f"Summary saved to: {summary_path}")

print("\n" + "=" * 80)
print("ANALYSIS COMPLETE")
print("=" * 80)
