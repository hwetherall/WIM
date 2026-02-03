# ==============================================================================
# WIM Case Study: 2017-18 Premier League Season
# "The Centurions" - Manchester City's 100-Point Record Season
# WIM Z-Score: +1.26 (1.26 Standard Deviations ABOVE Mean = Unusually Dominant)
# ==============================================================================

import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import os

# ==============================================================================
# 1. DATA: 2017-18 Premier League Final Standings
# ==============================================================================

# Source: FBRef / Premier League Official
data_2018 = {
    'Rank': [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20],
    'Team': [
        'Manchester City', 'Manchester Utd', 'Tottenham', 'Liverpool', 'Chelsea',
        'Arsenal', 'Burnley', 'Everton', 'Leicester City', 'Newcastle Utd',
        'Crystal Palace', 'Bournemouth', 'West Ham', 'Watford', 'Brighton',
        'Huddersfield', 'Southampton', 'Swansea City', 'Stoke City', 'West Brom'
    ],
    'MP': [38]*20,
    'W': [32, 25, 23, 21, 21, 19, 14, 13, 12, 12, 11, 11, 10, 11, 9, 9, 7, 8, 7, 6],
    'D': [4, 6, 8, 12, 7, 6, 12, 10, 11, 8, 11, 11, 12, 8, 13, 10, 15, 9, 12, 13],
    'L': [2, 7, 7, 5, 10, 13, 12, 15, 15, 18, 16, 16, 16, 19, 16, 19, 16, 21, 19, 19],
    'GF': [106, 68, 74, 84, 62, 74, 36, 44, 56, 39, 45, 45, 48, 44, 34, 28, 37, 28, 35, 31],
    'GA': [27, 28, 36, 38, 38, 51, 39, 58, 60, 47, 55, 61, 68, 64, 54, 58, 56, 56, 68, 56],
    'Pts': [100, 81, 77, 75, 70, 63, 54, 49, 47, 44, 44, 44, 42, 41, 40, 37, 36, 33, 33, 31]
}

df = pd.DataFrame(data_2018)

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
print("CASE STUDY: 2017-18 PREMIER LEAGUE SEASON")
print("'The Centurions' - Manchester City's Record 100-Point Season")
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
print(f"Champion: Manchester City (100 pts) - RECORD")
print(f"  -> Goals For: 106 (RECORD)")
print(f"  -> Goal Difference: +79 (RECORD)")
print(f"Points Gap (1st to 2nd): {df['Pts'].iloc[0] - df['Pts'].iloc[1]} points")
print(f"Points Gap (1st to 20th): {df['Pts'].iloc[0] - df['Pts'].iloc[-1]} points")

# Man City specific stats
mancity = df[df['Team'] == 'Manchester City'].iloc[0]
print(f"\nManchester City Goal Ratio: {mancity['Ratio']:.4f}")
print(f"Manchester City Log Ratio: {mancity['Log_Ratio']:.4f}")
print(f"  -> This is {mancity['Log_Ratio'] / df['Log_Ratio'].std():.2f} std devs from league mean")

print("\n" + "-" * 80)
print("WIM ANALYSIS")
print("-" * 80)
print(f"WIM (Wetherall Imbalance Measure): {wim:.4f}")
print(f"  -> EPL Historical Mean: {EPL_HISTORICAL['WIM_Mean']:.4f}")
print(f"  -> Z-Score: {wim_z:+.2f} standard deviations from mean")

print(f"\nWIM-TB (Top 4 + Bottom 4 Only): {wim_tb:.4f}")
print(f"  -> EPL Historical Mean: {EPL_HISTORICAL['WIM_TB_Mean']:.4f}")
print(f"  -> Z-Score: {wim_tb_z:+.2f} standard deviations from mean")

print(f"\nNoll-Scully Ratio: {noll_scully:.4f}")
print(f"  -> EPL Historical Mean: {EPL_HISTORICAL['NS_Mean']:.4f}")
print(f"  -> Z-Score: {ns_z:+.2f} standard deviations from mean")

print("\n" + "-" * 80)
print("KEY INSIGHT")
print("-" * 80)
print("""
The 2017-18 Premier League season had a WIM Z-Score of +1.26, meaning it was
1.26 STANDARD DEVIATIONS MORE IMBALANCED than the typical EPL season.

This quantifies Pep Guardiola's Manchester City dominance:
- 100 points (RECORD)
- 106 goals scored (RECORD)  
- +79 goal difference (RECORD)
- 19 points clear of 2nd place

The WIM captures what traditional metrics hint at: City's Goal Ratio of 3.93
(106 GF / 27 GA) was so extreme that it pulled the entire league's WIM upward.

CONTRAST WITH LEICESTER 2016:
- Leicester 2016: WIM Z = -1.02 (unusually BALANCED)
- Man City 2018: WIM Z = +1.26 (unusually IMBALANCED)

These two seasons represent OPPOSITE ends of the competitive balance spectrum,
and WIM correctly identifies both as statistical outliers.
""")

# ==============================================================================
# 5. VISUALIZATION 1: Goal Ratio Distribution
# ==============================================================================

fig, axes = plt.subplots(2, 2, figsize=(14, 12))

# Plot 1: Points For vs Against
ax1 = axes[0, 0]
colors = ['#6CABDD' if team == 'Manchester City' else '#0066cc' for team in df['Team']]
sizes = [200 if team == 'Manchester City' else 80 for team in df['Team']]
ax1.scatter(df['GF'], df['GA'], c=colors, s=sizes, alpha=0.7, edgecolors='black')
for i, team in enumerate(df['Team']):
    if team in ['Manchester City', 'West Brom', 'Stoke City', 'Liverpool']:
        ax1.annotate(team, (df['GF'].iloc[i], df['GA'].iloc[i]), 
                    fontsize=8, ha='left', va='bottom')
ax1.plot([20, 110], [20, 110], 'k--', alpha=0.3, label='Balance Line (GF=GA)')
ax1.set_xlabel('Goals For', fontsize=11)
ax1.set_ylabel('Goals Against', fontsize=11)
ax1.set_title('Goals For vs Goals Against\n(Light Blue = Manchester City)', fontsize=12)
ax1.legend()
ax1.grid(True, alpha=0.3)

# Plot 2: Log Ratio Distribution
ax2 = axes[0, 1]
sorted_log = df.sort_values('Log_Ratio', ascending=False)
colors2 = ['#6CABDD' if team == 'Manchester City' else '#cc0000' if sorted_log['Log_Ratio'].iloc[i] < 0 else '#0066cc' 
           for i, team in enumerate(sorted_log['Team'])]
bars = ax2.barh(range(20), sorted_log['Log_Ratio'], color=colors2, edgecolor='black')
ax2.set_yticks(range(20))
ax2.set_yticklabels(sorted_log['Team'], fontsize=9)
ax2.axvline(0, color='black', linewidth=2)
ax2.set_xlabel('Log(GF/GA)', fontsize=11)
ax2.set_title('Log Goal Ratio by Team\n(Red = Negative, Blue = Positive)', fontsize=12)
ax2.grid(True, alpha=0.3, axis='x')

# Highlight Man City's extreme position
mancity_idx = list(sorted_log['Team']).index('Manchester City')
ax2.annotate(f'Ratio: {mancity["Ratio"]:.2f}', 
            (sorted_log['Log_Ratio'].iloc[mancity_idx], mancity_idx),
            xytext=(10, 0), textcoords='offset points',
            fontsize=9, fontweight='bold', color='#6CABDD')

# Plot 3: Points Distribution with Gap Visualization
ax3 = axes[1, 0]
colors3 = ['#6CABDD' if team == 'Manchester City' else '#cc0000' if df['Pts'].iloc[i] < 35 else '#0066cc' 
           for i, team in enumerate(df['Team'])]
bars3 = ax3.bar(range(20), df['Pts'], color=colors3, edgecolor='black')
ax3.set_xticks(range(20))
ax3.set_xticklabels(df['Team'], rotation=45, ha='right', fontsize=8)
ax3.set_ylabel('Points', fontsize=11)
ax3.set_title('Final Points Distribution\n(Light Blue=Man City, Red=Relegated)', fontsize=12)
ax3.axhline(df['Pts'].mean(), color='orange', linestyle='--', label=f'Mean: {df["Pts"].mean():.1f}')
ax3.legend()
ax3.grid(True, alpha=0.3, axis='y')

# Add 19-point gap annotation
ax3.annotate('', xy=(0, 100), xytext=(1, 81),
            arrowprops=dict(arrowstyle='<->', color='red', lw=2))
ax3.annotate('19 pts gap', xy=(0.5, 90), fontsize=10, color='red', fontweight='bold')

# Plot 4: WIM Comparison with Historical + Leicester 2016
ax4 = axes[1, 1]
metrics = ['WIM', 'WIM-TB', 'Noll-Scully']
season_2018_vals = [wim, wim_tb, noll_scully]
historical_vals = [EPL_HISTORICAL['WIM_Mean'], EPL_HISTORICAL['WIM_TB_Mean'], EPL_HISTORICAL['NS_Mean']]
# Leicester 2016 values (from the other file)
leicester_2016_vals = [0.3885, 0.6024, 1.5636]

x = np.arange(len(metrics))
width = 0.25
bars1 = ax4.bar(x - width, season_2018_vals, width, label='2017-18 (Man City)', color='#6CABDD', edgecolor='black')
bars2 = ax4.bar(x, historical_vals, width, label='EPL Historical Avg', color='#888888', edgecolor='black')
bars3 = ax4.bar(x + width, leicester_2016_vals, width, label='2015-16 (Leicester)', color='#00ff00', edgecolor='black')

ax4.set_ylabel('Metric Value', fontsize=11)
ax4.set_title('2017-18 vs 2015-16 vs Historical\n(Higher WIM = More Imbalanced)', fontsize=12)
ax4.set_xticks(x)
ax4.set_xticklabels(metrics)
ax4.legend()
ax4.grid(True, alpha=0.3, axis='y')

# Add Z-score annotations
for i, (bar, z) in enumerate(zip(bars1, [wim_z, wim_tb_z, ns_z])):
    ax4.annotate(f'Z={z:+.2f}', (bar.get_x() + bar.get_width()/2, bar.get_height()),
                ha='center', va='bottom', fontsize=8, fontweight='bold', color='#6CABDD')

plt.tight_layout()
plot_path = os.path.join(script_dir, '2018_EPL_ManCity_Analysis.png')
plt.savefig(plot_path, dpi=300, bbox_inches='tight')
print(f"\nVisualization saved to: {plot_path}")

# ==============================================================================
# 6. VISUALIZATION 2: The Dominance Gap
# ==============================================================================

fig2, ax = plt.subplots(figsize=(12, 8))

# Create a waterfall-style chart showing the gap from Man City
gap_from_city = df['Pts'].iloc[0] - df['Pts']
colors_gap = ['#6CABDD' if team == 'Manchester City' else 
              '#ffcc00' if gap < 20 else 
              '#ff6600' if gap < 40 else '#cc0000' 
              for team, gap in zip(df['Team'], gap_from_city)]

bars = ax.barh(range(19, -1, -1), gap_from_city, color=colors_gap, edgecolor='black')
ax.set_yticks(range(19, -1, -1))
ax.set_yticklabels(df['Team'], fontsize=10)
ax.set_xlabel('Points Behind Manchester City', fontsize=12)
ax.set_title('2017-18 Premier League: The Dominance Gap\nPoints Behind Manchester City (100 pts)', fontsize=14)
ax.axvline(0, color='black', linewidth=2)

# Add point annotations
for i, (gap, team) in enumerate(zip(gap_from_city, df['Team'])):
    if gap > 0:
        ax.annotate(f'-{int(gap)}', (gap + 0.5, 19-i), va='center', fontsize=9)

ax.grid(True, alpha=0.3, axis='x')
plt.tight_layout()

gap_plot_path = os.path.join(script_dir, '2018_EPL_ManCity_DominanceGap.png')
plt.savefig(gap_plot_path, dpi=300, bbox_inches='tight')
print(f"Dominance Gap visualization saved to: {gap_plot_path}")

# ==============================================================================
# 7. SAVE DATA TO CSV
# ==============================================================================

csv_path = os.path.join(script_dir, '2018_EPL_ManCity_Data.csv')
df.to_csv(csv_path, index=False)
print(f"Data saved to: {csv_path}")

# Summary stats CSV
summary = pd.DataFrame({
    'Metric': ['WIM', 'WIM_TB', 'Noll_Scully', 'WIM_Z', 'WIM_TB_Z', 'NS_Z', 
               'Points_Gap_1st_2nd', 'Points_Gap_1st_20th', 'Champion_Points',
               'Champion_Goals_For', 'Champion_Goals_Against', 'Champion_Goal_Ratio'],
    'Value': [wim, wim_tb, noll_scully, wim_z, wim_tb_z, ns_z,
              df['Pts'].iloc[0] - df['Pts'].iloc[1], 
              df['Pts'].iloc[0] - df['Pts'].iloc[-1],
              df['Pts'].iloc[0],
              mancity['GF'], mancity['GA'], mancity['Ratio']]
})
summary_path = os.path.join(script_dir, '2018_EPL_ManCity_Summary.csv')
summary.to_csv(summary_path, index=False)
print(f"Summary saved to: {summary_path}")

print("\n" + "=" * 80)
print("ANALYSIS COMPLETE")
print("=" * 80)
