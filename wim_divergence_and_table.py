# ==============================================================================
# wim_divergence_and_table.py
#
# Two outputs for the journal submission:
#
# 1. DIVERGENCE ANALYSIS
#    For every league-season, compute |WIM_Z - NS_Z|.
#    High divergence = the two metrics are measuring genuinely different things.
#    This IS the empirical proof of WIM's independent contribution.
#    Also computes within-league r(WIM_Z, NS_Z) per league — the measure of
#    how much additional information WIM provides beyond Noll-Scully.
#
# 2. PUBLICATION-QUALITY SUMMARY TABLE
#    Table 1 for the paper: 16 leagues, all key metrics, formatted for print.
#
# Key literature framing:
#   - Gerrard & Kringstad (2022): multi-dimensionality is recognised within
#     outcome space. WIM adds a margin-dominance dimension orthogonal to both.
#   - Owen & King (2015): season-length bias in NS. WIM is scale-invariant.
#   - Deb (2022): closest neighbour — differentiate via cross-sport generality
#     and log-ratio aggregation.
#
# Input:  Data/Clean Data/wim_full_results.csv
# Output: Data/Clean Data/WIM_Divergence_Analysis.csv
#         Data/Clean Data/WIM_Divergence_Plot.png
#         Data/Clean Data/WIM_Summary_Table.csv
#         Data/Clean Data/WIM_Summary_Table.png   (print-ready figure)
# ==============================================================================

import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from matplotlib.gridspec import GridSpec
from scipy import stats
import os

SCRIPT_DIR  = os.path.dirname(os.path.abspath(__file__))
RESULTS_CSV = os.path.join(SCRIPT_DIR, 'Data', 'Clean Data', 'wim_full_results.csv')
OUT_DIR     = os.path.join(SCRIPT_DIR, 'Data', 'Clean Data')

BASKETBALL = {'NBA', 'NBL', 'Euroleague'}
MLB_FLAGGED = {'MLB'}

SPORT_MAP = {
    'Prem League':      'Football',
    'Bundesliga':       'Football',
    'La Liga':          'Football',
    'Ligue 1':          'Football',
    'Eredivisie':       'Football',
    'EFL Championship': 'Football',
    'MLS':              'Football',
    'A-League':         'Football',
    'AFL':              'Australian Rules',
    'NRL':              'Rugby League',
    'NFL':              'American Football',
    'NHL':              'Ice Hockey',
    'MLB':              'Baseball*',
    'NBA':              'Basketball',
    'NBL':              'Basketball',
    'Euroleague':       'Basketball',
}

SPORT_COLORS = {
    'Football':          '#1a6faf',
    'Australian Rules':  '#2e8b57',
    'Rugby League':      '#5b9e5b',
    'American Football': '#c0392b',
    'Ice Hockey':        '#e67e22',
    'Baseball*':         '#95a5a6',
    'Basketball':        '#7b2d8b',
}

def short(raw):
    return str(raw).replace('WIM Raw Data - ', '').strip()


# ==============================================================================
# LOAD
# ==============================================================================

def load():
    df = pd.read_csv(RESULTS_CSV)
    df['League_Short'] = df['League'].apply(short)
    df['Sport']        = df['League_Short'].map(SPORT_MAP).fillna('Other')
    df = df[df['Teams'] >= 5].dropna(subset=['WIM_Z', 'NS_Z']).copy()
    df['Divergence']   = (df['WIM_Z'] - df['NS_Z']).abs()
    return df


# ==============================================================================
# 1A. DIVERGENCE ANALYSIS — console output
# ==============================================================================

def divergence_analysis(df):
    print('\n' + '='*70)
    print('DIVERGENCE ANALYSIS: |WIM_Z - NS_Z|')
    print('='*70)
    print('High divergence = the two metrics are measuring different things.')
    print('The quadrant a season falls in determines the *direction* of divergence.\n')

    # ── Within-league r(WIM_Z, NS_Z) ─────────────────────────────────────────
    print('--- Within-League Correlation r(WIM_Z, NS_Z) ---')
    print('  (near 0 = metrics are independent; near 1 = metrics agree strongly)\n')

    corr_rows = []
    for league in sorted(df['League_Short'].unique()):
        sub = df[df['League_Short'] == league]
        if len(sub) < 4:
            r, p = np.nan, np.nan
        else:
            r, p = stats.pearsonr(sub['WIM_Z'], sub['NS_Z'])
        corr_rows.append({
            'League':  league,
            'Sport':   SPORT_MAP.get(league, ''),
            'N':       len(sub),
            'r':       r,
            'p':       p,
            'mean_div': sub['Divergence'].mean(),
        })

    corr_df = pd.DataFrame(corr_rows).sort_values('r')
    for _, row in corr_df.iterrows():
        flag = '  ** LOW CORRELATION — high WIM info gain **' if (
            not np.isnan(row['r']) and abs(row['r']) < 0.3) else ''
        p_str = f"p={row['p']:.3f}" if not np.isnan(row['p']) else 'n/a'
        print(f"  {row['League']:<20} r={row['r']:+.3f}  {p_str}  "
              f"n={row['N']}  mean|div|={row['mean_div']:.3f}{flag}")

    # ── Top 25 most divergent seasons overall ────────────────────────────────
    print('\n--- Top 25 Most Divergent Seasons (|WIM_Z - NS_Z|) ---')
    top = df.nlargest(25, 'Divergence').copy()
    top['Direction'] = top.apply(
        lambda r: 'WIM > NS  (High Margin, Low Outcome)'  if r['WIM_Z'] > r['NS_Z']
             else 'NS > WIM  (High Outcome, Low Margin)',
        axis=1
    )
    print(top[['League_Short','Season','WIM_Z','NS_Z','Divergence','Direction']].to_string(
        index=False, float_format='{:.3f}'.format))

    # ── By sport: average divergence ─────────────────────────────────────────
    print('\n--- Average Divergence by Sport ---')
    sport_div = df.groupby('Sport')['Divergence'].agg(['mean','max','count'])
    sport_div = sport_div.sort_values('mean', ascending=False)
    print(sport_div.round(3).to_string())

    # ── Save divergence CSV ───────────────────────────────────────────────────
    out = os.path.join(OUT_DIR, 'WIM_Divergence_Analysis.csv')
    df.sort_values('Divergence', ascending=False)[
        ['League_Short','Sport','Season','WIM','WIM_Z','Noll_Scully','NS_Z',
         'Divergence','Teams']
    ].to_csv(out, index=False)
    print(f'\nDivergence table saved: {out}')

    return corr_df, top


# ==============================================================================
# 1B. DIVERGENCE PLOT — two panels
# ==============================================================================

def plot_divergence(df, top25):
    """
    Panel A: Bar chart of within-league mean |WIM_Z - NS_Z|, coloured by sport.
    Panel B: Strip plot — top-25 divergent seasons labelled on a WIM_Z vs NS_Z axis.
    """
    fig = plt.figure(figsize=(16, 10))
    gs  = GridSpec(1, 2, figure=fig, width_ratios=[1, 1.4], wspace=0.35)
    ax1 = fig.add_subplot(gs[0])
    ax2 = fig.add_subplot(gs[1])

    # ── Panel A: mean divergence per league ───────────────────────────────────
    league_div = (df.groupby(['League_Short','Sport'])['Divergence']
                    .mean()
                    .reset_index()
                    .sort_values('Divergence', ascending=True))

    colors = [SPORT_COLORS.get(s, '#888') for s in league_div['Sport']]
    bars   = ax1.barh(league_div['League_Short'], league_div['Divergence'],
                      color=colors, edgecolor='white', linewidth=0.5, height=0.7)

    # Value labels
    for bar, val in zip(bars, league_div['Divergence']):
        ax1.text(val + 0.01, bar.get_y() + bar.get_height()/2,
                 f'{val:.2f}', va='center', fontsize=8)

    ax1.set_xlabel('Mean |WIM_Z - NS_Z|  per Season', fontsize=10)
    ax1.set_title('Average WIM–NS Divergence\nby League', fontsize=11, pad=10)
    ax1.axvline(df['Divergence'].mean(), color='black', linewidth=1,
                linestyle='--', alpha=0.5, label=f'Overall mean ({df["Divergence"].mean():.2f})')
    ax1.legend(fontsize=8)
    ax1.grid(axis='x', alpha=0.2)

    # Sport legend
    sport_patches = [mpatches.Patch(color=SPORT_COLORS[s], label=s, alpha=0.9)
                     for s in SPORT_COLORS if s in league_div['Sport'].values]
    ax1.legend(handles=sport_patches, loc='lower right', fontsize=7.5,
               title='Sport', title_fontsize=8)

    # ── Panel B: scatter of top-25 seasons ───────────────────────────────────
    # Background quadrant shading
    for (xmin, xmax, ymin, ymax, color) in [
        (0, 4,  0, 4,  '#fff3e0'),   # TR: High WIM, High NS
        (-4, 0, 0, 4,  '#fde8e8'),   # TL: High WIM, Low NS
        (-4, 0, -4, 0, '#e8f5e9'),   # BL: Low WIM, Low NS
        (0, 4,  -4, 0, '#e3f2fd'),   # BR: Low WIM, High NS
    ]:
        ax2.fill_between([xmin, xmax], [ymin, ymin], [ymax, ymax],
                         color=color, alpha=0.5, zorder=0)

    # All background points (faded)
    ax2.scatter(df['NS_Z'], df['WIM_Z'],
                c=[SPORT_COLORS.get(s, '#ccc') for s in df['Sport']],
                s=18, alpha=0.25, zorder=1)

    # Top-25 highlighted
    for _, row in top25.iterrows():
        sport = SPORT_MAP.get(row['League_Short'], 'Other')
        c = SPORT_COLORS.get(sport, '#333')
        ax2.scatter(row['NS_Z'], row['WIM_Z'],
                    color=c, s=90, edgecolors='black', linewidths=0.8,
                    zorder=5, alpha=0.9)

        # Label: league + year
        label = f"{row['League_Short'][:4]} '{str(int(row['Season']))[2:]}"
        # Offset to avoid overlaps
        dx = 0.12 if row['NS_Z'] < 1.5 else -0.12
        dy = 0.12 if row['WIM_Z'] < 1.5 else -0.12
        ax2.annotate(label,
                     xy=(row['NS_Z'], row['WIM_Z']),
                     xytext=(row['NS_Z'] + dx, row['WIM_Z'] + dy),
                     fontsize=6.5, color='#333',
                     arrowprops=dict(arrowstyle='-', color='#aaa', lw=0.5),
                     bbox=dict(boxstyle='round,pad=0.15', fc='white',
                               ec='none', alpha=0.7),
                     zorder=10)

    ax2.axhline(0, color='black', linewidth=1, alpha=0.4)
    ax2.axvline(0, color='black', linewidth=1, alpha=0.4)
    ax2.plot([-4, 4], [-4, 4], color='gray', linewidth=0.8, linestyle=':',
             alpha=0.5, label='WIM_Z = NS_Z (zero divergence)')

    # Quadrant labels
    kw = dict(fontsize=8, alpha=0.45, ha='center', style='italic')
    ax2.text( 2.5,  3.0, 'DOMINANT\n(High WIM & NS)',    color='#e65c00', **kw)
    ax2.text(-2.5,  3.0, 'CHAOTIC\n(High WIM, Low NS)',  color='#c0392b', **kw)
    ax2.text(-2.5, -3.0, 'COMPRESSED\n(Low WIM & NS)',   color='#27ae60', **kw)
    ax2.text( 2.5, -3.0, 'TIGHT\n(Low WIM, High NS)',    color='#2980b9', **kw)

    ax2.set_xlim(-4, 4)
    ax2.set_ylim(-4, 4)
    ax2.set_xlabel('Noll-Scully Z-Score', fontsize=10)
    ax2.set_ylabel('WIM Z-Score', fontsize=10)
    ax2.set_title('Top-25 Divergent Seasons\n(labelled; all others faded)',
                  fontsize=11, pad=10)
    ax2.legend(fontsize=7.5, loc='upper left')
    ax2.grid(True, alpha=0.12)

    fig.suptitle('WIM vs Noll-Scully Divergence Analysis\n'
                 'When Do the Two Metrics Disagree?',
                 fontsize=13, y=1.01)

    out = os.path.join(OUT_DIR, 'WIM_Divergence_Plot.png')
    fig.savefig(out, dpi=300, bbox_inches='tight')
    plt.close(fig)
    print(f'Divergence plot saved: {out}')


# ==============================================================================
# 2. PUBLICATION-QUALITY SUMMARY TABLE
# ==============================================================================

def build_summary_table(df):
    """
    For each league: WIM mean/SD, NS mean/SD, within-league r(WIM_Z, NS_Z),
    N seasons, sport group. Formatted as both CSV and a matplotlib table figure.
    """
    rows = []
    for league in df['League_Short'].unique():
        sub = df[df['League_Short'] == league]
        n   = len(sub)
        wim_mean = sub['WIM'].mean()
        wim_sd   = sub['WIM'].std(ddof=1)
        ns_mean  = sub['Noll_Scully'].mean()
        ns_sd    = sub['Noll_Scully'].std(ddof=1)
        wim_min  = sub['WIM'].min()
        wim_max  = sub['WIM'].max()
        mean_div = sub['Divergence'].mean()

        if n >= 4:
            r, p = stats.pearsonr(sub['WIM_Z'], sub['NS_Z'])
        else:
            r, p = np.nan, np.nan

        rows.append({
            'League':       league,
            'Sport':        SPORT_MAP.get(league, 'Other'),
            'Seasons':      n,
            'WIM_Mean':     wim_mean,
            'WIM_SD':       wim_sd,
            'WIM_Min':      wim_min,
            'WIM_Max':      wim_max,
            'NS_Mean':      ns_mean,
            'NS_SD':        ns_sd,
            'r_WIM_NS':     r,
            'p_WIM_NS':     p,
            'Mean_Div':     mean_div,
        })

    tbl = pd.DataFrame(rows)
    # Sort by WIM_Mean descending (most imbalanced at top)
    tbl = tbl.sort_values('WIM_Mean', ascending=False).reset_index(drop=True)

    # ── Save CSV ──────────────────────────────────────────────────────────────
    csv_out = os.path.join(OUT_DIR, 'WIM_Summary_Table.csv')
    tbl.to_csv(csv_out, index=False)
    print(f'\nSummary table CSV saved: {csv_out}')

    # ── Print nicely ──────────────────────────────────────────────────────────
    print('\n' + '='*70)
    print('TABLE 1: LEAGUE SUMMARY — WIM AND NOLL-SCULLY')
    print('='*70)
    print(f'{"League":<20} {"Sport":<18} {"N":>3}  '
          f'{"WIM":>6} {"(SD)":>5}  {"NS":>5} {"(SD)":>5}  '
          f'{"r":>6}  {"MeanDiv":>7}')
    print('-'*80)
    for _, row in tbl.iterrows():
        r_str  = f"{row['r_WIM_NS']:+.2f}" if not np.isnan(row['r_WIM_NS']) else '  n/a'
        flag   = ' *' if row['League'] in MLB_FLAGGED else ''
        bflag  = ' (b)' if row['League'] in BASKETBALL else ''
        print(f"  {row['League']:<18}{flag}{bflag}  {row['Sport']:<18} "
              f"{row['Seasons']:>3}  "
              f"{row['WIM_Mean']:>6.3f} ({row['WIM_SD']:.3f})  "
              f"{row['NS_Mean']:>5.2f} ({row['NS_SD']:.2f})  "
              f"{r_str}  {row['Mean_Div']:>7.3f}")
    print('-'*80)
    print('  * MLB: strategic score truncation partially invalidates WIM.')
    print('  (b) Basketball leagues: scoring structure inherently compresses WIM.')
    print(f'  r = within-league Pearson correlation between WIM_Z and NS_Z.')
    print(f'  MeanDiv = mean |WIM_Z - NS_Z| per season (divergence from NS).')

    # ── Build print-ready matplotlib table figure ─────────────────────────────
    _plot_summary_table(tbl)

    return tbl


def _plot_summary_table(tbl):
    """Render the summary table as a publication-style figure."""
    fig, ax = plt.subplots(figsize=(16, 9))
    ax.axis('off')

    # Column headers and data
    col_labels = [
        'League', 'Sport', 'N',
        'WIM\nMean', 'WIM\nSD',
        'NS\nMean', 'NS\nSD',
        'r(WIM,NS)', 'Mean\n|Div|'
    ]

    table_data = []
    row_colors = []

    sport_bg = {
        'Football':          '#dbeafe',
        'Australian Rules':  '#dcfce7',
        'Rugby League':      '#d1fae5',
        'American Football': '#fee2e2',
        'Ice Hockey':        '#fef3c7',
        'Baseball*':         '#f3f4f6',
        'Basketball':        '#ede9fe',
        'Other':             '#ffffff',
    }

    for _, row in tbl.iterrows():
        r_str = f"{row['r_WIM_NS']:+.2f}" if not np.isnan(row['r_WIM_NS']) else 'n/a'
        flag  = ' *' if row['League'] in MLB_FLAGGED else (
                ' (b)' if row['League'] in BASKETBALL else '')
        table_data.append([
            row['League'] + flag,
            row['Sport'],
            str(int(row['Seasons'])),
            f"{row['WIM_Mean']:.3f}",
            f"{row['WIM_SD']:.3f}",
            f"{row['NS_Mean']:.2f}",
            f"{row['NS_SD']:.2f}",
            r_str,
            f"{row['Mean_Div']:.3f}",
        ])
        bg = sport_bg.get(row['Sport'], '#ffffff')
        row_colors.append([bg] * len(col_labels))

    table = ax.table(
        cellText=table_data,
        colLabels=col_labels,
        cellLoc='center',
        loc='center',
        cellColours=row_colors,
    )
    table.auto_set_font_size(False)
    table.set_fontsize(9.5)
    table.scale(1, 1.6)

    # Style header row
    for j in range(len(col_labels)):
        table[0, j].set_facecolor('#1e3a5f')
        table[0, j].set_text_props(color='white', fontweight='bold')

    # Bold the WIM Mean column (col index 3)
    for i in range(1, len(table_data) + 1):
        table[i, 3].set_text_props(fontweight='bold')

    ax.set_title(
        'Table 1: WIM and Noll-Scully Summary Statistics by League\n'
        '* MLB: strategic score truncation partially invalidates WIM  '
        '(b) Basketball: scoring structure compresses WIM by design\n'
        'r = within-league correlation between WIM_Z and NS_Z  '
        '|Div| = mean |WIM_Z - NS_Z| (higher = more independent information)',
        fontsize=9.5, pad=14, loc='left'
    )

    out = os.path.join(OUT_DIR, 'WIM_Summary_Table.png')
    fig.savefig(out, dpi=300, bbox_inches='tight')
    plt.close(fig)
    print(f'Summary table figure saved: {out}')


# ==============================================================================
# MAIN
# ==============================================================================

def main():
    print('='*70)
    print('WIM DIVERGENCE ANALYSIS + SUMMARY TABLE')
    print('='*70)

    df = load()
    print(f'Loaded {len(df)} league-seasons across {df["League_Short"].nunique()} leagues.')

    corr_df, top25 = divergence_analysis(df)
    plot_divergence(df, top25)

    tbl = build_summary_table(df)

    print('\n' + '='*70)
    print('DONE. Outputs in Data/Clean Data/:')
    print('  WIM_Divergence_Analysis.csv')
    print('  WIM_Divergence_Plot.png')
    print('  WIM_Summary_Table.csv')
    print('  WIM_Summary_Table.png')
    print('='*70)


if __name__ == '__main__':
    main()
