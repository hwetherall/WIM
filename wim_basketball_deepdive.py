# ==============================================================================
# wim_basketball_deepdive.py
#
# Basketball Deep-Dive: Is low WIM sport-endemic to basketball, or NBA-specific?
#
# Produces two figures:
#   1. Absolute WIM vs Noll-Scully — all leagues (bubble chart)
#      Shows the basketball cluster clearly vs all other sports
#   2. WIM over time — the three basketball leagues side by side
#      Asks: do Euroleague and NBL track the NBA, or diverge?
#
# Input:  Data/Clean Data/wim_full_results.csv
#         Data/Clean Data/wim_league_averages.csv
# Output: Data/Clean Data/WIM_Basketball_Comparison.png
#         Data/Clean Data/WIM_Basketball_Timeseries.png
# ==============================================================================

import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import os

SCRIPT_DIR   = os.path.dirname(os.path.abspath(__file__))
RESULTS_CSV  = os.path.join(SCRIPT_DIR, 'Data', 'Clean Data', 'wim_full_results.csv')
AVERAGES_CSV = os.path.join(SCRIPT_DIR, 'Data', 'Clean Data', 'wim_league_averages.csv')
OUT_DIR      = os.path.join(SCRIPT_DIR, 'Data', 'Clean Data')

BASKETBALL_LEAGUES = {'NBA', 'Euroleague', 'NBL'}

# Sport groupings for colour coding
SPORT_GROUPS = {
    'Basketball': ['NBA', 'Euroleague', 'NBL'],
    'Football':   ['Prem League', 'Bundesliga', 'La Liga', 'Ligue 1',
                   'Eredivisie', 'EFL Championship', 'MLS', 'A-League'],
    'Rugby/AFL':  ['AFL', 'NRL'],
    'American':   ['NFL', 'NHL', 'MLB'],
}
GROUP_COLORS = {
    'Basketball': '#7b2d8b',   # purple
    'Football':   '#1a6faf',   # blue
    'Rugby/AFL':  '#2e8b57',   # green
    'American':   '#c0392b',   # red
}

def short_name(raw):
    return str(raw).replace('WIM Raw Data - ', '').strip()


# ==============================================================================
# FIGURE 1 — Absolute WIM vs Noll-Scully (all leagues)
# ==============================================================================

def plot_absolute_comparison(avg_df):
    """
    Bubble chart: x = Noll-Scully mean, y = WIM mean.
    Bubble size scaled to number of seasons in sample.
    Basketball leagues are annotated and highlighted.
    """
    fig, ax = plt.subplots(figsize=(13, 9))

    for _, row in avg_df.iterrows():
        league = row['League_Short']
        x = row['Noll_Scully']
        y = row['WIM']
        n = row['N_seasons']

        # Determine group and colour
        group = next(
            (g for g, members in SPORT_GROUPS.items() if league in members),
            'Other'
        )
        color = GROUP_COLORS.get(group, '#888888')

        is_bball = league in BASKETBALL_LEAGUES

        ax.scatter(x, y,
                   s=max(60, n * 8),        # size = seasons * 8
                   color=color,
                   alpha=0.85,
                   edgecolors='white' if not is_bball else 'black',
                   linewidths=0.5 if not is_bball else 2.0,
                   zorder=5 if is_bball else 3)

        # Label position — offset to avoid overlaps
        offset_map = {
            'NBA':            ( 0.04, -0.006),
            'Euroleague':     ( 0.04,  0.002),
            'NBL':            ( 0.04,  0.002),
            'Prem League':    ( 0.04, -0.004),
            'AFL':            ( 0.04,  0.002),
            'NHL':            (-0.20, -0.008),
            'MLB':            (-0.20,  0.004),
            'NFL':            ( 0.04, -0.008),
            'EFL Championship': (0.04, 0.002),
            'MLS':            ( 0.04,  0.003),
            'NRL':            ( 0.04, -0.006),
            'Ligue 1':        (-0.22, -0.006),
            'Bundesliga':     (-0.22,  0.004),
            'La Liga':        ( 0.04,  0.002),
            'Eredivisie':     (-0.22, -0.005),
            'A-League':       ( 0.04,  0.003),
        }
        dx, dy = offset_map.get(league, (0.04, 0.002))

        weight = 'bold' if is_bball else 'normal'
        fontsize = 9.5 if is_bball else 8.5
        ax.annotate(league,
                    xy=(x, y), xytext=(x + dx, y + dy),
                    fontsize=fontsize, fontweight=weight,
                    color='black',
                    arrowprops=dict(arrowstyle='-', color='#aaaaaa', lw=0.6)
                        if abs(dx) > 0.05 or abs(dy) > 0.005 else None,
                    zorder=10)

    # ── Shaded zone for basketball cluster ────────────────────────────────────
    bball = avg_df[avg_df['League_Short'].isin(BASKETBALL_LEAGUES)]
    if not bball.empty:
        pad_x = 0.15
        pad_y = 0.012
        x0 = bball['Noll_Scully'].min() - pad_x
        x1 = bball['Noll_Scully'].max() + pad_x
        y0 = bball['WIM'].min() - pad_y
        y1 = bball['WIM'].max() + pad_y
        rect = mpatches.FancyBboxPatch(
            (x0, y0), x1 - x0, y1 - y0,
            boxstyle='round,pad=0.01',
            linewidth=1.5, edgecolor='#7b2d8b',
            facecolor='#e8d5f0', alpha=0.4, zorder=1
        )
        ax.add_patch(rect)
        ax.text((x0 + x1) / 2, y1 + 0.005,
                'Basketball cluster\n(sport-endemic low WIM)',
                ha='center', va='bottom', fontsize=9,
                color='#7b2d8b', style='italic', zorder=10)

    # ── Legend by sport group ──────────────────────────────────────────────────
    legend_handles = [
        mpatches.Patch(color=GROUP_COLORS[g], label=g, alpha=0.85)
        for g in ['Basketball', 'Football', 'Rugby/AFL', 'American']
    ]
    ax.legend(handles=legend_handles,
              loc='upper left', fontsize=9, title='Sport group',
              title_fontsize=9, framealpha=0.9)

    # ── Annotation: NBA vs Euroleague NS gap ───────────────────────────────────
    nba_row = avg_df[avg_df['League_Short'] == 'NBA']
    eur_row = avg_df[avg_df['League_Short'] == 'Euroleague']
    if not nba_row.empty and not eur_row.empty:
        ns_nba = float(nba_row['Noll_Scully'].iloc[0])
        ns_eur = float(eur_row['Noll_Scully'].iloc[0])
        y_mid  = float(nba_row['WIM'].iloc[0])
        ax.annotate(
            '',
            xy=(ns_nba, y_mid - 0.004),
            xytext=(ns_eur + 0.05, y_mid - 0.004),
            arrowprops=dict(arrowstyle='<->', color='#7b2d8b', lw=1.5),
        )
        ax.text((ns_nba + ns_eur) / 2, y_mid - 0.009,
                'NBA-specific\noutcome predictability',
                ha='center', fontsize=8, color='#7b2d8b', style='italic')

    # ── Reference lines ────────────────────────────────────────────────────────
    ax.axhline(0.10, color='gray', linewidth=0.8, linestyle=':', alpha=0.5)
    ax.text(ax.get_xlim()[0] if ax.get_xlim()[0] > 0 else 0.9,
            0.102, 'WIM = 0.10', fontsize=7.5, color='gray', alpha=0.7)

    # ── Labels ─────────────────────────────────────────────────────────────────
    ax.set_xlabel('Noll-Scully (league average)  —  Outcome Predictability',
                  fontsize=11, labelpad=8)
    ax.set_ylabel('WIM (league average)  —  Margin Dominance',
                  fontsize=11, labelpad=8)
    ax.set_title(
        'WIM vs Noll-Scully: Absolute League Averages (All Sports)\n'
        'Bubble size = number of seasons in sample',
        fontsize=13, pad=12
    )
    ax.grid(True, alpha=0.18)

    out = os.path.join(OUT_DIR, 'WIM_Basketball_Comparison.png')
    fig.savefig(out, dpi=300, bbox_inches='tight')
    plt.close(fig)
    print(f"Saved: {out}")


# ==============================================================================
# FIGURE 2 — Basketball WIM over time
# ==============================================================================

def plot_basketball_timeseries(results_df):
    """
    Line chart of WIM by season for NBA, Euroleague, NBL.
    Overlaid: league means as dashed horizontals.
    Shaded band for the NBA Z-score range.
    """
    fig, ax = plt.subplots(figsize=(13, 7))

    bball_colors = {
        'NBA':        '#7b2d8b',
        'Euroleague': '#e67e22',
        'NBL':        '#27ae60',
    }
    bball_markers = {
        'NBA': 'o',
        'Euroleague': 's',
        'NBL': '^',
    }

    for league, color in bball_colors.items():
        sub = results_df[results_df['League_Short'] == league].sort_values('Season')
        if sub.empty:
            continue
        ax.plot(sub['Season'], sub['WIM'],
                color=color, linewidth=2.2,
                marker=bball_markers[league], markersize=6,
                label=f'{league}  (mean={sub["WIM"].mean():.4f})',
                zorder=5)
        # Mean line
        ax.axhline(sub['WIM'].mean(), color=color, linewidth=0.8,
                   linestyle='--', alpha=0.5)

    # ── Reference: lowest non-basketball league (MLB) ──────────────────────────
    mlb = results_df[results_df['League_Short'] == 'MLB'].sort_values('Season')
    if not mlb.empty:
        ax.plot(mlb['Season'], mlb['WIM'],
                color='#95a5a6', linewidth=1.2, linestyle=':',
                marker='x', markersize=5, alpha=0.6,
                label=f'MLB (lowest non-bball, mean={mlb["WIM"].mean():.4f})')
        ax.axhline(mlb['WIM'].mean(), color='#95a5a6', linewidth=0.7,
                   linestyle=':', alpha=0.4)

    # ── Gap annotation ─────────────────────────────────────────────────────────
    if not mlb.empty:
        nba = results_df[results_df['League_Short'] == 'NBA']
        if not nba.empty:
            mlb_mean = mlb['WIM'].mean()
            nba_mean = nba['WIM'].mean()
            x_ann = max(results_df[results_df['League_Short'].isin(
                BASKETBALL_LEAGUES)]['Season'].max(),
                mlb['Season'].max()) + 0.5
            ax.annotate('', xy=(x_ann, nba_mean), xytext=(x_ann, mlb_mean),
                        arrowprops=dict(arrowstyle='<->', color='black', lw=1.2))
            ax.text(x_ann + 0.3, (nba_mean + mlb_mean) / 2,
                    f'Gap\n{mlb_mean - nba_mean:.3f}',
                    fontsize=8, va='center', ha='left')

    ax.set_xlabel('Season (start year)', fontsize=11, labelpad=8)
    ax.set_ylabel('WIM Score  (lower = more balanced scoring)', fontsize=11, labelpad=8)
    ax.set_title(
        'Basketball WIM Over Time: NBA vs Euroleague vs NBL\n'
        'Is low WIM basketball-endemic, or NBA-specific?',
        fontsize=13, pad=12
    )
    ax.legend(loc='upper left', fontsize=9.5, framealpha=0.9)
    ax.grid(True, alpha=0.2)

    out = os.path.join(OUT_DIR, 'WIM_Basketball_Timeseries.png')
    fig.savefig(out, dpi=300, bbox_inches='tight')
    plt.close(fig)
    print(f"Saved: {out}")


# ==============================================================================
# CONSOLE SUMMARY
# ==============================================================================

def print_summary(avg_df, results_df):
    print("\n" + "=" * 65)
    print("BASKETBALL DEEP-DIVE SUMMARY")
    print("=" * 65)

    bball = avg_df[avg_df['League_Short'].isin(BASKETBALL_LEAGUES)].sort_values('WIM')
    non_bball = avg_df[~avg_df['League_Short'].isin(BASKETBALL_LEAGUES)].sort_values('WIM')

    print("\n--- Basketball Leagues ---")
    print(bball[['League_Short', 'WIM', 'Noll_Scully', 'N_seasons']].to_string(
        index=False, float_format='{:.4f}'.format))

    print("\n--- All Other Leagues (bottom 3 by WIM) ---")
    print(non_bball.head(3)[['League_Short', 'WIM', 'Noll_Scully', 'N_seasons']].to_string(
        index=False, float_format='{:.4f}'.format))

    lowest_non_bball = non_bball.iloc[0]['WIM']
    highest_bball    = bball.iloc[-1]['WIM']
    print(f"\n  Gap (lowest non-basketball WIM minus highest basketball WIM):")
    print(f"  {lowest_non_bball:.4f} ({non_bball.iloc[0]['League_Short']}) "
          f"- {highest_bball:.4f} ({bball.iloc[-1]['League_Short']}) "
          f"= {lowest_non_bball - highest_bball:.4f}")

    print("\n--- NS comparison within basketball ---")
    nba_ns  = float(avg_df[avg_df['League_Short'] == 'NBA']['Noll_Scully'].iloc[0])
    eur_ns  = float(avg_df[avg_df['League_Short'] == 'Euroleague']['Noll_Scully'].iloc[0])
    nbl_ns  = float(avg_df[avg_df['League_Short'] == 'NBL']['Noll_Scully'].iloc[0])
    print(f"  NBA NS:        {nba_ns:.3f}")
    print(f"  Euroleague NS: {eur_ns:.3f}")
    print(f"  NBL NS:        {nbl_ns:.3f}")
    print(f"  -> NBA NS is {nba_ns / eur_ns:.1f}x higher than Euroleague")
    print(f"  -> Conclusion: LOW WIM is basketball-endemic; HIGH NS is NBA-specific")

    print("\n--- Interpretation for paper ---")
    print("  WIM correctly identifies basketball as a low-margin-dominance sport.")
    print("  The scoring structure of basketball (high scores, narrow ratios)")
    print("  inherently compresses ln(PF/PA) toward 0 for all teams.")
    print("  This is a genuine structural finding, NOT a WIM weakness.")
    print("  The NBA's exceptional NS (2.68) vs Euroleague (1.69) reflects")
    print("  the NBA's unique superstar concentration and outcome predictability,")
    print("  a dimension that WIM correctly does NOT capture (by design).")


# ==============================================================================
# MAIN
# ==============================================================================

def main():
    print("=" * 65)
    print("WIM BASKETBALL DEEP-DIVE")
    print("=" * 65)

    results_df = pd.read_csv(RESULTS_CSV)
    results_df['League_Short'] = results_df['League'].apply(short_name)

    avg_df = pd.read_csv(AVERAGES_CSV)
    avg_df['League_Short'] = avg_df['League'].apply(short_name)

    # Add season counts
    counts = results_df.groupby('League_Short').size().reset_index(name='N_seasons')
    avg_df = avg_df.merge(counts, on='League_Short', how='left')

    print_summary(avg_df, results_df)

    print("\nGenerating charts...")
    plot_absolute_comparison(avg_df)
    plot_basketball_timeseries(results_df)

    print("\nDone.")


if __name__ == '__main__':
    main()
