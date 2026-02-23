# ==============================================================================
# WIM Quadrant Plot: WIM_Z vs NS_Z Cross-League Scatter
#
# The core Figure 1 argument: WIM and Noll-Scully measure different things.
# Four quadrants reveal structurally distinct types of competitive seasons.
#
# Input:  Data/Clean Data/wim_full_results.csv  (run wim_analysis.py first)
# Output: Data/Clean Data/WIM_Quadrant_Plot.png
# ==============================================================================

import pandas as pd
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
from scipy import stats
import os

# ==============================================================================
# CONFIG
# ==============================================================================

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
INPUT_CSV  = os.path.join(SCRIPT_DIR, 'Data', 'Clean Data', 'wim_full_results.csv')
OUTPUT_PNG = os.path.join(SCRIPT_DIR, 'Data', 'Clean Data', 'WIM_Quadrant_Plot.png')

# Seasons to annotate (league_short_name, season_year, label)
ANNOTATIONS = [
    ('Prem League', 2018, 'Man City\n2017-18'),
    ('Prem League', 2016, 'Leicester\n2015-16'),
    ('Prem League', 2004, 'Arsenal\n2003-04'),
    ('Prem League', 2010, 'Chelsea\n2009-10'),
    ('AFL',         2012, 'AFL 2012'),
    ('Ligue 1',     2006, 'Ligue 1\n2005-06'),
]

# Leagues to always annotate their cluster centroid
CLUSTER_LABELS = {
    'NBA': ('NBA', 'bottom right'),
    'NHL': ('NHL', 'bottom left'),
    'MLB': ('MLB*', 'bottom left'),
}

# Clean display names (strip the "WIM Raw Data - " prefix)
def short_name(raw):
    return raw.replace('WIM Raw Data - ', '').strip()

# 14-league colour palette (tab20 has 20 entries — enough headroom)
LEAGUE_COLORS = {
    'Prem League':      '#e6194b',   # red
    'Bundesliga':       '#3cb44b',   # green
    'La Liga':          '#ffe119',   # yellow
    'Ligue 1':          '#4363d8',   # blue
    'Eredivisie':       '#f58231',   # orange
    'EFL Championship': '#911eb4',   # purple
    'A-League':         '#42d4f4',   # cyan
    'MLS':              '#f032e6',   # magenta
    'AFL':              '#bfef45',   # lime
    'NRL':              '#fabed4',   # pink
    'NFL':              '#469990',   # teal
    'NBA':              '#dcbeff',   # lavender
    'NHL':              '#9A6324',   # brown
    'MLB':              '#aaffc3',   # mint
}

# Quadrant background colours (very faint)
QUAD_ALPHA = 0.06
QUAD_COLORS = {
    'TL': '#d62728',   # Top-left    — Chaotic
    'TR': '#ff7f0e',   # Top-right   — Dominant
    'BL': '#2ca02c',   # Bottom-left — Compressed
    'BR': '#1f77b4',   # Bottom-right— Tight
}


# ==============================================================================
# LOAD & CLEAN
# ==============================================================================

def load_data():
    df = pd.read_csv(INPUT_CSV)

    # Strip league prefix for display
    df['League_Short'] = df['League'].apply(short_name)

    # Drop clearly bad rows (data artefacts: <5 teams)
    df = df[df['Teams'] >= 5].copy()

    # Drop rows missing either Z-score
    df = df.dropna(subset=['WIM_Z', 'NS_Z']).copy()

    # Sanity-cap extreme Z-scores that would wreck axis scaling
    # (Nothing beyond ±4 is plotted, but we keep the data)
    df['WIM_Z_plot'] = df['WIM_Z'].clip(-4, 4)
    df['NS_Z_plot']  = df['NS_Z'].clip(-4, 4)

    return df


# ==============================================================================
# QUADRANT SUMMARY
# ==============================================================================

def quadrant_summary(df):
    """Print how many seasons land in each quadrant."""
    tl = df[(df['WIM_Z'] >  0) & (df['NS_Z'] <= 0)]
    tr = df[(df['WIM_Z'] >  0) & (df['NS_Z'] >  0)]
    bl = df[(df['WIM_Z'] <= 0) & (df['NS_Z'] <= 0)]
    br = df[(df['WIM_Z'] <= 0) & (df['NS_Z'] >  0)]

    print("\n" + "=" * 70)
    print("QUADRANT SUMMARY")
    print("=" * 70)
    for label, subset in [
        ('TOP-LEFT    (High WIM, Low NS)  — Chaotic Blowouts',    tl),
        ('TOP-RIGHT   (High WIM, High NS) — Dominant',            tr),
        ('BOTTOM-LEFT (Low WIM, Low NS)   — Compressed / Open',   bl),
        ('BOTTOM-RIGHT(Low WIM, High NS)  — Tight but Predictable', br),
    ]:
        print(f"\n  {label}  [{len(subset)} seasons]")
        # Show top 5 most extreme by distance from origin
        subset = subset.copy()
        subset['dist'] = np.sqrt(subset['WIM_Z']**2 + subset['NS_Z']**2)
        top = subset.nlargest(5, 'dist')[['League_Short', 'Season', 'WIM_Z', 'NS_Z']]
        print(top.to_string(index=False, float_format='{:.3f}'.format))

    # Overall correlation
    r, p = stats.pearsonr(df['WIM_Z'], df['NS_Z'])
    print(f"\n  Overall Pearson r(WIM_Z, NS_Z) = {r:.3f}  (p = {p:.4f})")
    print("  -> " + (
        "Strong positive correlation — metrics largely agree."        if r > 0.6  else
        "Moderate correlation — metrics often agree, sometimes diverge." if r > 0.3  else
        "Weak/no correlation — metrics are measuring different things."
    ))


# ==============================================================================
# PLOT
# ==============================================================================

def build_plot(df):
    fig, ax = plt.subplots(figsize=(14, 10))

    # ── Quadrant background shading ──────────────────────────────────────────
    xmin, xmax = -3.8, 3.8
    ymin, ymax = -3.8, 3.8

    ax.axhspan(0, ymax, xmin=0.5, xmax=1.0,  alpha=QUAD_ALPHA, color=QUAD_COLORS['TR'], zorder=0)
    ax.axhspan(0, ymax, xmin=0.0, xmax=0.5,  alpha=QUAD_ALPHA, color=QUAD_COLORS['TL'], zorder=0)
    ax.axhspan(ymin, 0, xmin=0.0, xmax=0.5,  alpha=QUAD_ALPHA, color=QUAD_COLORS['BL'], zorder=0)
    ax.axhspan(ymin, 0, xmin=0.5, xmax=1.0,  alpha=QUAD_ALPHA, color=QUAD_COLORS['BR'], zorder=0)

    # ── Scatter — one series per league ──────────────────────────────────────
    leagues = sorted(df['League_Short'].unique())
    for league in leagues:
        sub = df[df['League_Short'] == league]
        color = LEAGUE_COLORS.get(league, '#888888')
        # NBA gets a larger marker to make the cluster pop
        size = 80 if league == 'NBA' else 50
        zorder = 5 if league == 'NBA' else 3
        ax.scatter(sub['NS_Z_plot'], sub['WIM_Z_plot'],
                   color=color, s=size, alpha=0.80,
                   edgecolors='white', linewidths=0.4,
                   label=league, zorder=zorder)

    # ── Annotations: specific seasons of interest ─────────────────────────────
    annotated = set()
    for league_short, season, label in ANNOTATIONS:
        row = df[(df['League_Short'] == league_short) & (df['Season'] == season)]
        if row.empty:
            continue
        x = float(row['NS_Z_plot'].iloc[0])
        y = float(row['WIM_Z_plot'].iloc[0])
        key = (round(x, 2), round(y, 2))
        if key in annotated:
            continue
        annotated.add(key)

        ax.annotate(
            label,
            xy=(x, y), xycoords='data',
            xytext=(x + 0.25, y + 0.25), textcoords='data',
            fontsize=7.5, color='#333333', fontweight='bold',
            arrowprops=dict(arrowstyle='->', color='#555555', lw=0.8),
            bbox=dict(boxstyle='round,pad=0.2', fc='white', ec='none', alpha=0.7),
            zorder=10,
        )

    # ── NBA cluster centroid label ─────────────────────────────────────────────
    nba = df[df['League_Short'] == 'NBA']
    if not nba.empty:
        cx, cy = nba['NS_Z_plot'].mean(), nba['WIM_Z_plot'].mean()
        ax.annotate(
            'NBA cluster',
            xy=(cx, cy), xycoords='data',
            xytext=(cx + 0.5, cy - 0.6), textcoords='data',
            fontsize=8.5, color='#5c3a9e', fontweight='bold',
            arrowprops=dict(arrowstyle='->', color='#5c3a9e', lw=1.2),
            bbox=dict(boxstyle='round,pad=0.3', fc='#f0eaff', ec='#5c3a9e', alpha=0.85),
            zorder=10,
        )

    # ── Regression line (overall trend) ───────────────────────────────────────
    x_all = df['NS_Z_plot'].values
    y_all = df['WIM_Z_plot'].values
    m, b, r, p, _ = stats.linregress(x_all, y_all)
    x_line = np.linspace(xmin, xmax, 200)
    ax.plot(x_line, m * x_line + b,
            color='black', linewidth=1.0, linestyle='--', alpha=0.35,
            label=f'Overall trend  r={r:.2f}', zorder=2)

    # ── Reference lines ────────────────────────────────────────────────────────
    ax.axhline(0, color='black', linewidth=1.2, alpha=0.5, zorder=1)
    ax.axvline(0, color='black', linewidth=1.2, alpha=0.5, zorder=1)

    # ── Quadrant labels ────────────────────────────────────────────────────────
    label_kw = dict(fontsize=10, alpha=0.55, ha='center', va='center',
                    style='italic', zorder=1)
    ax.text(-2.5,  2.8, 'TOP-LEFT\nHigh Margin Dominance\nLow Outcome Predictability\n"Chaotic Blowouts"',
            color=QUAD_COLORS['TL'], **label_kw)
    ax.text( 2.5,  2.8, 'TOP-RIGHT\nHigh Margin Dominance\nHigh Outcome Predictability\n"Dominant Season"',
            color=QUAD_COLORS['TR'], **label_kw)
    ax.text(-2.5, -2.8, 'BOTTOM-LEFT\nLow Margin Dominance\nLow Outcome Predictability\n"Compressed / Open"',
            color=QUAD_COLORS['BL'], **label_kw)
    ax.text( 2.5, -2.8, 'BOTTOM-RIGHT\nLow Margin Dominance\nHigh Outcome Predictability\n"Tight but Predictable"',
            color=QUAD_COLORS['BR'], **label_kw)

    # ── Formatting ─────────────────────────────────────────────────────────────
    ax.set_xlim(xmin, xmax)
    ax.set_ylim(ymin, ymax)

    ax.set_xlabel('Noll-Scully Z-Score  (← More Balanced  |  More Unbalanced →)',
                  fontsize=12, labelpad=8)
    ax.set_ylabel('WIM Z-Score  (← Lower Margin Dominance  |  Higher Margin Dominance →)',
                  fontsize=12, labelpad=8)
    ax.set_title(
        'WIM vs Noll-Scully: Do They Measure the Same Thing?\n'
        'Each point = one league-season  |  Z-scores within each league',
        fontsize=13, pad=12
    )

    ax.grid(True, alpha=0.15)

    # ── Legend (two columns, outside plot) ────────────────────────────────────
    handles, labels = ax.get_legend_handles_labels()
    ax.legend(handles, labels,
              loc='upper left', bbox_to_anchor=(1.01, 1.0),
              fontsize=8.5, ncol=1, framealpha=0.9,
              title='League', title_fontsize=9)

    plt.tight_layout(rect=[0, 0, 0.86, 1])

    fig.savefig(OUTPUT_PNG, dpi=300, bbox_inches='tight')
    plt.close(fig)
    print(f"\nQuadrant plot saved to: {OUTPUT_PNG}")


# ==============================================================================
# MAIN
# ==============================================================================

def main():
    print("=" * 70)
    print("WIM QUADRANT PLOT — WIM_Z vs NS_Z Cross-League Scatter")
    print("=" * 70)

    df = load_data()
    print(f"Loaded {len(df)} league-seasons across {df['League_Short'].nunique()} leagues.")

    quadrant_summary(df)
    build_plot(df)

    print("\nDone.")


if __name__ == '__main__':
    main()
