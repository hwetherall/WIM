# ==============================================================================
# clean_basketball_data.py
#
# Cleans and converts Euroleague and NBL Excel files into the standard
# "WIM Raw Data - *.csv" format used by wim_analysis.py.
#
# Key conventions (per project notes):
#   - The year above each table is the SEASON START year
#     (e.g. "2024" or "2024-2025" both mean the 2024-25 season → Year = 2024)
#   - PS/G (points scored per game) and PA/G (points against per game)
#     are multiplied by games played to get season totals
#
# Known data issues:
#   - NBL 2020-21: PA/G = PS/G for every team (source error) → excluded
#   - Euroleague pre-2016/17: unequal games per team (Top-16 format)
#     → WIM still valid (ratio-based); NS flagged as less reliable
#
# Output:
#   Data/Raw Data/WIM Raw Data - Euroleague.csv
#   Data/Raw Data/WIM Raw Data - NBL.csv
# ==============================================================================

import pandas as pd
import numpy as np
import os
import re

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
RAW_DIR    = os.path.join(SCRIPT_DIR, 'Data', 'Raw Data')

# ==============================================================================
# SHARED HELPERS
# ==============================================================================

def extract_start_year(label):
    """
    Extract the season start year from a label.
    Handles: '2024', '2024-2025', '2024-25', '2024/25', '2024/2025'
    Returns an int or None.
    """
    s = str(label).strip()
    # Match a 4-digit year at the start
    m = re.match(r'^(\d{4})', s)
    if m:
        return int(m.group(1))
    return None


def clean_team_name(name):
    """Strip playoff markers (* † etc.) and extra whitespace."""
    if pd.isna(name):
        return None
    return re.sub(r'[\*†‡\u200c\s]+$', '', str(name)).strip()


# ==============================================================================
# NBL CLEANER
# ==============================================================================

def parse_nbl(path):
    """
    Parse NBL_2020-2025.xlsx.
    Format: multiple season blocks stacked in Sheet1.
    Each block: season-label row → 'Regular Season' row → header row (W L W/L% GB PS/G PA/G) → team rows
    """
    df_raw = pd.read_excel(path, sheet_name='Sheet1', header=None, dtype=str)

    seasons = []      # list of dicts: {year, team, gf, ga, wins, gp}
    bad_seasons = []  # seasons we are skipping with reasons

    current_year = None
    header_found = False

    for idx, row in df_raw.iterrows():
        cell0 = str(row[0]).strip() if pd.notna(row[0]) else ''
        cell1 = str(row[1]).strip() if pd.notna(row[1]) else ''

        # ── Season label row (e.g. "2024-2025" or "2025-2026") ──────────────
        year = extract_start_year(cell0)
        if year and year >= 2000:
            current_year = year
            header_found = False
            continue

        # ── Header row (contains 'W' and 'PS/G') ────────────────────────────
        row_vals = [str(v).strip() for v in row.values]
        if 'W' in row_vals and 'PS/G' in row_vals:
            # Map column letters to positions
            cols = list(row.values)
            try:
                w_idx   = next(i for i, v in enumerate(cols) if str(v).strip() == 'W')
                l_idx   = next(i for i, v in enumerate(cols) if str(v).strip() == 'L')
                psg_idx = next(i for i, v in enumerate(cols) if str(v).strip() == 'PS/G')
                pag_idx = next(i for i, v in enumerate(cols) if str(v).strip() == 'PA/G')
                header_found = True
                col_map = {'w': w_idx, 'l': l_idx, 'psg': psg_idx, 'pag': pag_idx}
            except StopIteration:
                header_found = False
            continue

        # ── Team data row ───────────────────────────────────────────────────
        if not header_found or current_year is None:
            continue
        if pd.isna(row[0]) or cell0 == '' or cell0 == 'nan':
            continue
        if cell0.lower() in ('regular season', 'nan'):
            continue

        team = clean_team_name(cell0)
        if not team:
            continue

        try:
            wins = int(float(row[col_map['w']]))
            losses = int(float(row[col_map['l']]))
            psg  = float(row[col_map['psg']])
            pag  = float(row[col_map['pag']])
        except (ValueError, TypeError, KeyError):
            continue

        gp = wins + losses
        if gp == 0:
            continue

        seasons.append({
            'Year':         current_year,
            'Team':         team,
            'For':          round(psg * gp, 1),
            'Agn':          round(pag * gp, 1),
            'Wins':         wins,
            'GamesPlayed':  gp,
        })

    result = pd.DataFrame(seasons)
    if result.empty:
        print("  WARNING: No NBL data parsed.")
        return result

    # ── Flag and remove the 2020-21 season (PA/G = PS/G for all teams) ──────
    by_year = result.groupby('Year')
    good_years = []
    for yr, grp in by_year:
        ratio_diff = (grp['For'] - grp['Agn']).abs().mean()
        if ratio_diff < 1.0:
            print(f"  EXCLUDED NBL {yr}: PA/G ~= PS/G for all teams (source data error, "
                  f"avg |For-Agn| = {ratio_diff:.2f}). WIM would be meaningless (~0).")
            bad_seasons.append(yr)
        else:
            good_years.append(grp)

    if not good_years:
        print("  WARNING: No valid NBL seasons after filtering.")
        return pd.DataFrame()

    result = pd.concat(good_years, ignore_index=True)
    print(f"  NBL: {len(result['Year'].unique())} valid seasons "
          f"({sorted(result['Year'].unique())}), "
          f"{len(result)} team-season rows.")
    return result


# ==============================================================================
# EUROLEAGUE CLEANER
# ==============================================================================

def parse_euroleague(path):
    """
    Parse Euroleague_2000-2025.xlsx.

    Expected layout (inspect and adjust if source differs):
      - Multiple season blocks, each starting with a season-label row
      - Column headers include: Team/Club, PS/G or PF, PA/G or PA,
        W, L, GP (or derivable from W+L)
      - Pre-2016/17: unequal games per team (Top-16 format)
        → we include these seasons but set a flag column

    The function is flexible: it scans for any row containing 'W' and
    a points-for column to identify headers, then reads team rows until
    the next blank/season row.
    """
    xl = pd.ExcelFile(path)
    print(f"  Euroleague sheets: {xl.sheet_names}")

    # Try to read all sheets and combine
    all_frames = []
    for sheet in xl.sheet_names:
        df_raw = pd.read_excel(path, sheet_name=sheet, header=None, dtype=str)
        all_frames.append(df_raw)

    df_raw = pd.concat(all_frames, ignore_index=True)

    seasons = []
    current_year = None
    header_found = False
    col_map = {}
    unequal_game_years = set()

    # Column name patterns to detect
    PF_PATTERNS  = ['ps/g', 'pf', 'pts', 'pointsfor', 'points for', 'scored', 'ppg', 'for']
    PA_PATTERNS  = ['pa/g', 'pa', 'pointsagainst', 'points against', 'conceded', 'papg', 'agn', 'against']
    W_PATTERNS   = ['w', 'wins', 'won']
    L_PATTERNS   = ['l', 'losses', 'lost']
    GP_PATTERNS  = ['gp', 'g', 'games', 'played', 'mp']
    TEAM_PATTERNS = ['team', 'club', 'squad', 'name']

    def match_col(cols, patterns):
        """Return index of first column whose lowercased value matches any pattern."""
        for pat in patterns:
            for i, c in enumerate(cols):
                if str(c).strip().lower() == pat:
                    return i
        return None

    per_game = {}  # track whether PF/PA columns are per-game or totals

    for idx, row in df_raw.iterrows():
        cell0 = str(row[0]).strip() if pd.notna(row[0]) else ''

        # ── Season label ────────────────────────────────────────────────────
        year = extract_start_year(cell0)
        if year and 1999 <= year <= 2026:
            current_year = year
            header_found = False
            col_map = {}
            continue

        # Also check if any cell in the row looks like a season year
        for ci in range(min(3, len(row))):
            val = str(row[ci]).strip() if pd.notna(row[ci]) else ''
            yr2 = extract_start_year(val)
            if yr2 and 1999 <= yr2 <= 2026 and not header_found:
                current_year = yr2
                break

        # ── Header detection ────────────────────────────────────────────────
        row_lower = [str(v).strip().lower() for v in row.values]

        has_pf = any(p in row_lower for p in PF_PATTERNS)
        has_w  = any(p in row_lower for p in W_PATTERNS)

        if has_pf and has_w:
            cols = list(row.values)
            cols_lower = [str(c).strip().lower() for c in cols]

            team_idx = match_col(cols_lower, TEAM_PATTERNS)
            if team_idx is None:
                # fallback: first non-empty column
                team_idx = 0

            pf_idx = match_col(cols_lower, PF_PATTERNS)
            pa_idx = match_col(cols_lower, PA_PATTERNS)
            w_idx  = match_col(cols_lower, W_PATTERNS)
            l_idx  = match_col(cols_lower, L_PATTERNS)
            gp_idx = match_col(cols_lower, GP_PATTERNS)

            if pf_idx is not None and pa_idx is not None and w_idx is not None:
                col_map = {
                    'team': team_idx, 'pf': pf_idx, 'pa': pa_idx,
                    'w': w_idx, 'l': l_idx, 'gp': gp_idx,
                }
                # Detect per-game columns
                pf_label = str(cols[pf_idx]).strip().lower()
                per_game[current_year] = ('/' in pf_label or 'pg' in pf_label
                                          or pf_label in ('ppg', 'ps/g', 'pa/g'))
                header_found = True
            continue

        # ── Team data row ────────────────────────────────────────────────────
        if not header_found or current_year is None or not col_map:
            continue
        if pd.isna(row[0]) or cell0 == '' or cell0 == 'nan':
            continue

        # Skip sub-header rows
        if cell0.lower() in ('team', 'club', 'squad', 'regular season', 'group stage'):
            continue

        team = clean_team_name(row[col_map['team']])
        if not team or len(team) < 2:
            continue

        try:
            pf_raw = float(row[col_map['pf']])
            pa_raw = float(row[col_map['pa']])
            wins   = int(float(row[col_map['w']]))
        except (ValueError, TypeError):
            continue

        # Games played
        gp = None
        if col_map.get('gp') is not None:
            try:
                gp = int(float(row[col_map['gp']]))
            except (ValueError, TypeError):
                pass

        if gp is None and col_map.get('l') is not None:
            try:
                losses = int(float(row[col_map['l']]))
                gp = wins + losses
            except (ValueError, TypeError):
                pass

        if not gp or gp == 0:
            continue

        # Convert per-game to totals if needed
        if per_game.get(current_year, False):
            pf_total = round(pf_raw * gp, 1)
            pa_total = round(pa_raw * gp, 1)
        else:
            pf_total = pf_raw
            pa_total = pa_raw

        seasons.append({
            'Year':              current_year,
            'Team':              team,
            'For':               pf_total,
            'Agn':               pa_total,
            'Wins':              wins,
            'GamesPlayed':       gp,
        })

    result = pd.DataFrame(seasons)
    if result.empty:
        print("  WARNING: No Euroleague data parsed — check file structure.")
        return result

    # ── Flag pre-2016/17 unequal-games seasons ───────────────────────────────
    for yr, grp in result.groupby('Year'):
        gp_vals = grp['GamesPlayed'].unique()
        if len(gp_vals) > 1 and yr < 2016:
            unequal_game_years.add(yr)

    if unequal_game_years:
        print(f"  NOTE: Euroleague seasons with unequal games per team "
              f"(pre-2016/17 Top-16 format): {sorted(unequal_game_years)}")
        print(f"  WIM is ratio-based and remains valid. "
              f"Noll-Scully figures for these seasons are less reliable.")

    # ── Exclude seasons where PA ~= PF for all teams (source data error) ─────
    bad_euro = []
    good_frames = []
    for yr, grp in result.groupby('Year'):
        ratio_diff = (grp['For'] - grp['Agn']).abs().mean()
        if ratio_diff < 1.0:
            print(f"  EXCLUDED Euroleague {yr}: PA ~= PF for all teams "
                  f"(source data error, avg|For-Agn|={ratio_diff:.2f}). "
                  f"Likely COVID-disrupted season with incomplete/bad data.")
            bad_euro.append(yr)
        else:
            good_frames.append(grp)

    if not good_frames:
        print("  WARNING: No valid Euroleague seasons after filtering.")
        return pd.DataFrame()

    result = pd.concat(good_frames, ignore_index=True)

    print(f"  Euroleague: {len(result['Year'].unique())} valid seasons "
          f"({sorted(result['Year'].unique())}), "
          f"{len(result)} team-season rows.")
    return result


# ==============================================================================
# MAIN
# ==============================================================================

def main():
    print("=" * 65)
    print("BASKETBALL DATA CLEANER")
    print("=" * 65)

    # ── NBL ──────────────────────────────────────────────────────────────────
    nbl_path = os.path.join(RAW_DIR, 'NBL_2020-2025.xlsx')
    if os.path.exists(nbl_path):
        print(f"\nProcessing NBL: {nbl_path}")
        nbl_df = parse_nbl(nbl_path)
        if not nbl_df.empty:
            out_path = os.path.join(RAW_DIR, 'WIM Raw Data - NBL.csv')
            nbl_df.to_csv(out_path, index=False)
            print(f"  Saved: {out_path}")
            print(nbl_df.groupby('Year')[['For','Agn','GamesPlayed']].mean().round(1))
    else:
        print(f"\nNBL file not found: {nbl_path}")

    # ── Euroleague ────────────────────────────────────────────────────────────
    euro_path = os.path.join(RAW_DIR, 'Euroleague_2000-2025.xlsx')
    if os.path.exists(euro_path):
        print(f"\nProcessing Euroleague: {euro_path}")
        euro_df = parse_euroleague(euro_path)
        if not euro_df.empty:
            out_path = os.path.join(RAW_DIR, 'WIM Raw Data - Euroleague.csv')
            euro_df.to_csv(out_path, index=False)
            print(f"  Saved: {out_path}")
            print(euro_df.groupby('Year')[['For','Agn','GamesPlayed']].mean().round(1))
    else:
        print(f"\nEuroleague file not found at: {euro_path}")
        print("  → Place Euroleague_2000-2025.xlsx in Data/Raw Data/ and re-run.")

    print("\nDone.")


if __name__ == '__main__':
    main()
