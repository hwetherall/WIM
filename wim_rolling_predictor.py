# ==============================================================================
# WIM Rolling Predictor - Predictive Testing Suite
# ==============================================================================
# Implements the "Structural Lock" hypothesis:
#   Rolling WIM computed mid-season predicts title persistence and upset rates.
#
# Backtests:
#   1. Title Persistence  - Does MD19 WIM predict whether the leader wins?
#   2. Upset Frequency    - Do favorites overperform in high-WIM regimes?
#
# Outputs predictions for EPL 2025-26 with pre-registered timestamps.
# ==============================================================================

import pandas as pd
import numpy as np
import os
import glob
import datetime
import warnings
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

warnings.filterwarnings('ignore')

# ==============================================================================
# CONFIGURATION
# ==============================================================================

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
DATA_DIR = os.path.join(SCRIPT_DIR, 'Data', 'Raw Data', 'Football-Data')
OUTPUT_DIR = os.path.join(SCRIPT_DIR, 'Prediction-Output')

CHECKPOINT_MATCHDAYS = [10, 15, 19, 25, 30, 38]
CURRENT_SEASON = '2025'
MIN_GP_FOR_BACKTEST = 5  # ignore matches before all teams have 5 games

# ==============================================================================
# 1. DATA LOADING
# ==============================================================================

def load_season(filepath):
    """Load a single season CSV, handling files with inconsistent column counts."""
    import csv

    with open(filepath, 'r', encoding='utf-8', errors='replace') as f:
        reader = csv.reader(f)
        header = next(reader)
        n_cols = len(header)
        data_rows = []
        for row in reader:
            # Truncate extra columns or pad missing ones
            trimmed = row[:n_cols]
            if len(trimmed) < n_cols:
                trimmed.extend([''] * (n_cols - len(trimmed)))
            data_rows.append(trimmed)

    df = pd.DataFrame(data_rows, columns=header)
    df.replace('', np.nan, inplace=True)

    df['Date'] = pd.to_datetime(df['Date'], dayfirst=True, errors='coerce')
    df['FTHG'] = pd.to_numeric(df['FTHG'], errors='coerce')
    df['FTAG'] = pd.to_numeric(df['FTAG'], errors='coerce')

    df = df.dropna(subset=['FTHG', 'FTAG', 'HomeTeam', 'AwayTeam', 'Date'])
    df['FTHG'] = df['FTHG'].astype(int)
    df['FTAG'] = df['FTAG'].astype(int)

    df = df.sort_values('Date').reset_index(drop=True)
    return df


def load_all_seasons():
    """Load all Prem League CSVs from Football-Data/."""
    pattern = os.path.join(DATA_DIR, 'Prem_[Ll]eague_*.csv')
    files = sorted(glob.glob(pattern))

    seasons = {}
    for f in files:
        year = os.path.basename(f).split('_')[-1].replace('.csv', '')
        seasons[year] = load_season(f)
        print(f"  Loaded {year}: {len(seasons[year])} matches, "
              f"{seasons[year]['HomeTeam'].nunique()} teams")
    return seasons


# ==============================================================================
# 2. ODDS ADAPTERS
# ==============================================================================

def get_best_odds(row, cols):
    """
    Return (home_odds, draw_odds, away_odds) using the best available source.
    Fallback: Market Avg > BetBrain Avg > Pinnacle > Bet365 > Ladbrokes > WH
    """
    chains = [
        ('AvgH', 'AvgD', 'AvgA'),
        ('BbAvH', 'BbAvD', 'BbAvA'),
        ('PSH', 'PSD', 'PSA'),
        ('PSCH', 'PSCD', 'PSCA'),
        ('B365H', 'B365D', 'B365A'),
        ('LBH', 'LBD', 'LBA'),
        ('WHH', 'WHD', 'WHA'),
    ]
    for h, d, a in chains:
        if h in cols and d in cols and a in cols:
            try:
                oh, od, oa = float(row[h]), float(row[d]), float(row[a])
                if oh > 1 and od > 1 and oa > 1:
                    return oh, od, oa
            except (ValueError, TypeError):
                continue
    return np.nan, np.nan, np.nan


def get_asian_handicap(row, cols):
    """Return (line, home_odds, away_odds) for Asian handicap."""
    chains = [
        ('AHh', 'AvgAHH', 'AvgAHA'),
        ('BbAHh', 'BbAvAHH', 'BbAvAHA'),
        ('AHh', 'B365AHH', 'B365AHA'),
        ('BbAHh', 'BbMxAHH', 'BbMxAHA'),
    ]
    for lc, hc, ac in chains:
        if lc in cols and hc in cols and ac in cols:
            try:
                line = float(row[lc])
                oh, oa = float(row[hc]), float(row[ac])
                if oh > 1 and oa > 1 and not np.isnan(line):
                    return line, oh, oa
            except (ValueError, TypeError):
                continue
    return np.nan, np.nan, np.nan


# ==============================================================================
# 3. SEASON TRACKER (Rolling WIM Engine)
# ==============================================================================

class SeasonTracker:
    """Processes a season match-by-match, maintaining rolling WIM state."""

    def __init__(self, season_year):
        self.season_year = season_year
        self.teams = {}
        self.match_count = 0
        self.checkpoints = {}
        self.match_records = []

    def _ensure_team(self, name):
        if name not in self.teams:
            self.teams[name] = {
                'gf': 0, 'ga': 0, 'w': 0, 'd': 0, 'l': 0, 'gp': 0, 'pts': 0
            }

    def _team_log_ratio(self, name):
        t = self.teams.get(name)
        if t is None or t['gp'] == 0:
            return 0.0
        gf = max(t['gf'], 1)
        ga = max(t['ga'], 1)
        return np.log(gf / ga)

    # ------------------------------------------------------------------
    # Core metric computations (from current cumulative state)
    # ------------------------------------------------------------------

    def compute_wim(self):
        active = [t for t in self.teams.values() if t['gp'] > 0]
        if not active:
            return 0.0
        log_ratios = []
        for t in active:
            gf, ga = max(t['gf'], 1), max(t['ga'], 1)
            log_ratios.append(abs(np.log(gf / ga)))
        return float(np.mean(log_ratios))

    def compute_wim_tb(self):
        active = {n: t for n, t in self.teams.items() if t['gp'] > 0}
        if len(active) < 8:
            return np.nan
        ratios = {n: max(t['gf'], 1) / max(t['ga'], 1) for n, t in active.items()}
        ranked = sorted(ratios, key=ratios.get, reverse=True)
        tb = ranked[:4] + ranked[-4:]
        vals = [abs(np.log(ratios[n])) for n in tb]
        return float(np.mean(vals))

    def compute_ns(self):
        active = [t for t in self.teams.values() if t['gp'] > 0]
        if len(active) < 2:
            return 0.0
        win_pcts = [t['w'] / t['gp'] for t in active]
        avg_gp = np.mean([t['gp'] for t in active])
        asd = float(np.std(win_pcts, ddof=0))
        isd = 0.5 / np.sqrt(avg_gp) if avg_gp > 0 else 0.0
        return asd / isd if isd > 0 else 0.0

    def min_games_played(self):
        if not self.teams:
            return 0
        return min(t['gp'] for t in self.teams.values())

    def get_table(self):
        rows = []
        for name, t in self.teams.items():
            gf, ga = max(t['gf'], 1), max(t['ga'], 1)
            rows.append({
                'Team': name, 'GP': t['gp'], 'W': t['w'], 'D': t['d'],
                'L': t['l'], 'GF': t['gf'], 'GA': t['ga'],
                'GD': t['gf'] - t['ga'], 'Pts': t['pts'],
                'Ratio': t['gf'] / max(t['ga'], 1),
                'LogRatio': np.log(gf / ga),
            })
        return sorted(rows, key=lambda x: (-x['Pts'], -x['GD'], -x['GF']))

    def get_leader(self):
        table = self.get_table()
        if len(table) < 2:
            return ('', 0, 0)
        return (table[0]['Team'], table[0]['Pts'],
                table[0]['Pts'] - table[1]['Pts'])

    # ------------------------------------------------------------------
    # Snapshot for checkpoint storage
    # ------------------------------------------------------------------

    def _snapshot(self):
        leader, pts, gap = self.get_leader()
        table = self.get_table()
        bottom_3 = [r['Team'] for r in table[-3:]] if len(table) >= 3 else []
        return {
            'matchday': self.min_games_played(),
            'matches_played': self.match_count,
            'wim': self.compute_wim(),
            'wim_tb': self.compute_wim_tb(),
            'ns': self.compute_ns(),
            'leader': leader,
            'leader_pts': pts,
            'points_gap': gap,
            'bottom_3': bottom_3,
            'table': table,
        }

    # ------------------------------------------------------------------
    # Process one match
    # ------------------------------------------------------------------

    def process_match(self, row, cols):
        home, away = row['HomeTeam'], row['AwayTeam']
        hg, ag = int(row['FTHG']), int(row['FTAG'])
        result = str(row['FTR']).strip()

        self._ensure_team(home)
        self._ensure_team(away)

        # --- Pre-match state (for backtest records) ---
        pre_wim = self.compute_wim()
        pre_ns = self.compute_ns()
        pre_min_gp = self.min_games_played()
        home_lr = self._team_log_ratio(home)
        away_lr = self._team_log_ratio(away)

        # --- Update cumulative stats ---
        for team, gf, ga in [(home, hg, ag), (away, ag, hg)]:
            self.teams[team]['gf'] += gf
            self.teams[team]['ga'] += ga
            self.teams[team]['gp'] += 1

        if result == 'H':
            self.teams[home]['w'] += 1
            self.teams[home]['pts'] += 3
            self.teams[away]['l'] += 1
        elif result == 'A':
            self.teams[away]['w'] += 1
            self.teams[away]['pts'] += 3
            self.teams[home]['l'] += 1
        elif result == 'D':
            self.teams[home]['d'] += 1
            self.teams[away]['d'] += 1
            self.teams[home]['pts'] += 1
            self.teams[away]['pts'] += 1

        self.match_count += 1

        # --- Odds ---
        odds_h, odds_d, odds_a = get_best_odds(row, cols)
        ah_line, ah_h, ah_a = get_asian_handicap(row, cols)

        self.match_records.append({
            'season': self.season_year,
            'date': row['Date'],
            'home': home, 'away': away,
            'hg': hg, 'ag': ag, 'result': result,
            'match_num': self.match_count,
            'min_gp': pre_min_gp,
            'wim': pre_wim,
            'ns': pre_ns,
            'odds_h': odds_h, 'odds_d': odds_d, 'odds_a': odds_a,
            'ah_line': ah_line,
            'home_log_ratio': home_lr,
            'away_log_ratio': away_lr,
            'wim_margin': home_lr - away_lr,
        })

        # --- Checkpoint check ---
        new_min_gp = self.min_games_played()
        for cp in CHECKPOINT_MATCHDAYS:
            if new_min_gp >= cp and cp not in self.checkpoints:
                self.checkpoints[cp] = self._snapshot()

    # ------------------------------------------------------------------
    # Process entire season
    # ------------------------------------------------------------------

    def process_season(self, df):
        cols = df.columns.tolist()
        for _, row in df.iterrows():
            self.process_match(row, cols)

        final_md = self.min_games_played()
        self.checkpoints['final'] = self._snapshot()

        return self.checkpoints, self.match_records


# ==============================================================================
# 4. MAIN PROCESSING LOOP
# ==============================================================================

def process_all_seasons(seasons):
    """Run every season through the tracker, collect checkpoints & matches."""
    all_checkpoints = {}
    all_match_records = {}

    for year, df in seasons.items():
        tracker = SeasonTracker(year)
        cp, mr = tracker.process_season(df)
        all_checkpoints[year] = cp
        all_match_records[year] = mr

    return all_checkpoints, all_match_records


# ==============================================================================
# 5. BACKTEST 1 — TITLE PERSISTENCE
# ==============================================================================

def run_title_backtest(all_checkpoints):
    """At matchday 19, does the leader go on to win the title?"""

    rows = []
    for year, cps in sorted(all_checkpoints.items()):
        if year == CURRENT_SEASON:
            continue
        if 19 not in cps or 'final' not in cps:
            continue

        md19 = cps[19]
        final = cps['final']

        rows.append({
            'season': year,
            'md19_wim': md19['wim'],
            'md19_wim_tb': md19['wim_tb'],
            'md19_ns': md19['ns'],
            'md19_leader': md19['leader'],
            'md19_gap': md19['points_gap'],
            'champion': final['leader'],
            'leader_held': int(md19['leader'] == final['leader']),
        })

    df = pd.DataFrame(rows)
    if df.empty:
        return df

    # Z-scores relative to cross-season distribution at MD19
    for col, zcol in [('md19_wim', 'wim_z'), ('md19_ns', 'ns_z')]:
        mu, sigma = df[col].mean(), df[col].std(ddof=1)
        df[zcol] = (df[col] - mu) / sigma if sigma > 0 else 0.0

    return df


def print_title_backtest(bt):
    """Pretty-print title persistence results."""
    print("\n" + "=" * 80)
    print("BACKTEST 1: TITLE PERSISTENCE (Matchday 19 Leader -> Champion?)")
    print("=" * 80)

    if bt.empty:
        print("No completed seasons with MD19 data found.")
        return

    # Summary table
    cols = ['season', 'md19_leader', 'champion', 'leader_held',
            'md19_gap', 'md19_wim', 'wim_z', 'md19_ns', 'ns_z']
    print(bt[cols].to_string(index=False, float_format='{:.3f}'.format))

    total = len(bt)
    held = bt['leader_held'].sum()
    print(f"\nOverall: MD19 leader won the title {held}/{total} "
          f"({100*held/total:.0f}%) of the time")

    # WIM stability: correlation with final WIM
    print("\n--- WIM Stability ---")
    final_wims = []
    for _, row in bt.iterrows():
        # We stored final WIM in checkpoints but not in bt directly;
        # report md19 stats instead
        pass
    print(f"MD19 WIM  mean={bt['md19_wim'].mean():.4f}  "
          f"std={bt['md19_wim'].std():.4f}")
    print(f"MD19 NS   mean={bt['md19_ns'].mean():.4f}  "
          f"std={bt['md19_ns'].std():.4f}")

    # Segmented analysis
    print("\n--- Segmented by WIM_Z ---")
    for label, mask in [
        ('WIM_Z > +1.0 (Polarized)', bt['wim_z'] > 1.0),
        ('-1.0 <= WIM_Z <= +1.0 (Normal)', (bt['wim_z'] >= -1.0) & (bt['wim_z'] <= 1.0)),
        ('WIM_Z < -1.0 (Compressed)', bt['wim_z'] < -1.0),
    ]:
        sub = bt[mask]
        if len(sub) == 0:
            print(f"  {label}: No seasons")
            continue
        rate = sub['leader_held'].mean()
        avg_gap = sub['md19_gap'].mean()
        print(f"  {label}: {len(sub)} seasons, "
              f"leader held {100*rate:.0f}%, avg gap {avg_gap:.1f} pts")

    # Compare WIM vs Gap as predictors
    print("\n--- Predictive Comparison ---")
    if len(bt) >= 5:
        from scipy import stats as sp_stats
        r_wim, p_wim = sp_stats.pointbiserialr(bt['leader_held'], bt['wim_z'])
        r_gap, p_gap = sp_stats.pointbiserialr(bt['leader_held'], bt['md19_gap'])
        r_ns, p_ns = sp_stats.pointbiserialr(bt['leader_held'], bt['ns_z'])
        print(f"  WIM_Z  vs leader_held: r={r_wim:.3f}, p={p_wim:.3f}")
        print(f"  Gap    vs leader_held: r={r_gap:.3f}, p={p_gap:.3f}")
        print(f"  NS_Z   vs leader_held: r={r_ns:.3f}, p={p_ns:.3f}")


# ==============================================================================
# 6. BACKTEST 2 — UPSET FREQUENCY
# ==============================================================================

def run_upset_backtest(all_match_records):
    """Do favorites overperform in high-WIM regimes?"""

    records = []
    for year, mr in all_match_records.items():
        if year == CURRENT_SEASON:
            continue
        records.extend(mr)

    df = pd.DataFrame(records)
    if df.empty:
        return df

    # Filter: need valid odds and enough games played
    df = df.dropna(subset=['odds_h', 'odds_a'])
    df = df[df['min_gp'] >= MIN_GP_FOR_BACKTEST]

    # Identify favorite (team with lower odds = higher implied prob)
    df['fav'] = np.where(df['odds_h'] <= df['odds_a'], 'H', 'A')
    df['fav_odds'] = np.where(df['fav'] == 'H', df['odds_h'], df['odds_a'])
    df['fav_implied_prob'] = 1.0 / df['fav_odds']

    # Did favorite win?
    df['fav_won'] = ((df['fav'] == 'H') & (df['result'] == 'H')) | \
                    ((df['fav'] == 'A') & (df['result'] == 'A'))
    df['fav_won'] = df['fav_won'].astype(int)

    # WIM regime: above or below median
    wim_median = df['wim'].median()
    df['high_wim'] = df['wim'] > wim_median

    # Asian handicap comparison (where available)
    ah_mask = df['ah_line'].notna()
    df.loc[ah_mask, 'margin'] = df.loc[ah_mask, 'hg'] - df.loc[ah_mask, 'ag']

    return df


def print_upset_backtest(ub):
    """Pretty-print upset frequency results."""
    print("\n" + "=" * 80)
    print("BACKTEST 2: UPSET FREQUENCY (WIM Regime -> Favorite Win Rate)")
    print("=" * 80)

    if ub.empty:
        print("No valid match data for backtest.")
        return

    print(f"Total matches analysed: {len(ub):,}")
    print(f"WIM median (split point): {ub['wim'].median():.4f}")

    for label, mask in [
        ('HIGH WIM (above median - polarized)', ub['high_wim']),
        ('LOW WIM  (below median - compressed)', ~ub['high_wim']),
    ]:
        sub = ub[mask]
        actual_rate = sub['fav_won'].mean()
        implied_rate = sub['fav_implied_prob'].mean()
        edge = actual_rate - implied_rate
        n = len(sub)
        print(f"\n  {label}")
        print(f"    Matches: {n:,}")
        print(f"    Favorite win rate (actual):  {100*actual_rate:.1f}%")
        print(f"    Favorite win rate (implied): {100*implied_rate:.1f}%")
        print(f"    Edge (actual - implied):     {100*edge:+.1f}%")

    # Quartile analysis for finer granularity
    ub['wim_quartile'] = pd.qcut(ub['wim'], 4, labels=['Q1_Low', 'Q2', 'Q3', 'Q4_High'])
    print("\n  --- By WIM Quartile ---")
    for q in ['Q1_Low', 'Q2', 'Q3', 'Q4_High']:
        sub = ub[ub['wim_quartile'] == q]
        if len(sub) == 0:
            continue
        actual = sub['fav_won'].mean()
        implied = sub['fav_implied_prob'].mean()
        print(f"    {q}: n={len(sub):,}  actual={100*actual:.1f}%  "
              f"implied={100*implied:.1f}%  edge={100*(actual-implied):+.1f}%")

    # Asian handicap analysis
    ah_sub = ub.dropna(subset=['ah_line', 'margin'])
    if len(ah_sub) > 50:
        print(f"\n  --- Asian Handicap vs WIM Margin ---")
        print(f"    Matches with AH data: {len(ah_sub):,}")
        corr = ah_sub['wim_margin'].corr(ah_sub['ah_line'])
        print(f"    Correlation (WIM margin vs AH line): {corr:.3f}")
        ah_sub['wim_fav_home'] = ah_sub['wim_margin'] > 0
        ah_sub['ah_fav_home'] = ah_sub['ah_line'] < 0
        agree = (ah_sub['wim_fav_home'] == ah_sub['ah_fav_home']).mean()
        print(f"    WIM and AH agree on favorite: {100*agree:.1f}%")

        # When they disagree, who's right?
        disagree = ah_sub[ah_sub['wim_fav_home'] != ah_sub['ah_fav_home']]
        if len(disagree) > 10:
            wim_right = ((disagree['wim_fav_home'] & (disagree['margin'] > 0)) |
                         (~disagree['wim_fav_home'] & (disagree['margin'] < 0))).mean()
            print(f"    When they disagree ({len(disagree)} matches):")
            print(f"      WIM correct: {100*wim_right:.1f}%  "
                  f"AH correct: {100*(1-wim_right):.1f}%")


# ==============================================================================
# 7. CURRENT SEASON ANALYSIS (EPL 2025-26)
# ==============================================================================

def analyze_current_season(all_checkpoints, all_match_records):
    """Compute rolling WIM for current season and generate predictions."""
    if CURRENT_SEASON not in all_checkpoints:
        print("Current season data not found.")
        return None

    cps = all_checkpoints[CURRENT_SEASON]

    # Collect historical WIM at each available checkpoint
    # to compute Z-scores for the current season
    historical = {}  # matchday -> list of WIM values from past seasons
    for year, ycp in all_checkpoints.items():
        if year == CURRENT_SEASON:
            continue
        for md, snap in ycp.items():
            if isinstance(md, int):
                historical.setdefault(md, []).append(snap['wim'])

    print("\n" + "=" * 80)
    print(f"CURRENT SEASON ANALYSIS: EPL {CURRENT_SEASON}-"
          f"{int(CURRENT_SEASON)+1}")
    print("=" * 80)

    # Rolling WIM at each checkpoint
    print("\n--- Rolling WIM at Checkpoints ---")
    print(f"{'MD':>4}  {'WIM':>7}  {'WIM_Z':>7}  {'NS':>7}  "
          f"{'Leader':<20}  {'Gap':>4}")

    current_wim_z_at_latest = None
    latest_md = 0

    for md in sorted(k for k in cps if isinstance(k, int)):
        snap = cps[md]
        hist = historical.get(md, [])
        if hist and len(hist) > 1:
            mu, sigma = np.mean(hist), np.std(hist, ddof=1)
            wim_z = (snap['wim'] - mu) / sigma if sigma > 0 else 0.0
        else:
            wim_z = np.nan

        print(f"{md:>4}  {snap['wim']:>7.4f}  {wim_z:>7.2f}  "
              f"{snap['ns']:>7.4f}  {snap['leader']:<20}  {snap['points_gap']:>4}")

        if md > latest_md:
            latest_md = md
            current_wim_z_at_latest = wim_z

    # Current table
    if latest_md in cps:
        snap = cps[latest_md]
        print(f"\n--- Current Table (Matchday {latest_md}) ---")
        print(f"{'Pos':>4}  {'Team':<22}  {'GP':>3}  {'W':>3}  {'D':>3}  "
              f"{'L':>3}  {'GF':>3}  {'GA':>3}  {'GD':>4}  {'Pts':>4}  "
              f"{'Ratio':>6}  {'LogR':>6}")
        for i, t in enumerate(snap['table'], 1):
            print(f"{i:>4}  {t['Team']:<22}  {t['GP']:>3}  {t['W']:>3}  "
                  f"{t['D']:>3}  {t['L']:>3}  {t['GF']:>3}  {t['GA']:>3}  "
                  f"{t['GD']:>4}  {t['Pts']:>4}  {t['Ratio']:>6.2f}  "
                  f"{t['LogRatio']:>6.3f}")

    # Title persistence prediction based on backtest
    print("\n--- Predictions ---")
    if current_wim_z_at_latest is not None and latest_md in cps:
        leader = cps[latest_md]['leader']
        gap = cps[latest_md]['points_gap']
        print(f"Current WIM_Z at MD{latest_md}: {current_wim_z_at_latest:+.2f}")
        print(f"Current leader: {leader} (+{gap} pts)")

        if current_wim_z_at_latest > 1.0:
            print(f">> STRUCTURAL LOCK signal: League is historically polarized.")
            print(f">> Prediction: {leader} very likely to hold the title.")
        elif current_wim_z_at_latest > 0:
            print(f">> Mild polarization. Leader has an edge but not locked in.")
        elif current_wim_z_at_latest > -1.0:
            print(f">> Normal balance. Title race could go either way.")
        else:
            print(f">> COMPRESSED league. High probability of a late title change.")
            print(f">> Prediction: Title race will go to the wire.")

    return {
        'latest_md': latest_md,
        'wim_z': current_wim_z_at_latest,
        'checkpoints': cps,
    }


# ==============================================================================
# 8. VISUALIZATION
# ==============================================================================

def plot_rolling_wim(all_checkpoints):
    """Plot WIM trajectory across matchdays for all seasons."""
    fig, ax = plt.subplots(figsize=(14, 8))

    for year, cps in sorted(all_checkpoints.items()):
        mds = sorted(k for k in cps if isinstance(k, int))
        if len(mds) < 2:
            continue
        wims = [cps[md]['wim'] for md in mds]
        style = {'linewidth': 3, 'color': '#e74c3c', 'zorder': 10} \
            if year == CURRENT_SEASON \
            else {'linewidth': 1, 'alpha': 0.4, 'color': '#95a5a6'}
        label = f'{year}-{int(year)+1}' if year == CURRENT_SEASON else None
        ax.plot(mds, wims, label=label, **style)

    # Historical mean line
    hist_means = {}
    for year, cps in all_checkpoints.items():
        if year == CURRENT_SEASON:
            continue
        for md, snap in cps.items():
            if isinstance(md, int):
                hist_means.setdefault(md, []).append(snap['wim'])
    mean_mds = sorted(hist_means)
    mean_vals = [np.mean(hist_means[md]) for md in mean_mds]
    ax.plot(mean_mds, mean_vals, 'k--', linewidth=2, label='Historical Mean')

    ax.set_title('Rolling WIM Trajectory: EPL Seasons (2000-Present)', fontsize=14)
    ax.set_xlabel('Matchday (min games played by all teams)', fontsize=12)
    ax.set_ylabel('WIM Score', fontsize=12)
    ax.legend(loc='upper left', fontsize=10)
    ax.grid(True, alpha=0.3)

    path = os.path.join(OUTPUT_DIR, 'rolling_wim_trajectory.png')
    fig.savefig(path, dpi=200, bbox_inches='tight')
    plt.close(fig)
    print(f"\nChart saved: {path}")


def plot_title_backtest(bt):
    """Scatter plot: WIM_Z vs Points Gap, colored by leader_held."""
    if bt.empty or len(bt) < 3:
        return

    fig, ax = plt.subplots(figsize=(10, 7))

    held = bt[bt['leader_held'] == 1]
    lost = bt[bt['leader_held'] == 0]

    ax.scatter(held['wim_z'], held['md19_gap'], c='green', s=100,
               label='Leader Won Title', zorder=5, edgecolors='black')
    ax.scatter(lost['wim_z'], lost['md19_gap'], c='red', s=100,
               label='Leader Lost Title', zorder=5, edgecolors='black',
               marker='X')

    for _, r in bt.iterrows():
        ax.annotate(r['season'], (r['wim_z'], r['md19_gap']),
                    fontsize=7, ha='center', va='bottom')

    ax.axvline(0, color='black', linewidth=1)
    ax.set_xlabel('WIM Z-Score at Matchday 19', fontsize=12)
    ax.set_ylabel('Points Gap at Matchday 19', fontsize=12)
    ax.set_title('Title Persistence: Does Midseason WIM Predict the Champion?',
                 fontsize=13)
    ax.legend(fontsize=10)
    ax.grid(True, alpha=0.3)

    path = os.path.join(OUTPUT_DIR, 'backtest_title_persistence.png')
    fig.savefig(path, dpi=200, bbox_inches='tight')
    plt.close(fig)
    print(f"Chart saved: {path}")


# ==============================================================================
# 9. SAVE OUTPUTS
# ==============================================================================

def save_outputs(bt, ub, current_info, all_checkpoints):
    """Save CSV files and prediction log."""
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    # Title backtest CSV
    if not bt.empty:
        path = os.path.join(OUTPUT_DIR, 'backtest_title_persistence.csv')
        bt.to_csv(path, index=False)
        print(f"Saved: {path}")

    # Upset backtest summary CSV
    if not ub.empty:
        summary_rows = []
        for label, mask in [('HIGH_WIM', ub['high_wim']), ('LOW_WIM', ~ub['high_wim'])]:
            sub = ub[mask]
            summary_rows.append({
                'regime': label,
                'n_matches': len(sub),
                'fav_win_rate_actual': sub['fav_won'].mean(),
                'fav_win_rate_implied': sub['fav_implied_prob'].mean(),
                'edge': sub['fav_won'].mean() - sub['fav_implied_prob'].mean(),
            })
        pd.DataFrame(summary_rows).to_csv(
            os.path.join(OUTPUT_DIR, 'backtest_upset_summary.csv'), index=False)
        print(f"Saved: {os.path.join(OUTPUT_DIR, 'backtest_upset_summary.csv')}")

    # Rolling WIM time-series CSV (all seasons)
    ts_rows = []
    for year, cps in sorted(all_checkpoints.items()):
        for md in sorted(k for k in cps if isinstance(k, int)):
            snap = cps[md]
            ts_rows.append({
                'season': year, 'matchday': md,
                'wim': snap['wim'], 'wim_tb': snap['wim_tb'],
                'ns': snap['ns'], 'leader': snap['leader'],
                'points_gap': snap['points_gap'],
            })
    if ts_rows:
        path = os.path.join(OUTPUT_DIR, 'rolling_wim_timeseries.csv')
        pd.DataFrame(ts_rows).to_csv(path, index=False)
        print(f"Saved: {path}")

    # Pre-registered predictions
    if current_info:
        ts = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
        cps = current_info['checkpoints']
        latest_md = current_info['latest_md']
        snap = cps.get(latest_md, {})
        leader = snap.get('leader', 'Unknown')
        gap = snap.get('points_gap', 0)
        wim_z = current_info.get('wim_z', 0)

        lines = [
            "=" * 70,
            "PRE-REGISTERED PREDICTIONS -- WIM Structural Lock Hypothesis",
            f"Generated: {ts}",
            f"Season: EPL {CURRENT_SEASON}-{int(CURRENT_SEASON)+1}",
            f"Data through: Matchday {latest_md}",
            "=" * 70,
            "",
            f"Current WIM_Z: {wim_z:+.2f}",
            f"Current Leader: {leader} (+{gap} points)",
            "",
            "PREDICTION 1 -- Title Race:",
        ]

        if wim_z is not None and wim_z > 1.0:
            lines.append(f"  {leader} will win the Premier League title.")
            lines.append(f"  Confidence: HIGH (WIM_Z = {wim_z:+.2f}, structural lock)")
        elif wim_z is not None and wim_z > 0:
            lines.append(f"  {leader} is the most likely champion but not locked in.")
            lines.append(f"  Confidence: MODERATE (WIM_Z = {wim_z:+.2f})")
        elif wim_z is not None and wim_z > -1.0:
            lines.append(f"  Title race is open. {leader} leads but challengers remain viable.")
            lines.append(f"  Confidence: LOW (WIM_Z = {wim_z:+.2f})")
        else:
            lines.append(f"  Title race is wide open. Compressed league structure.")
            lines.append(f"  Confidence: VERY LOW that {leader} holds on (WIM_Z = {wim_z:+.2f})")

        lines.append("")
        lines.append("PREDICTION 2 -- Relegation:")
        bottom_3 = snap.get('bottom_3', [])
        lines.append(f"  Current bottom 3: {', '.join(bottom_3)}")
        if wim_z is not None and wim_z > 0.5:
            lines.append("  Structural lock applies: bottom 3 are likely to go down.")
        else:
            lines.append("  League is compressed enough that survival is possible.")

        lines.append("")
        lines.append("PREDICTION 3 -- Betting Regime:")
        if wim_z is not None and wim_z > 0.5:
            lines.append("  HIGH WIM regime: favor favorites in upcoming matches.")
        else:
            lines.append("  LOW/NORMAL WIM regime: underdogs may outperform market odds.")

        lines.append("")
        lines.append("=" * 70)

        path = os.path.join(OUTPUT_DIR, 'predictions_2025.txt')
        with open(path, 'w') as f:
            f.write('\n'.join(lines))
        print(f"Saved: {path}")


# ==============================================================================
# 10. MAIN
# ==============================================================================

def main():
    print("=" * 80)
    print("WIM ROLLING PREDICTOR -- Structural Lock Hypothesis")
    print("=" * 80)

    # --- Load data ---
    print("\nLoading match data...")
    seasons = load_all_seasons()
    print(f"Loaded {len(seasons)} seasons.\n")

    # --- Process all seasons ---
    print("Processing seasons through rolling WIM engine...")
    all_checkpoints, all_match_records = process_all_seasons(seasons)
    print("Done.\n")

    # --- Create output directory ---
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    # --- Backtest 1: Title Persistence ---
    bt = run_title_backtest(all_checkpoints)
    print_title_backtest(bt)

    # --- Backtest 2: Upset Frequency ---
    ub = run_upset_backtest(all_match_records)
    print_upset_backtest(ub)

    # --- Current Season ---
    current_info = analyze_current_season(all_checkpoints, all_match_records)

    # --- Visualizations ---
    print("\nGenerating charts...")
    plot_rolling_wim(all_checkpoints)
    plot_title_backtest(bt)

    # --- Save everything ---
    print("\nSaving outputs...")
    save_outputs(bt, ub, current_info, all_checkpoints)

    print("\n" + "=" * 80)
    print("COMPLETE. All outputs saved to Prediction-Output/")
    print("=" * 80)


if __name__ == '__main__':
    main()
