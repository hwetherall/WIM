# WIM Project (Wetherall Imbalance Measure)

## What is the WIM Score?

The **Wetherall Imbalance Measure (WIM)** is a statistical metric designed to quantify competitive balance in sports leagues. Unlike simple points spreads or win percentages, WIM focuses on the *scoring ratio* between teams, providing a more nuanced view of dominance and competitiveness.

### The Formula
$$
WIM = \frac{1}{n} \sum_{i=1}^{n} \left| \ln\left(\frac{\text{Points For}_i}{\text{Points Against}_i}\right) \right|
$$

Where:
*   **Points For (PF)**: Goals/Points scored by a team.
*   **Points Against (PA)**: Goals/Points conceded by a team.
*   **Log-Ratio**: $\ln(PF/PA)$ treats dominance symmetrically (e.g., doubling your opponent's score is the mathematical inverse of them doubling yours).
*   **Mean Absolute**: We take the average magnitude of these ratios across the entire league for a season.

**Interpretation:**
*   **Lower WIM**: A more **balanced** league. Teams are evenly matched; games are tighter.
*   **Higher WIM**: A more **imbalanced** league. Strong teams dominate weak teams by large margins.

---

## The Analysis Tool

The core analysis is performed by our Python scripts, which automate the following pipeline:

1.  **Data Ingestion**: Loads historical season data for 16 leagues across 6 sports.
2.  **Cleaning**: Normalises column names and filters invalid data (see `clean_basketball_data.py` for sport-specific handling).
3.  **Calculation**: Computes WIM, Noll-Scully, Win% SD, and HHI for every season.
4.  **Z-Score Normalisation**: Compares seasons against each league's own historical average.
5.  **Visualisation**: Generates cross-league and within-league charts.

**Key scripts:**
| Script | Purpose |
|---|---|
| `wim_analysis.py` | Main pipeline — all 16 leagues |
| `wim_quadrant_plot.py` | WIM_Z vs NS_Z quadrant scatter (Figure 1) |
| `wim_basketball_deepdive.py` | Basketball cross-league comparison |
| `wim_rolling_predictor.py` | Rolling/midseason WIM, EPL backtesting |
| `clean_basketball_data.py` | Converts Euroleague/NBL Excel to WIM format |

---

## Cross-League Dataset (16 Leagues, ~300 Seasons)

| League | Sport | WIM (avg) | Noll-Scully (avg) | Seasons |
|---|---|---|---|---|
| Premier League | Football | 0.416 | 1.932 | 20 |
| Eredivisie | Football | 0.406 | 1.852 | 20 |
| Bundesliga | Football | 0.386 | 1.754 | 20 |
| La Liga | Football | 0.383 | 1.920 | 20 |
| A-League | Football | 0.341 | 1.426 | 11 |
| Ligue 1 | Football | 0.328 | 1.624 | 20 |
| EFL Championship | Football | 0.261 | 1.437 | 20 |
| MLS | Football | 0.238 | 1.158 | 20 |
| NFL | American Football | 0.235 | 1.550 | 11 |
| NRL | Rugby League | 0.211 | 1.540 | 20 |
| AFL | Australian Rules | 0.193 | 1.758 | 25 |
| NHL | Ice Hockey | 0.144 | 1.700 | 19 |
| MLB | Baseball* | 0.135 | 1.824 | 20 |
| Euroleague | Basketball | 0.062 | 1.689 | 24 |
| NBL | Basketball | 0.054 | 1.623 | 5 |
| NBA | Basketball | 0.036 | 2.676 | 20 |

*MLB note: strategic score truncation (no running up the score in the bottom of the 9th inning) partially invalidates WIM as a structural signal. Treated as indicative only.

---

## Key Findings

### Finding 1: WIM and Noll-Scully Measure Different Things

The Noll-Scully ratio (the dominant measure since the 1970s) tracks **outcome predictability** — how dispersed win percentages are. WIM tracks **margin dominance** — how extreme the scoring ratios are. These can give opposite readings of the same season.

The quadrant plot (`WIM_Quadrant_Plot.png`) plots WIM_Z vs NS_Z for all 300+ league-seasons, revealing four structurally distinct season types:

| Quadrant | Meaning | Example |
|---|---|---|
| **Top-Right** (High WIM, High NS) | Dominant — extreme scoring AND predictable outcomes | Man City 2017-18 |
| **Bottom-Left** (Low WIM, Low NS) | Compressed — tight scoring AND unpredictable outcomes | Leicester 2015-16 |
| **Bottom-Right** (Low WIM, High NS) | Tight but Predictable — consistent winners, close games | NBA (cluster) |
| **Top-Left** (High WIM, Low NS) | Chaotic — extreme margins but unpredictable outcomes | Eredivisie 2018-19 |

Overall correlation between WIM_Z and NS_Z across all seasons: **r = 0.625** — strong enough to confirm WIM measures something real, loose enough to confirm it measures something *distinct*.

### Finding 2: Basketball is a Structurally Different Sport (and That's the Point)

All three basketball leagues cluster at WIM < 0.065, more than double the gap to the next-lowest non-basketball league (MLB at 0.135):

| League | WIM | Noll-Scully |
|---|---|---|
| NBA | 0.036 | **2.676** |
| NBL | 0.054 | 1.623 |
| Euroleague | 0.062 | 1.689 |
| — | — | — |
| MLB (next lowest) | 0.135 | 1.824 |

**Low WIM is basketball-endemic**, driven by the sport's high-possession structure: many possessions per game create strong regression to the mean in scoring ratios. Even a dominant NBA team rarely achieves a season PF/PA ratio above 1.15 (ln ≈ 0.14).

**High NS is NBA-specific.** The Euroleague and NBL have ordinary Noll-Scully scores (~1.65), comparable to the AFL or NHL. The NBA's exceptional NS of 2.68 reflects superstar player concentration unique to that market — a dimension WIM is not designed to capture, and correctly does not.

### Finding 3: WIM is Not Predictive of Title Races (and That's OK)

Rolling WIM tested against 25 completed EPL seasons found **zero predictive power** for title persistence (r = -0.004, p = 0.986). The only marginal predictor was the simple points gap.

This is an informative null: WIM measures **league structure**, not the identity of winners. A polarised league (high WIM) means strong tier separation — but multiple teams can be in the top tier, and WIM does not distinguish between them. The finding reinforces that WIM captures a distinct dimension of competitive balance that existing metrics do not.

WIM team-level log-ratios do correlate at r = 0.83 with bookmaker Asian handicap lines, confirming the metric captures genuine competitive structure (markets price in the same information independently).

---

## Case Studies: The Premier League

We have applied WIM to four iconic Premier League seasons to test its validity. The results reveal that "dominance" comes in different flavours.

### 1. The Most Balanced: Arsenal 2003-04 ("The Invincibles")
*   **WIM Z-Score**: `-1.78` (Historically Balanced)
*   **Insight**: Counter-intuitively, the "Invincibles" season was the *most balanced* in our dataset. While Arsenal didn't lose, they didn't crush teams by massive margins (Goal Diff +47). The rest of the league was incredibly tight, with bottom teams remaining competitive. Arsenal's achievement was navigating a minefield, not a cakewalk.
*   [Read full analysis](Result-Sections/Arsenal_2004.md)

### 2. The Most Imbalanced: Chelsea 2009-10 ("The Ancelotti Machine")
*   **WIM Z-Score**: `+1.76` (Historically Imbalanced)
*   **Insight**: This season represents "Peak Polarisation." The top teams were ruthless (Chelsea scored 103 goals), while the bottom teams were historically weak (Wigan conceded 79). The gap between the best and worst was at its absolute widest.
*   [Read full analysis](Result-Sections/Chelsea_2010.md)

### 3. The "Miracle" Context: Leicester City 2015-16
*   **WIM Z-Score**: `-1.02` (Unusually Balanced)
*   **Insight**: WIM proves Leicester's title wasn't just a fluke run; the entire league environment was favourable. The traditional "Big 6" underperformed while mid-table teams overperformed, creating a flatter competitive landscape that allowed a consistent outsider to rise.
*   [Read full analysis](Result-Sections/Leicester_2016.md)

### 4. True Dominance: Man City 2017-18 ("The Centurions")
*   **WIM Z-Score**: `+1.25` (Unusually Imbalanced)
*   **Insight**: Unlike Arsenal's Invincibles, Pep Guardiola's Centurions *did* crush the league. With 100 points and a massive +79 Goal Difference, their extreme scoring ratio pulled the entire league's WIM upward, statistically confirming their dominance.
*   [Read full analysis](Result-Sections/ManCity_2018.md)

---

## Data Notes

*   **Euroleague pre-2016/17**: Teams played unequal numbers of games under the old Top-16 format. WIM (ratio-based) remains valid; Noll-Scully figures for those seasons are less reliable.
*   **Euroleague 2020-21 & 2021-22**: Excluded — COVID-disrupted seasons where source data shows PA = PF for all teams (data error).
*   **NBL 2020-21**: Excluded — same PA = PF data error.
*   **AFL 2000**: Excluded — single-team row (data artefact).
*   **MLB**: Included but flagged. Baseball's rules prevent running up the score (no batting in the bottom of the 9th if ahead), which truncates the scoring ratio distribution and partially invalidates WIM as a structural signal.
