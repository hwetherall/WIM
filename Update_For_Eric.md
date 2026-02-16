# WIM Project Update — Predictive Testing Results

Eric,

Following your three recommendations at dinner (literature review, log-ratio defence, and predictive testing), I've been working on the third one first — testing whether WIM has any predictive power, or whether it's purely descriptive. Below is a detailed summary of what we built, what we found, and what it means for the paper. The results are genuinely interesting, though not in the direction I expected.

---

## Quick Recap: What WIM Is

In case it's useful to have this written down in one place:

WIM (Wetherall Imbalance Measure) is a single number that captures how competitively balanced a sports league season is. The formula:

**WIM = Mean( |Ln(Points For / Points Against)| )**

For each team, we take the natural log of their scoring ratio (goals scored / goals conceded in football, points for / points against in AFL, etc.). The log ensures symmetry — a team that outscores opponents 2:1 produces the same magnitude as one outscored 1:2. We then take the absolute value of each team's log-ratio and average across the whole league.

- **Low WIM**: Teams are closely matched. Scoring margins are tight across the board.
- **High WIM**: The league is structurally polarized. Strong teams crush weak teams by large margins.

The existing standard metric in the sports economics literature is the Noll-Scully ratio, which has been used since the 1970s. Noll-Scully measures how dispersed win percentages are — essentially, how predictable outcomes are. Our key descriptive finding (which I showed you at dinner) is that Noll-Scully and WIM can give opposite readings of the same season, because Noll-Scully measures **outcome predictability** (did the better team win?) while WIM measures **margin dominance** (by how much?). A league can have predictable outcomes but tight margins, or unpredictable outcomes but blowout margins. Only WIM distinguishes these cases.

---

## The Predictive Question

You rightly pushed me on whether WIM is "just reading yesterday's newspaper in greater detail" or whether it has forward-looking value. To test this, I built a system that computes WIM not just at the end of a season, but **during** a season, match by match. This is what I'm calling "rolling WIM" — after each matchday, we recalculate WIM from cumulative season-to-date goals for every team.

The central hypothesis I tested was what I called the **"Structural Lock"**: if rolling WIM is high at the halfway point of a season (the league is structurally polarized), the current league leader should be more likely to hold their position and win the title. The reasoning is that high WIM reflects genuine tier separation — the strong teams aren't just on a lucky streak, they're structurally dominant — so the leader should be "locked in." Conversely, if midseason WIM is low (a compressed league), upsets should be more likely and the title race should be more open.

---

## What We Built

I sourced match-by-match data for every Premier League season from 2000-01 through the current 2025-26 season (26 seasons, ~9,500 matches total) from football-data.co.uk. Critically, this data also includes bookmaker odds for every match — market prices from Bet365, Pinnacle, BetBrain averages, and market-wide averages — which means we can backtest against what the market thought at the time without needing any additional data.

The script (`wim_rolling_predictor.py`) processes every match chronologically within each season, maintaining a running tally of goals scored and conceded for every team. At configurable checkpoints (matchday 10, 15, 19, 25, 30, and season end), it takes a snapshot: the rolling WIM, the rolling Noll-Scully, who leads the table, and what the points gap is. It then runs two formal backtests and applies the results to the current season.

---

## Backtest 1: Title Persistence

**Question**: At matchday 19 (the halfway point of a 38-game EPL season), does WIM predict whether the current league leader goes on to win the title?

**Method**: For each of the 25 completed seasons, I recorded who led the table at matchday 19, the WIM at that point, the Noll-Scully at that point, and the points gap to second place. I then checked whether the MD19 leader actually won the championship. I computed Z-scores for WIM and Noll-Scully relative to the cross-seasonal distribution, and tested correlations using point-biserial correlation.

**Results**:

The matchday 19 leader went on to win the title **16 out of 25 times (64%)**. The key finding is what predicts whether they hold:

| Predictor | Correlation with Title Won | p-value |
|---|---|---|
| **Points gap at MD19** | r = 0.380 | p = 0.061 |
| Noll-Scully Z at MD19 | r = 0.181 | p = 0.387 |
| **WIM Z at MD19** | **r = -0.004** | **p = 0.986** |

WIM has **zero predictive power** for title persistence. The correlation is as close to nothing as you can get. The only variable with marginal significance is the simple points gap — which, in retrospect, is the obvious answer. If you're 13 points clear at Christmas, you probably win. If you're level, it's a coin flip.

The segmented analysis is even more striking:

| Midseason WIM | Seasons | Leader Held Title |
|---|---|---|
| WIM_Z > +1.0 (Polarized) | 3 | **33%** |
| -1.0 to +1.0 (Normal) | 18 | **72%** |
| WIM_Z < -1.0 (Compressed) | 4 | **50%** |

If anything, the relationship runs **opposite** to what I hypothesized. In the three most polarized midseasons, the leader held only once (Man City 2017-18, the 100-point season). The other two — Arsenal leading in 2007-08 and Liverpool leading in 2018-19 — lost to rivals (Man United and Man City respectively) who were also operating at extreme scoring ratios.

**Interpretation**: This makes sense once you think about it. High WIM at midseason means the league has strong tier separation — but it means **multiple** elite teams are generating extreme scoring ratios, not just the leader. Arsenal in 2007-08 had a WIM contribution (log-ratio) of about +0.90, but Man United had +0.85. Both were pulling the league's WIM upward. WIM correctly identifies that the league is polarized, but the title is decided *within the top tier*, and WIM doesn't tell you which team in the top tier prevails.

This is actually a valuable finding for the paper. It cleanly demonstrates that WIM measures **league structure** — the shape of the competitive landscape — not the identity of the winner. This is conceptually distinct from what points gap or Noll-Scully measure, and it supports the argument that WIM captures a dimension of competitive balance that existing metrics miss.

---

## Backtest 2: Upset Frequency

**Question**: In matches played during high-WIM regimes (when the league is structurally polarized), do favorites win more often than the bookmaker's odds imply?

**Method**: For every match with valid bookmaker odds in the dataset (8,158 matches after filtering), I identified the bookmaker favorite (the team with lower odds), recorded whether they won, and segmented by whether the season-to-date WIM was above or below the historical median at the time of the match.

**Results**:

| WIM Regime | Matches | Fav Win Rate (Actual) | Fav Win Rate (Market Implied) | Edge |
|---|---|---|---|---|
| High WIM (above median) | 4,079 | 55.2% | 56.4% | -1.2% |
| Low WIM (below median) | 4,079 | 53.6% | 55.7% | -2.2% |

In both regimes, the market slightly overestimates the favorite's chances (the "edge" is negative, meaning you'd lose money backing favorites at the offered odds). This is the bookmaker's margin — it's how they make money, and it's well-known in the literature.

The interesting finding is that favorites win **more often** in high-WIM regimes (55.2% vs 53.6%), which is consistent with the logic — when the league is polarized, better teams really are better, and upsets are rarer. But the market **already prices this in**: the implied probability is also higher in high-WIM regimes (56.4% vs 55.7%). The net edge is slightly less negative in high WIM (-1.2% vs -2.2%), but not enough to generate a profitable strategy.

I also compared WIM-derived team-level margins to the Asian handicap line (the bookmaker's explicit prediction of the expected goal margin). The correlation between WIM's implied margin and the market's handicap line is **-0.83** (the sign is negative because of how the handicap is defined, but the magnitude indicates strong agreement). When WIM and the market disagree on who should be favored, the market is right **68%** of the time.

**Interpretation**: The betting market is efficient. It already incorporates the structural information that WIM captures, probably through other means (individual team analysis, historical performance models, in-house ratings). WIM doesn't provide an informational edge for individual match predictions. This isn't surprising — billions of dollars flow through these markets, and sophisticated quantitative models are already in use. But it does confirm that WIM is measuring something *real* — it correlates strongly with market prices, which are the most information-dense signals available.

---

## Current Season: EPL 2025-26

The rolling WIM chart below tells a striking story (the file is saved in `Prediction-Output/rolling_wim_trajectory.png`). The gray lines are historical EPL seasons, the dashed black line is the 25-year historical mean, and the red line is the current 2025-26 season:

The current season started near the historical average (WIM = 0.51 at matchday 10, Z = -0.07) but has been **declining steadily** and now sits at **WIM = 0.316 at matchday 25, with a Z-score of -1.60**. This makes it one of the most compressed — most competitively balanced — EPL seasons in the 25-year dataset.

The current table at matchday 25:

| Pos | Team | Pts | GF | GA | GD | Ratio |
|---|---|---|---|---|---|---|
| 1 | Arsenal | 56 | 49 | 17 | +32 | 2.88 |
| 2 | Man City | 50 | 51 | 24 | +27 | 2.12 |
| 3 | Aston Villa | 47 | 36 | 27 | +9 | 1.33 |
| 4 | Man United | 44 | 46 | 36 | +10 | 1.28 |
| ... | | | | | | |
| 18 | West Ham | 23 | 31 | 48 | -17 | 0.65 |
| 19 | Burnley | 15 | 25 | 49 | -24 | 0.51 |
| 20 | Wolves | 8 | 16 | 48 | -32 | 0.33 |

Arsenal lead by 6 points, but the mid-table compression is remarkable — 12 teams sit between 29 and 44 points. The gap between 3rd and 16th is only 18 points. For context, in the 2017-18 season (Man City's 100-point year, WIM_Z = +1.86 at midseason), the equivalent gap was 33 points.

**What WIM tells us**: This is a season where the competitive structure is historically flat. Arsenal are clear leaders, but below them the league is a tightly packed mass. WIM can't tell us whether Arsenal will hold on (that's a function of their points gap, which is a separate variable). But it tells us the *environment* they're operating in is one where the chasing pack, while individually unexceptional, is collectively deep. If Arsenal falter, there are multiple teams positioned to capitalize.

---

## What This Means for the Paper

The predictive testing exercise produced what I'd call an **"informative null result"**:

1. **WIM is not predictive of title races.** It has zero correlation with whether the midseason leader holds on. This means WIM measures something genuinely distinct from "who will win" — it measures the structural shape of competition.

2. **WIM is not predictive beyond what markets already price.** Bookmaker odds already incorporate the information WIM captures, and they do it slightly better at the individual match level.

3. **WIM *is* measuring something real.** Its team-level log-ratios correlate 0.83 with market handicap lines, confirming it captures genuine competitive structure. And the favorite win rate *is* higher in high-WIM regimes, confirming the theoretical link between league polarization and outcome certainty.

For positioning the paper, I think this is actually cleaner than if WIM had turned out to be a magical predictor. The contribution is:

- **WIM is a descriptive tool that captures a dimension of competitive balance that Noll-Scully misses.** (The "False Equivalents" we discussed at dinner — Man City 2018 vs NBA 2023, Leicester 2016 vs NFL 2016 — remain the core contribution.)
- **WIM is validated by market prices** (strong correlation with Asian handicap lines), but is not redundant with them.
- **WIM does not predict winners**, which reinforces that it measures **league structure**, not team quality. This is a conceptually important distinction for the competitive balance literature.

The paper doesn't need WIM to be predictive. It needs WIM to be **informative about a dimension of competition that existing metrics ignore**. The descriptive evidence already demonstrates this. The predictive testing demonstrates that this dimension exists independently of title certainty, which actually strengthens the case that WIM is capturing something new.

---

## Next Steps

I'm continuing to work on the literature review and the log-ratio defence in parallel. For the predictive testing, I plan to:

1. **Document the current season as it plays out** — updating the rolling WIM weekly through May and recording whether the compressed-league prediction (title race stays open) holds.
2. **Extend the rolling WIM analysis to other leagues** once the RA delivers the match-level data for Ligue 1, AFL, etc. Cross-league predictive testing with a larger sample would strengthen the "informative null" considerably.
3. **Consider alternative predictive hypotheses** — even though WIM doesn't predict title persistence, it might predict other things: total goals in a season, the tightness of the final standings, the number of "surprise" results. These are secondary, but worth exploring.

All the code, data, and outputs are in the project repo. The key script is `wim_rolling_predictor.py` and the outputs are in `Prediction-Output/`.

Happy to walk through any of this over a call if that's easier.

Best,
Noel
