# WIM Project — To-Do List

## Data
- [x] Source Euroleague basketball data — `Euroleague_2000-2025.xlsx` (25 seasons, pre-2016/17 has unequal games per team)
- [x] Source NBL (Australian basketball) data — `NBL_2020-2025.xlsx` (5 seasons; 2020-21 has bad PA/G data — PS/G duplicated, flag and exclude that season)
- [x] **Clean & convert Euroleague to `WIM Raw Data - Euroleague.csv`** — 24 valid seasons (2020-21 and 2021-22 excluded: COVID data error, PA=PF for all teams). Pre-2016/17 unequal games flagged; WIM valid, NS less reliable for those years.
- [x] **Clean & convert NBL to `WIM Raw Data - NBL.csv`** — 5 valid seasons 2021-2025 (2020-21 excluded: bad data). Small sample; treat as indicative.
- [x] Check if NBA finding (low WIM, high NS) holds in Euroleague/NBL — **CONFIRMED: Low WIM is basketball-endemic (all three leagues WIM < 0.065). High NS is NBA-specific (NBA 2.68 vs Euroleague 1.69, NBL 1.62).**
- [ ] **Investigate AFL as a high-scoring sport** — AFL avg ~100 pts/team/game yet WIM=0.193 (far above basketball). Understand why ratio variance remains high despite high scoring volume. Compare to NBA mechanics.
- [ ] **Source VFL (Victorian Football League) and WAFL (Western Australian Football League) data** — test if AFL's WIM profile is AFL-specific or Australian Rules endemic. Analogous to NBA vs Euroleague/NBL comparison. VFL = semi-pro Victoria; WAFL = state-level WA competition. Note: if VFL/WAFL WIM > AFL WIM, confirms talent-stratification gradient (weaker comps get blown out more); if similar, confirms sport-endemic scoring structure sets the floor.

## Analysis & Figures
- [x] Run full pipeline on all 16 leagues (AFL, A-League, Bundesliga, EFL Championship, Eredivisie, Euroleague, La Liga, Ligue 1, MLB, MLS, NBA, NBL, NFL, NHL, NRL, EPL)
- [x] Build WIM_Z vs NS_Z Quadrant Plot — cross-league divergence scatter (`wim_quadrant_plot.py`, output: `Data/Clean Data/WIM_Quadrant_Plot.png`). 26 seasons in Top-Left quadrant; strongest case: Eredivisie 2018-19.
- [x] Basketball deep-dive — absolute WIM comparison chart + timeseries (`wim_basketball_deepdive.py`). Outputs: `WIM_Basketball_Comparison.png`, `WIM_Basketball_Timeseries.png`.
- [x] **Formal divergence analysis: rank all seasons by |WIM_Z - NS_Z|** — `wim_divergence_and_table.py`. Key findings: Euroleague r=-0.03 (near-zero within-league correlation, highest info gain); NFL mean div=0.886; top divergent season: Euroleague 2018 (div=3.01). Output: `WIM_Divergence_Analysis.csv`, `WIM_Divergence_Plot.png`.
- [x] **Build publication-quality cross-league summary table** — Table 1 for paper. All 16 leagues with WIM mean/SD, NS mean/SD, within-league r, mean divergence. Output: `WIM_Summary_Table.csv`, `WIM_Summary_Table.png`.
- [ ] Extend rolling WIM backtesting beyond EPL to other leagues with match-level data (Bundesliga, La Liga)
- [ ] Explore secondary predictive hypotheses: does WIM predict total goals in a season? Final standings spread? Number of "surprise" results?

## Methodology
- [ ] Write formal MLB exclusion note: strategic score truncation (no running up score in bottom 9th) invalidates ln(PF/PA) as a structural signal — include as methodological footnote
- [ ] Write theoretical defence of log-ratio transformation (per Eric's recommendation at dinner)
- [ ] Address why mean absolute value rather than std dev or variance
- [ ] **Document the basketball compression effect**: basketball's high-possession game produces inherently low PF/PA ratios. This is a WIM structural finding, not a flaw.

## Paper Structure
- [ ] Literature review (Eric's recommendation #1)
- [ ] **Finalise cross-sport "False Equivalents" cases**: (a) NBA low WIM vs high NS — basketball compression + superstar concentration; (b) Man City 2018 vs Leicester 2016 within EPL; (c) Eredivisie 2019 as Top-Left quadrant example (high WIM, low NS)
- [ ] Formalise the Man City 2018 vs Leicester 2016 descriptive comparison using the quadrant framework
- [ ] Write up "informative null" predictive finding as a feature not a bug — WIM measures structure not identity

## Validation
- [ ] Document EPL 2025-26 rolling WIM weekly through May — test whether compressed-league prediction (title race stays open) holds
- [ ] After VFL/WAFL data arrives: run AFL/VFL/WAFL side-by-side comparison

## Code / Infrastructure
- [x] Clean up league name display in plots (strip "WIM Raw Data - " prefix) — done in all new scripts; legacy `wim_analysis.py` plots still show raw names
- [ ] Update `wim_analysis.py` chart titles to use short league names
