# WIM Publication Roadmap

**Prepared:** 9 July 2026  
**Objective:** submit a publication-ready economics article by **9 January 2027**.

## 1. The paper to write

Write this as a **measurement paper**, not a prediction paper and not yet a causal policy paper.

> Existing competitive-balance measures principally describe the distribution of outcomes. The Wetherall Imbalance Measure (WIM) is a complementary, season-level measure of **scoring-ratio (margin) dominance**. It reveals variation that outcome-based measures do not, subject to explicit limits on cross-sport comparability.

The best initial fit is the *Journal of Sports Economics*. Keep the *Journal of Quantitative Analysis in Sports* as a technical-methods fallback. Do not target general economics outlets such as *Economic Inquiry*, *JEBO*, or *European Economic Review* with the current design; those outlets would require a substantially broader economic or causal contribution.

The existing “WIM is not predictive of title races” result is useful, but secondary. Phrase it as **no evidence of title-persistence prediction in 25 completed EPL seasons**, not as proof of zero predictive power.

## 2. Where the project stands

### Assets worth preserving

- A clear, interpretable metric: mean absolute log scoring ratio.
- A broad descriptive database: 16 leagues, roughly 300 league-seasons, across six sports.
- Useful cross-league figures already exist, especially `WIM_Quadrant_Plot.png` and `WIM_Divergence_Plot.png`.
- The core empirical finding is compelling: WIM and Noll–Scully frequently give meaningfully different season rankings, including well chosen EPL and basketball cases.
- The rolling EPL work and bookmaker data provide a potential external-validation appendix.
- The literature research has identified the right intellectual neighbourhood: competitive-balance measurement, margin/goal-based measures, log-ratios, and betting-market uncertainty.

### Publication blockers

1. **Raw WIM is scale-invariant, but not automatically comparable across sports.** Multiplying all scores by a constant leaves WIM unchanged; different scoring processes, numbers of possessions, schedule lengths, and score variance do not. The basketball result makes this limitation visible. The paper must either use within-league standardisation as its primary cross-sport comparison or build and validate a null-calibrated WIM.
2. **The Noll–Scully comparator needs a league-appropriate implementation.** The current code uses wins/games, treating draws as zero. The standard ideal-SD benchmark also assumes binary outcomes. Football draws, NFL ties, NHL overtime rules, and unequal schedules require an explicit alternative or a limitation statement before existing divergence claims are final.
3. **Data provenance and reproducibility are incomplete.** The raw files lack a source manifest, retrieval dates, license/terms notes, transformations, and a consistent season-label convention. AFL and Premier League raw files also contain repeated header rows that the analysis silently drops.
4. **The research design must handle unequal and disrupted schedules consistently.** This includes pre-2016 Euroleague, MLS, COVID-shortened seasons, and other incomplete schedules. Current special handling is not yet systematic.
5. **The project needs a formal methods and robustness section.** The formula is intuitive, but reviewers will expect properties, comparator checks, a sensitivity analysis, and uncertainty around empirical claims.
6. **The code is not currently reproducible in a fresh environment.** The present runtime lacks the packages listed in `requirements.txt`; `scipy` is used but not listed; there are no automated tests or a one-command pipeline.

## 3. Scope discipline

### Include in the first submission

- Static season-level WIM, its mathematical properties, and its relation to outcome-based balance measures.
- A harmonised, documented primary panel and a broader 16-league validation panel.
- Within-league WIM standardisation as the primary comparison across sports.
- A null-calibration exercise if it can be completed rigorously.
- One transparent external validation using rolling EPL/match-market data, preferably in an appendix.

### Defer unless it directly fixes a reviewer-facing problem

- VFL/WAFL expansion.
- Weekly documentation of the current EPL season.
- New “structural lock” title predictions.
- Broad adjacent-field literature beyond log-ratio and inequality foundations.
- Attendance, broadcast, or policy regressions without a credible identification strategy.

## 4. Required empirical design

Create three clearly labelled samples before generating final results.

1. **Reference sample:** league-seasons with complete data, documented schedules, and a consistent inclusion rule. Use this for headline descriptive results.
2. **Harmonised sample:** a common calendar window (for example, 2005–2023 or 2005–2024, depending on verified coverage) for across-league comparisons.
3. **Full validation sample:** all 16 leagues, with every non-standard schedule, data anomaly, short panel, and COVID season visibly flagged.

Use raw WIM only to describe scoring-ratio dominance in a given competition. Use WIM Z-scores or a successfully validated null-standardised WIM for comparisons across sports.

For each league-season, record actual games, expected games, team count, average scoring, schedule type, data source, and inclusion status. Run all headline results both including and excluding disrupted/unequal-schedule seasons.

## 5. Step-by-step work plan

### Step 1 — Freeze the question and submission strategy (9–17 July)

- Write a one-page research contract: research question, precise contribution, target journal, primary sample, primary outcome comparator, and claims that are out of scope.
- Replace “WIM is a universal cross-sport balance score” with the narrower claim above until calibration evidence supports more.
- Decide that the paper’s evidence is descriptive and construct-validating, not causal.
- Set a data freeze date and a season-label convention, for example `2024–25` rather than an ambiguous `2025`.

**Deliverable:** two-page extended abstract and an outline approved by the advisor/co-author.

### Step 2 — Build a reproducible data package (10–31 July)

- Create a data dictionary and a source manifest for every league: publisher, URL/citation, retrieval date, license/redistribution status, raw fields, transformations, exclusions, and known anomalies.
- Separate immutable source data, cleaned data, generated tables, and generated figures.
- Add automated validation: unique team-season rows, valid numerical values, positive PF/PA, PF total equals PA total within a season (with a stated tolerance), actual games, and repeated-header detection.
- Remove the arbitrary “replace zero with one” calculation rule. Require strictly positive season totals or route a true zero through a documented, pre-specified sensitivity treatment.
- Create a locked environment with all dependencies, including `scipy`, pinned versions, and a fixed random seed for simulations.
- Add a minimal test suite with hand-calculated WIM examples and a single command that rebuilds every table and figure.

**Deliverable:** a fresh environment can rebuild the current outputs from documented inputs with no manual edits.

### Step 3 — Verify and organise the literature (17 July–14 August)

- Treat the existing AI research files as a discovery list, not a citable bibliography. Obtain and read the original papers; verify every author, year, title, journal, DOI, and substantive claim.
- Build a citation manager library with three tags: foundations (Rottenberg, Noll/Scully, Fort/Quirk), measurement competitors (including outcome, entropy, and goal/margin measures), and economic relevance/market validation.
- Write a short comparison matrix: object measured, data level, treatment of draws/schedule length, cross-league claim, and direct distinction from WIM.
- Give special attention to the closest novelty risks: Deb’s goal-based index, Salaga’s margin-based work, log-ratio sports-performance work, and modern corrected outcome-balance measures.

**Deliverable:** a verified 25–35 reference bibliography and a 1,500-word literature section draft.

### Step 4 — Formalise the measure and repair the benchmark (3–28 August)

- State WIM’s domain and prove its core properties: scale invariance, reciprocal symmetry, team-order invariance, and zero under team-level scoring parity.
- State what WIM does **not** guarantee: invariance to game count, schedule strength, score-generating process, or cardinal cross-sport comparability.
- Construct toy leagues showing identical outcome dispersion with different scoring-ratio dominance, and vice versa.
- Recalculate outcome comparators using a documented tie-aware convention. Report a sensitivity table using at least two defensible conventions where relevant.
- Add established alternatives: win/points dispersion, HHI/concentration, and a margin-based comparator. Do not make the argument depend on one version of Noll–Scully.

**Deliverable:** locked methods section, comparator appendix, and reproducible toy examples.

### Step 5 — Calibrate the cross-sport claim (17 August–18 September)

- Make within-league standardised WIM the default headline statistic.
- Test a null-calibration: simulate balanced seasons using each league’s documented team count, schedule, scoring level, and score dispersion; then express observed WIM relative to its null distribution.
- If a credible common null cannot be built across the reference sample, drop raw cross-sport ranking claims. Present the basketball compression result as a substantive finding and a limitation, not proof that the raw values are universally comparable.
- Pre-specify the main panel, the full-sample panel, and exclusion/sensitivity panels before looking at the refreshed figures.

**Decision gate (18 September):** retain a null-standardised cross-sport result only if its assumptions can be clearly defended. Otherwise the paper’s cross-sport evidence remains comparative within leagues.

### Step 6 — Run the primary analysis and robustness checks (21 September–16 October)

- Rebuild all results from the cleaned reference sample.
- Report correlations, rank agreement, and concrete matched examples; do not rely solely on a pooled Pearson correlation as proof of a distinct construct.
- Demonstrate whether WIM retains a distinct association with future or held-out margin characteristics after controlling for outcome-based measures. A first-half/second-half design is preferable where match data are available.
- Test sensitivity to: common years, COVID exclusions, unequal schedules, team-count changes, top/bottom variants, score transformations, and each outcome-comparator convention.
- Report confidence intervals or bootstrap intervals for central cross-league summaries rather than only point estimates and p-values.

**Deliverable:** a locked results folder with a pre-written robustness table and a decision log for every changed result.

### Step 7 — Use rolling EPL and betting analysis correctly (5–23 October)

- Update the 2025–26 EPL file to a complete season or remove the current-season narrative. The repository’s prediction file is frozen at matchday 25 (12 February 2026), so it cannot be presented as a current result in July 2026.
- Relegate title-persistence testing to an appendix unless a larger multi-league sample becomes available quickly.
- For the market exercise, test a precise external-validation claim: whether a pre-match team-level WIM component is associated with closing Asian-handicap expectations, with clustered uncertainty and comparison to simple goal difference.
- Do not frame failure to beat bookmakers as a failure of WIM; do not frame an in-sample correlation as proof of independent predictive value.

**Deliverable:** one bounded external-validation table/figure and an appendix write-up.

### Step 8 — Produce the first complete paper (19 October–20 November)

- Write in this order: methods, data, results, introduction, literature, discussion, abstract.
- Keep the main text to a journal-appropriate length; move league-by-league narration, rolling prediction, every case study, and full tables to an online appendix.
- Retain the quadrant plot after refreshing it. Simplify the legend/annotation density and make colour choices accessible.
- Add a conceptual first figure and a calibration/robustness figure. Use the existing divergence plot as a secondary figure or appendix figure.

**Deliverable (20 November):** a complete draft, replication package, cover-letter skeleton, and a one-page non-technical summary.

### Step 9 — Serious review and revision (23 November–18 December)

- Send the full draft to the advisor/co-author and 2–3 independent readers with a short review rubric: novelty, construct definition, comparator fairness, cross-sport claim, data trust, and readability.
- Run a referee simulation: identify the three most damaging plausible objections and make each answer visible in the manuscript or appendix.
- Freeze data and code by 18 December. New analyses after this date require an explicit reason and a rerun of the full pipeline.

**Deliverable:** revised submission manuscript with a response-to-readers memo.

### Step 10 — Submission package (21 December–9 January)

- Apply the selected journal’s current formatting, anonymisation, data-availability, conflict, and AI-use policies.
- Check all citations against original sources, every number against regenerated outputs, and every figure against its underlying table.
- Deposit code and redistributable data; provide lawful retrieval instructions for restricted sources.
- Ask one final reader to read only the abstract, introduction, figures, and conclusion. Submit by 9 January 2027.

**Deliverable:** submitted manuscript, replication archive, and an archived version of the exact submission files.

## 6. Proposed paper structure and evidence package

1. **Introduction:** competitive balance is multi-dimensional; identify the margin-dominance gap and contributions.
2. **Literature:** distinguish outcome dispersion, persistence, concentration, and existing margin/goal measures.
3. **Measure:** definition, properties, examples, and limits.
4. **Data and design:** sources, samples, inclusion rules, comparators, and reproducibility.
5. **Results:** WIM versus outcome-based measures, matched cases, and calibrated cross-league evidence.
6. **Validation and robustness:** schedule/disruption sensitivity, held-out margins, and bounded betting-market validation.
7. **Discussion:** what WIM adds, what it does not measure, and implications for how leagues describe balance.

Suggested main-text exhibits:

- **Table 1:** data coverage, schedule features, scoring environment, and sources.
- **Figure 1:** toy example where outcome balance and margin dominance separate.
- **Figure 2:** refreshed WIM versus outcome-balance quadrant plot.
- **Table 2:** comparison with all benchmark measures, including sensitivity to draws.
- **Figure 3:** null calibration or explicit within-league standardisation evidence.
- **Table 3:** robustness and held-out-margin validation.
- **Figure 4:** two carefully selected real-world cases, not a long collection of anecdotes.

## 7. Immediate next actions

1. Write and agree the two-page research contract.
2. Build the data/source manifest and validation checks before adding a single new league.
3. Recompute the benchmark measures with a tie-aware and schedule-aware specification.
4. Decide, by 18 September, whether null-calibrated raw cross-sport comparison is defensible.
5. Start the methods and literature sections now; do not wait for every robustness result to be complete.

If these five actions happen on schedule, the project has a realistic path to a high-quality sports-economics submission inside six months.
