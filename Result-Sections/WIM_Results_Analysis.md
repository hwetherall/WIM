# WIM Results Analysis: What Stands Out

Using the **Wetherall Imbalance Measure (WIM)** framework from the README—where **lower WIM = more balanced** (tight, even competition) and **higher WIM = more imbalanced** (strong teams dominating by large margins)—here’s what stands out across the full results (including Bundesliga, EFL Championship, Eredivisie, La Liga, Ligue 1, MLS, NHL, NRL).

---

## 1. League-Level Picture: Who Is Most Balanced?

**Most imbalanced on average (higher WIM):**
- **Premier League** (0.416), **Eredivisie** (0.406), **Bundesliga** (0.386), **La Liga** (0.383)  
- Top European football leagues consistently show the widest scoring-ratio gaps between teams.

**Most balanced on average (lower WIM):**
- **NBA** (0.036) — by far the most balanced league in the dataset.  
- **MLB** (0.135), **NHL** (0.144) — also very balanced.  
- **AFL** (0.193), **NRL** (0.211) — moderate.

So across sports: **European top-flight football is systematically more imbalanced** than North American major leagues and some other competitions (e.g. EFL Championship, MLS), which fits the idea of a few dominant clubs vs. more parity elsewhere.

---

## 2. Outlier Seasons (|WIM Z-Score| > 2)

These are seasons that were **historically unusual** for their league—either much more balanced or much more imbalanced than that league’s own norm.

### Historically **imbalanced** (high WIM_Z)
- **La Liga 2014** — WIM_Z +2.61. Peak polarization; strong sides crushing weak ones by big margins.  
- **NHL 2021** — WIM_Z +2.38. Shortened/realigned season; a few teams dominated on scoring ratio.  
- **NBA 2008** — WIM_Z +2.41. Unusually wide spread in point differentials for the NBA.  
- **NRL 2021** — WIM_Z +2.08. Very uneven scoring ratios across the league.  
- **Bundesliga 2023** — WIM_Z +1.80 (and WIM_TB_Z +2.29). Top/bottom gap in scoring ratio was historically large (e.g. Bayern-style dominance).  
- **Eredivisie 2023** — WIM_Z +1.90. Similar story: a few teams far ahead on goals for/against.

### Historically **balanced** (low WIM_Z)
- **Bundesliga 2010** — WIM_Z **-2.35**. Unusually tight competition; no one ran away on scoring ratio.  
- **NFL 2022** — WIM_Z -2.31. One of the most balanced NFL seasons in the sample.  
- **EFL Championship 2007** — WIM_Z -1.95. Very compressed; small gaps between teams on points for/against.  
- **NBA 2023** — WIM_Z -2.20. Unusually balanced for the NBA.

### Data artifact
- **AFL 2000** — WIM_Z +4.14. Only one team in the data for that season; not meaningful for competitive balance.

---

## 3. Stories That Fit the README “Case Study” Style

### “Invincibles-style” balance: Bundesliga 2009–10
- **Bundesliga 2010** has the largest **negative** WIM Z-Score (-2.35) in the dataset.  
- Interpretation: the league was **unusually balanced** that year—no team dominated by huge scoring margins.  
- So even if one club had a great record, the *spread* of scoring ratios was historically tight (no “cakewalk” in terms of crushing everyone).

### “Peak polarization”: La Liga 2013–14
- **La Liga 2014** has WIM_Z +2.61.  
- Interpretation: **peak imbalance**—strong teams thumping weak ones by large margins (e.g. big goal differences).  
- Analogous to the README’s “Chelsea 2009–10” and “Man City 2017–18” examples: dominance reflected in scoring ratios.

### “Miracle” context: EFL Championship 2006–07
- **EFL Championship 2007** has WIM_Z -1.95 (very balanced).  
- Interpretation: the league was **unusually even**; a surprise champion or playoff winner would have had a favorable, flatter competitive environment—similar in spirit to the README’s Leicester 2015–16 narrative.

### Recent dominance: Bundesliga & Eredivisie 2022–23
- **Bundesliga 2023** and **Eredivisie 2023** both show high WIM and high WIM_Z.  
- Interpretation: **true dominance** in the README sense—a few teams not only won a lot but did so with very large scoring-ratio advantages (crushing the rest of the league on goals for/against).

---

## 4. What WIM Adds: Compared to Noll-Scully and General Consensus

These three seasons show what WIM tells you that **Noll-Scully** (win-distribution balance) and **general opinion** often don’t.

### 4a. EFL Championship 2006–07 (WIM_Z −1.95)

| Metric        | 2007 value | EFL Championship context |
|---------------|------------|---------------------------|
| WIM           | 0.181      | Lowest in the league’s 19-year sample; next lowest 2006 (0.211), 2008 (0.198). |
| Noll-Scully   | **0.94**   | Below 1.0 = more balanced than “ideal” random; also the lowest in the sample. |
| WinPct_SD     | 0.069      | Lowest of any EFL season in the data. |

**General consensus:** “Tight division.” West Brom won with 88 points; Stoke, Hull, Bristol City, and others were in the mix. People remember it as competitive and open.

**What Noll-Scully already tells you:** Win percentages were very evenly spread—no one ran away with wins, and the spread was even tighter than in a random game. So the *table* was balanced.

**What WIM adds:** It focuses on **scoring ratios** (goals for vs goals against), not just W–L. In 2006–07, no team had an extreme ratio: leaders West Brom 88–55, Stoke 69–55, Hull 65–47; bottom sides like Colchester 42–61, Scunthorpe 46–69. There were no 100+ goal attacks or 80+ goal sieves like in other seasons (e.g. 2005 Reading 99–32, or 2008 Wolves 68–40). So:

- **WIM says:** The league was balanced not only in *results* (wins) but in *scorelines*—few big blowouts, no team systematically thrashing or being thrashed.
- **Why that’s distinct:** You could have a season where Noll-Scully is low (everyone near 50% wins) but WIM high—e.g. half the teams win 5–0, the other half lose 0–5. In 2007 both are low: even results *and* even margins. So WIM backs up “tight division” with a specific claim: **the way games were won and lost was also tight** (close scorelines, compressed goal differences). That’s not something Noll-Scully or “it was a close league” alone tells you.

---

### 4b. Bundesliga 2009–10 (WIM_Z −2.35)

| Metric        | 2010 value | Bundesliga context |
|---------------|------------|--------------------|
| WIM           | 0.290      | By far the lowest in the sample; next lowest 2017 (0.315), 2022 (0.344). |
| Noll-Scully   | 1.46       | Below league average (NS_Z −1.26); win distribution was also relatively even. |
| Raw picture   | Dortmund 67–22, Bayern 81–40, Leverkusen 64–44; middle pack (Mainz 52–39, Nürnberg 47–45, Köln 47–62) very tight; bottom St Pauli 35–68, Frankfurt 31–49. |

**General consensus:** “Dortmund’s breakthrough”; “Bayern didn’t win”; “competitive title race.” The story is the *race* (who won), not whether the league had big scoring gaps.

**What Noll-Scully already tells you:** Win totals were spread more evenly than in a typical Bundesliga season—so the *table* was more balanced.

**What WIM adds:** It measures the **spread of scoring power** (PF/PA) across *all* 18 teams. In 2010 that spread was historically small for the Bundesliga: even the top sides didn’t have the kind of crushing ratios you see in other years (e.g. Bayern in many seasons), and the middle and lower teams had relatively tight for/against. So:

- **WIM says:** The league was unusually balanced in terms of **who scored what vs conceded what**—not just who got more wins. Dortmund’s title wasn’t in a league where one giant was smashing everyone; it was in a league where *no one* had an extreme scoring ratio by Bundesliga standards.
- **Why that’s distinct:** General opinion focuses on “Dortmund beat Bayern.” WIM adds: **the whole distribution of dominance (scoring ratios) was compressed.** So you get an “Invincibles-style” reading: a champion in a *genuinely* tight league, not a surprise in a league that was still dominated by huge margins elsewhere.

---

### 4c. La Liga 2013–14 (WIM_Z +2.61)

| Metric        | 2014 value | La Liga context |
|---------------|------------|-----------------|
| WIM           | 0.547      | Highest in the sample; next 2009 (0.398), 2013 (0.396). |
| Noll-Scully   | 2.39       | High (NS_Z +1.79); win distribution was very uneven. |
| Raw picture   | Barcelona 110–21, Real Madrid 118–38, Atlético 67–29, Valencia 70–32; bottom Córdoba 22–58, Almería 35–64, Granada 26–57. |

**General consensus:** “Atlético’s miracle”; “incredibly competitive”; “title race to the wire.” The narrative is the *race* (Atleti breaking the duopoly) and the drama of the final day.

**What Noll-Scully already tells you:** Win distribution was very uneven—some teams won a lot, some very little. So Noll-Scully already flags that the league was *imbalanced* in terms of results.

**What WIM adds:** It focuses on **scoring ratios**. In 2014 the top sides had enormous PF/PA (Barca and Real especially), and the bottom had very poor ones. So:

- **WIM says:** Even though the *title race* was close and historic, the **league structure** was highly polarized: the elite (and near-elite) routinely thumped the rest by large margins. So you had “competitive at the top” and “blowouts elsewhere” at the same time.
- **Why that’s distinct:** General opinion and media focus on “Atlético won, how competitive!” WIM adds: **the season was also one of the most imbalanced in La Liga history in terms of goals for and against.** So it was a “miracle” *in spite of* a league where the big clubs had huge scoring power over the bottom half—which makes Atleti’s achievement a different kind of story (overcoming a very top-heavy league) rather than a “everyone was even” story. Noll-Scully says “uneven wins”; WIM says “uneven *scorelines* and dominance”—the gap between crushing the weak and being crushed was at a peak.

---

## 5. Cross-League Takeaways

1. **WIM and Noll–Scully** are only weakly correlated in this set (-0.10). So WIM is picking up **scoring-ratio imbalance** (who blows out whom), which is partly distinct from **win-distribution** (Noll–Scully). Both are useful.

2. **Soccer vs. North American leagues**: European top flights sit at the high-WIM end; NBA/NHL/MLB at the low end. That fits structural differences: a few super-clubs vs. drafts/salary caps and more parity.

3. **Second-tier and expanding leagues**: EFL Championship and MLS sit between the two extremes—more balanced than top-flight soccer, less than NBA/NHL/MLB.

4. **Outlier seasons** (|Z| > 2) are good candidates for **case studies** (e.g. Bundesliga 2010, La Liga 2014, EFL 2007, NRL 2021, NHL 2021) in the same spirit as the README’s Premier League examples.

---

## 6. Summary Table: Outlier Seasons (|WIM_Z| > 2)

| League           | Season | WIM   | WIM_Z  | Interpretation        |
|------------------|--------|-------|--------|------------------------|
| AFL              | 2000   | 0.464 | +4.14  | Artifact (1 team)      |
| La Liga          | 2014   | 0.547 | +2.61  | Historically imbalanced |
| NBA              | 2008   | 0.048 | +2.41  | Unusually imbalanced   |
| NHL              | 2021   | 0.204 | +2.38  | Historically imbalanced |
| NRL              | 2021   | 0.310 | +2.08  | Historically imbalanced |
| Bundesliga      | 2010   | 0.290 | **-2.35** | Historically balanced  |
| NFL             | 2022   | 0.164 | -2.31  | Historically balanced  |
| NBA             | 2023   | 0.026 | -2.20  | Historically balanced  |

These seasons are the ones that most “stand out” in terms of WIM, and are the best candidates for deeper narrative analysis (e.g. league structure, rule changes, or iconic teams) in line with the README’s WIM case studies.
