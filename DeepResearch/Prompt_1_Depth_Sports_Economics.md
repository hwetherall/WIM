# Deep Research Prompt: Sports Economics Literature (Depth)

## Your Task

I am writing an academic journal article introducing a new measure of competitive balance in sports leagues. I need you to conduct a thorough literature review within the **sports economics** field. I do not want a general summary of the field. I want you to identify **10-15 specific academic papers** that are most relevant to my work — papers I will need to cite, papers I will need to position against, and papers that might challenge or support my methodology.

For each paper, I need:
- Full citation (authors, title, journal, year)
- A 3-5 sentence summary of what the paper does
- Why it is specifically relevant to my work (how I should cite it, whether I agree or disagree with it, whether it supports or challenges my approach)

## What My Paper Does

I have developed a new metric called the **Wetherall Imbalance Measure (WIM)** for quantifying competitive balance in sports leagues. The formula is:

**WIM = (1/n) * SUM( |Ln(Points_For_i / Points_Against_i)| )**

For each team in a league season, we compute the natural log of their scoring ratio (goals scored / goals conceded in soccer, points for / points against in AFL or NFL, etc.). We take the absolute value of each team's log-ratio, then average across all teams. Higher WIM means more imbalance (dominant teams crushing weak ones); lower WIM means tighter competition.

Key properties of WIM:
- **Symmetric**: A team outscoring opponents 2:1 has the same magnitude as one outscored 1:2. The log-ratio treats dominance and weakness as mirror images.
- **Scale-invariant**: Because we use ratios, not differences, WIM can be compared across sports with different scoring volumes (e.g., NBA where teams score ~110 points vs soccer where teams score ~1.5 goals). A team that doubles its opponent's score produces the same log-ratio regardless of absolute scoring levels.
- **Captures margin dominance, not just outcome predictability**: This is the core distinction from existing metrics. WIM measures HOW MUCH teams win or lose by, not just WHETHER they win or lose.

We also compute a variant called **WIM-TB** (Top 4 + Bottom 4 only) to capture polarization at the extremes.

## How WIM Differs From Existing Metrics

The paper's central argument is that competitive balance has been treated as one-dimensional, but it is actually two-dimensional:

1. **Outcome Predictability**: How consistently do the same teams win? (Measured by Noll-Scully, HHI, Win % Standard Deviation)
2. **Margin Dominance**: When a team wins, by how much? How structurally separated are the tiers? (Measured by WIM)

These can move independently. We demonstrate this with two "False Equivalent" comparisons:

- **Man City 2017-18 EPL (NS=2.23) vs NBA 2022-23 (NS=2.18)**: Noll-Scully says these seasons were equally imbalanced. WIM reveals they are polar opposites — the EPL season was historically imbalanced (WIM Z = +1.4, top teams crushing bottom teams) while the NBA season was the most balanced in 20 years (WIM Z = -2.2, tight margins despite predictable outcomes).

- **Leicester 2015-16 EPL (NS=1.56) vs NFL 2016 (NS=1.58)**: Noll-Scully says identical balance. WIM shows the EPL was still structurally twice as unequal as the NFL (WIM 0.39 vs 0.18), because even in a "miracle" EPL season, the salary-cap-free structure produces wider scoring margins than a salary-capped league.

## What I Need From the Literature

### Category 1: The Canonical Competitive Balance Papers
The foundational papers that define the field and introduce the metrics I'm positioning against. I need to know exactly what Noll-Scully is, who formalized HHI for sports, and what the key papers are on the "uncertainty of outcome" hypothesis. These are the papers every referee will expect me to cite.

### Category 2: Critiques of Existing Metrics
Has anyone previously argued that Noll-Scully or HHI are insufficient? Has anyone pointed out the dimension I'm identifying — that outcome predictability and margin dominance are distinct? Has anyone proposed alternatives that address similar concerns? I need to know what ground has already been covered so I don't claim novelty that doesn't exist.

### Category 3: Cross-League Comparison Methodologies
Papers that have attempted to compare competitive balance across different sports or leagues (e.g., comparing the NFL to the EPL, or European soccer to American sports). What methods do they use? How do they handle the apples-to-oranges problem of different scoring systems? WIM's scale invariance via log-ratios is one of its key selling points — has anyone done something similar?

### Category 4: Log-Ratio and Ratio-Based Methods in Sports
Has anyone used log-ratios of scoring for anything in sports economics or sports analytics? This could include Pythagorean win expectation models (which use scoring ratios), Elo ratings, or any other approach that works with ratios rather than differences. I need to know the intellectual lineage of the log-ratio approach, even if no one has applied it to competitive balance measurement specifically.

### Category 5: Predictive vs Descriptive Competitive Balance
Papers that distinguish between ex-ante (before the season) and ex-post (after the season) measures of competitive balance, or that test whether competitive balance metrics have predictive power for economic outcomes (attendance, TV revenue, betting markets). I have tested WIM's predictive power against betting markets and found it has no edge over market prices for individual matches, but it does correlate strongly (r=0.83) with Asian handicap lines, confirming it measures something real. I need to position this finding within the existing predictive literature.

### Category 6: Competitive Balance and Economic Outcomes
Papers linking competitive balance measures to economic outcomes — attendance, broadcast revenue, fan engagement, team valuations. If WIM captures a dimension that Noll-Scully misses, the policy implication is that regulators (salary caps, revenue sharing, luxury taxes) may be optimizing for the wrong dimension. I need papers that have investigated the economic consequences of competitive balance to support this argument.

## Important Notes

- I am targeting journals like the **Journal of Sports Economics**, **Economic Inquiry**, **Journal of Economic Behavior & Organization**, or **European Economic Review**. The papers you find should be at a level appropriate for these outlets.
- I need **specific papers with full citations**, not general topic summaries. If you're unsure whether a paper exists, say so rather than fabricating a citation.
- Prioritize papers from the last 20 years, but include seminal older works (Rottenberg 1956, Noll 1974, Scully 1989, Fort & Quirk) that are foundational.
- If there is a paper that does something very close to what WIM does, I need to know about it immediately — this is either my biggest threat (if they anticipated the idea) or my strongest citation (if they identified the gap I'm filling).
