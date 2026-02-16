# Deep Research Prompt: Adjacent Fields (Breadth)

## Your Task

I have developed a new metric for measuring competitive balance in sports leagues. The metric uses log-ratios of scoring to quantify structural inequality. I want you to **search broadly across academic disciplines outside of sports economics** for research that uses similar mathematical approaches, measures similar phenomena, or addresses analogous problems. This is a discovery exercise — I'm looking for unexpected connections, not exhaustive coverage.

For each finding, give me:
- The field or discipline
- A brief description of the approach or paper (2-3 sentences)
- Why it might be relevant to my work (the connection you see)

I'd like you to explore at least 8-10 different fields or sub-fields. Breadth is more important than depth here.

## What My Metric Does (Technically)

The Wetherall Imbalance Measure (WIM) computes competitive balance in a league as:

**WIM = (1/n) * SUM( |Ln(X_i / Y_i)| )**

Where X_i is entity i's "output" (goals scored, points earned, revenue generated) and Y_i is entity i's "input cost" or "opposition output" (goals conceded, points against). The log-ratio has three key mathematical properties:

1. **Symmetry**: Ln(2/1) = -Ln(1/2). Dominance and subordination are treated as mirror images.
2. **Scale invariance**: The metric works regardless of the absolute magnitude of X and Y. A ratio of 2:1 produces the same log-value whether the underlying numbers are 2 and 1, or 200 and 100.
3. **Additive decomposability**: Log-ratios can be summed and averaged in ways that raw ratios cannot (because the log transforms a multiplicative scale into an additive one).

We then take the mean of the absolute values across all entities in the system to get a single measure of how "spread out" the system is.

We also use **Z-score normalization** to compare across different systems (e.g., comparing a soccer league to a basketball league by expressing each season's WIM as standard deviations from that league's historical mean).

## Fields to Explore

Below are starting points, but please go beyond these if you find relevant work elsewhere. I want surprises.

### 1. Industrial Organization / Market Concentration
The Herfindahl-Hirschman Index (HHI) measures market concentration using squared market shares. Are there IO papers that use log-ratio approaches instead of or in addition to HHI? Has anyone argued that HHI misses a dimension of market structure, analogous to how we argue Noll-Scully misses margin dominance? Are there measures of competitive intensity (not just concentration) in IO that parallel what WIM captures?

### 2. Income / Wealth Inequality (Economics)
The Gini coefficient, Theil index, and Atkinson index all measure distributional inequality. The **Theil index** in particular uses logarithms of income ratios — it may be mathematically related to WIM. Has anyone in the inequality literature made a distinction analogous to our "outcome predictability vs margin dominance" distinction? For instance, the difference between measuring whether rich people exist (concentration) vs how much richer they are than everyone else (dispersion of ratios)?

### 3. Ecology / Biodiversity
Ecologists measure species diversity using indices like the Shannon-Wiener index (which uses logarithms of proportions) and Simpson's index. These measure how "balanced" an ecosystem is — whether one species dominates or many coexist. Is there a mathematical parallel to WIM? Has anyone in ecology distinguished between different dimensions of diversity the way we distinguish predictability from margin dominance?

### 4. Information Theory
Shannon entropy measures the "surprise" or "uncertainty" in a probability distribution using logarithms. WIM's use of log-ratios may have an information-theoretic interpretation — it could be related to the Kullback-Leibler divergence between a league's actual scoring distribution and a perfectly balanced benchmark. Has anyone applied information-theoretic measures to competitive systems?

### 5. Financial Economics / Portfolio Theory
Log-returns are standard in finance (Ln(P_t / P_{t-1})). The volatility of log-returns measures how "spread out" price movements are — analogous to how WIM measures how spread out scoring ratios are. Has anyone used similar log-ratio dispersion measures to quantify market inequality, competition among firms, or concentration of returns? Are there parallels between WIM and measures like tracking error or active share in portfolio management?

### 6. Network Science / Tournament Theory
In networks, some nodes are hubs (highly connected) while others are peripheral. Measures of network inequality (degree distribution, centrality dispersion) may be structurally similar to WIM. In tournament theory (the economic theory of rank-order competitions, not sports tournaments), measures of how "spread out" effort or performance is across competitors may be related.

### 7. Physics / Statistical Mechanics
Physicists measure inequality in energy distributions (Boltzmann distribution, Gini-like measures for particle energies). The concept of "entropy" in statistical mechanics is directly related to the logarithmic measures used in information theory and potentially WIM. Are there physics papers on competitive systems or "winner-take-all" dynamics that use similar math?

### 8. Political Science / Electoral Competition
Measures of party system fragmentation (effective number of parties, electoral disproportionality indices) assess how "balanced" a political system is. Some of these use logarithmic transformations. Has anyone in political science distinguished between different dimensions of party competition in a way that parallels our predictability vs margin distinction?

### 9. Education / Testing
Psychometrics deals with measuring differences between test-takers. Item Response Theory uses log-odds (logits) extensively. Are there measures of "spread" or "inequality" in educational outcomes that use log-ratio approaches? The distinction between whether the same students always score highest (predictability) vs how far apart the scores are (margin) might resonate in this field.

### 10. Any Other Field
If you find something relevant in a field I haven't listed — operations research, computer science, evolutionary biology, linguistics, anything — include it. The most valuable finding would be a paper from a completely unexpected field that uses essentially the same mathematical structure as WIM for a different application.

## What I'm Looking For

The ideal finding is a paper or method from another field that either:
- **Uses the same math** (mean absolute log-ratio) for a different application — this would let me cite intellectual precedent from outside sports economics
- **Makes the same conceptual distinction** (e.g., concentration vs dispersion, predictability vs magnitude) — this would let me frame WIM within a broader theoretical tradition
- **Provides an axiomatic framework** that my metric satisfies — if someone in IO or inequality measurement has already proved that log-ratio-based measures satisfy desirable axioms (symmetry, scale invariance, decomposability), I can adopt their framework rather than building my own from scratch

I'm writing a paper for economics journals, so connections to economics and adjacent quantitative social sciences are most valuable, but I genuinely want breadth here. Something from ecology or physics that maps perfectly onto my problem would be a powerful citation.
