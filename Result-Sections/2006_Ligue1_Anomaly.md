PS C:\Users\hweth\OneDrive\Desktop\WIM Project\Result-Sections> python 2006_Ligue1_Anomaly.py
================================================================================
CASE STUDY: 2005-06 LIGUE 1 SEASON
'The Anomaly' - Lyon's Dominance vs The Flat Earth
================================================================================

--------------------------------------------------------------------------------
FINAL STANDINGS WITH WIM METRICS
--------------------------------------------------------------------------------
 Rank          Team  Pts  GF  GA  Ratio  Log_Ratio  Abs_Log_Ratio
    1          Lyon   81  64  27 2.3704     0.8630         0.8630
    2     Marseille   64  53  38 1.3947     0.3327         0.3327
    3      Toulouse   58  44  43 1.0233     0.0230         0.0230
    4        Rennes   57  38  30 1.2667     0.2364         0.2364
    5          Lens   57  47  41 1.1463     0.1366         0.1366
    6      Bordeaux   57  39  35 1.1143     0.1082         0.1082
    7       Sochaux   57  46  48 0.9583    -0.0426         0.0426
    8       Auxerre   54  41  41 1.0000     0.0000         0.0000
    9        Monaco   51  45  38 1.1842     0.1691         0.1691
   10         Lille   50  45  43 1.0465     0.0455         0.0455
   11 Saint-Étienne   49  52  50 1.0400     0.0392         0.0392
   12       Le Mans   49  45  46 0.9783    -0.0220         0.0220
   13         Nancy   49  37  44 0.8409    -0.1733         0.1733
   14       Lorient   49  33  40 0.8250    -0.1924         0.1924
   15     Paris S-G   48  42  42 1.0000     0.0000         0.0000
   16          Nice   43  34  40 0.8500    -0.1625         0.1625
   17  Valenciennes   43  36  48 0.7500    -0.2877         0.2877
   18        Troyes   39  39  54 0.7222    -0.3254         0.3254
   19         Sedan   35  46  58 0.7931    -0.2318         0.2318
   20        Nantes   34  29  49 0.5918    -0.5245         0.5245

--------------------------------------------------------------------------------
SEASON SUMMARY STATISTICS
--------------------------------------------------------------------------------
Champion: Lyon (81 pts) - 5th Consecutive Title
  -> Goals For: 64
  -> Goal Difference: +37
Points Gap (1st to 2nd): 17 points
Points Gap (2nd to 17th): 21 points (Incredibly tight!)

--------------------------------------------------------------------------------
WIM ANALYSIS
--------------------------------------------------------------------------------
WIM (Wetherall Imbalance Measure): 0.1958
  -> Ligue 1 Historical Mean: 0.3373
  -> Z-Score: -2.57 standard deviations from mean

WIM-TB (Top 4 + Bottom 4 Only): 0.3713
  -> Ligue 1 Historical Mean: 0.5875
  -> Z-Score: -2.40 standard deviations from mean

Noll-Scully Ratio: 1.2674
  -> Ligue 1 Historical Mean: 1.5846
  -> Z-Score: -1.59 standard deviations from mean

--------------------------------------------------------------------------------
KEY INSIGHT
--------------------------------------------------------------------------------

The 2005-06 Ligue 1 season had a WIM Z-Score of -2.57. This is a MASSIVE outlier.
It is the most statistically balanced season in our entire dataset across all leagues.

This presents a paradox: Lyon won the league easily (17 points clear).
How can the league be "balanced" if one team dominated?

The answer lies in the "Flat Earth" below Lyon.
- The gap between 2nd place (Marseille) and 17th place (Valenciennes) was only 21 points.
- 16 teams had a Goal Difference between +15 and -15.
- No other team was truly "good" or truly "bad".

WIM correctly identifies that while the *Champion* was an outlier, the *League Structure*
was incredibly compressed. This is a unique "Monopoly vs Equality" structure that
WIM visualizes perfectly.


Visualization saved to: C:\Users\hweth\OneDrive\Desktop\WIM Project\Result-Sections\2006_Ligue1_Anomaly_Analysis.png
Data saved to: C:\Users\hweth\OneDrive\Desktop\WIM Project\Result-Sections\2006_Ligue1_Anomaly_Data.csv
Summary saved to: C:\Users\hweth\OneDrive\Desktop\WIM Project\Result-Sections\2006_Ligue1_Anomaly_Summary.csv

================================================================================
ANALYSIS COMPLETE