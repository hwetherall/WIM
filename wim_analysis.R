# ==============================================================================
# WIM (Wetherall Imbalance Measure) - The Flight Pack
# Author: The Flying Economist (feat. Gemini)
# Date: Dec 2025
# ==============================================================================

# 1. LOAD LIBRARIES ------------------------------------------------------------
# If you don't have these installed, run install.packages() BEFORE you fly!

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(stringr)

# 2. THE WIM CALCULATOR FUNCTION -----------------------------------------------
# This function takes a raw dataframe, cleans it, and returns the WIM score per season

calculate_wim <- function(df, league_name) {
  
  # A. Basic Cleaning
  # -----------------
  # Remove rows where the "Team" or "Squad" column contains "Team", "Squad" or "Ladder"
  # This handles the repeated headers in your raw data
  
  # Identify the Team column (it's usually 'Team' or 'Squad')
  team_col <- names(df)[grepl("Team|Squad", names(df), ignore.case = TRUE)][1]
  
  clean_df <- df %>%
    filter(!grepl("Team|Squad|Ladder", !!sym(team_col), ignore.case = TRUE)) %>%
    filter(!is.na(!!sym(team_col))) # Remove empty rows
  
  # B. Column Mapping (The "Rosetta Stone")
  # ---------------------------------------
  # We need to find Points For and Points Against, regardless of what they are called.
  
  col_names <- names(clean_df)
  
  # Find "Points For" (Matches: 'For', 'GF', 'PointsFor')
  for_col <- col_names[grepl("^For$|^GF$|^Points.?For", col_names, ignore.case = TRUE)][1]
  
  # Find "Points Against" (Matches: 'Agn', 'GA', 'Points.?Against')
  agn_col <- col_names[grepl("^Agn$|^GA$|^Points.?Against", col_names, ignore.case = TRUE)][1]
  
  # Find "Year" or "Season"
  year_col <- col_names[grepl("Year|Season", col_names, ignore.case = TRUE)][1]
  
  # Check if we found them
  if(is.na(for_col) | is.na(agn_col)) {
    warning(paste("Could not find scoring columns for", league_name))
    return(NULL)
  }
  
  # C. The Calculation
  # ------------------
  wim_results <- clean_df %>%
    mutate(
      # Ensure data is numeric (remove commas if necessary)
      PF = as.numeric(gsub(",", "", !!sym(for_col))),
      PA = as.numeric(gsub(",", "", !!sym(agn_col))),
      Season = as.numeric(!!sym(year_col))
    ) %>%
    # Remove rows with 0 points to avoid log(-Inf) errors
    filter(PF > 0, PA > 0) %>%
    group_by(Season) %>%
    summarise(
      WIM = mean(abs(log(PF / PA))),
      Teams = n(),
      League = league_name
    ) %>%
    ungroup()
  
  return(wim_results)
}

# 3. LOAD AND PROCESS DATA -----------------------------------------------------
# Update these filenames if you saved them differently!

files <- list(
  "AFL" = "WIM Raw Data - AFL.csv",
  "Ligue 1" = "WIM Raw Data - Ligue 1.csv",
  "Premier League" = "WIM Raw Data - Prem League.csv"
)

all_wim_data <- data.frame()

for (league in names(files)) {
  file_path <- files[[league]]
  
  if(file.exists(file_path)) {
    message(paste("Processing", league, "..."))
    
    # Read CSV (suppress warnings about column types)
    raw_data <- read_csv(file_path, show_col_types = FALSE)
    
    # Calculate WIM
    league_wim <- calculate_wim(raw_data, league)
    
    # Combine
    all_wim_data <- bind_rows(all_wim_data, league_wim)
  } else {
    warning(paste("File not found:", file_path))
  }
}

# 4. VISUALIZATION -------------------------------------------------------------
# The "Money Plot"

p <- ggplot(all_wim_data, aes(x = Season, y = WIM, color = League)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(0, NA)) + # Start y-axis at 0 (Perfect Balance)
  labs(
    title = "The Wetherall Imbalance Measure (WIM)",
    subtitle = "Comparing Competitive Balance Across Leagues (Lower is Better)",
    y = "WIM Score (Mean Log-Ratio Deviation)",
    x = "Season",
    caption = "0 = Perfectly Balanced League"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "bottom"
  )

# Save the plot
ggsave("WIM_Plot.png", plot = p, width = 12, height = 8, dpi = 300)
cat("\nPlot saved as 'WIM_Plot.png'\n")

# 5. PRINT SUMMARY TABLE -------------------------------------------------------

cat("\n=== WIM Results Summary ===\n")
print(all_wim_data %>% arrange(League, Season))
cat("\n")
