# WIM Analysis Runner Script
# This script installs packages and runs the full analysis

# Create a log file to capture all output
sink("analysis_output.txt", split = TRUE)
cat("=== WIM Analysis Log ===\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R Version:", R.version.string, "\n\n")

# Function to install packages if needed
install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing package:", pkg, "\n")
    install.packages(pkg, repos = "https://cran.rstudio.com/", quiet = TRUE)
  } else {
    cat("Package", pkg, "is already installed\n")
  }
}

# Install required packages
cat("\n=== Checking/Installing Packages ===\n")
packages <- c("dplyr", "readr", "tidyr", "ggplot2", "stringr")
for (pkg in packages) {
  install_if_missing(pkg)
}

# Load packages
cat("\n=== Loading Packages ===\n")
library(dplyr, warn.conflicts = FALSE)
library(readr)
library(tidyr)
library(ggplot2)
library(stringr)
cat("All packages loaded successfully!\n")

# Now run the main analysis
cat("\n=== Running WIM Analysis ===\n")

# ===============================================================================
# THE WIM CALCULATOR FUNCTION
# ===============================================================================
calculate_wim <- function(df, league_name) {
  
  # A. Basic Cleaning
  team_col <- names(df)[grepl("Team|Squad", names(df), ignore.case = TRUE)][1]
  
  clean_df <- df %>%
    filter(!grepl("Team|Squad|Ladder", !!sym(team_col), ignore.case = TRUE)) %>%
    filter(!is.na(!!sym(team_col)))
  
  # B. Column Mapping
  col_names <- names(clean_df)
  for_col <- col_names[grepl("^For$|^GF$|^Points.?For", col_names, ignore.case = TRUE)][1]
  agn_col <- col_names[grepl("^Agn$|^GA$|^Points.?Against", col_names, ignore.case = TRUE)][1]
  year_col <- col_names[grepl("Year|Season", col_names, ignore.case = TRUE)][1]
  
  # Check if we found them
  if(is.na(for_col) | is.na(agn_col)) {
    warning(paste("Could not find scoring columns for", league_name))
    return(NULL)
  }
  
  cat("  Found columns - For:", for_col, ", Against:", agn_col, ", Year:", year_col, "\n")
  
  # C. The Calculation
  wim_results <- clean_df %>%
    mutate(
      PF = as.numeric(gsub(",", "", !!sym(for_col))),
      PA = as.numeric(gsub(",", "", !!sym(agn_col))),
      Season = as.numeric(!!sym(year_col))
    ) %>%
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

# ===============================================================================
# LOAD AND PROCESS DATA
# ===============================================================================
files <- list(
  "AFL" = "WIM Raw Data - AFL.csv",
  "Ligue 1" = "WIM Raw Data - Ligue 1.csv",
  "Premier League" = "WIM Raw Data - Prem League.csv"
)

all_wim_data <- data.frame()

for (league in names(files)) {
  file_path <- files[[league]]
  
  if(file.exists(file_path)) {
    cat("\nProcessing", league, "...\n")
    
    # Read CSV
    raw_data <- read_csv(file_path, show_col_types = FALSE)
    cat("  Loaded", nrow(raw_data), "rows\n")
    
    # Calculate WIM
    league_wim <- calculate_wim(raw_data, league)
    
    if (!is.null(league_wim)) {
      cat("  Calculated WIM for", nrow(league_wim), "seasons\n")
      all_wim_data <- bind_rows(all_wim_data, league_wim)
    }
  } else {
    cat("WARNING: File not found:", file_path, "\n")
  }
}

# ===============================================================================
# CREATE VISUALIZATION
# ===============================================================================
cat("\n=== Creating Visualization ===\n")

p <- ggplot(all_wim_data, aes(x = Season, y = WIM, color = League)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(limits = c(0, NA)) +
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
cat("Plot saved as 'WIM_Plot.png'\n")

# ===============================================================================
# PRINT SUMMARY TABLE
# ===============================================================================
cat("\n=== WIM Results Summary ===\n")
print(all_wim_data %>% arrange(League, Season))

# Calculate some interesting statistics
cat("\n=== Key Insights ===\n")
summary_stats <- all_wim_data %>%
  group_by(League) %>%
  summarise(
    Avg_WIM = round(mean(WIM), 4),
    Min_WIM = round(min(WIM), 4),
    Max_WIM = round(max(WIM), 4),
    Most_Balanced_Year = Season[which.min(WIM)],
    Least_Balanced_Year = Season[which.max(WIM)]
  )
print(summary_stats)

cat("\n=== Analysis Complete! ===\n")

# Close the log file
sink()

cat("\nAll output saved to 'analysis_output.txt'\n")
cat("Plot saved to 'WIM_Plot.png'\n")


