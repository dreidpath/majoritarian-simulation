# ==============================================================================
# Results Processing and Reporting for Majoritarian Simulation Project
# Author: Daniel D. Reidpath
# Description: This script handles the processing, summarisation, and reporting
#              of simulation results, including Gini coefficients and other metrics.
# ==============================================================================

# --- Initialisation -----------------------------------------------------------

# Load necessary libraries
library(tidyverse)

# Define constants
output_dir <- "output"           # Directory where results are stored
results_file <- file.path(output_dir, "gini_results.csv")
summary_file <- file.path(output_dir, "gini_summary.csv")

# --- Function to Load and Process Results -------------------------------------

process_results <- function(results_file, summary_file) {
  # Load results from CSV file
  if (!file.exists(results_file)) {
    stop("Results file not found: ", results_file)
  }
  
  results <- read.csv(results_file)
  
  # Summarize results: mean, median, and range of Gini coefficients by method
  summary <- results %>%
    group_by(method) %>%
    summarise(
      mean_gini = mean(gini_coefficient, na.rm = TRUE),
      median_gini = median(gini_coefficient, na.rm = TRUE),
      min_gini = min(gini_coefficient, na.rm = TRUE),
      max_gini = max(gini_coefficient, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Save summary to a CSV file
  write.csv(summary, file = summary_file, row.names = FALSE)
  
  message("Summary saved to ", summary_file)
  
  return(list(results = results, summary = summary))
}

# --- Main Execution -----------------------------------------------------------

# Process and summarize results
results_data <- process_results(results_file, summary_file)

# --- Completion ---------------------------------------------------------------
message("Results processing and reporting completed successfully!")
