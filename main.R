# ==============================================================================
# Main Script for Majoritarian Simulation Project
# Author: [Your Name]
# Description: This script serves as the main entry point for running the
#              majoritarian simulation, visualizing the results, and exporting outputs.
# ==============================================================================

# --- Initialisation -----------------------------------------------------------

# Load necessary libraries
library(tidyverse)
library(patchwork)

# Source helper and function files
source("R/helpers.R")           # Utility functions (matrix manipulation, etc.)
source("R/simulation.R")        # Grid generation functions (elite, random, capture)
source("R/visualisation.R")     # Plotting functions (heatmap, Lorenz curve, etc.)

# Define constants
n_policies <- 1000               # Number of policies
pop_size <- 101                  # Population grid size (pop_size x pop_size)
policy_impact <- c(0.1, 0.25, 0.501, 0.75, 0.90)  # Proportion of population benefiting

output_dir <- "output"           # Directory to save outputs
if (!dir.exists(output_dir)) dir.create(output_dir)


# --- Function to Run Simulations ----------------------------------------------

run_simulation <- function(method, method_name) {
  # method: Function to generate the grid (elite, random, capture)
  # method_name: String identifier for the method (used in file names)
  
  # Create an empty list to store results
  results <- list()
  
  # Loop over the different policy impacts
  for (impact in policy_impact) {
    # Generate the population grid based on the policy impact
    pop_grid <- method(n_policies, pop_size, policy_impact = impact)
    
    # Generate plots
    heat <- heatplot(pop_grid, max_value = n_policies) + ggtitle("")
    pol_cov <- pol_cover(n_policies, pop_grid) + ggtitle("")
    lorenz_result <- lorenz_curve(pop_grid)  # Now returns both plot and Gini coefficient
    lorenz <- lorenz_result$plot
    gini <- lorenz_result$gini
    
    # Store results in the list
    results[[length(results) + 1]] <- list(
      policy_impact = impact,
      gini_coefficient = gini,
      heatmap = heat,
      policy_coverage = pol_cov,
      lorenz_curve = lorenz
    )
  }
  
  # Combine plots using patchwork
  plot_list <- lapply(results, function(res) list(res$heatmap, res$policy_coverage, res$lorenz_curve))
  combined_plot <- wrap_plots(do.call(c, plot_list), ncol = 3)
  
  # Save the combined plot
  output_file <- file.path(output_dir, paste0("combined_", method_name, ".svg"))
  ggsave(filename = output_file, plot = combined_plot, device = "svg", width = 8, height = 8)
  
  # Save Gini coefficients to a CSV file
  gini_data <- data.frame(
    policy_impact = sapply(results, function(res) res$policy_impact),
    gini_coefficient = sapply(results, function(res) res$gini_coefficient)
  )
  write.csv(gini_data, file = file.path(output_dir, paste0("gini_", method_name, ".csv")), row.names = FALSE)
  
  message("Saved plot and Gini data for ", method_name, " to ", output_dir)
}


# --- Run Simulations ----------------------------------------------------------

# Initialize a data frame to store consolidated Gini results
all_gini_results <- data.frame()

# Run simulations for different scenarios
run_simulation(elite, "elite")      # Simulate elite policy impact
run_simulation(random, "random")    # Simulate random policy impact
run_simulation(capture, "capture")  # Simulate centralized policy impact

# Consolidate Gini results
methods <- c("elite", "random", "capture")
for (method in methods) {
  gini_file <- file.path(output_dir, paste0("gini_", method, ".csv"))
  if (file.exists(gini_file)) {
    method_results <- read.csv(gini_file)
    method_results$method <- method  # Add method column
    all_gini_results <- rbind(all_gini_results, method_results)
  }
}

# Save consolidated results
write.csv(all_gini_results, file = file.path(output_dir, "gini_results.csv"), row.names = FALSE)

# --- Process and Report Results ------------------------------------------------
source("R/results.R")  # Process and report results

# --- Completion ---------------------------------------------------------------
message("All simulations and plots completed successfully!")
