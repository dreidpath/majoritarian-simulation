# ==============================================================================
# Visualisation Functions for Majoritarian Simulation Project
# Author: Daniel D. Reidpath
# Description: This script contains functions to visualize population grids and
#              policy impact distributions, including heatmaps, Lorenz curves,
#              and policy coverage plots.
# ==============================================================================

# --- Heatmap Visualisation ----------------------------------------------------

#' Heatmap of Policy Benefits
#'
#' Creates a heatmap showing the distribution of policy benefits across the
#' population grid.
#'
#' @param pop_grid Matrix, the population grid with policy benefits.
#' @param max_value Numeric, the maximum value for the color scale.
#' @return ggplot object representing the heatmap.
heatplot <- function(pop_grid, max_value) {
  popgridDF <- mattodf(pop_grid)  # Convert matrix to data frame
  
  ggplot(popgridDF, aes(x = row, y = col, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "blue", limits = c(0, max_value)) +
    theme_minimal() +
    labs(
      title = "Heatmap of Policy Benefits",
      x = "",
      y = "",
      fill = "Benefit"
    ) +
    coord_fixed(ratio = 1)  # Ensure square aspect ratio
}

# --- Lorenz Curve Visualisation -----------------------------------------------

#' Lorenz Curve of Policy Benefits
#'
#' Plots the Lorenz curve to illustrate inequality in policy benefit distribution.
#'
#' @param pop_grid Matrix, the population grid with policy benefits.
#' @return A list containing:
#'   - ggplot object representing the Lorenz curve.
#'   - Numeric value representing the Gini coefficient.
lorenz_curve <- function(pop_grid) {
  benefits <- sort(as.vector(pop_grid))  # Flatten and sort grid
  total_benefits <- sum(benefits)
  
  cumulative_benefits <- cumsum(benefits) / total_benefits
  cumulative_population <- seq_along(benefits) / length(benefits)
  
  lorenz_df <- data.frame(
    population = c(0, cumulative_population),
    benefits = c(0, cumulative_benefits)
  )
  
  # Calculate the Gini coefficient
  gini <- gini_coefficient(benefits)
  
  lorenz_plot <- ggplot(lorenz_df, aes(x = population, y = benefits)) +
    geom_line(color = "blue", linewidth = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Lorenz Curve of Policy Benefits",
      x = "Proportion of Population",
      y = "Proportion of Benefits"
    ) +
    theme_minimal()
  
  return(list(plot = lorenz_plot, gini = gini))
}

# --- Policy Coverage Visualisation --------------------------------------------

#' Policy Coverage Plot
#'
#' Creates a line graph illustrating the proportion of the population receiving
#' policy benefits at various thresholds.
#'
#' @param n_policies Integer, number of policies.
#' @param pop_grid Matrix, the population grid with policy benefits.
#' @return ggplot object representing the policy coverage plot.
pol_cover <- function(n_policies, pop_grid) {
  policy_num <- n_policies:1
  tmp <- sapply(policy_num, function(n) sum(pop_grid >= n))  # Count cells with at least n benefits
  
  coverage_df <- data.frame(
    prop_pop = sort(policy_num / n_policies, decreasing = TRUE),
    prop_benefit = tmp / (nrow(pop_grid) * ncol(pop_grid))
  )
  
  ggplot(coverage_df, aes(x = prop_benefit, y = prop_pop)) +
    geom_line(color = "darkred") +
    geom_vline(xintercept = min(tmp / (nrow(pop_grid) * ncol(pop_grid))), linetype = "dashed") +
    labs(
      title = "Policy Coverage Across Population",
      x = "Proportion of Population",
      y = "Proportion of Benefits"
    ) +
    theme_bw() +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1))
}
