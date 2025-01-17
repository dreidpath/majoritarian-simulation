# ==============================================================================
# Simulation Functions for Majoritarian Simulation Project
# Author: Daniel D. Reidpath
# Description: This script contains functions to generate population grids
#              based on different policy impact scenarios (elite, random, capture).
# ==============================================================================

# --- Elite Policy Impact Simulation -------------------------------------------

#' Generate Grid for Elite Policy Impact
#'
#' Simulates a scenario where policy benefits are concentrated on an elite subset
#' of the population.
#'
#' @param n_policies Integer, number of policies.
#' @param pop_size Integer, size of the population grid (pop_size x pop_size).
#' @param policy_impact Numeric, proportion of the population benefiting from policies.
#' @return Matrix representing the population grid with policy benefits.
elite <- function(n_policies = 1000, pop_size = 101, policy_impact = 0.501) {
  pop_grid <- matrix(0, nrow = pop_size, ncol = pop_size)  # Initialize grid

  policy_pop <- find_min_y_vectorized(pop_size, policy_impact)  # Determine size of impacted population
  policy_matrix <- matrix(1, nrow = policy_pop, ncol = policy_pop)

  for (i in 1:n_policies) {
    beneficiaries <- random_matrix(pop_size, policy_pop)  # Randomly select elite beneficiaries
    pop_grid[beneficiaries$rows, beneficiaries$cols] <-
      pop_grid[beneficiaries$rows, beneficiaries$cols] + policy_matrix
  }

  return(pop_grid)
}

# --- Random Policy Impact Simulation ------------------------------------------

#' Generate Grid for Random Policy Impact
#'
#' Simulates a scenario where policy benefits are distributed randomly across the population.
#'
#' @param n_policies Integer, number of policies.
#' @param pop_size Integer, size of the population grid (pop_size x pop_size).
#' @param policy_impact Numeric, proportion of the population benefiting from policies.
#' @return Matrix representing the population grid with random policy benefits.
random <- function(n_policies = 1000, pop_size = 101, policy_impact = 0.501) {
  pop_grid <- matrix(0, nrow = pop_size, ncol = pop_size)  # Initialize grid

  policy_pop <- find_min_y_vectorized(pop_size, policy_impact)  # Determine size of impacted population

  for (i in 1:n_policies) {
    # Randomly assign policy benefits
    beneficiaries <- sample(
      x = c(rep(1, policy_pop^2), rep(0, (pop_size^2 - policy_pop^2))),
      size = pop_size^2
    )
    beneficiaries <- matrix(beneficiaries, nrow = pop_size, ncol = pop_size)
    pop_grid <- pop_grid + beneficiaries
  }

  return(pop_grid)
}

# --- Centralized Policy Impact Simulation -------------------------------------

#' Generate Grid for Centralized Policy Impact
#'
#' Simulates a scenario where policies consistently benefit the same central
#' portion of the population grid.
#'
#' @param n_policies Integer, number of policies.
#' @param pop_size Integer, size of the population grid (pop_size x pop_size).
#' @param policy_impact Numeric, proportion of the population benefiting from policies.
#' @return Matrix representing the population grid with centralized policy benefits.
capture <- function(n_policies = 1000, pop_size = 101, policy_impact = 0.501) {
  pop_grid <- matrix(0, nrow = pop_size, ncol = pop_size)  # Initialize grid

  policy_pop <- find_min_y_vectorized(pop_size, policy_impact)  # Determine size of impacted population
  policy_matrix <- matrix(1, nrow = policy_pop, ncol = policy_pop)

  start_row <- ceiling((pop_size - policy_pop) / 2) + 1
  start_col <- ceiling((pop_size - policy_pop) / 2) + 1
  end_row <- start_row + policy_pop - 1
  end_col <- start_col + policy_pop - 1

  for (i in 1:n_policies) {
    pop_grid[start_row:end_row, start_col:end_col] <-
      pop_grid[start_row:end_row, start_col:end_col] + policy_matrix
  }

  return(pop_grid)
}
