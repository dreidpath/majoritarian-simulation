# ==============================================================================
# Helper Functions for Majoritarian Simulation Project
# Author: Daniel D. Reidpath
# Description: This script contains utility functions used throughout the
#              majoritarian simulation project, such as matrix manipulation
#              and data transformation.
# ==============================================================================

# --- Function to Generate Random Sub-Matrix Indices ---------------------------

#' Generate Random Sub-Matrix Indices
#'
#' Selects a random m x m sub-matrix within an n x n matrix.
#' Ensures that the sub-matrix is centered around the central point of the matrix.
#'
#' @param n Integer, dimension of the square matrix (n x n). Must be greater than 0 and odd.
#' @param m Integer, dimension of the sub-matrix (m x m). Must be less than n.
#' @return A list with row and column indices for the selected sub-matrix.
random_matrix <- function(n, m) {
  if (m >= n || n %% 2 == 0) {
    stop("m must be less than n AND n must be odd.")
  }

  center <- (n + 1) / 2
  start_row <- sample(max(1, center - (m - 1)):min(n - m + 1, center), 1)
  start_col <- sample(max(1, center - (m - 1)):min(n - m + 1, center), 1)

  list(rows = start_row:(start_row + m - 1), cols = start_col:(start_col + m - 1))
}

# --- Function to Find Minimum y for Policy Impact -----------------------------

#' Find Minimum y for Policy Impact
#'
#' Computes the smallest integer y such that (y^2 / n^2) >= k for a given
#' proportion k, ensuring sufficient coverage.
#'
#' @param n Integer, size of the matrix (n x n).
#' @param k_vector Numeric vector, proportions in the interval [0, 1].
#' @return Numeric vector of minimum y values satisfying the condition.
find_min_y_vectorized <- function(n, k_vector) {
  if (!is.numeric(n) || length(n) != 1 || n <= 0 || floor(n) != n) {
    stop("`n` must be a positive integer.")
  }
  if (!is.numeric(k_vector) || any(k_vector < 0) || any(k_vector > 1)) {
    stop("All elements in `k_vector` must be numeric and in the interval [0, 1].")
  }

  y <- ceiling(n * sqrt(k_vector))
  y[k_vector == 0] <- 0

  cat("Proportion Sought (k) | Proportion Achieved (y^2 / n^2)\n")
  cat("-------------------------------------------------------\n")
  achieved <- (y^2) / (n^2)
  for (i in seq_along(k_vector)) {
    cat(sprintf("       %0.4f         |               %0.6f\n", k_vector[i], achieved[i]))
  }

  return(y)
}

# --- Function to Convert Matrix to Data Frame ---------------------------------

#' Convert Matrix to Data Frame
#'
#' Transforms a matrix into a tidy data frame with row, column, and value columns.
#'
#' @param mat Numeric or character matrix to convert.
#' @return Data frame with columns: value, row, col.
mattodf <- function(mat) {
  values <- as.vector(mat)
  rows <- rep(seq_len(nrow(mat)), times = ncol(mat))
  cols <- rep(seq_len(ncol(mat)), each = nrow(mat))
  data.frame(value = values, row = rows, col = cols)
}

# --- Function to Calculate Gini Coefficient -----------------------------------

#' Calculate Gini Coefficient
#'
#' Computes the Gini coefficient given a numeric vector of values.
#'
#' @param x Numeric vector. Values should represent a distribution (e.g., income).
#' @return Numeric value representing the Gini coefficient.
gini_coefficient <- function(x) {
  if (!is.numeric(x) || any(x < 0)) {
    stop("Input vector must be numeric and non-negative.")
  }
  
  x <- sort(x)
  n <- length(x)
  cumulative_values <- cumsum(x)
  G <- sum((2 * seq_len(n) - n - 1) * x) / (n * sum(x))
  return(G)
}
# --- Additional Helper Functions Placeholder ----------------------------------

# Place additional helper functions here as needed.
