# Load required libraries
library(splines)
library(plotly)  # For 3D plotting

# Set seed for reproducibility
set.seed(807)

# Parameters
n <- 600

# Generate covariates X and Z
X <- matrix(rnorm(n * 2, mean = 0, sd = 1), ncol = 2)  # X ~ N(0, I) in R^2
Z <- matrix(rnorm(n * 2, mean = 0, sd = sqrt(2)), ncol = 2)  # Z ~ N(0, 2*I) in R^2

# Define the varying coefficient functions f1 and f2
f1 <- function(z) {
  9 * cos(z) + exp(z) / 2 - z^2 / 7
}

f2 <- function(z) {
  8 * sin(z) + z^2 / 5 - z^3 / 6
}

# Generate error term epsilon ~ N(0, 0.64 * I)
epsilon <- rnorm(n, mean = 0, sd = 0.8)  # sd = sqrt(0.64) = 0.8

# Generate response Y
Y <- X[, 1] * f1(Z[, 1]) + X[, 2] * f2(Z[, 2]) + epsilon

# Combine into a data frame for convenience
data <- data.frame(
  Y = Y,
  X1 = X[, 1],
  X2 = X[, 2],
  Z1 = Z[, 1],
  Z2 = Z[, 2]
  #epsilon = epsilon
)

# Optional: View first few rows
head(data)

write.csv(data, file = "Synthetic_data_L8_example3.csv")
