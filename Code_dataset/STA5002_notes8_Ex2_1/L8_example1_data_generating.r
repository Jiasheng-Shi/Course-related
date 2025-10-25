
setwd("/Users/shij/Dropbox/Class/STAT5002_CUHKSZ/25-26_Term1_STAT5002_Lecture_Notes/L8data/")
# Set seed for reproducibility
set.seed(807)

# Generate simulated data
n <- 600  # Sample size
p <- 2    # Number of covariates

# Generate covariates (X1, X2)
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
colnames(X) <- c("X1", "X2")

# True parameter beta (unknown parameter of interest)
beta_true <- c(1.5, -0.8)  # True coefficients

# True index: mu = X^T * beta
mu_true <- X %*% beta_true

# True nonlinear function f (this is what we want to estimate)
f_true <- function(x) {
  # A nonlinear function: combination of sine, quadratic, and cubic terms
  return(15 * sin(x) + 0.5 * x^2 - 0.1 * x^3)
}

# Generate error term
sigma <- 0.8  # Standard deviation of error
epsilon <- rnorm(n, mean = 0, sd = sigma)

# Generate response Y
Y <- f_true(mu_true) + epsilon

# Create a data frame for easier handling
sim_data <- data.frame(Y = Y, X1 = X[,1], X2 = X[,2])

# Visualize the true relationship between Y and the true index mu_true
par(mfrow = c(1, 2))

# Plot Y vs. true index (shows the nonlinear relationship)
plot(mu_true, Y, pch = 16, col = "blue", 
     xlab = "True Index (X^Tβ)", ylab = "Y",
     main = "True Relationship: Y = f(X^Tβ) + ε")
lines(sort(mu_true), f_true(sort(mu_true)), col = "red", lwd = 2)

# Plot Y vs. X1 and X2 (shows why we need dimension reduction)
plot(X[,1], Y, pch = 16, col = "green", 
     xlab = "X1", ylab = "Y", main = "Y vs. X1")
plot(X[,2], Y, pch = 16, col = "orange", 
     xlab = "X2", ylab = "Y", main = "Y vs. X2")

par(mfrow = c(1, 1))


# Install plotly if not already installed
# install.packages("plotly")

# Load required package
library(plotly)

# Create a grid for surface plotting
x1_grid <- seq(min(X[,1]), max(X[,1]), length.out = 50)
x2_grid <- seq(min(X[,2]), max(X[,2]), length.out = 50)
grid <- expand.grid(X1 = x1_grid, X2 = x2_grid)

# Calculate true index and true Y values on the grid
mu_grid <- grid$X1 * beta_true[1] + grid$X2 * beta_true[2]
y_grid <- f_true(mu_grid)

# Create 3D surface plot of the true relationship
p <- plot_ly(
  x = x1_grid,
  y = x2_grid,
  z = matrix(y_grid, nrow = 50, ncol = 50),
  type = "surface",
  colorscale = "Viridis",
  showscale = FALSE
) %>%
  layout(
    scene = list(
      xaxis = list(title = "X1"),
      yaxis = list(title = "X2"),
      zaxis = list(title = "Y")
    )
  )

# Display the plot
p

mydat<-data.frame(Y=Y,X1=X[,1],X2=X[,2])
saveRDS(mydat,file = "Synthetic_data_L8_example1.rds")

