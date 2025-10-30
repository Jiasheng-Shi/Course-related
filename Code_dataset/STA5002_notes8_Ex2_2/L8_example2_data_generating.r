# Set seed for reproducibility
set.seed(807)

# Parameters
n <- 600
beta_0 <- 0.8

# Generate data
X <- rnorm(n, mean = 0, sd = 1)
Z <- rnorm(n, mean = 1, sd = 1)
epsilon <- rnorm(n, mean = 0, sd = sqrt(0.64))

# Define the function f_0(z)
f_0 <- function(z) {
  9 * cos(z) + exp(z) / 2 - z^2 / 7
}

# Generate Y
Y <- X * beta_0 + f_0(Z) + epsilon

# Create a data frame
data <- data.frame(X = X, Z = Z, Y = Y)

library(plotly)
library("RColorBrewer")

# Create a grid for the surface plot
x_range <- seq(min(X), max(X), length.out = 50)
z_range <- seq(min(Z), max(Z), length.out = 50)
X_grid <- outer(x_range, z_range, function(x, z) x)
Z_grid <- outer(x_range, z_range, function(x, z) z)

# Calculate the true surface (without noise)
Y_surface <- beta_0 * X_grid + f_0(Z_grid)

# ylgn_colors <- c("#F2E9E4", "#C9ADA7", "#9A8C98", "#4A4E69", "#22223B")
# ylgn_colors <- c("#D5BDAF","#E3D5CA","#F5EBE0","#D6CCC2","#EDEDE9")
# ylgn_colors <- c("#9c89b8","#f0a6ca","#efc3e6","#f0e6ef","#b8bedd")
# 
# # Create equally spaced positions from 0 to 1
# positions <- seq(0, 1, length.out = length(ylgn_colors))
# 
# # Build custom colorscale: list of c(position, color) pairs
# custom_ylgn <- mapply(function(pos, col) {
#   c(pos, col)
# }, positions, ylgn_colors, SIMPLIFY = FALSE)

# Add surface plot (true underlying function) - this uses type="surface" internally
plot_ly() %>% 
  add_surface(
    x = x_range,
    y = z_range,
    z = Y_surface,
    colorscale = "Viridis",
    opacity = 0.807,
    showscale = FALSE,
    name = "True Surface"
  )

write.csv(data, file="Synthetic_data_L8_example2.csv", col.names = TRUE)
