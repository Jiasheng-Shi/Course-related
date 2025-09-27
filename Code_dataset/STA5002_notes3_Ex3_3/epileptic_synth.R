# Load required library
library(MASS)  # for rnegbin()

setwd("/Users/shij/Documents/GitHub/Course-related/Code_dataset/STA5002_notes3_Ex3_3/")

# Set seed for reproducibility
set.seed(0807)

# Simulation parameters
n <- 100        # number of patients
T <- 10         # number of weeks
beta0 <- 1.5    # intercept (log baseline rate)
beta1 <- -0.5   # treatment effect (log multiplicative effect)
beta2 <- 0.8    # saturation amplitude
gamma <- 0.3    # rate of saturation
phi <- 1.5      # dispersion parameter (overdispersion)

# Generate treatment indicator
Trt <- rbinom(n, size = 1, prob = 0.5)  # 50% randomized

# Create long-format data for computation
long_data <- expand.grid(
  i = 1:n,
  t = 1:T
) |> 
  # Sort by patient and time
  dplyr::arrange(i, t)

# Add treatment
long_data$Trt <- Trt[long_data$i]

# Compute linear predictor and mean
long_data$eta <- beta0 + 
  beta1 * long_data$Trt + 
  beta2 * (1 - exp(-gamma * long_data$t))
long_data$mu <- exp(long_data$eta)

# Compute subject-level theta for negative binomial
mu_i <- tapply(long_data$mu, long_data$i, mean)
theta_i <- mu_i / (phi - 1)
long_data$theta <- theta_i[long_data$i]

# Simulate seizure counts
long_data$Y <- rnegbin(n = nrow(long_data), mu = long_data$mu, theta = long_data$theta)

# Convert to wide format: one row per patient
epileptic_synth_wide <- reshape(
  long_data,
  direction = "wide",
  idvar = "i",
  timevar = "t",
  v.names = "Y",
  sep = ""  # so columns are Y1, Y2, ..., YT
)

# Reorder columns: i, Trt, then Y1 to YT
Y_cols <- paste0("Y", 1:T)
epileptic_synth_wide <- epileptic_synth_wide[c("i", "Trt", Y_cols)]

# Optional: rename i to PatientID for clarity
names(epileptic_synth_wide)[1] <- "PatientID"

# Convert PatientID to integer (was factor)
epileptic_synth_wide$PatientID <- as.integer(epileptic_synth_wide$PatientID)

# View first few rows
View(epileptic_synth_wide)

# Save as CSV
write.csv(epileptic_synth_wide, file = "epileptic_synth_wide.csv", row.names = FALSE)
