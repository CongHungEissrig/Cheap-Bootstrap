# Initialize Seed
set.seed(456)

# Source Function from bootstrap_regression.R
source("R/bootstrap_regression.R")


# Parameters
B <- c(1, 2, 5, 10, 50, 100)  # Vector of bootstrap replication sizes
N <- 1000                     # Number of simulated datasets
data_size <- 100              # Number of observations per dataset
alpha <- 0.05                 # Significance level
sigma <- 5                    # Standard deviation of the error term
mean <- 10                    # Mean of explanatory variables
sd <- 2                       # Standard deviation of explanatory variables


# True value of regression coefficients
params_list <- list(
  beta1 = 1,
  beta2 = 1,
  beta3 = 1,
  beta4 = 1,
  beta5 = 1
)

# ----------------------
# Run the bootstrap regression
# ----------------------
mult_model <- Bootstrap_Regression(
  B = B,
  N = N,
  data_size = data_size,
  params_list = params_list,
  alpha = alpha,
  sigma = sigma,
  mean = mean,
  sd = sd
)

# Save the result
saveRDS(mult_model, "results/regression/mult_model.rds")

