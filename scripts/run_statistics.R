# Initialize Seed
set.seed(123)

# Source Function from bootstrap_statistics.R
source("R/bootstrap_statistics.R")

# Parameters
B <- c(1, 2, 5, 10, 50, 100)   # Vector of bootstrap replication sizes
N <- 1000                      # Number of simulated datasets
data_size <- 100               # Number of observations per dataset
alpha <- 0.05                  # Significance level
p <- 0.75                      # 75% Quantile

# ----------------------
# Normally distributed data 
# ----------------------
res_normal <- Bootstrap_Statistic(
  B = B,
  N = N,
  data_size = data_size,
  alpha = alpha,
  p = p,
  distribution = "normal",
  params = list(mean = 10, sd = 2)
)

# Save the result
saveRDS(res_normal, "results/statistics/normal.rds")

# ----------------------
# Gamma distributed data
# ----------------------
res_gamma <- Bootstrap_Statistic(
  B = B,
  N = N,
  data_size = data_size,
  alpha = 0.05,
  p = 0.75,
  distribution = "gamma",
  params = list(shape = 5, rate = 2)
)

# Save the result
saveRDS(res_gamma, "results/statistics/gamma.rds")





