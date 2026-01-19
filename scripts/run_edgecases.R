# Initialize Seed
set.seed(123)

# Source Function from bootstrap_statistics.R
source("R/bootstrap_statistics.R")

# Parameters
B <- c(1, 2, 5, 10, 50, 100)
N <- 1000
alpha <- 0.05

# -----------------------------
# Extreme quantile: p = 0.99
# -----------------------------

# Normal
res_normal_p99 <- Bootstrap(
  B = B,
  N = N,
  data_size = 100,
  alpha = alpha,
  p = 0.99,
  distribution = "normal",
  params = list(mean = 10, sd = 2)
)

saveRDS(res_normal_p99, "results/statistics/normal_p99.rds")

# Gamma
res_gamma_p99 <- Bootstrap(
  B = B,
  N = N,
  data_size = 100,
  alpha = alpha,
  p = 0.99,
  distribution = "gamma",
  params = list(shape = 5, rate = 2)
)
saveRDS(res_gamma_p99, "results/statistics/gamma_p99.rds")


# -----------------------------
# Extreme quantile: p = 0.01
# -----------------------------

# Normal
res_normal_p01 <- Bootstrap(
  B = B,
  N = N,
  data_size = 100,
  alpha = alpha,
  p = 0.01,
  distribution = "normal",
  params = list(mean = 10, sd = 2)
)
saveRDS(res_normal_p01, "results/statistics/normal_p01.rds")

# Gamma
res_gamma_p01 <- Bootstrap(
  B = B,
  N = N,
  data_size = 100,
  alpha = alpha,
  p = 0.01,
  distribution = "gamma",
  params = list(shape = 5, rate = 2)
)

saveRDS(res_gamma_p01, "results/statistics/gamma_p01.rds")


# -----------------------------
# Small sample size: data_size = 20
# -----------------------------

# Normal
res_normal_ds20 <- Bootstrap(
  B = B,
  N = N,
  data_size = 20,
  alpha = alpha,
  p = 0.75,
  distribution = "normal",
  params = list(mean = 10, sd = 2)
)
saveRDS(res_normal_ds20, "results/statistics/normal_ds20.rds")

# Gamma
res_gamma_ds20 <- Bootstrap(
  B = B,
  N = N,
  data_size = 20,
  alpha = alpha,
  p = 0.75,
  distribution = "gamma",
  params = list(shape = 5, rate = 2)
)
saveRDS(res_gamma_ds20, "results/statistics/gamma_ds20.rds")



# -----------------------------
# Small sample size: data_size = 10
# -----------------------------

# Normal
res_normal_ds10 <- Bootstrap(
  B = B,
  N = N,
  data_size = 10,
  alpha = alpha,
  p = 0.75,
  distribution = "normal",
  params = list(mean = 10, sd = 2)
)

saveRDS(res_normal_ds10, "results/statistics/normal_ds10.rds")

# Gamma
res_gamma_ds10 <- Bootstrap(
  B = B,
  N = N,
  data_size = 10,
  alpha = alpha,
  p = 0.75,
  distribution = "gamma",
  params = list(shape = 5, rate = 2)
)

saveRDS(res_gamma_ds10, "results/statistics/gamma_ds10.rds")




