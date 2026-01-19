# Initialize Seed

set.seed(123)

# Source Function from bootstrap_statistics.R
source("R/bootstrap_statistics.R")

# Parameter
B <- c(1, 2, 5, 10, 50, 100)   # Bootstrap-Replikationen
N <- 1000                   # Anzahl der Simulationen
data_size <- 100
alpha <- 0.05
p <- 0.75

# --- Normalverteilung ---
res_normal <- Bootstrap(
  B = B,
  N = N,
  data_size = data_size,
  alpha = alpha,
  p = p,
  distribution = "normal",
  params = list(mean = 10, sd = 2)
)

saveRDS(res_normal, "results/statistics/normal.rds")

# --- Gammaverteilung ---
res_gamma <- Bootstrap(
  B = B,
  N = N,
  data_size = data_size,
  alpha = 0.05,
  p = 0.75,
  distribution = "gamma",
  params = list(shape = 5, rate = 2)
)

saveRDS(res_gamma, "results/statistics/gamma.rds")





