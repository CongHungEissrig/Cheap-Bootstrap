# Initialize Seed

set.seed(456)

# Source Function from bootstrap_regression.R
source("R/bootstrap_regression.R")


# --- 3. Parameter ---
B <- c(1, 2, 5, 10, 50, 100)    # Anzahl Bootstrap-Replikationen
N <- 1000                    # Anzahl Simulationsläufe
data_size <- 100             # Stichprobengröße
alpha <- 0.05                # Signifikanzniveau für Intervalle
sigma <- 5                   # Standardabweichung Fehlerterm
mean <- 10                   # Mittelwert der X-Variablen
sd <- 2                      # Standardabweichung der X-Variablen

# Regressionskoeffizienten
params_list <- list(
  beta1 = 1,
  beta2 = 1,
  beta3 = 1,
  beta4 = 1,
  beta5 = 1
)

# Bootstrap 
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

saveRDS(mult_model, "results/regression/mult_model.rds")

