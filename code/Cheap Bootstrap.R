# Cheap Bootstrap

# Basic Numerical Demonstration
# Cheap Bootstrap, Basic Bootstrap, Percentile Bootstrap and Standard Error Bootstrap
# Estimate a 95% confidence interval of the 0.6 quantile of an exponential distribution with unit rate from iid data
# n = 100, B = c(1, 2, 5, 10, 50)
# For each B, we generate synthetic data 1000 times, each time running all the competing methods and outputting the empirical coverage
# and interval width statistics from the 1000 experimental repetitions


set.seed(123)
n <- 100 
B <- 50
p <- 0.6
lambda <- 1
alpha = 0.05

# True 0.6 Quantile of Exponential Distribution
true_q <- (-log(1-p))/lambda
true_q # 0.9162907

# Exponential Distribution
x <- rexp(n, lambda)
x

# Sample 0.6 Quantile of iid data
sample_q <- quantile(x, p)
sample_q # 1.080463

# Cheap Bootstrap (only one Bootstrap Replication)
B <- 1

q_replication <- numeric(B)
for (b in 1:B) {
  x_star <- sample(x, n, replace = TRUE)
  q_replication[b] <- quantile(x_star, p)
}

q_replication


# Confidence Interval for Cheap Bootstrap 
S_squared <- (1/B)*sum((q_replication - sample_q)^2)
# if B = 1: abs(q_replication - sample_q)
lower_CBCI <- sample_q - qt(1-alpha/2, df = B)*sqrt(S_squared)
upper_CBCI <- sample_q + qt(1-alpha/2, df = B)*sqrt(S_squared)
CheapBoot_CI <- sprintf("95%% Konfidenzintervall: [%.4f, %.4f]", lower_CBCI, upper_CBCI)
CheapBoot_CI
coverage <- ifelse(((lower_CBCI <= true_q & true_q <= upper_CBCI)), TRUE, FALSE)
coverage


# For each B, we create 1000 times synthetic data with size n = 100
# For starters only B = 1
set.seed(123)
n <- 100
B <- 1
R <- 1000
p <- 0.6
lambda <- 1
alpha = 0.05

# True 0.6 Quantile of Exponential Distribution
true_q <- (-log(1-p))/lambda
true_q # 0.9162907

lower_CBCI <- numeric(R)
upper_CBCI <- numeric(R)
coverage <- logical(R)

for (r in 1:R) {
  x <- rexp(n, lambda)
  sample_q <- quantile(x, p)
  
  q_replication <- numeric(B)
  
  for (b in 1:B) {
    x_star <- sample(x, n, replace = TRUE)
    q_replication[b] <- quantile(x_star, p)
  }

  S_squared <- (1/B)*sum((q_replication - sample_q)^2)
  lower_CBCI[r] <- sample_q - qt(1-alpha/2, df = B)*sqrt(S_squared)
  upper_CBCI[r] <- sample_q + qt(1-alpha/2, df = B)*sqrt(S_squared)
  coverage[r] <- ifelse(((lower_CBCI[r] <= true_q & true_q <= upper_CBCI[r])), TRUE, FALSE)
}

empirical_coverage <- mean(coverage)
width_mean <- mean(upper_CBCI - lower_CBCI)

cat(sprintf("Empirical coverage: %.4f\n", empirical_coverage)) # 0.9070 (bei Henry Lam waren es 0.92)
cat(sprintf("Width Mean: %.4f\n", width_mean)) # 2.4745 (bei Henry Lam waren es 2.42)


# For B = 1, 2, 5, 10, 50
set.seed(123)
n <- 100
B_values <- c(1, 2, 5, 10, 50)
R <- 1000
p <- 0.6
lambda <- 1
alpha = 0.05
true_q <- (-log(1 - p))/lambda

results <- data.frame(ReplicationSizeB = B_values, 
                      EmpiricalCoverage = NA_real_, 
                      WidthMean = NA_real_, 
                      stringsAsFactors = FALSE)
for (i in seq_along(B_values)) {
  B <- B_values[i]
  
  lower_CBCI <- numeric(R)
  upper_CBCI <- numeric(R)
  coverage <- logical(R)
  
  for (r in 1:R) {
    x <- rexp(n, lambda)
    sample_q <- quantile(x, p)
    q_replication <- numeric(B)
    for (b in 1:B) {
      x_star <- sample(x, n, replace = TRUE)
      q_replication[b] <- quantile(x_star, p)
    }
    S_squared <- (1/B) * sum((q_replication - sample_q)^2)
    
    t_dist <- qt(1 - alpha/2, df = B)
    lower_CBCI[r] <- sample_q - t_dist * sqrt(S_squared)
    upper_CBCI[r] <- sample_q + t_dist * sqrt(S_squared)
    coverage[r] <- (lower_CBCI[r] <= true_q & true_q <= upper_CBCI[r])
  }
  results$EmpiricalCoverage[i] <- mean(coverage)
  results$WidthMean[i] <- mean(upper_CBCI - lower_CBCI)
}

results
