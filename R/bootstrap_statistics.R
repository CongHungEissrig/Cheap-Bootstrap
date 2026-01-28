# =============================================================================
# Function: Bootstrap_Statistic
# =============================================================================
# Description:
# Simulates datasets and constructs confidence intervals for three
# statistics: mean, median, and a specified quantile, using three bootstrap
# methods: Cheap, Normal, and Percentile.
# 
# Inputs:
# B           : Vector of bootstrap replication sizes (e.g., c(1, 2, 5, 10, 50))
# N           : Number of simulated datasets
# data_size   : Number of observations per dataset
# alpha       : Significance level for confidence intervals (default: 0.05)
# p           : Quantile (default: 0.75)
# distribution: Distribution of the simulated data ("normal" or "gamma")
# params      : List of distribution parameters
#               For "normal": mean, sd
#               For "gamma" : shape, rate
#
# Outputs:
#   List with three elements:
#     - B                  : The bootstrap sizes used
#     - empirical_coverage : Data frame with the empirical coverage of intervals
#     - widths_mean        : Data frame with the mean widths of intervals
# =============================================================================


Bootstrap_Statistic <- function(B,
                      N,
                      data_size,
                      alpha = 0.05,
                      p = 0.75,
                      distribution = c("normal", "gamma"),
                      params = list(
                        mean = NA,
                        sd = NA,
                        shape = NA,
                        rate = NA
                      )) {
  stats_names <- c("mean", "median", "quantile")
  methods_names <- c("Cheap", "Normal", "Percentile")
  
  # Compute "true" values for the statistics based on the chosen distribution

  if (distribution == "normal") {
    mu <- params$mean
    sigma <- params$sd
    true_stats <- c(
      mean = mu,
      median = mu,
      quantile = mu + sigma * qnorm(p)
    )
  } else {
    shape <- params$shape
    rate  <- params$rate
    true_stats <- c(
      mean = shape / rate,
      median = qgamma(0.5, shape, rate),
      quantile = qgamma(p, shape, rate)
    )
  }
  
  # Initialize matrices to store results
  empirical_coverage <- matrix(NA,
                               nrow = length(B),
                               ncol = length(stats_names) * length(methods_names))
  mean_widths <- matrix(NA,
                        nrow = length(B),
                        ncol = length(stats_names) * length(methods_names))
  
  colnames(empirical_coverage) <- colnames(mean_widths) <-
    as.vector(sapply(methods_names, function(m)
      paste0(m, "_", stats_names)))
  
  # Loop over different bootstrap replication sizes
  for (i in seq_along(B)) {
    boot_size <- B[i]
    
    # Matrices to store results for each simulated dataset
    coverage_matrix <- matrix(NA,
                              nrow = N,
                              ncol = length(stats_names) * length(methods_names))
    widths_matrix <- matrix(NA,
                            nrow = N,
                            ncol = length(stats_names) * length(methods_names))
    colnames(coverage_matrix) <- colnames(widths_matrix) <- colnames(empirical_coverage)
    
    
    # Loop over simulated datasets
    for (j in seq_len(N)) {
      
      # ----------------------
      # Simulate dataset
      # ----------------------
      data <- if (distribution == "normal") {
        rnorm(data_size, params$mean, params$sd)
      } else {
        rgamma(data_size, params$shape, params$rate)
      }
      
      # ----------------------
      # Compute statistics for original dataset
      # ----------------------
      original_stats <- c(
        mean = mean(data),
        median = median(data),
        quantile = as.numeric(quantile(data, p))
      )
      
      # ----------------------
      # Bootstrap Algorithm
      # ----------------------
      boot_reps <- matrix(NA, nrow = boot_size, ncol = length(stats_names))
      colnames(boot_reps) <- stats_names
      for (b in seq_len(boot_size)) {
        sample_data <- sample(data, size = data_size, replace = TRUE)
        boot_reps[b, ] <- c(
          mean = mean(sample_data),
          median = median(sample_data),
          quantile = as.numeric(quantile(sample_data, p))
        )
      }
      
      # ----------------------
      # Compute confidence intervals for each statistic
      # and check whether the true parameter lies within each interval 
      # ----------------------
      for (stat in stats_names) {
        
        # ----------------------
        # Cheap Bootstrap
        # ----------------------
        S_squared <- (1 / boot_size) * sum((boot_reps[, stat] - original_stats[stat])^2)
        cheap_lower <- original_stats[stat] - qt(1 - alpha / 2, df = boot_size) * sqrt(S_squared)
        cheap_upper <- original_stats[stat] + qt(1 - alpha / 2, df = boot_size) * sqrt(S_squared)
        coverage_matrix[j, paste0("Cheap_", stat)] <- as.numeric(cheap_lower <= true_stats[stat] &
                                                                   true_stats[stat] <= cheap_upper)
        widths_matrix[j, paste0("Cheap_", stat)] <- cheap_upper - cheap_lower
        
        # ----------------------
        # Normal Bootstrap
        # ----------------------
        se <- sd(boot_reps[, stat])
        normal_lower <- original_stats[stat] - qnorm(1 - alpha / 2) * se
        normal_upper <- original_stats[stat] + qnorm(1 - alpha / 2) * se
        coverage_matrix[j, paste0("Normal_", stat)] <- as.numeric(normal_lower <= true_stats[stat] &
                                                                    true_stats[stat] <= normal_upper)
        widths_matrix[j, paste0("Normal_", stat)] <- normal_upper - normal_lower
        
        # ----------------------
        # Percentile Bootstrap
        # ----------------------
        perc_lower <- quantile(boot_reps[, stat], alpha / 2)
        perc_upper <- quantile(boot_reps[, stat], 1 - alpha / 2)
        coverage_matrix[j, paste0("Percentile_", stat)] <- as.numeric(perc_lower <= true_stats[stat] &
                                                                        true_stats[stat] <= perc_upper)
        widths_matrix[j, paste0("Percentile_", stat)] <- perc_upper - perc_lower
        
        
      }
    }
    
    # Average results over N simulated datasets
    empirical_coverage[i, ] <- colMeans(coverage_matrix, na.rm = TRUE)
    mean_widths[i, ] <- colMeans(widths_matrix, na.rm = TRUE)
    
  }
  
  # Return results as a list
  return(list(
    B = B,
    empirical_coverage = as.data.frame(empirical_coverage),
    widths_mean = as.data.frame(mean_widths)
  ))
}