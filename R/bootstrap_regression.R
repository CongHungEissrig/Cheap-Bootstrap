Bootstrap_Regression <- function(B, N, data_size, params_list, alpha, sigma, mean, sd) {
  
  # Matrices for Empirical Coverage and Average Interval Width for the different methods
  d <- length(params_list)
  beta_vec <- unlist(params_list)
  
  cheap_empirical_coverage <- matrix(NA, nrow = length(B), ncol = d)
  cheap_widths_mean <- matrix(NA, nrow = length(B), ncol = d)
  
  normal_empirical_coverage <- matrix(NA, nrow = length(B), ncol = d)
  normal_widths_mean <- matrix(NA, nrow = length(B), ncol = d)
  
  percentile_empirical_coverage <- matrix(NA, nrow = length(B), ncol = d)
  percentile_widths_mean <- matrix(NA, nrow = length(B), ncol = d)
  
  lm_empirical_coverage <- matrix(NA, nrow = length(B), ncol = d)
  lm_widths_mean <- matrix(NA, nrow = length(B), ncol = d)
  
  
  colnames(cheap_empirical_coverage) <-
    colnames(normal_empirical_coverage) <-
    colnames(percentile_empirical_coverage) <- 
    colnames(lm_empirical_coverage) <- names(params_list)
  
  colnames(cheap_widths_mean) <-
    colnames(normal_widths_mean) <-
    colnames(percentile_widths_mean) <- 
    colnames(lm_widths_mean) <- names(params_list)
  
  
  for (i in seq_along(B)) {
    boot_size <- B[i]
    
    cover_cheap <- cover_normal <- cover_percentile <- cover_lm <- matrix(0, N, d)
    widths_cheap <- widths_normal <- widths_percentile <- widths_lm <- matrix(0, N, d)
    
    for (j in seq_len(N)) {
      
      # Data Generating Process 
      X <- matrix(rnorm(data_size * d, mean = mean, sd = sd), ncol = d)
      colnames(X) <- names(beta_vec)
      y <- X %*% beta_vec + rnorm(data_size, 0, sigma)
      data <- data.frame(y = y, X)
      
      # Multiple Linear Regression Model and Coefficients
      model <- lm(y ~ ., data = data)
      original_stat <- coef(model)[-1]
      lm_ci <- confint(model, level = 1 - alpha)[-1, ]
      
      # Bootstrap Algorithm
      boot_rep <- matrix(NA, nrow = boot_size, ncol = d)
      for (b in seq_len(boot_size)) {
        idx <- sample.int(nrow(data), replace = TRUE)
        boot_sample <- data[idx, ]
        boot_model <- lm(y ~ ., data = boot_sample)
        boot_rep[b, ] <- coef(boot_model)[-1]
      }
      
      
      # Intervals 
      for (k in seq_len(d)) {
        
        boot_k <- boot_rep[, k]
        beta_hat <- original_stat[k]
        beta_true <- beta_vec[k]
        
        # Cheap Method 
        
        S_squared <- (1/boot_size) * sum((boot_k - beta_hat)^2)
        cheap_lower <- beta_hat - qt(1 - alpha/2, df = boot_size) * sqrt(S_squared)
        cheap_upper <- beta_hat + qt(1 - alpha/2, df = boot_size) * sqrt(S_squared)
        
        cover_cheap[j, k] <- cheap_lower <= beta_true && beta_true <= cheap_upper
        widths_cheap[j, k] <- cheap_upper - cheap_lower
        
        
        # Normal Method
        se <- sd(boot_k)
        normal_lower <- beta_hat - qnorm(1 - alpha / 2) * se
        normal_upper <- beta_hat + qnorm(1 - alpha / 2) * se
        
        cover_normal[j, k] <- normal_lower <= beta_true && beta_true <= normal_upper
        widths_normal[j, k] <- normal_upper - normal_lower
        
        # Percentile Method
        perc_lower <- quantile(boot_k, alpha / 2)
        perc_upper <- quantile(boot_k, 1 - alpha / 2)
        
        cover_percentile[j, k] <- perc_lower <= beta_true && beta_true <= perc_upper
        widths_percentile[j, k] <- perc_upper - perc_lower
        
        # Linear Model 
        lm_lower <- lm_ci[k, 1]
        lm_upper <- lm_ci[k, 2]
        
        cover_lm[j, k] <- lm_lower <= beta_true && beta_true <= lm_upper
        widths_lm[j, k] <- lm_upper - lm_lower
      }
    }
    
    # Average over N
    cheap_empirical_coverage[i, ] <- colMeans(cover_cheap, na.rm = TRUE)
    cheap_widths_mean[i, ] <- colMeans(widths_cheap, na.rm = TRUE)
    
    normal_empirical_coverage[i, ] <- colMeans(cover_normal, na.rm = TRUE)
    normal_widths_mean[i, ] <- colMeans(widths_normal, na.rm = TRUE)
    
    percentile_empirical_coverage[i, ] <- colMeans(cover_percentile, na.rm = TRUE)
    percentile_widths_mean[i, ] <- colMeans(widths_percentile, na.rm = TRUE)
    
    lm_empirical_coverage[i, ] <- colMeans(cover_lm, na.rm = TRUE)
    lm_widths_mean[i, ] <- colMeans(widths_lm, na.rm = TRUE)
    
  }
  return(list(
    cheap = list(
      coverage = data.frame(B = B, cheap_empirical_coverage),
      widths   = data.frame(B = B, cheap_widths_mean)
    ),
    normal = list(
      coverage = data.frame(B = B, normal_empirical_coverage),
      widths   = data.frame(B = B, normal_widths_mean)
    ),
    percentile = list(
      coverage = data.frame(B = B, percentile_empirical_coverage),
      widths   = data.frame(B = B, percentile_widths_mean)
    ),
    lm = list(
      coverage = data.frame(B = B, lm_empirical_coverage), # B Column is optional, 
      widths   = data.frame(B = B, lm_widths_mean) # B column is optional 
    )
  ))
}
