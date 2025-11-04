# Standard Error Bootstrap (Standard Interval)
# Percentile Bootstrap 

# Packages
library(boot)
library(ggplot2)

# Student-Dataset will act as a Sample from a Population
student <- read.csv("data/StudentsPerformance.xls")

# Bootstrap for Mean

# Function for Mean
mean_fun <- function(data, idx) {
  df <- data[idx]
  c(mean(df))
}

# Bootstrap for Mean
set.seed(123)
boot_mean <- boot(student$math.score, mean_fun, R = 250)
boot_mean

# Bootstrap Distribution
plot(boot_mean)

# Confidence Interval (Standard, Percentile)
boot.ci(boot.out = boot_mean, type = c("norm", "perc"))


# Bootstrap for Median

# Function for Median
median_fun <- function(data, idx) {
  df <- data[idx]
  c(median(df))
}

# Bootstrap for Median
set.seed(123)
boot_median <-boot(student$math.score, median_fun, R = 250)
boot_median

# Plot the bootstrap distribution
plot(boot_median)

# Confidence Intervals (Standard, Percentile)
boot.ci(boot.out = boot_median, 
        type = c("norm", "perc"))


