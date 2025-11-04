# Standard Error Bootstrap (Standard Interval)
# Percentile Bootstrap 

# Student-Dataset will act as a Sample from a Population
set.seed(123)
student <- read.csv("data/StudentsPerformance.xls")

# We observe the math score of these 1000 students
# Calculate sample mean and sample median
student_mean <- mean(student$math.score)
student_median <- median(student$math.score)
student_sd <- sd(student$math.score)

student_mean
student_median
student_sd

# Bootstrap Algorithm Settings
B <- 250
n <- nrow(student)
bootstrap_means <- numeric(B)
bootstrap_medians <- numeric(B)

# Bootstrap Algorithm for Sample Mean/Median
# Empirical Distribution (Dataset), Bootstrap Sample and Bootstrap Replication
for (b in seq_len(B)) {
  bootstrap_sample <- sample(student$math.score, nrow(student), replace = TRUE)
  bootstrap_means[b] <- mean(bootstrap_sample)
  bootstrap_medians[b] <- median(bootstrap_sample)
}

# Bootstrap Replications
bootstrap_means
bootstrap_medians

# Calculate Standard Error with Bootstrap Replications via Sample Standard Deviation
bootmeans_se <- sd(bootstrap_means)
bootmedians_se <- sd(bootstrap_medians)

bootmeans_se
bootmedians_se

# Construct 95%-Confidence Interval (Standard Interval)
alpha <- 0.05
mean_CI <- c(student_mean - qnorm(1 - alpha/2)*bootmeans_se,
             student_mean + qnorm(1 - alpha/2)*bootmeans_se)

median_CI <- c(student_median - qnorm(1 - alpha/2)*bootmedians_se,
               student_median + qnorm(1 - alpha/2)*bootmedians_se)
mean_CI
median_CI

# Construct Percentile Confidence Interval
diff_mean <- bootstrap_means - student_mean
diff_median <- bootstrap_medians - student_median

q_mean <- quantile(diff_mean, c(1 - alpha/2, alpha/2))
q_median <- quantile(diff_median, c(1- alpha/2, alpha/2))

mean_basic_CI <- c(student_mean - q_mean[1], student_mean - q_mean[2])
median_basic_CI <- c(student_median - q_median[1], student_median - q_median[2])

mean_basic_CI
median_basic_CI
