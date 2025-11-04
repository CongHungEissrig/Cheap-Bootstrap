library(readxl)
library(dplyr)
library(ggplot2)


# Nochmal ausprobieren wo student unser original sample dataset ist von dem wir die Population nicht wissen
# Einfach Bootstrap Algorithm durchführen und die bootstrap sample mean mit original sample mean vergleichen


# Nochmal ausprobieren wo Student unser census dataset ist, wir dann random samples draus ziehen und dann 
# confidence interval bilden
# Mit mehr als 20 oder 30 (mindestens 50 Random Samples)

# Berechne Mean von Census Data
# Berechne Mean von Bootstrap Sample Mean
# Confidence Interval Bilden von Bootstrap Sample Mean
# Nachchecken ob das Confidence Interval die Richtige Answer von Mean von Census Data behält

# Test: Draw 100 real samples of size n from the census/original data set
# For each 100 draw 1000 Bootstrap samples and compute for each the bootstrap sample mean
# From that bootstrap sampling distribution of 1000 means, compute a confidence interval
# Check whether the confidence interval contains the right answer of 47.24


# Original Dataset with Known Mean and Standard Deviation 
# For Testing Purposes Only (In Practice the Probability Distribution is Unknown)

# Stichprobe von Population und möchten dann Aussagen über die Population anhand dieser Stichprobe machen.
student <- read.csv("data/StudentsPerformance.xls")
student_mean <- mean(student$math.score)
student_mean
student_sd <- sd(student$math.score)
student_sd

B <- 1000
boot_means <- numeric(1000)

# Empirische Verteilung, Bootstrap Samples, Vector of Bootstrap Replication
for (b in 1:B) {
  bs_sample <- sample(student$math.score, nrow(student), replace = TRUE)
  boot_means[b] <- mean(bs_sample)
}
boot_means

# Standard of boot_means
boot_se <- sd(boot_means)
boot_se

# Interpretation
# Average Math score of student population based on this dataset is 66.08504, up to an error of 0.4663178 Punkten

