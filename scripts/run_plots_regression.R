# ---- Packages ----
library(dplyr)
library(tidyr)
library(ggplot2)


# ---- Source Functions ----
source("R/plot_regression.R")

# ---- Ergebnisse laden ----
mult_model <- readRDS("results/regression/mult_model.rds") 

betas <- c("beta1", "beta2", "beta3", "beta4", "beta5")

# dir.create("figures/linear_regression/coverage", recursive = TRUE, showWarnings = FALSE)
# dir.create("figures/linear_regression/width", recursive = TRUE, showWarnings = FALSE)

for (b in betas) {
  # Coverage
  cov_plot <- plot_regression_coverage(mult_model, beta = b, alpha = 0.05)
  ggsave(
    filename = paste0("figures/linear_regression/coverage/coverage_", b, ".pdf"),
    plot = cov_plot,
    width = 12,
    height = 12,
    units = "cm"
  )
  
  # Widths
  width_plot <- plot_regression_widths(mult_model, beta = b, alpha = 0.05)
  ggsave(
    filename = paste0("figures/linear_regression/width/width_", b, ".pdf"),
    plot = width_plot,
    width = 12,
    height = 12,
    units = "cm"
  )
}
