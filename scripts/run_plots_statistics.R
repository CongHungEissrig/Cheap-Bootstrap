# Packages
library(ggplot2)
library(tidyr)
library(dplyr)

# Source Function from bootstrap_statistics.R
source("R/plot_statistics.R")


# Load result
result_normal <- readRDS("results/statistics/normal.rds")
result_gamma  <- readRDS("results/statistics/gamma.rds")

# Create Coverage and Width Plots
coverage_normal <- plot_coverage(result_normal, alpha = 0.05)
coverage_gamma  <- plot_coverage(result_gamma, alpha = 0.05)

width_normal <- plot_widths(result_normal)
width_gamma  <- plot_widths(result_gamma)

# Lists of coverage and width plots for different distributions
coverage_plots_ds100 <- list(normal = coverage_normal, gamma = coverage_gamma)
width_plots_ds100    <- list(normal = width_normal,    gamma = width_gamma)


# Save coverage and width plots
for (dist in names(coverage_plots_ds100)) {
  for (stat in names(coverage_plots_ds100[[dist]])) {
    
    # Coverage
    ggsave(
      filename = paste0("figures/coverage/coverage_", dist, "_", stat, ".pdf"),
      plot = coverage_plots_ds100[[dist]][[stat]],
      width = 12,
      height = 12,
      units = "cm"
    )
    
    # Width 
    ggsave(
      filename = paste0("figures/width/width_", dist, "_", stat, ".pdf"),
      plot = width_plots_ds100[[dist]][[stat]],
      width = 12,
      height = 12,
      units = "cm"
    )
  }
}


