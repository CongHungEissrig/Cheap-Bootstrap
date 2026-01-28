# Packages
library(ggplot2)
library(tidyr)
library(dplyr)

# Source Function from plot_statistics.R to plot the results
source("R/plot_statistics.R")

# Load Results 
result_normal_ds20 <- readRDS("results/statistics/normal_ds20.rds")
result_gamma_ds20  <- readRDS("results/statistics/gamma_ds20.rds")

result_normal_p99 <- readRDS("results/statistics/normal_p99.rds")
result_gamma_p99  <- readRDS("results/statistics/gamma_p99.rds")


# Create Coverage and Width Plots
coverage_normal_ds20 <- plot_coverage(result_normal_ds20, alpha = 0.05)
coverage_gamma_ds20  <- plot_coverage(result_gamma_ds20,  alpha = 0.05)

width_normal_ds20 <- plot_widths(result_normal_ds20)
width_gamma_ds20  <- plot_widths(result_gamma_ds20)

coverage_normal_p99 <- plot_coverage(result_normal_p99, alpha = 0.05)
coverage_gamma_p99  <- plot_coverage(result_gamma_p99,  alpha = 0.05)

width_normal_p99 <- plot_widths(result_normal_p99)
width_gamma_p99  <- plot_widths(result_gamma_p99)


# Lists of coverage and width plots for different distributions and scenarios
coverage_plots_ds20 <- list(normal = coverage_normal_ds20, gamma = coverage_gamma_ds20)
width_plots_ds20    <- list(normal = width_normal_ds20,    gamma = width_gamma_ds20)

coverage_plots_p99  <- list(normal = coverage_normal_p99,  gamma = coverage_gamma_p99)
width_plots_p99     <- list(normal = width_normal_p99,     gamma = width_gamma_p99)


# Save coverage and width plots for sample size n = 20
for (dist in names(coverage_plots_ds20)) {
  # Coverage
  for (stat in names(coverage_plots_ds20[[dist]])) {
    ggsave(
      filename = paste0("figures/edgecases/coverage/coverage_", dist, "_", stat, "_ds20.pdf"),
      plot = coverage_plots_ds20[[dist]][[stat]],
      width = 12,
      height = 12,
      units = "cm"
    )
    
  # Width
    ggsave(
      filename = paste0("figures/edgecases/width/width_", dist, "_", stat, "_ds20.pdf"),
      plot = width_plots_ds20[[dist]][[stat]],
      width = 12,
      height = 12,
      units = "cm"
    )
  }
}


# Save coverage and width plots for extreme quantile p = 0.99
for (dist in names(coverage_plots_p99)) {
  
  # Coverage
  ggsave(
    filename = paste0("figures/edgecases/quantile/coverage_", dist, "_quantile_p99.pdf"),
    plot = coverage_plots_p99[[dist]][["quantile"]],
    width = 12,
    height = 12,
    units = "cm"
  )
  
  # Width
  ggsave(
    filename = paste0("figures/edgecases/quantile/width_", dist, "_quantile_p99.pdf"),
    plot = width_plots_p99[[dist]][["quantile"]],
    width = 12,
    height = 12,
    units = "cm"
  )
}
