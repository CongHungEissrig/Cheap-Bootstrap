# Packages
library(ggplot2)
library(tidyr)
library(dplyr)

# Source Function to plot
source("R/plot_statistics.R")


# ---- Ergebnisse laden ----
result_normal <- readRDS("results/statistics/normal.rds")
result_gamma  <- readRDS("results/statistics/gamma.rds")

# ---- Coverage Plots ----
coverage_normal <- plot_coverage(result_normal, alpha = 0.05)
coverage_gamma  <- plot_coverage(result_gamma, alpha = 0.05)

# ---- Width Plots ----
width_normal <- plot_widths(result_normal)
width_gamma  <- plot_widths(result_gamma)

# ---- Coverage & Width speichern für normale Fälle ----
coverage_plots_ds100 <- list(normal = coverage_normal, gamma = coverage_gamma)
width_plots_ds100    <- list(normal = width_normal,    gamma = width_gamma)

for (dist in names(coverage_plots_ds100)) {
  for (stat in names(coverage_plots_ds100[[dist]])) {
    # Coverage speichern
    ggsave(
      filename = paste0("figures/coverage/coverage_", dist, "_", stat, ".pdf"),
      plot = coverage_plots_ds100[[dist]][[stat]],
      width = 12,
      height = 12,
      units = "cm"
    )
    
    # Width speichern
    ggsave(
      filename = paste0("figures/width/width_", dist, "_", stat, ".pdf"),
      plot = width_plots_ds100[[dist]][[stat]],
      width = 12,
      height = 12,
      units = "cm"
    )
  }
}


