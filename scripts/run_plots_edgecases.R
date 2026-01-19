# Packages
library(ggplot2)
library(tidyr)
library(dplyr)

# Source Function to plot
source("R/plot_statistics.R")

# ---- Ergebnisse laden / erzeugen ----
result_normal_ds20 <- readRDS("results/statistics/normal_ds20.rds")
result_gamma_ds20  <- readRDS("results/statistics/gamma_ds20.rds")

result_normal_p99 <- readRDS("results/statistics/normal_p99.rds")
result_gamma_p99  <- readRDS("results/statistics/gamma_p99.rds")


# ---- Coverage und Width Plots ----
coverage_normal_ds20 <- plot_coverage(result_normal_ds20, alpha = 0.05)
coverage_gamma_ds20  <- plot_coverage(result_gamma_ds20,  alpha = 0.05)

width_normal_ds20 <- plot_widths(result_normal_ds20)
width_gamma_ds20  <- plot_widths(result_gamma_ds20)

coverage_normal_p99 <- plot_coverage(result_normal_p99, alpha = 0.05)
coverage_gamma_p99  <- plot_coverage(result_gamma_p99,  alpha = 0.05)

width_normal_p99 <- plot_widths(result_normal_p99)
width_gamma_p99  <- plot_widths(result_gamma_p99)


# ---- Listen der Plots ----
coverage_plots_ds20 <- list(normal = coverage_normal_ds20, gamma = coverage_gamma_ds20)
width_plots_ds20    <- list(normal = width_normal_ds20,    gamma = width_gamma_ds20)

coverage_plots_p99  <- list(normal = coverage_normal_p99,  gamma = coverage_gamma_p99)
width_plots_p99     <- list(normal = width_normal_p99,     gamma = width_gamma_p99)


# ---- Coverage & Width speichern für n = 20 ----
for (dist in names(coverage_plots_ds20)) {
  for (stat in names(coverage_plots_ds20[[dist]])) {
    ggsave(
      filename = paste0("figures/edgecases/coverage/coverage_", dist, "_", stat, "_ds20.pdf"),
      plot = coverage_plots_ds20[[dist]][[stat]],
      width = 12,
      height = 12,
      units = "cm"
    )
    
    ggsave(
      filename = paste0("figures/edgecases/width/width_", dist, "_", stat, "_ds20.pdf"),
      plot = width_plots_ds20[[dist]][[stat]],
      width = 12,
      height = 12,
      units = "cm"
    )
  }
}


# ---- Coverage & Width speichern für extreme Quantile p = 0.99 ----
for (dist in names(coverage_plots_p99)) {
  ggsave(
    filename = paste0("figures/edgecases/quantile/coverage_", dist, "_quantile_p99.pdf"),
    plot = coverage_plots_p99[[dist]][["quantile"]],
    width = 12,
    height = 12,
    units = "cm"
  )
  
  ggsave(
    filename = paste0("figures/edgecases/quantile/width_", dist, "_quantile_p99.pdf"),
    plot = width_plots_p99[[dist]][["quantile"]],
    width = 12,
    height = 12,
    units = "cm"
  )
}
