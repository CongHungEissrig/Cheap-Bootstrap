plot_coverage <- function(result, alpha) {
  df_coverage <- result$empirical_coverage
  df_coverage$B <- result$B
  
  df_coverage_long <- df_coverage %>% 
    pivot_longer(
      cols = -B,
      names_to = c("method", "statistic"),
      names_sep = "_", 
      values_to = "coverage"
    )
  
  plot <- ggplot(data = df_coverage_long, mapping = aes(x = B, y = coverage, group = method, color = method)) + 
    geom_point(size = 3) +
    geom_line(size = 1.5) +
    theme_bw() + 
    scale_y_continuous(limits = c(0, 1)) +
    geom_hline(yintercept = 1 - alpha, linetype = "dashed", size = 1) +
    scale_color_manual(values = c(
      "Cheap"       = "#E69F00",
      "Normal"      = "#56B4E9",
      "Percentile"  = "#009E73"
    )) +
    labs(y = "Empirical Coverage", x = "Number of Bootstrap Replications", color = "Method") +
    theme(
      legend.position = c(0.98, 0.02),
      legend.justification = c("right", "bottom"),
      legend.box.just = "right",
      legend.background = element_rect(
        fill = "white",
        color = "grey80"
      ),
      legend.box.background = element_blank()
    )
  plot_mean <- plot %+% filter(df_coverage_long, statistic == "mean")
  plot_median <- plot %+% filter(df_coverage_long, statistic == "median")
  plot_quantile <- plot %+% filter(df_coverage_long, statistic == "quantile")
  
  # Als Liste zurückgeben
  return(list(
    mean = plot_mean,
    median = plot_median,
    quantile = plot_quantile
  ))
  
}



plot_widths <- function(result) {
  
  df_widths <- result$widths_mean
  df_widths$B <- result$B
  
  df_widths_long <- df_widths %>% 
    pivot_longer(
      cols = -B,
      names_to = c("method", "statistic"),
      names_sep = "_", 
      values_to = "widths_mean"
    )
  # Maximaler Wert über alle Statistiken
  y_max <- max(df_widths_long$widths_mean, na.rm = TRUE)
  
  plot <- ggplot(data = df_widths_long, mapping = aes(x = B, y = widths_mean, group = method, color = method)) + 
    geom_point(size = 3) +
    geom_line(size = 1.5) +
    theme_bw() + 
    scale_color_manual(values = c(
      "Cheap"       = "#E69F00",
      "Normal"      = "#56B4E9",
      "Percentile"  = "#009E73"
    )) +
    scale_y_continuous(limits = c(0, y_max), breaks = seq(0, ceiling(y_max), by = 1)  ) +
    labs(y = "Mean Interval Width", x = "Number of Bootstrap Replications", color = "Method") +
    theme(
      legend.position = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.box.just = "right",
      legend.background = element_rect(
        fill = "white",
        color = "grey80"
      ),
      legend.box.background = element_blank()
    ) 
  
  
  plot_mean <- plot %+% filter(df_widths_long, statistic == "mean")
  plot_median <- plot %+% filter(df_widths_long, statistic == "median")
  plot_quantile <- plot %+% filter(df_widths_long, statistic == "quantile")
  
  
  return(list(
    mean = plot_mean,
    median = plot_median,
    quantile = plot_quantile
  ))
}