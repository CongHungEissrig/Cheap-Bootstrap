plot_regression_coverage <- function(model, beta = "beta1", alpha = 0.05) {
  
  # Coverage nur für ein Beta
  df_coverage_beta <- data.frame(
    B = model$cheap$coverage$B,
    Cheap      = model$cheap$coverage[[beta]],
    Normal     = model$normal$coverage[[beta]],
    Percentile = model$percentile$coverage[[beta]],
    LM         = model$lm$coverage[[beta]]
  )
  
  # Long format für Bootstrap-Methoden
  df_coverage_long <- df_coverage_beta %>%
    pivot_longer(
      cols = c("Cheap", "Normal", "Percentile"),
      names_to = "method",
      values_to = "coverage"
    )
  
  # LM-Segmente
  lm_segments <- df_coverage_beta %>%
    mutate(xmin = B - 1, xmax = B + 1)
  
  # Plot
  ggplot() +
    geom_point(
      data = df_coverage_long,
      aes(x = B, y = coverage, color = method),
      size = 3
    ) +
    geom_line(
      data = df_coverage_long,
      aes(x = B, y = coverage, color = method),
      size = 1.5
    ) +
    geom_hline(
      yintercept = 1 - alpha,
      linetype = "dashed",
      linewidth = 1,
      color = "grey30"
    ) +
    geom_segment(
      data = lm_segments,
      aes(x = xmin, xend = xmax, y = LM, yend = LM, color = "LM (confint)"),
      size = 1.5
    ) +
    theme_bw() +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_manual(
      values = c(
        "Cheap"        = "#E69F00",
        "Normal"       = "#56B4E9",
        "Percentile"   = "#009E73",
        "LM (confint)" = "#3C5488"
      ),
      breaks = c("Cheap", "Normal", "Percentile", "LM (confint)")
    ) +
    labs(
      x = "Number of Bootstrap Replications",
      y = "Empirical Coverage",
      color = "Method"
    ) +
    theme(
      legend.position = c(0.98, 0.02),
      legend.justification = c("right", "bottom"),
      legend.background = element_rect(
        fill = "white",
        color = "grey80"
      ),
      legend.box.background = element_blank()
    )
}


plot_regression_widths <- function(model, beta = "beta1") {
  
  # Widths nur für ein Beta
  df_widths_beta <- data.frame(
    B = model$cheap$widths$B,
    Cheap      = model$cheap$widths[[beta]],
    Normal     = model$normal$widths[[beta]],
    Percentile = model$percentile$widths[[beta]],
    LM         = model$lm$widths[[beta]]
  )
  
  # Long format für Bootstrap-Methoden
  df_widths_long <- df_widths_beta %>%
    pivot_longer(
      cols = c("Cheap", "Normal", "Percentile"),
      names_to = "method",
      values_to = "width"
    )
  
  # LM-Segmente
  lm_segments <- df_widths_beta %>%
    mutate(xmin = B - 1, xmax = B + 1)
  
  # Plot
  ggplot() +
    geom_point(
      data = df_widths_long,
      aes(x = B, y = width, color = method),
      size = 3
    ) +
    geom_line(
      data = df_widths_long,
      aes(x = B, y = width, color = method),
      size = 1.5
    ) +
    geom_segment(
      data = lm_segments,
      aes(x = xmin, xend = xmax, y = LM, yend = LM, color = "LM (confint)"),
      size = 1.5
    ) +
    theme_bw() +
    labs(
      x = "Number of Bootstrap Replications",
      y = "Mean Interval Width",
      color = "Method"
    ) +
    scale_color_manual(
      values = c(
        "Cheap"        = "#E69F00",
        "Normal"       = "#56B4E9",
        "Percentile"   = "#009E73",
        "LM (confint)" = "#3C5488"
      ),
      breaks = c("Cheap", "Normal", "Percentile", "LM (confint)")
    ) +
    theme(
      legend.position = c(0.98, 0.95),
      legend.justification = c("right", "top"),
      legend.background = element_rect(
        fill = "white",
        color = "grey80"
      ),
      legend.box.background = element_blank()
    )
}