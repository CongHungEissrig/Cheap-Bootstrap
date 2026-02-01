# Cheap Bootstrap

This repository contains the code for the simulation studies conducted as part of a Bachelor’s thesis at Ludwig-Maximilians-Universität München (LMU) on the **Cheap Bootstrap**, a computationally efficient alternative to conventional bootstrap methods for statistical inference.

Overview
-----------------
The **Cheap Bootstrap**, introduced by Lam (2022), allows the construction of valid confidence intervals with as few as one bootstrap replication, leading to substantial reductions in computational cost compared to conventional bootstrap approaches such as the Normal Bootstrap and Percentile Bootstrap.
The primary goal of this project is to empirically evaluate the performance of the Cheap Bootstrap intervals in comparison to conventional bootstrap methods, focusing on:
- **Empirical Coverage**
- **Mean Confidence Interval Width**
  
The evaluation is conducted using **normally and gamma-distributed data** and considers several parameters, including the **mean**, **median**, and selected **quantiles**. In addition, the performance of the confidence intervals is examined under **small sample sizes**, for **extreme quantiles**, and within **classical multiple linear regression models**.

Project Structure
-----------------
The repository is organized as follows:
Functions are defined in `R/`, executed in `scripts/`, results are saved in `results/`, and plots are stored in `figures/`.

```bash
├── R/                          # Define Functions
│   ├── bootstrap_statistics.R  # Compute bootstrap confidence intervals for mean, median, and quantiles using Normal, Percentile, and Cheap Bootstrap
│   ├── bootstrap_regression.R  # Compute bootstrap confidence intervals for regression coefficients using Normal, Percentile, and Cheap Bootstrap
│   ├── plot_statistics.R       # Plot empirical coverage and interval width for confidence intervals of summary statistics
│   └── plot_regression.R       # Plot empirical coverage and interval width for confidence intervals of regression coefficients
│
├── scripts/                    # Scripts that execute the R/ functions with specified simulation parameters to generate results
│   ├── run_statistics.R        # Executes the function in bootstrap_statistics.R on simulated datasets for summary statistics
│   ├── run_edgecases.R         # Executes the function in bootstrap_statistics.R on simulated datasets for summary statistics under small sample sizes and extreme quantiles
│   ├── run_regression.R        # Executes the function in bootstrap_regression.R on simulated datasets for regression coefficients
│   ├── run_plot_statistics.R   # Generates plots using plot_statistics.R for figures/ from run_statistics.R
│   ├── run_plots_edgecases.R   # Generates plots using plot_statistics.R for figures/ from run_edgecases.R
│   └── run_plots_regression.R  # Generates plots using plot_regression.R for figures/ from run_regression.R
│
├── results/                    # Simulation outputs (table of empirical coverage and average interval width)
│   ├── statistics/             # Results for summary statistics
│   │   ├── gamma/              # Gamma-distributed data
│   │   │   ├── gamma.rds       # Results for sample size = 100
│   │   │   ├── gamma_ds10.rds  # Results for sample size = 10
│   │   │   ├── gamma_ds20.rds  # Results for sample size = 20
│   │   │   ├── gamma_p01.rds   # Results for 1st percentile
│   │   │   └── gamma_p99.rds   # Results for 99th percentile
│   │   └── normal/             # Normally distributed data
│   │       ├── normal.rds
│   │       ├── normal_ds10.rds
│   │       ├── normal_ds20.rds   
│   │       ├── normal_p01.rds
│   │       └── normal_p99.rds   
│   │
│   └── regression/             # Results for regression models
│       └── mult_model.rds
│              
├── figures/                    # Plots of simulation results
│   ├── coverage/               # Empirical coverage plots
│   │   ├── coverage_gamma_mean.pdf
│   │   ├── coverage_gamma_median.pdf
│   │   ├── coverage_gamma_quantile.pdf
│   │   ├── coverage_normal_mean.pdf
│   │   ├── coverage_normal_median.pdf
│   │   └── coverage_normal_quantile.pdf
│   │
│   ├── width/                  # Mean confidence interval width plots
│   │   ├── width_gamma_mean.pdf
│   │   ├── width_gamma_median.pdf
│   │   ├── width_gamma_quantile.pdf
│   │   ├── width_normal_mean.pdf
│   │   ├── width_normal_median.pdf
│   │   └── width_normal_quantile.pdf
│   │
│   ├── edgecases/              # Small sample sizes and extreme quantiles
│   │   ├── coverage/           # Empirical coverage under small sample sizes (data size = 20)
│   │   │   ├── coverage_gamma_mean_ds20.pdf
│   │   │   ├── coverage_gamma_median_ds20.pdf
│   │   │   ├── coverage_gamma_quantile_ds20.pdf
│   │   │   ├── coverage_normal_mean_ds20.pdf
│   │   │   ├── coverage_normal_median_ds20.pdf
│   │   │   └── coverage_normal_quantile_ds20.pdf
│   │   │
│   │   ├── width/              # Mean Interval width under small sample sizes
│   │   │   ├── width_gamma_mean_ds20.pdf
│   │   │   ├── width_gamma_median_ds20.pdf
│   │   │   ├── width_gamma_quantile_ds20.pdf
│   │   │   ├── width_normal_mean_ds20.pdf
│   │   │   ├── width_normal_median_ds20.pdf
│   │   │   └── width_normal_quantile_ds20.pdf
│   │   │
│   │   └── quantile/           # Extreme quantile (p = 0.99)
│   │       ├── coverage_gamma_quantile_p99.pdf
│   │       ├── coverage_normal_quantile_p99.pdf
│   │       ├── width_gamma_quantile_p99.pdf
│   │       └── width_normal_quantile_p99.pdf
│   │
│   └── linear_regression/      # Multiple linear regression results
│       ├── coverage/           # Empirical coverage of regression coefficients
│       │   ├── coverage_beta1.pdf
│       │   ├── coverage_beta2.pdf
│       │   ├── coverage_beta3.pdf
│       │   ├── coverage_beta4.pdf
│       │   └── coverage_beta5.pdf
│       │
│       └── width/              # Mean Interval width of regression coefficients
│           ├── width_beta1.pdf
│           ├── width_beta2.pdf
│           ├── width_beta3.pdf
│           ├── width_beta4.pdf
│           └── width_beta5.pdf
├── thesis/                     # PDF of Bachelor Thesis
│   └── Bachelor_Thesis_Cheap_Bootstrap.pdf
└── README.md                   # Project overview and documentation
```
