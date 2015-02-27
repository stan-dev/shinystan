plot_names <- c("plot_hists_rep_vs_obs",
                "plot_dens_rep_vs_obs",
                "plot_obs_vs_avg_y_rep",
                "plot_hist_resids",
                "plot_avg_rep_vs_avg_resid_rep",
                "plot_test_statistics"
) 

plot_descriptions <- c(plot_hists_rep_vs_obs = "Distributions of observed data and a random sample of replications",
                       plot_dens_rep_vs_obs = "Density estimate of observed data (blue) and a random sample of replications",
                       plot_obs_vs_avg_y_rep = "Observed data vs average simulated value",
                       plot_hist_resids = "Distribution of a single realization of the residuals (with normal density curve)",
                       plot_avg_rep_vs_avg_resid_rep = "Average simulated value vs average residual",
                       plot_test_statistics = "Distributions of test statistics \\(T(y^{rep})\\). The blue lines show \\(T(y)\\), the value of the statistic computed from the observed data.")

