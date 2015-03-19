# This file is part of shinyStan
# Copyright (C) 2015 Jonah Sol Gabry & Stan Development Team
#
# shinyStan is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
# 
# shinyStan is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# this program; if not, see <http://www.gnu.org/licenses/>.



plot_names <- c("plot_hists_rep_vs_obs",
                "plot_dens_rep_vs_obs",
                "plot_obs_vs_avg_y_rep",
                "plot_hist_resids",
                "plot_avg_rep_vs_avg_resid_rep",
                "plot_test_statistics"
) 

plot_descriptions <- c(plot_hists_rep_vs_obs = "Distributions of observed data and a random sample of replications",
                       plot_dens_rep_vs_obs = "Density estimate of observed data (blue) and a random sample of replications",
                       plot_obs_vs_avg_y_rep = "Observations vs average simulated value",
                       plot_hist_resids = "Residuals",
                       plot_avg_rep_vs_avg_resid_rep = "Average simulated value vs average residual",
                       plot_test_statistics = "Distributions of test statistics \\(T(y^{rep})\\)")

