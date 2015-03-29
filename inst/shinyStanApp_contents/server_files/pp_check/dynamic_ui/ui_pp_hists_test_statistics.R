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



output$ui_pp_hists_test_statistics <- renderUI({
  div(
    br(),
    h4(withMathJax(plot_descriptions["plot_test_statistics"])),
    helpText("The blue lines show \\(T(y)\\), the value of the statistic computed from the observed data."),
    radioButtons("pp_hists_test_statistics_type", label = "", choices = list(Histograms = "histogram", Densities = "density"), inline = TRUE),
    fluidRow(
      column(6, plotOutput("pp_hists_test_statistics_mean_out", height = "200px")),
      column(6, plotOutput("pp_hists_test_statistics_sd_out", height = "200px"))
    ),
    br(),
    fluidRow(
      column(6, plotOutput("pp_hists_test_statistics_min_out", height = "200px")),
      column(6, plotOutput("pp_hists_test_statistics_max_out", height = "200px"))
    ),
    br()
  )
  
})