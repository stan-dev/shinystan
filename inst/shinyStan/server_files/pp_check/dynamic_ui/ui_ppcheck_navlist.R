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



output$ui_ppcheck_navlist <- renderUI({
  navlistPanel(id = "pp_navlist", widths = c(4,8), well = FALSE,  
               "Data",
               tabPanel("Select data",
                        uiOutput("ui_pp_data")
               ),
               "Plots",
               tabPanel("Distribution of observed data vs replications",
                        uiOutput("ui_pp_hists_rep_vs_obs")
               ),
               tabPanel("Distributions of test statistics",
                        uiOutput("ui_pp_hists_test_statistics")
               ),
               tabPanel("Scatters",
                        uiOutput("ui_pp_scatters")
               ),
               tabPanel("Histogram of residuals",
                        uiOutput("ui_pp_hist_resids")
               ),
               "About",
               tabPanel("About graphical posterior predictive checking",
                        uiOutput("ui_pp_about")
               ),
               tabPanel("Tutorial",
                        includeMarkdown("markdown/pp_check_tutorial.md")
               )
               
  )
})
