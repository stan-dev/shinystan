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



output$ui_pp_hist_resids <- renderUI({
  div(
    br(),
    h4(withMathJax(plot_descriptions["plot_hist_resids"])),
    plotOutput("pp_hist_resids_out"),
    actionButton("resample_resids_go", label = "Show a different replication", icon = icon("refresh"))
    )
})