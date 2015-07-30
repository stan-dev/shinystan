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



output$ui_pp_scatters <- renderUI({
  div(
    br(),
    h5(withMathJax(plot_descriptions["plot_obs_vs_avg_y_rep"])),
    checkboxInput("pp_zoom_to_zero", "Zoom to include (0,0)", value = FALSE),
    plotOutput("pp_y_vs_avg_rep_out", height = "250px"),
    h5(withMathJax(plot_descriptions["plot_avg_rep_vs_avg_resid_rep"])),
    plotOutput("pp_avg_rep_vs_avg_resid_rep_out", height = "250px"),
    br()
  )
})
