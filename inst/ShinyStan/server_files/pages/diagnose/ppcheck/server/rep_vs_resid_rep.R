# This file is part of shinystan
# Copyright (C) Jonah Gabry
#
# shinystan is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
# 
# shinystan is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# this program; if not, see <http://www.gnu.org/licenses/>.


pp_avg_rep_vs_avg_resid_rep <- reactive({
  pp_tests()
  y <- get_y()
  yrep <- get_yrep()
  rowMeans_resids <- rowMeans(y - yrep)
  rowMeans_yrep <- rowMeans(yrep)
  do.call(".pp_avg_rep_vs_avg_resid_rep", args = list(
    rowMeans_yrep = rowMeans_yrep,
    rowMeans_resids = rowMeans_resids
  ))
})

output$pp_avg_rep_vs_avg_resid_rep_out <- renderPlot({
  pp_avg_rep_vs_avg_resid_rep()
}, bg = "transparent")
