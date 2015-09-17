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


pp_y_vs_avg_rep <- reactive({
  pp_tests()
  y <- get_y()
  yrep <- get_yrep()
  zoom <- input$pp_zoom_to_zero
  do.call(".pp_y_vs_avg_rep", args = list(
    y = y, 
    colMeans_yrep = colMeans(yrep),
    zoom_to_zero = zoom
  ))
})

output$pp_y_vs_avg_rep_out <- renderPlot({
  pp_y_vs_avg_rep()
}, bg = "transparent")
