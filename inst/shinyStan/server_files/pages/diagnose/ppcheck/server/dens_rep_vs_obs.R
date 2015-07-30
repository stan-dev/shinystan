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



pp_dens_rep_vs_obs <- reactive({
  pp_tests()
  y <- get(input$y_name)
  y_rep <- y_rep()
  # sample_ids <- sample_ids_for_dens()
  sample_ids <- sample_ids_for_hist()
  y_rep_samp <- y_rep[sample_ids, ]
  
  max_y_rep_dens <- apply(y_rep, 1, function(x) density(x)$y)
  y_lim <- c(0, max(density(y)$y, max_y_rep_dens))
  x_lim <- range(c(y, y_rep))
  do.call(".pp_dens_rep_vs_obs", args = list(
    y = y, 
    y_rep_samp = y_rep_samp,
    y_lim = y_lim,
    x_lim = x_lim
  ))
})

output$pp_dens_rep_vs_obs_out <- renderPlot({
  x <- suppressMessages(pp_dens_rep_vs_obs())
  suppress_and_print(x)
}, bg = "transparent")
