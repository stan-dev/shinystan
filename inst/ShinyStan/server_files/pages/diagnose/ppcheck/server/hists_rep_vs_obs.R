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

pp_hists_rep_vs_obs <- reactive({
  pp_tests()
  validate(need(input$pp_hists_rep_vs_obs_type, message = "Loading..."))
  y <- get(input$y_name)
  y_rep <- y_rep()
  sample_ids <- sample_ids_for_hist()
  y_rep_samp <- y_rep[sample_ids, ]
  rownames(y_rep_samp) <- paste("y_rep", sample_ids)
  geom <- input$pp_hists_rep_vs_obs_type 
  if (geom == "density" & input$pp_hists_rep_vs_obs_overlay == TRUE) {
    x_lim <- range(c(y, y_rep))
    out <- do.call(".pp_dens_rep_vs_obs", args = list(
      y = y, 
      y_rep_samp = y_rep_samp,
      x_lim = x_lim
    ))
    return(out)
  }
  graphs <- .pp_hists_rep_vs_obs(y = y, y_rep_samp = y_rep_samp, geom = geom)
  suppressMessages(do.call(gridExtra::grid.arrange, c(graphs, ncol = 3)))
})

output$pp_hists_rep_vs_obs_out <- renderPlot({
  pp_hists_rep_vs_obs()
}, bg = "transparent")
