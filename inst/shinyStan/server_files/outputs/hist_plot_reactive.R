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



# two functions: hist_plot, hist_plot_multiview
hist_plot <- reactive({

  validate(need(input$param, message = FALSE),
           need(!is.null(input$hist_chain), message = FALSE))

  chain <- input$hist_chain
  if (is.na(chain)) chain <- 0

  binwd <- input$hist_binwd
  if (is.na(binwd)) binwd <- 0

  do.call(".param_hist", args = list(
    param       = input$param,
    dat         = par_samps_post_warmup(),
    chain       = chain,
    binwd       = binwd,
    fill_color  = input$hist_fill_color,
    line_color  = input$hist_line_color,
    transform_x = input$hist_transform_x
  ))
})


hist_plot_multiview <- reactive({

  if (input$param == "") {
    return()
  }

  do.call(".param_hist", args = list(
    param       = input$param,
    dat         = par_samps_post_warmup(),
    chain       = 0,
    binwd       = 0,
    title       = FALSE
  ))
})
