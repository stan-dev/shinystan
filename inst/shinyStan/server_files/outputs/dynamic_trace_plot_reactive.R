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




dynamic_trace_plot <- reactive({

  validate(need(input$param, message = FALSE),
           need(input$dynamic_trace_chain, message = FALSE))

  stack <- input$dynamic_trace_stack == "stacked"
  chain <- input$dynamic_trace_chain
  if (is.na(chain)) chain <- 0 # set chain to zero (all chains) if input field is blank

  do.call(".param_trace_dynamic", args = list(
    param_samps = par_samps_all(),
    param_name = input$param,
    chain = chain,
#     warmup_val = warmup_val,
#     warmup_shade = (input$dynamic_trace_warmup_shade == "show"),
    stack = stack,
    grid = (input$dynamic_trace_grid == "show")
  ))
})

dynamic_trace_plot_multiview <- reactive({
  if (input$param == "") {
    return()
  }

  stack <- FALSE  # stack <- input$dynamic_trace_stack
  chain <- 0      # input$dynamic_trace_chain

  do.call(".param_trace_dynamic", args = list(
    param_samps = if (input$multiview_warmup) 
      par_samps_all() else par_samps_post_warmup(),
    chain = chain,
    stack = stack)
  )
})

output$dynamic_trace_plot_multiview_out <- dygraphs::renderDygraph(
  dynamic_trace_plot_multiview()
)
