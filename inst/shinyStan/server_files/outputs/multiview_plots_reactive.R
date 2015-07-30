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

autocorr_plot_multiview <- reactive({
  validate(need(input$param, message = FALSE),
           need(!is.null(input$multiview_warmup), message = "Loading..."))
  
  if (is.null(input$multiview_warmup)) warmup <- FALSE
  else warmup <- input$multiview_warmup

  lags <- min(25, round((nIter-warmup_val)/2))

  if (warmup == FALSE) {
    samps <- par_samps_post_warmup()
  } else {
    samps <- par_samps_all()
  }
  
  do.call(".autocorr_single_plot", args = list(
    samps = samps,
    lags = lags
  ))
})


density_plot_multiview <- reactive({
  
  validate(need(input$param, message = FALSE),
           need(!is.null(input$multiview_warmup), message = "Loading..."))
  
  if (input$multiview_warmup == FALSE) {
    samps <- par_samps_post_warmup()
  } else {
    samps <- par_samps_all()
  }
  
  do.call(".param_dens", args = list(
    param       = input$param,
    dat         = samps,
    chain       = 0,
    chain_split = FALSE,
    fill_color  = "gray20",
    line_color  = "lightgray",
    point_est   = "None",
    CI          = "None",
    #     y_breaks    = "None",
    x_breaks    = "Some",
    title       = FALSE
  ))
})
