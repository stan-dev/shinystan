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

multiview_samps <- reactive({
  validate(need(input$param, message = FALSE),
           need(!is.null(input$multiview_warmup), message = "Loading..."))
  if (!input$multiview_warmup) 
    par_samps_post_warmup()
  else 
    par_samps_all()
})

dynamic_trace_plot_multiview <- reactive({
  if (input$param == "") return()
  stack <- FALSE  
  chain <- 0      
  do.call(".param_trace_dynamic", args = list(
    param_samps = multiview_samps(),
    chain = chain,
    stack = stack)
  )
})
autocorr_plot_multiview <- reactive({
  lags <- min(25, round((nIter-warmup_val)/2))
  do.call(".autocorr_single_plot", args = list(
    samps = multiview_samps(),
    lags = lags
  ))
})
density_plot_multiview <- reactive({
  do.call(".param_dens", args = list(
    param       = input$param,
    dat         = multiview_samps(),
    chain       = 0,
    chain_split = FALSE,
    fill_color  = base_fill,
    line_color  = vline_base_clr,
    point_est   = "None",
    CI          = "None",
    x_breaks    = "Some",
    title       = FALSE
  ))
})

output$multiview_param_name <- renderUI(strong(style = "font-size: 250%; color: #f9dd67;", 
                                               input$param))
output$multiview_trace_out <- dygraphs::renderDygraph(dynamic_trace_plot_multiview())
output$multiview_density_out <- renderPlot(density_plot_multiview(), 
                                           bg = "transparent")
output$multiview_autocorr_out <- renderPlot(autocorr_plot_multiview(), 
                                            bg = "transparent")

# download multiview plot
# output$download_multiview <- downloadHandler(
#   filename = 'shinystan_multiview.RData',
#   content = function(file) {
#     param_name <- input$param
#     shinystan_multiview <- list()
#     shinystan_multiview[[paste0("trace_", param_name)]] <- trace_plot_multiview()
#     shinystan_multiview[[paste0("density", param_name)]] <- density_plot_multiview()
#     shinystan_multiview[[paste0("ac_", param_name)]] <- autocorr_plot_multiview()
#     save(shinystan_multiview, file = file)
#   }
# )
