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


# autocorrelation plot ----------------------------------------------------
calc_height_autocorr_plot <- reactive({
  params <- input$ac_params
  params <- .update_params_with_groups(params, param_names)
  LL <- length(params)
  LL <- ifelse(LL < 8, 8, LL)
  round(60*LL)
})

autocorr_plot <- reactive({
  validate(need(input$ac_lags, message = "Loading..."))
  samps <- if (!input$ac_warmup) samps_post_warmup else samps
  do.call(".autocorr_plot", args = list(
    samps           = samps,
    params          = input$ac_params,
    all_param_names = param_names,
    lags            = input$ac_lags,
    flip            = input$ac_flip,
    combine_chains  = input$ac_combine,
    partial         = input$ac_partial,
    nChains         = nChains
  ))
})

output$autocorr_plot_out <- renderPlot({
  autocorr_plot()
}, bg = "transparent")

# download the plot
output$download_autocorr <- downloadHandler(
  filename = paste0('shinystan_autocorr.RData'),
  content = function(file) {
    shinystan_autocorr <- autocorr_plot
    save(shinystan_autocorr, file = file)
  }
)
