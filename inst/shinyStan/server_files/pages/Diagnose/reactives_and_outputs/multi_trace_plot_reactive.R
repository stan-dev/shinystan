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


# multiparameter traceplots -----------------------------------------------
calc_height_trace_plot <- reactive({
  params <- input$multi_trace_params
  grid <- FALSE
  if (!is.null(input$multi_trace_layout)) {
    if (input$multi_trace_layout == "Grid") grid <- TRUE
  }
  params <- .update_params_with_groups(params, param_names)
  LL <- length(params)
  if (LL == 0) LL <- 4
  if (LL == 1) LL <- 2
  if (grid) {
    if (LL > 5) return(30*LL)
    if (LL < 5) return(60*LL)
  }
  round(100*LL)
})

# multi_trace_plot
multi_trace_plot <- reactive({
  validate(need(!is.null(input$multi_trace_rect), message = "Loading..."))
  x1 <- input$multi_xzoom[1]
  x2 <- input$multi_xzoom[2]
  dat <- samps_all[x1:x2,,,drop=FALSE]
  # zoom <- "On"
  do.call(".param_trace_multi", args = list(
    params      = input$multi_trace_params,
    all_param_names = param_names,
    dat         = dat,
    chain       = input$multi_trace_chain,
    warmup_val  = warmup_val,
    palette     = input$multi_trace_palette ,
    rect        = input$multi_trace_rect,
    rect_color  = "skyblue",
    rect_alpha  = input$multi_trace_rect_alpha,
    layout      = input$multi_trace_layout,
    x1          = x1,
    x2          = x2
  ))
})

output$multi_trace_plot_out <- renderPlot({
  x <- multi_trace_plot()
  suppressWarnings(print(x)) # this avoids warnings about removing rows when using tracezoom feature
}, height = calc_height_trace_plot, bg = "transparent")

# download the plot
output$download_multi_trace <- downloadHandler(
  filename = paste0('shinystan_multi_trace.RData'),
  content = function(file) {
    shinystan_multi_trace <- multi_trace_plot()
    save(shinystan_multi_trace, file = file)
  }
)
