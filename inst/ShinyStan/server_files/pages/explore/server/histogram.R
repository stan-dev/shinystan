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


# histogram ---------------------------------------------------------------
hist_transform_x <- eventReactive(
  input$hist_transform_x_go > 0,
  input$hist_transform_x
)

histogram_plot <- reactive({
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
    transform_x = hist_transform_x()
  ))
})

output$hist_plot_out <- renderPlot({
  x <- histogram_plot()
  suppress_and_print(x)
}, bg = "transparent")

# download plot
output$download_histogram <- downloadHandler(
  filename = 'shinystan-histogram-gg.RData',
  content = function(file) {
    shinystan_histogram_gg <- histogram_plot()
    save(shinystan_histogram_gg, file = file)
  }
)
output$save_pdf_histogram = downloadHandler(
  filename = "shinstan-histogram.pdf",
  content = function(file) {
    ggsave(file, plot = histogram_plot(), device = pdf)
})
