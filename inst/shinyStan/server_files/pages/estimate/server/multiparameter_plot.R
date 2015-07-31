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


# multiparameter plot -----------------------------------------------------
calc_height_param_plot <- reactive({
  params <- input$params_to_plot
  params <- .update_params_with_groups(params, param_names)
  LL <- length(params)
  LL <- ifelse(LL < 8, 8, LL)
  if (!is.null(input$param_plot_color_by_rhat)){
    # delay until input is ready
    if (input$param_plot_color_by_rhat == TRUE) {
      LL <- LL + 1
    }
  }
  round(50*LL)
})

plot_param_vertical <- reactive({
  validate(need(input$param_plot_fill_color, message = "Loading..."))
  if (is.null(input$param_plot_ci_level)) {
    # delay until input is ready
    return()
  }
  
  customize <- !is.null(input$param_plot_show_density)
  do.call(".multiparam_plot", args = list(
    samps           = samps_post_warmup,
    params          = input$params_to_plot,
    all_param_names = param_names,
    CI.level        = input$param_plot_ci_level/100,
    rhat_values     = fit_summary[, "Rhat"],
    show_density    = ifelse(customize, input$param_plot_show_density, FALSE), # == "yes", FALSE),
    show_ci_line    = ifelse(customize, input$param_plot_show_ci_line, TRUE), # == "yes", TRUE),
    color_by_rhat   = ifelse(customize, input$param_plot_color_by_rhat, FALSE), # == "yes", FALSE),
    rhat_palette    = ifelse(customize, input$param_plot_rhat_palette, "Oranges"),
    point_est       = ifelse(customize, input$param_plot_point_est, "Median"),
    fill_color      = ifelse(customize, input$param_plot_fill_color, "gray35"),
    outline_color   = ifelse(customize, input$param_plot_outline_color, "black"),
    est_color       = ifelse(customize, input$param_plot_est_color, "black")
  ))
})


output$plot_param_vertical_out <- renderPlot({
  plot_param_vertical()
}, height = calc_height_param_plot, bg = "transparent")

# download the plot
output$download_multiparam_plot <- downloadHandler(
  filename = 'shinystan_param_plot.RData',
  content = function(file) {
    shinystan_param_plot <- plot_param_vertical()
    save(shinystan_param_plot, file = file)
  }
)
