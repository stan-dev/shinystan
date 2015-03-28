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



output$ui_multiparam_customize <- renderUI({

  #   my_show_ci_line <- "yes"
  #   my_color_by_rhat <- "no"
  #   my_show_density <- "no"
  my_show_ci_line <- TRUE
  my_color_by_rhat <- FALSE
  my_show_density <- FALSE
  my_point_est <- "Median"
  my_fill_color <- "gray35"
  my_outline_color <- "gray35"
  my_est_color <- "lightgray"
  my_rhat_palette <- "Oranges"

  #   if (input$user_param_plot_customize == TRUE) {
  #     ok <- exists("shinystan_settings_param_plot")
  #     validate(need(ok == TRUE, message = ""))
  #     user_param_plot <- shinystan_settings_param_plot
  #     my_point_est <- user_param_plot$point_est
  #     my_fill_color <- user_param_plot$fill_color
  #     my_outline_color <- user_param_plot$outline_color
  #     my_est_color <- user_param_plot$est_color
  #   }

  #   if (input$user_param_plot_customize == TRUE) {
  #     ok <- exists("shinystan_settings_param_plot")
  #     validate(need(ok == TRUE, message = "Sorry, can't find any user param_plot settings."))
  #     user_param_plot <- shinystan_settings_param_plot
  #     my_show_density <- user_param_plot$show_density
  #     my_show_ci_line <- user_param_plot$show_ci_line
  #     my_color_by_rhat <- user_param_plot$color_by_rhat
  #   }

  absolutePanel(id = "controls_multiparam", class = "panel panel-default hvr-glow", fixed = FALSE,
    top = 400, right = 20, width = 240,
    draggable = TRUE,
    div(class="shinystan_customize", "shinyStan customize"),
    wellPanel(style = "background-color: #222222; padding-top: 10px ; padding-bottom: 10px;",
              bsCollapse(# open = "multiparam_options_collapse",
                bsCollapsePanel(title = "Options", id = "multiparam_options_collapse",
                                checkboxInput("param_plot_show_density", label = "Kernal density estimates", value = my_show_density),
                                checkboxInput("param_plot_show_ci_line", label = "95% interval line", value = my_show_ci_line),
                                radioButtons("param_plot_point_est", label = "Point estimate", choices = c("Median", "Mean"), selected = my_point_est, inline = TRUE)
                ),
                bsCollapsePanel(title = "Aesthetics", id = "multiparam_colors_collapse",
                                withMathJax(),
                                checkboxInput("param_plot_color_by_rhat", label = "Color point est. by \\(\\hat{R}\\)", value = my_color_by_rhat),
                                selectInput("param_plot_fill_color", span(style = "font-size: 12px", "Density/CI color"), choices = colors(), selected = my_fill_color, selectize = TRUE),
                                selectInput("param_plot_outline_color", span(style = "font-size: 12px", "Outline color"), choices = colors(), selected = my_outline_color, selectize = TRUE),
                                conditionalPanel(condition = "input.param_plot_color_by_rhat == false",
                                                 selectInput("param_plot_est_color", span(style = "font-size: 12px", "Point estimate color"), choices = colors(), selected = my_est_color, selectize = TRUE)),
                                conditionalPanel(condition = "input.param_plot_color_by_rhat == true",
                                                 selectInput("param_plot_rhat_palette", span(style = "font-size: 12px", "Rhat palette"), choices = c("Blues", "Grays", "Greens", "Oranges", "Purples", "Reds"), selected = my_rhat_palette, selectize=TRUE))
                ),
                bsCollapsePanel(title = "Sorting", id = "multiparam_options_sorting",
                                # checkboxInput("param_plot_sort_j", label = "Sort j", value = FALSE),
                                radioButtons("param_plot_sort_j", label = "Sort parameters in select list by", choices = c(Row = TRUE, Column = FALSE), selected = TRUE, inline = TRUE),
                                span(style = "font-size: 12px;","If applicable, sort with x[1,2] before x[2,1] or vice-versa")
                )
              ),
              hr(class = "hr hr_controls"),
              downloadButton("download_multiparam_plot", "Save as ggplot2 object")
    )
  )

})
