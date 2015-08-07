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

output$ui_multiparam_customize <- renderUI({
  my_show_ci_line <- TRUE
  my_color_by_rhat <- FALSE
  my_show_density <- FALSE
  my_point_est <- "Median"
  my_fill_color <- "#590815"
  my_outline_color <- "#487575"
  my_est_color <- "#B2001D"
  my_rhat_palette <- "Blues"
  
  absolutePanel(id = "controls_multiparam", 
                class = "draggable_controls",
                fixed = TRUE,
                top = 190, right = 20, width = 200,
                draggable = TRUE,
                shinyjs::hidden(
                  div(id = "multiparam_options",
                      wellPanel(
                        class = "optionswell",
                        strongBig("Parameter estimates"),
                        hr(class = "hroptions"),
                        selectInput("multiparam_options_display", label = strongBig("Control"),
                                    choices = c("Options", "Aesthetics", "Sorting"),
                                    selected = "Options", width = "100%"),
                        conditionalPanel(condition = "input.multiparam_options_display == 'Options'",
                                         checkboxInput("param_plot_show_density", label = "Kernal density estimates", value = my_show_density),
                                         checkboxInput("param_plot_show_ci_line", label = "95% interval line", value = my_show_ci_line),
                                         radioButtons("param_plot_point_est", label = "Point estimate", choices = c("Median", "Mean"), selected = my_point_est, inline = TRUE),
                                         downloadButton("download_multiparam_plot", "Save ggplot2 object")
                        ),
                        conditionalPanel(condition = "input.multiparam_options_display == 'Aesthetics'",
                                         withMathJax(),
                                         checkboxInput("param_plot_color_by_rhat", label = "Color point est. by \\(\\hat{R}\\)", value = my_color_by_rhat),
                                         shinyjs::colourInput("param_plot_fill_color", span(style = "font-size: 12px", "Density/CI color"), my_fill_color),
                                         shinyjs::colourInput("param_plot_outline_color", span(style = "font-size: 12px", "Outline color"), my_outline_color),
                                         conditionalPanel(condition = "input.param_plot_color_by_rhat == false",
                                                          shinyjs::colourInput("param_plot_est_color", span(style = "font-size: 12px", "Point estimate color"), my_est_color)),
                                         conditionalPanel(condition = "input.param_plot_color_by_rhat == true",
                                                          selectInput("param_plot_rhat_palette", span(style = "font-size: 12px", "Rhat palette"), choices = c("Blues", "Grays", "Greens", "Oranges", "Purples", "Reds"), selected = my_rhat_palette, selectize=TRUE))
                        ),
                        conditionalPanel(condition = "input.multiparam_options_display == 'Sorting'",
                                         radioButtons("param_plot_sort_j", label = "Sort parameters in select list by", choices = c(Row = TRUE, Column = FALSE), selected = TRUE, inline = TRUE),
                                         helpText(style = "font-size: 12px;","If applicable, sort with x[1,2] before x[2,1] or vice-versa")
                        ),
                        br()
                      )
                  )
                )
  )
})
