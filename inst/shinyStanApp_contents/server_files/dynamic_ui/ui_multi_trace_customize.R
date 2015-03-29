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



output$ui_multi_trace_customize <- renderUI({
  my_palette <- "Default"
  my_rect <- "Warmup"
  my_rect_alpha <- 0.15
  my_layout <- "Long"
  
  #   if (input$user_multi_trace_customize == TRUE) {
  #     ok <- exists("shinystan_settings_multi_trace")
  #     validate(need(ok == TRUE, message = "Sorry, can't find any user multi_trace plot settings."))
  #     user_multi_trace <- shinystan_settings_multi_trace
  #     my_palette <- user_multi_trace$palette
  #     my_rect <- user_multi_trace$rect
  #     my_rect_alpha <- user_multi_trace$rect_alpha
  #     my_layout <- user_multi_trace$layout
  #   }
  
  
  absolutePanel(id = "controls_multi_trace", class = "panel panel-default hvr-glow", fixed = TRUE,
                top = 400, right = 20, width = 270,
                draggable = TRUE,
                div(class = "shinystan_customize", "shinyStan customize"),
                wellPanel(style = "background-color: #222222; padding-top: 10px ; padding-bottom: 10px;",
                          bsCollapse(
                            bsCollapsePanel(title = "Options", id = "multi_trace_options_collapse",
                                            numericInput("multi_trace_chain", label = "Chain (0 = all chains)", min = 0, max = object@nChains, step = 1, value = 0),
                                            #                       checkboxInput("multi_trace_warmup", label = "Include warmup", TRUE),
                                            radioButtons("multi_trace_layout", label = "Layout",
                                                         choices = c("Long", "Grid"), selected = my_layout, inline = TRUE)
                                            
                            ),
                            bsCollapsePanel(title = "Aesthetics", id = "multi_trace_colors_collapse",
                                            selectizeInput("multi_trace_palette", "Color palette", choices = c("Default", "Brewer (spectral)", "Rainbow", "Gray"), selected = my_palette),
                                            selectInput("multi_trace_rect", label = "Shading", choices = c("None", "Samples", "Warmup"), selected = my_rect, size = 2, selectize = FALSE),
                                            sliderInput("multi_trace_rect_alpha", "Shading opacity", value = my_rect_alpha, min = 0, max = 1, step = 0.01)
                                            
                            )
                          ),
                          hr(class = "hr hr_controls"),
                          downloadButton("download_multi_trace", "Save as ggplot2 object")
                )
  )
  
})
