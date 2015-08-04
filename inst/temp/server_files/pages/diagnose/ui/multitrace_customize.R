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


# 
# output$ui_multitrace_customize <- renderUI({
#   my_palette <- "Default"
#   my_rect <- "Warmup"
#   my_rect_alpha <- 0.15
#   my_layout <- "Long"
#   
#   absolutePanel(id = "controls_multitrace", 
#                 class = "draggable_controls",
#                 fixed = TRUE,
#                 # top = 175, right = 20, width = 270,
#                 top = 300, right = 20, width = 270,
#                 draggable = TRUE,
#                 shinyjs::hidden(
#                   div(id = "multitrace_options",
#                       wellPanel(
#                         class = "optionswell",
#                         strongBig("Parameter estimates"),
#                         hr(class = "hroptions"),
#                         selectInput("multitrace_options_display", label = strongBig("Control"),
#                                     choices = c("Options", "Aesthetics"),
#                                     selected = "Options", width = "100%"),
#                         conditionalPanel(condition = "input.multitrace_options_display == 'Options'",
#                                          numericInput("multitrace_chain", label = "Chain (0 = all chains)", min = 0, max = object@nChains, step = 1, value = 0),
#                                          radioButtons("multitrace_layout", label = "Layout",
#                                                       choices = c("Long", "Grid"), selected = my_layout, inline = TRUE),
#                                          downloadButton("download_multitrace", "Save as ggplot2 object")
#                         ),
#                         conditionalPanel(condition = "input.multitrace_options_display == 'Aesthetics'",
#                                          selectizeInput("multitrace_palette", "Color palette", choices = c("Default", "Brewer (spectral)", "Rainbow", "Gray"), selected = my_palette),
#                                          # selectInput("multitrace_rect", label = "Shading", choices = c("None", "Samples", "Warmup"), selected = my_rect, size = 2, selectize = FALSE),
#                                          radioButtons("multitrace_rect", label = "Shading", choices = c("None", "Samples", "Warmup"), selected = my_rect, inline = TRUE),
#                                          sliderInput("multitrace_rect_alpha", "Shading opacity", value = my_rect_alpha, min = 0, max = 1, step = 0.01)
#                         )
#                       )
#                   )
#                 )
#   )
# })
