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



output$ui_warnings_customize <- renderUI({
  absolutePanel(id = "controls_warnings", 
                class = "draggable_controls",
                fixed = TRUE,
                top = 175, right = 20, width = 270,
                draggable = TRUE,
                shinyjs::hidden(
                  div(id = "rhat_warnings_options",
                      wellPanel(
                        class = "optionswell",
                        strongBig(style = "align: right;", "Warnings"),
                        hr(class = "hroptions"),
                        withMathJax(),
                        sliderInput("n_eff_threshold", "\\(n_{eff} / N\\) warning threshold", 
                                    ticks = FALSE, value = 10, min = 0, max = 100, step = 5, post = "%"),
                        sliderInput("mcse_threshold", "\\(\\text{se}_{mean} / sd\\) warning threshold", 
                                    ticks = FALSE, value = 10, min = 0, max = 100, step = 5, post = "%"),
                        sliderInput("rhat_threshold", "\\(\\hat{R}\\) warning threshold", 
                                    ticks = FALSE, value = 1.1, min = 1, max = 1.2, step = 0.01)
                      )
                  )
                )
  )
  
#   absolutePanel(id = "controls_warnings", class = "panel panel-default hvr-glow", fixed = TRUE,
#                 top = 150, right = 20, width = 270,
#                 draggable = TRUE,
#                 div(class = "shinystan_customize", "shinyStan customize"),
#                 wellPanel(style = "background-color: #222222; padding-top: 10px ; padding-bottom: 0px;",
#                           bsCollapse(
#                             bsCollapsePanel(title = "Options",
#                                             withMathJax(),
#                                             sliderInput("n_eff_threshold", "\\(n_{eff} / N\\) warning threshold", ticks = FALSE, value = 10, min = 0, max = 100, step = 5, post = "%"),
#                                             sliderInput("mcse_threshold", "\\(\\text{se}_{mean} / sd\\) warning threshold", ticks = FALSE, value = 10, min = 0, max = 100, step = 5, post = "%"),
#                                             sliderInput("rhat_threshold", "\\(\\hat{R}\\) warning threshold", ticks = FALSE, value = 1.1, min = 1, max = 1.2, step = 0.01)
#                             )
#                           )
#                 )
#   )
})
