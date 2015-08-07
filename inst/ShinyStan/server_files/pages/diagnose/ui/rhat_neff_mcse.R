# This file is part of shinystan
# Copyright (C) 2015 Jonah Gabry
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

output$ui_rhat_neff_mcse <- renderUI({
  div(
    withMathJax(),
    splitLayout(h4("\\(n_{eff} / N\\)", align = "center"),
                h4("\\(mcse / sd\\)", align = "center"),
                h4("\\(\\hat{R}\\)", align = "center")),
    splitLayout(
      plotOutput("n_eff_plot_out", height = "250px"),
      plotOutput("mcse_over_sd_plot_out", height = "250px"),
      plotOutput("rhat_plot_out", height = "250px"),
      cellArgs = list(class = "plot_hover_shadow")
    )
  )
})

output$ui_rhat_neff_mcse_warnings <- renderUI({
  div(
    fluidRow(
      column(4, strong(textOutput("n_eff_warnings_title"))),
      column(4, strong(textOutput("mcse_over_sd_warnings_title"))),
      column(4, strong(textOutput("rhat_warnings_title")))
    ),
    tags$style(type="text/css", "#n_eff_warnings_title, #rhat_warnings_title, #mcse_over_sd_warnings_title {font-size: 13px;}"),
    br(),
    fluidRow(
      column(4, div(style = "color: #006dcc;", textOutput("n_eff_warnings"))),
      column(4, div(style = "color: #006dcc;", textOutput("mcse_over_sd_warnings"))),
      column(4, div(style = "color: #006dcc;", textOutput("rhat_warnings")))
    ),
    tags$style(type="text/css", "#n_eff_warnings, #rhat_warnings, #mcse_over_sd_warnings {font-size: 12px;}")
  )
  
})

output$ui_warnings_customize <- renderUI({
  absolutePanel(id = "controls_warnings", 
                class = "draggable_controls",
                fixed = TRUE,
                top = 210, right = 20, width = 200,
                draggable = TRUE,
                shinyjs::hidden(
                  div(id = "rhat_warnings_options",
                      wellPanel(
                        class = "optionswell",
                        strongBig("Warnings"),
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
})
