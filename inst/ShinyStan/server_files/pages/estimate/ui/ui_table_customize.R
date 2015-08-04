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

output$ui_table_customize <- renderUI({
  absolutePanel(id = "controls_table", 
                class = "draggable_controls",
                fixed = TRUE,
                top = 240, right = 20, width = 250,
                draggable = TRUE,
                shinyjs::hidden(
                  div(id = "table_options",
                      wellPanel(
                        class = "optionswell",
                        strongBig("Summary stats"),
                        hr(class = "hroptions"),
                        selectInput("table_options_display", label = strongBig("Control"),
                                    choices = c("Options", "LaTeX"),
                                    selected = "Options", width = "100%"),
                        conditionalPanel(condition = "input.table_options_display == 'Options'",
                                         numericInput("stats_digits", label = "Digits", 
                                                      value = 1, min = 0, max = 7, step = 1),
                                         checkboxInput("user_regex", "Regex searching", value = TRUE)
                        ),
                        conditionalPanel(condition = "input.table_options_display == 'LaTeX'",
                                           helpText(withMathJax("Print \\(\\LaTeX\\) table to R console")),
                                         helpText(style = "font-size 11px;", "(scroll for more options)"),
                                           selectizeInput("tex_params", width = "100%", 
                                                          label = "Parameters", multiple = TRUE,
                                                          choices = .make_param_list_with_groups(object), 
                                                          options = list(placeholder = "Default = All")),
                                           numericInput("tex_digits", label = "Digits", 
                                                        value = input$stats_digits, min = 0),
                                         div(style = "font-size: 11px; padding: 1px;",
                                         checkboxGroupInput("tex_columns", label = "Columns",
                                                            choices = c("Rhat", "Effective sample size (n_eff)" = "n_eff", "Posterior mean" = "mean", "Posterior standard deviation" = "sd", "Monte Carlo uncertainty (se_mean)" = "se_mean", "Quantile: 2.5%" = "2.5%", "Quantile: 25%" = "25%", "Quantile: 50%" = "50%", "Quantile: 75%" = "75%", "Quantile: 97.5%" = "97.5%"),
                                                            selected = c("Rhat", "n_eff", "mean", "sd", "2.5%", "50%", "97.5%"))
                                         ),
                                         textInput("tex_caption", label = "Caption"),
                                           checkboxGroupInput("tex_pkgs", "Packages",
                                                              choices = c("Booktabs", "Longtable"),
                                                              selected = "Booktabs"
                                           ),
                                           actionButton("tex_go", withMathJax("Print \\(\\LaTeX\\)"), 
                                                        icon = icon("print", lib = "glyphicon")),
                                           br(),br()
                        )
                      )
                  )
                )
  )
})

