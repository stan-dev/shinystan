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

pkgs <- c("shiny", "shinyBS", "ggplot2", "gtools", "plyr", "reshape2", "dygraphs", 
          "xts", "xtable", "gridExtra", "markdown", "DT", "threejs")
invisible(lapply(pkgs, FUN = require, character.only = TRUE))

# load the helper functions
source("helper_functions/shinyStan_helpers.R", local = TRUE)
source("helper_functions/utils.R", local = TRUE)
source("helper_functions/summary_stats.R", local = TRUE)

# load pp_check plot_names and plot_descriptions
source("server_files/pp_check/plot_names_descriptions.R", local = TRUE)

# give shinystan_object shorter name
object <- shinystan_object
show_model_name <- h4(style = "padding: 0px 0px 10px 10px; color: #337ab7; opacity: 0.95; ", paste("Model name:", object@model_name))

# Begin shinyUI -----------------------------------------------------------
# _________________________________________________________________________


navbarPage(title = strong(style = "color: #f9dd67; ", "shinyStan"),
           windowTitle = "shinyStan", collapsible = FALSE, id = "nav",
           inverse = TRUE, header = show_model_name, position = "fixed-top",
           
           #### TAB: ESTIMATE ####
           tabPanel(title = "Estimate", icon = icon("stats", lib = "glyphicon"),
                    withMathJax(),
                    
                    tabsetPanel(
                      #### multiparameter plot ####
                      tabPanel("Parameters plot", icon = icon("bar-chart-o", "fa-2x"),
                               uiOutput("ui_multiparam_selectize"),
                               conditionalPanel(condition = "input.multiparam_options == true",
                                                uiOutput("ui_multiparam_customize")),
                               plotOutput("plot_param_vertical_out", width = "90%")
                      ),
                      #### Posterior summary statistics ####
                      tabPanel("Posterior summary statistics", icon = icon("table", "fa-2x"),
                               br(),
                               fluidRow(
                                 column(3, 
                                        bsCollapse(
                                          bsCollapsePanel(title = "View Table Options", id = "stats_table_options_collapse",
                                                          actionLink("btn_open_glossary", "Open glossary", icon = icon("book", lib = "glyphicon")),
                                                          uiOutput("glossary_modal"),
                                                          br(),
                                                          numericInput("stats_digits", label = strong(style = "color: black;", "Digits"), value = 1, min = 0, max = 7, step = 1),
                                                          checkboxInput("user_regex",strong(style = "color: black;","Regex searching"), value = TRUE),
                                                          checkboxGroupInput("stats_columns", label = strong(style = "color: black;", "Columns"),
                                                                             choices = c("Rhat", "Effective sample size (n_eff)" = "n_eff", "Posterior mean" = "mean", "Posterior standard deviation" = "sd", "Monte Carlo uncertainty (se_mean)" = "se_mean", "Quantile: 2.5%" = "2.5%", "Quantile: 25%" = "25%", "Quantile: 50%" = "50%", "Quantile: 75%" = "75%", "Quantile: 97.5%" = "97.5%"),
                                                                             selected = c("Rhat", "n_eff", "mean", "sd", "2.5%", "50%", "97.5%")),
                                                          
                                                          downloadButton("download_all_summary", "Save"),
                                                          actionButton("tex_options", withMathJax("\\(\\LaTeX\\)"), icon = icon("print", lib = "glyphicon")),
                                                          uiOutput("ui_tex_modal")
                                          )
                                        )
                                 ),
                                 column(9, DT::dataTableOutput("all_summary_out"))
                               )
                      )
                    ) # End tabsetPanel
           ), # End ESTIMATE
           
           #### TAB: DIAGNOSE ####
           tabPanel(title = "Diagnose", icon = icon("medkit"),
                    tabsetPanel(
                      ### sampler parameters ####
                      tabPanel("HMC/NUTS (stats)",
                               actionLink("btn_open_nuts_glossary", "Open glossary", icon = icon("book", lib = "glyphicon")),
                               uiOutput("nuts_glossary_modal"),
                               h2("Summary of sampler parameters"),
                               uiOutput("ui_sampler_stats_customize"),
                               DT::dataTableOutput("sampler_summary"),
                               br()
                      ),
                      tabPanel("HMC/NUTS (plots)",
                               wellPanel(
                                 fluidRow(
                                   column(3, h4(textOutput("diagnostic_chain_text"))),
                                   column(4, conditionalPanel(condition = "input.diagnostics_navlist == 'By model parameter'", 
                                                              h5("Parameter"))),
                                   column(4, conditionalPanel(condition = "input.diagnostics_navlist == 'By model parameter'", 
                                                              h5("Transformation f(x) =")))
                                 ),
                                 fluidRow(
                                   column(3, div(style = "width: 100px;", numericInput("diagnostic_chain", label = NULL, value = 0, min = 0, max = object@nChains))),
                                   column(4, conditionalPanel(condition = "input.diagnostics_navlist == 'By model parameter'", 
                                                              selectizeInput(inputId = "diagnostic_param", 
                                                                             label = NULL, 
                                                                             choices = .make_param_list(object), 
                                                                             selected = .make_param_list(object)[1], 
                                                                             multiple = FALSE))),
                                   column(3, conditionalPanel(condition = "input.diagnostics_navlist == 'By model parameter'", 
                                                              textInput("diagnostic_param_transform", 
                                                                        label = NULL, 
                                                                        value = "x"))),
                                   column(2, conditionalPanel(condition = "input.diagnostics_navlist == 'By model parameter'", 
                                                              actionButton("diagnostic_param_transform_go", "Transform")
                                   )
                                   )
                                 )
                               ),
                               navlistPanel(id = "diagnostics_navlist",
                                            tabPanel("Sample information",
                                                     h2("Sample information"),
                                                     uiOutput("ui_diagnostics_sample")
                                            ),
                                            tabPanel("N divergent information",
                                                     h2("N divergent information"),
                                                     uiOutput("ui_diagnostics_ndivergent")
                                            ),
                                            tabPanel("Tree depth information",
                                                     h2("Tree depth information"),
                                                     uiOutput("ui_diagnostics_treedepth")
                                            ),
                                            tabPanel("Step size information",
                                                     h2("Step size information"),
                                                     uiOutput("ui_diagnostics_stepsize")
                                            ),
                                            tabPanel("By model parameter",
                                                     uiOutput("ui_diagnostics_parameter")
                                            ),
#                                             tabPanel("Help",
#                                                      uiOutput("ui_diagnostics_help")
#                                             ),
                                            well = FALSE,
                                            widths = c(2, 10)
                               )
                      ),
                      #### Rhat, ESS, MCSE, diagnostics ####
                      tabPanel("\\(\\hat{R}, n_{eff}, \\text{se}_{mean}\\)", # icon = icon("bar-chart-o", "fa-2x"),
                               fluidRow(
                                 column(2, actionLink("btn_open_glossary_copy", "Open glossary", 
                                                      icon = icon("book", lib = "glyphicon"))
                                 )
                               ),
                               fluidRow(
                                 column(3, splitLayout(includeHTML("html/warnings_options.html"), 
                                                       span("Customize"), cellWidths = c("25%","75%")))
                               ),
                               uiOutput("glossary_modal_copy"),
                               uiOutput("ui_rhat_neff_mcse"),
                               hr(),
                               uiOutput("ui_rhat_neff_mcse_warnings"),
                               conditionalPanel(condition = "input.warnings_options == true",
                                                uiOutput("ui_warnings_customize"))
                      ),
                      #### autocorrelation plot ####
                      tabPanel("Autocorrelation", # icon = icon("bar-chart-o", "fa-2x"),
                               conditionalPanel(condition = "input.ac_options == true",
                                                uiOutput("ui_autocorr_customize")
                               ),
                               wellPanel(
                                 fluidRow(
                                   column(6, selectizeInput("ac_params", width = "100%", label = h5("Select or enter parameter names"), 
                                                            choices = .make_param_list_with_groups(object), multiple = TRUE)),
                                   column(2, offset = 4, tags$div(h5("Customize"),includeHTML("html/ac_options.html")))
                                 )
                               ),
                               plotOutput("autocorr_plot_out")
                      ),
                      #### multiparameter trace plots ####
                      tabPanel("Trace", # icon = icon("bar-chart-o", "fa-2x"),
                               wellPanel(
                                 fluidRow(
                                   column(6, selectizeInput("multi_trace_params", width = '100%', 
                                                            label = h5("Select or enter parameter names"), 
                                                            choices = .make_param_list_with_groups(object), multiple = TRUE)),
                                   column(3, offset = 1, sliderInput("multi_xzoom", width = "75%",
                                                                     label = h5("Iterations"), min = 1, max = object@nIter, 
                                                                     step = 1, value = c(object@nWarmup + 1, object@nIter), ticks = FALSE)),
                                   column(2, tags$div(h5("Customize"),includeHTML("html/multi_trace_options.html")))
                                 )
                               ),
                               conditionalPanel(condition = "input.multi_trace_options == true",
                                                uiOutput("ui_multi_trace_customize")),
                               plotOutput("multi_trace_plot_out"),
                               br()
                      ),
                      #### PPcheck ####
                      tabPanel(title = "PPcheck", # icon = icon("bar-chart-o", "fa-2x"),
                               h2("Graphical posterior predictive checks"),
                               h6("Experimental feature"),
                               uiOutput("ui_ppcheck_navlist")
                      ) # End PPCHECK
                      
                    ) # End tabsetPanel
           ), # End DIAGNOSE
           
           #### TAB: EXPLORE ####
           tabPanel(title = "Explore", icon = icon("eye-open", lib = "glyphicon"),
                    fluidRow(
                      column(3, selectizeInput(inputId = "param", label = h4("Select parameter"), choices = .make_param_list(object), selected = .make_param_list(object)[1], multiple = FALSE)),
                      column(7, offset = 1, DT::dataTableOutput("parameter_summary_out"))
                    ),
                    hr(),
                    navlistPanel(well = FALSE,
                                 #### multiview ####
                                 tabPanel("Multiview", icon = icon("th-large", lib = "glyphicon"),
                                          bsCollapse(
                                            bsCollapsePanel(title = "View Options", id = "multiview_collapse",
                                                            checkboxInput("multiview_warmup", label = strong(style = "color: white;", "Include warmup"), value = FALSE),
                                                            hr(),
                                                            downloadButton("download_multiview", "Save as ggplot2 objects")
                                            )
                                          ),
                                          splitLayout(h5("Density"), h5("Autocorrelation")),
                                          splitLayout(plotOutput("multiview_density", height = "150"), 
                                                      plotOutput("multiview_autocorr", height = "150"),
                                                      cellArgs = list(class = "plot_hover_shadow")
                                          ),
                                          h5("Trace: iterations vs. sampled values"),
                                          plotOutput("multiview_trace", height = "150")
                                 ),
                                 #### dynamic trace plot ####
                                 tabPanel("Dynamic trace", # icon = icon("line-chart"),
                                          bsCollapse(
                                            bsCollapsePanel(title = "View Options", id = "dynamic_trace_collapse",
                                                            fluidRow(
                                                              column(3, numericInput("dynamic_trace_chain", 
                                                                                     label = strong("Chain (0 = all)"), 
                                                                                     min = 0, max = object@nChains, step = 1, value = 0)),
                                                              column(4, radioButtons("dynamic_trace_stack", 
                                                                                     label = strong("Lines"), 
                                                                                     choices = list(Normal = "normal", Stacked = "stacked"), 
                                                                                     selected = "normal", inline = TRUE)),
                                                              column(3, radioButtons("dynamic_trace_grid", 
                                                                                     label = strong("Grid"), 
                                                                                     choices = list(Show = "show", Hide = "hide"), 
                                                                                     selected = "hide", inline = TRUE))
                                                            ),
                                                            hr(),
                                                            uiOutput("ui_dynamic_trace_helptext")
                                            )
                                          ),
                                          dygraphs::dygraphOutput("dynamic_trace_plot_out"),
                                          br(), br()
                                 ),
                                 #### density/histogram ####
                                 tabPanel("Density & histogram", # icon = icon("area-chart"),
                                          radioButtons("distribution", label = "", choices = c("Density", "Histogram"), inline = TRUE),
                                          conditionalPanel(condition = "input.distribution == 'Density'",
                                                           uiOutput("ui_density_customize"),
                                                           fluidRow(
                                                             column(4, textInput("dens_transform_x", strong(style = "font-size: 11px;","Transform"), value = "x")),
                                                             column(2, actionButton("dens_transform_x_go", label = strong(style = "font-size: 11px;","Transform")))
                                                           ),
                                                           plotOutput("density_plot_out", height = "250px")
                                          ),
                                          conditionalPanel(condition = "input.distribution == 'Histogram'",
                                                           uiOutput("ui_hist_customize"),
                                                           fluidRow(
                                                             column(4, textInput("hist_transform_x", strong(style = "font-size: 11px;","Transform"), value = "x")),
                                                             column(2, actionButton("hist_transform_x_go", label = strong(style = "font-size: 11px;","Transform")))
                                                           ),
                                                           plotOutput("hist_plot_out", height = "250px")
                                          ),
                                          br()
                                 ),
                                 #### trivariate plot #####
                                 tabPanel("Trivariate", 
                                          uiOutput("ui_triviariate_customize"),
                                          uiOutput("ui_trivariate_select"),
                                          withMathJax(),
                                          fluidRow(
                                            column(3, textInput("trivariate_transform_x", label = strong(style = "font-size: 11px;","Transform x"), value = "x")),
                                            column(3, textInput("trivariate_transform_y", label = strong(style = "font-size: 11px;", "Transform y"), value = "y")),
                                            column(3, textInput("trivariate_transform_z", label = strong(style = "font-size: 11px;", "Transform z"), value = "z")),
                                            column(2, actionButton("trivariate_transform_go", label = strong(style = "font-size: 11px;","Transform")))
                                          ),
                                          br(),
                                          threejs::scatterplotThreeOutput("trivariate_plot_out"),
                                          br()
                                 ),
                                 #### bivariate plot #####
                                 tabPanel("Bivariate",
                                          uiOutput("ui_bivariate_customize"),
                                          fluidRow(
                                            column(4, selectizeInput("bivariate_param_y", label = strong(style = "color: #337ab7;", "y-axis"), 
                                                                     choices = rev(.make_param_list(object)), 
                                                                     selected = rev(.make_param_list(object))[1], multiple = FALSE)),
                                            column(3, textInput("bivariate_transform_y", 
                                                                label = strong(style = "font-size: 11px;","Transform y"), value = "y")),
                                            column(3, textInput("bivariate_transform_x", 
                                                                label = strong(style = "font-size: 11px;","Transform x"), value = "x")),
                                            column(2, actionButton("bivariate_transform_go", 
                                                                   label = strong(style = "font-size: 11px;","Transform")))
                                          ),
                                          plotOutput("bivariate_plot_out", height = "350px"),
                                          helpText(style = "font-size: 11px", "For Stan models using the NUTS algorithm, red points indicate iterations that encountered a divergent transition.",  
                                                   "Yellow points indicate a transition that hit the maximum treedepth",
                                                   "rather than terminated its evolution normally."),
                                          br()
                                 )
                                 
                    ) # End navlist
           ), # End EXPLORE
           
           #### MENU: More ####
           navbarMenu(title = "More",
                      
                      #### TAB: Model Code ####
                      tabPanel(title = "Model Code", 
                               h4("Model Code"),
                               tags$textarea(id="model_code", 
                                             style="background: transparent; border-width: .5px;", 
                                             object@model_code)
                      ), # END TAB: Model Code
                      #### TAB: Notes ####
                      tabPanel(title = "Notepad",
                               helpText(strong("Use this space to store notes about your model")),
                               helpText("The text will be saved in the", code("user_model_info"),
                                        "slot of your", code("shinystan"), 
                                        "object and displayed here each time you launch the app for this model.",
                                        bsButton("btn_user_model_info_why", label = "Read more about the 'Notes' tab", 
                                                 style = "link", size = "mini")
                               ),
                               h4("Notes"),
                               tags$textarea(id="user_model_info", 
                                             style="background: transparent; border-width: .5px; border-color: #222222", 
                                             rows=20, cols=60, object@user_model_info),
                               br(),
                               fluidRow(
                                 column(3, actionButton("save_user_model_info", label = "Save notes", icon = icon("download"))),
                                 column(8, offset = 1, textOutput("user_text_saved")),
                                 tags$style(type = "text/css", "#user_text_saved {color: gray;}")
                               ),
                               hr(),
                               uiOutput("user_model_info_modal")
                      ),  # END TAB: Notes
                      #### TAB: About ####
                      tabPanel(title = "About",
                               uiOutput("ui_about")
                      ), # END TAB: About
                      #### TAB: Help ####
                      tabPanel(title = "Help",
                               uiOutput("ui_help")
                      ), # END Help
                      #### TAB: Appearance ####
                      tabPanel("Appearance",
                               h3("Appearance settings"),
                               br(),br(),
                               selectInput("background_texture", "Background texture", 
                                           choices = c("Plain (white)" = "default", "Subtle" = "subtle",  
                                                       "Concrete" = "concrete", "White brick" = "whitebrick", 
                                                       "Vignette" = "vignette", "Sweater" = "sweater", 
                                                       "Stucco" = "stucco", "Crumpled paper" = "crumpled", 
                                                       "Green cup" = "greencup"), selected = "default"),
                               br(),br(),
                               selectInput("body_font", "Font family", 
                                           choices = c(Default = "default", 
                                                       Arial = "Arial, Helvetica, sans-serif", 
                                                       Corbel = "'Corbel'", 
                                                       Georgia = "Georgia, serif",
                                                       "Palatino Linotype" = "'Palatino Linotype', 'Book Antiqua', Palatino, serif", 
                                                       Tahoma = "Tahoma, Geneva, sans-serif",
                                                       "Times New Roman" = "'Times New Roman', Times, serif", 
                                                       Trebuchet = "'Trebuchet MS', Helvetica, sans-serif",
                                                       Verdana = "Verdana, Geneva, sans-serif")),
                               uiOutput("ui_background_texture"),
                               uiOutput("ui_body_font")
                      )
           ), # END navbarMenu MORE
           
           #### QUIT ####
           tabPanel(strong(style = "color: #f9dd67;", "Quit"), value = "quit", icon = icon("close"),
                    h1("Thanks for using shinyStan."),
                    br(),br(),
                    h5("It's safe to close this browser window.")
           ),
           
           #### include css style sheets ####
           includeCSS("css/shinyStan.css"),
           includeCSS("css/shinyStan_datatables.css"),
           includeCSS("css/shinyStan_dygraphs.css")
           
) # END navbarPage

