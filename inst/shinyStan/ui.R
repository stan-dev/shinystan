# shinyStan
# Copyright (c) 2015 Jonah Gabry & shinyStan team
# All rights reserved.

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


pkgs <- c("shiny", "shinyBS", "ggplot2", "gtools", "plyr", "reshape2", "dygraphs", "xts", "threejs", "xtable", "gridExtra")
invisible(lapply(X = pkgs, FUN = library, character.only = TRUE))

# load the helper functions
source("helper_functions/shinyStan_helpers.R", local = TRUE)

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
                                 column(3, uiOutput("ui_summary_stats_customize")),
                                 column(9, dataTableOutput("all_summary_out"))
                               )
                      )
                    ) # End tabsetPanel
           ), # End ESTIMATE
           
           #### TAB: DIAGNOSE ####
           tabPanel(title = "Diagnose", icon = icon("medkit"),
                    tabsetPanel(
                      #### sampler parameters ####
                      tabPanel("HMC/NUTS", icon = icon("table", "fa-2x"),
                               actionLink("btn_open_glossary_nuts", "Open glossary", icon = icon("book", lib = "glyphicon")),
                               uiOutput("glossary_modal_nuts"),
                               h3("Summary of sampler parameters"),
                               uiOutput("ui_sampler_stats_customize"),
                               dataTableOutput("sampler_summary"),
                               hr(),
                               fluidRow(
                                 column(4, strong("n_divergent (post-warmup)")),
                                 column(8, strong("treedepth (post-warmup)"))
                               ),
                               fluidRow(
                                 column(4, plotOutput("sampler_plot_divergent_out", height = "150px")),
                                 column(8, 
                                        fluidRow(
                                          column(4, plotOutput("sampler_plot_treedepth_out", height = "150px")),  
                                          column(4, plotOutput("sampler_plot_treedepth0_out", height = "150px")),
                                          column(4, plotOutput("sampler_plot_treedepth1_out", height = "150px"))
                                        )
                                 )
                               ),
                               br()
                      ),
                      #### Rhat, ESS, MCSE, diagnostics ####
                      tabPanel("\\((\\hat{R}, n_{eff}, \\text{se}_{mean}) \\text{ diagnostics} \\)", icon = icon("bar-chart-o", "fa-2x"),
                               fluidRow(
                                 column(2, 
                                        actionLink("btn_open_glossary_copy", "Open glossary", icon = icon("book", lib = "glyphicon"))
                                 )
                               ),
                               fluidRow(
                                 column(3, splitLayout(includeHTML("html/warnings_options.html"), span("Customize"), cellWidths = c("25%","75%")))
                               ),
                               uiOutput("glossary_modal_copy"),
                               splitLayout(h4("\\(n_{eff} / N\\)", align = "center"),
                                           h4("\\(\\text{se}_{mean} / sd\\)", align = "center"),
                                           h4("\\(\\hat{R}\\)", align = "center")),
                               splitLayout(
                                 plotOutput("n_eff_plot_out", height = "250px"),
                                 plotOutput("mcse_over_sd_plot_out", height = "250px"),
                                 plotOutput("rhat_plot_out", height = "250px")
                               ),
                               hr(),
                               fluidRow(
                                 column(4, strong(textOutput("n_eff_warnings_title"))),
                                 column(4, strong(textOutput("mcse_over_sd_warnings_title"))),
                                 column(4, strong(textOutput("rhat_warnings_title")))
                               ),
                               tags$style(type="text/css", "#n_eff_warnings_title, #rhat_warnings_title, #mcse_over_sd_warnings_title {font-size: 13px;}"),
                               br(),
                               fluidRow(
                                 column(4, div(style = "color: #337ab7;", textOutput("n_eff_warnings"))),
                                 column(4, div(style = "color: #337ab7;", textOutput("mcse_over_sd_warnings"))),
                                 column(4, div(style = "color: #337ab7;", textOutput("rhat_warnings")))
                               ),
                               tags$style(type="text/css", "#n_eff_warnings, #rhat_warnings, #mcse_over_sd_warnings {font-size: 12px;}"),
                               conditionalPanel(condition = "input.warnings_options == true",
                                                uiOutput("ui_warnings_customize"))
                      ),
                      #### autocorrelation plot ####
                      tabPanel("Autocorrelation", icon = icon("bar-chart-o", "fa-2x"),
                               conditionalPanel(condition = "input.ac_options == true",
                                                uiOutput("ui_autocorr_customize")
                               ),
                               wellPanel(
                                 fluidRow(
                                   column(6, selectizeInput("ac_params", width = "100%", label = h5("Select or enter parameter names"), choices = .make_param_list_with_groups(object), multiple = TRUE)),
                                   column(2, offset = 4, tags$div(h5("Customize"),includeHTML("html/ac_options.html")))
                                 )
                               ),
                               plotOutput("autocorr_plot_out")
                      ),
                      #### multiparameter trace plots ####
                      tabPanel("Trace", icon = icon("bar-chart-o", "fa-2x"),
                               wellPanel(
                                 fluidRow(
                                   column(6, selectizeInput("multi_trace_params", width = '100%', label = h5("Select or enter parameter names"), choices = .make_param_list_with_groups(object), multiple = TRUE)),
                                   column(3, offset = 1, sliderInput("multi_xzoom", width = "75%",label = h5("Iterations range"), min = 1, max = object@nIter, step = 1, value = c(object@nWarmup + 1, object@nIter), ticks = FALSE)),
                                   column(2, tags$div(h5("Customize"),includeHTML("html/multi_trace_options.html")))
                                 )
                               ),
                               conditionalPanel(condition = "input.multi_trace_options == true",
                                                uiOutput("ui_multi_trace_customize")),
                               plotOutput("multi_trace_plot_out"),
                               br()
                      ),
                      #### PPcheck ####
                      tabPanel(title = "PPcheck", icon = icon("bar-chart-o", "fa-2x"),
                               h2("Graphical posterior predictive checks"),
                               uiOutput("ui_ppcheck_navlist")
                      ) # End PPCHECK
                      
                    ) # End tabsetPanel
           ), # End DIAGNOSE
           
           #### TAB: EXPLORE ####
           tabPanel(title = "Explore", icon = icon("eye-open", lib = "glyphicon"),
                    fluidRow(
                      column(3, selectizeInput(inputId = "param", label = h4("Select parameter"), choices = .make_param_list(object), multiple = FALSE)),
                      # summary stats
                      column(7, offset = 1, dataTableOutput("parameter_summary_out"))
                    ),
                    hr(),
                    navlistPanel(well = FALSE,
                                 #### multiview ####
                                 tabPanel("Multiview", icon = icon("th-large", lib = "glyphicon"),
                                          uiOutput("ui_multiview_customize"),
                                          splitLayout(h5("Density"), h5("Autocorrelation")),
                                          splitLayout(plotOutput("multiview_density", height = "150"), 
                                                      plotOutput("multiview_autocorr", height = "150")),
                                          h5("Trace: iterations vs. sampled values"),
                                          plotOutput("multiview_trace", height = "150")
                                 ),
                                 #### dynamic trace plot ####
                                 tabPanel("Dynamic trace", # icon = icon("line-chart"),
                                          uiOutput("ui_dynamic_trace_customize"),
                                          dygraphs::dygraphOutput("dynamic_trace_plot_out"),
                                          br(), br()
                                 ),
                                 #### density/histogram ####
                                 tabPanel("Density & histogram", # icon = icon("area-chart"),
                                          radioButtons("distribution", label = "", choices = c("Density", "Histogram"), inline = TRUE),
                                          conditionalPanel(condition = "input.distribution == 'Density'",
                                                           uiOutput("ui_density_customize"),
                                                           textInput("dens_transform_x", "Transform", value = "x"),
                                                           plotOutput("density_plot_out")
                                          ),
                                          conditionalPanel(condition = "input.distribution == 'Histogram'",
                                                           uiOutput("ui_hist_customize"),
                                                           textInput("hist_transform_x", "Transform", value = "x"),
                                                           plotOutput("hist_plot_out")
                                          )
                                          
                                 ),
                                 #### trivariate plot #####
                                 tabPanel("Dynamic 3D scatterplot", 
                                          uiOutput("ui_triviariate_customize"),
                                          fluidRow(
                                            column(3, selectizeInput("trivariate_param_x", label = strong(style = "color: #337ab7;", "x-axis"), choices = .make_param_list(object), multiple = FALSE)),
                                            column(3, selectizeInput("trivariate_param_y", label = strong(style = "color: #337ab7;", "y-axis"), choices = .make_param_list(object), multiple = FALSE)),
                                            column(3, selectizeInput("trivariate_param_z", label = strong(style = "color: #337ab7;", "z-axis"), choices = rev(.make_param_list(object)), multiple = FALSE))
                                          ),
                                          fluidRow(
                                            column(3, textInput("trivariate_transform_x", label = "Transform x", value = "x")),
                                            column(3, textInput("trivariate_transform_y", label = "Transform y", value = "y")),
                                            column(3, textInput("trivariate_transform_z", label = "Transform z", value = "z"))
                                          ),
                                          br(),
                                          threejs::scatterplotThreeOutput("trivariate_plot_out"),
                                          br()
                                 ),
                                 #### bivariate plot #####
                                 tabPanel("Bivariate",
                                          uiOutput("ui_bivariate_customize"),
                                          fluidRow(
                                            column(4, selectizeInput("bivariate_param_y", label = strong(style = "color: #337ab7;", "y-axis"), choices = rev(.make_param_list(object)), multiple = FALSE)),
                                            column(3, offset = 2, textInput("bivariate_transform_y", label = "Transform y", value = "y")),
                                            column(3, textInput("bivariate_transform_x", label = "Transform x", value = "x"))
                                          ),
                                          tags$style(type='text/css', "#bivariate_transform_y_go, #bivariate_transform_x_go { margin-top: 24px; margin-left: -25px; }"),
                                          plotOutput("bivariate_plot_out")
                                 )
                                 
                    ) # End navlist
           ), # End EXPLORE
           #### MENU: More ####
           navbarMenu(title = "More",
                      
                      #### TAB: Model Code ####
                      tabPanel(title = "Model Code", # h4(style = "padding: 0px;","Model Code"),
                               h4("Model Code"),
                               tags$textarea(id="model_code", style="background: transparent; border-width: .5px;", object@model_code)
                      ), # END TAB: Model Code
                      #### TAB: Notes ####
                      tabPanel(title = "Notes",
                               
                               helpText(strong("Use this space to store notes about your model")),
                               helpText("The text will be saved in the", code("user_model_info"),
                                        "slot of your", code("shinystan"), "object and displayed here
                                    each time you launch the app for this model.",
                                        bsButton("btn_user_model_info_why", label = "Read more about the 'Notes' tab", style = "link", size = "mini")
                               ),
                               h4("Notes"),
                               tags$textarea(id="user_model_info", style="background: transparent; border-width: .5px; border-color: #222222", rows=20, cols=60, object@user_model_info),
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
                      #### TAB: Settings ####
                      tabPanel("Settings",
                               selectInput("background_texture", "Background texture", choices = c("Plain (white)" = "default", "Subtle" = "subtle",  "Concrete" = "concrete", "White brick" = "whitebrick", "Vignette" = "vignette", "Sweater" = "sweater", "Stucco" = "stucco", "Crumpled paper" = "crumpled", "Green cup" = "greencup"), selected = "default"),
                               uiOutput("ui_background_texture")
                      )
           ), # END navbarMenu MORE
           #### QUIT ####
           tabPanel(tags$div(style = "color: #f9dd67;", "Quit"), value = "quit",
                    h1("Thanks for using shinyStan."),
                    br(),br(),
                    h5("It's safe to close this browser window.")
           ),
           
           #### includeCSS ####
           includeCSS("shinyStan.css")
           
           
) # END navbarPage

