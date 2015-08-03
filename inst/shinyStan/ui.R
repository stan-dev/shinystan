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

source("global_utils.R", local = TRUE)
helpers <- list.files("helper_functions", full.names = TRUE, recursive = TRUE)
for (h in helpers) source(h, local = TRUE)
# load pp_check plot_names and plot_descriptions
source("server_files/utilities/ppcheck_names_descriptions.R", local = TRUE)

# give shinystan_object shorter name
object <- shinystan_object

# Begin shinyUI -----------------------------------------------------------
# _________________________________________________________________________
tagList(
  shinyjs::useShinyjs(),
  includeCSS("css/ShinyStan.css"),
  includeCSS("css/ShinyStan_datatables.css"),
  includeCSS("css/ShinyStan_dygraphs.css"),
  navbarPage(title = NULL,
             windowTitle = "ShinyStan", collapsible = TRUE, id = "nav",
             inverse = FALSE, position = "fixed-top",
             theme = shinythemes::shinytheme("flatly"),
             
             tabPanel(title = strong(style = "color: #B2011D;", "ShinyStan"),
                      div(id = "logos",
                          div(id = "logo1", 
                              img(src = "stan_logo.png", id = "stan-logo", width = "20%")),
                          div(
                            id = "logo2",
                            img(src = "wide_ensemble.png", id = "wide-ensemble", width = "100%")
                          )
                      ),
                      div(class = "home-links",
                          div(id = "shinystan-title", "ShinyStan"),
                          div(id = "model-name", 
                              h4(paste("Model")),
                              h5(object@model_name)
                              ),
                          br(),
                          div(style = "text-decoration: underline;",
                            h3(toc_entry("Diagnose")),
                            h3(toc_entry("Estimate")),
                            h3(toc_entry("Explore"))
                          )
                          # div(toc_entry("Code"), toc_entry("Help"), toc_entry("About"), toc_entry("Quit"))
                      )
             ),
             
             #### PAGE: ESTIMATE ####
             tabPanel(title = "Estimate", icon = icon("stats", lib = "glyphicon"),
                      withMathJax(),
                      
                      tabsetPanel(
                        #### multiparameter plot ####
                        tabPanel("Parameters plot", icon = icon("bar-chart-o", "fa-2x"),
                                 wellPanel(
                                   fluidRow(
                                     column(6, uiOutput("ui_multiparam_selectize")),
                                     column(3, offset = 1, sliderInput("param_plot_ci_level", h5("Credible interval"), width = "75%", ticks = FALSE, min = 50, max = 95, value = 50, step = 5, post = "%")),
                                     column(2, a_options("multiparam"))
                                   )
                                 ),
                                 uiOutput("ui_multiparam_customize"),
                                 plotOutput("multiparam_plot_out", width = "90%")
                        ),
                        #### posterior summary statistics ####
                        tabPanel("Posterior summary statistics", icon = icon("table", "fa-2x"),
                                 br(),
                                 fluidRow(
                                   column(10, DT::dataTableOutput("all_summary_out")),
                                   column(2, 
                                          a_glossary("open_glossary_from_table"),
                                          a_options("table"),
                                          uiOutput("ui_table_customize")
                                          # uiOutput("ui_tex_modal")
                                   )
                                   )
                        )
                      ) # End tabsetPanel
             ), # End ESTIMATE
             
             #### PAGE: DIAGNOSE ####
             tabPanel(title = "Diagnose", icon = icon("medkit"),
                      tabsetPanel(
                        
                        #### hmc/nuts plots ####
                        tabPanel("HMC/NUTS (plots)",
                                 uiOutput("ui_diagnostics_customize"),
                                 navlistPanel(id = "diagnostics_navlist",
                                              tabPanel("By model parameter", uiOutput("ui_diagnostics_parameter")),
                                              tabPanel("Sample information", uiOutput("ui_diagnostics_sample")),
                                              tabPanel("Treedepth information", uiOutput("ui_diagnostics_treedepth")),
                                              tabPanel("N divergent information", uiOutput("ui_diagnostics_ndivergent")),
                                              tabPanel("Step size information", uiOutput("ui_diagnostics_stepsize")),
                                              # tabPanel("Help", uiOutput("ui_diagnostics_help")),
                                              well = FALSE,
                                              widths = c(2, 10)
                                 ) # End navlistPanel
                        ),
                        #### hmc/nuts stats ####
                        tabPanel("HMC/NUTS (stats)",
                                 fluidRow(column(3, offset = 9, a_glossary("open_glossary_from_nuts_table"))), 
                                 h2("Summary of sampler parameters"),
                                 uiOutput("ui_sampler_stats_customize"),
                                 DT::dataTableOutput("sampler_summary"),
                                 br()
                        ),
                        #### rhat, n_eff, mcse ####
                        tabPanel("\\(\\hat{R}, n_{eff}, \\text{se}_{mean}\\)", # icon = icon("bar-chart-o", "fa-2x"),
                                 fluidRow(
                                   column(9, uiOutput("ui_rhat_neff_mcse"),
                                          hr(),
                                          uiOutput("ui_rhat_neff_mcse_warnings")),
                                   column(3, 
                                          a_glossary("open_glossary_from_rhat"),
                                          a_options("rhat_warnings"),
                                          uiOutput("ui_warnings_customize"))
                                 )
                        ),
                        #### autocorrelation ####
                        tabPanel("Autocorrelation", 
                                 uiOutput("ui_autocorr_customize"),
                                 wellPanel(
                                   fluidRow(
                                     column(8, selectizeInput("ac_params", width = "100%", label = h5("Select or enter parameter names"), 
                                                              choices = .make_param_list_with_groups(object), multiple = TRUE)),
                                     column(3, offset = 1, a_options("autocorr"))
                                   )
                                 ),
                                 plotOutput("autocorr_plot_out")
                        ),
#                         #### multiparameter traceplot ####
#                         tabPanel("Trace", # icon = icon("bar-chart-o", "fa-2x"),
#                                  wellPanel(
#                                    fluidRow(
#                                      column(6, selectizeInput("multitrace_params", width = '100%', 
#                                                               label = h5("Select or enter parameter names"), 
#                                                               choices = .make_param_list_with_groups(object), multiple = TRUE)),
#                                      column(3, offset = 1, sliderInput("multi_xzoom", width = "75%",
#                                                                        label = h5("Iterations"), min = 1, max = object@nIter, 
#                                                                        step = 1, value = c(object@nWarmup + 1, object@nIter), ticks = FALSE)),
#                                      column(2, a_options("multitrace"))
#                                    )
#                                  ),
#                                  uiOutput("ui_multitrace_customize"),
#                                  plotOutput("multitrace_plot_out"),
#                                  br()
#                         ),
                        #### ppcheck ####
                        tabPanel(title = "PPcheck", # icon = icon("bar-chart-o", "fa-2x"),
                                 h2("Graphical posterior predictive checks"),
                                 h6("Experimental feature"),
                                 uiOutput("ui_ppcheck_navlist")
                        )
                        
                      ) # End tabsetPanel
             ), # End DIAGNOSE
             
             #### PAGE: EXPLORE ####
             tabPanel(title = "Explore", icon = icon("eye-open", lib = "glyphicon"),
                      fluidRow(
                        column(3, selectizeInput(inputId = "param", label = h4("Select parameter"), 
                                                 choices = .make_param_list(object), 
                                                 selected = .make_param_list(object)[1], 
                                                 multiple = FALSE)),
                        column(7, offset = 1, DT::dataTableOutput("parameter_summary_out"))
                      ),
                      navlistPanel(well = FALSE, widths = c(3, 9),
                                   #### multiview ####
                                   tabPanel("Multiview", icon = icon("th-large", lib = "glyphicon"),
                                            checkboxInput("multiview_warmup", label = strong("Include warmup"), value = FALSE),
                                            splitLayout(h5("Kernel Density Estimate"), h5("Autocorrelation")),
                                            splitLayout(plotOutput("multiview_density_out", height = "150"), 
                                                        plotOutput("multiview_autocorr_out", height = "150"),
                                                        cellArgs = list(class = "plot_hover_shadow")
                                            ),
                                            h5("Trace"),
                                            dygraphs::dygraphOutput("multiview_trace_out", height = "200px"),
                                            uiOutput("ui_dynamic_trace_helptext")
                                   ),
                                   #### bivariate #####
                                   tabPanel("Bivariate",
                                            selectizeInput("bivariate_param_y", label = strong(style = "color: #006DCC;", "y-axis"), 
                                                           choices = rev(.make_param_list(object)), 
                                                           selected = rev(.make_param_list(object))[1], multiple = FALSE),
                                            a_options("bivariate"),
                                            uiOutput("ui_bivariate_customize"),
                                            plotOutput("bivariate_plot_out", height = "350px"),
                                            helpText(style = "font-size: 11px", "For Stan models using the NUTS algorithm, red points indicate iterations that encountered a divergent transition.",  
                                                     "Yellow points indicate a transition that hit the maximum treedepth",
                                                     "rather than terminated its evolution normally."),
                                            hr(),
                                            downloadButton("download_bivariate", "Save as ggplot2 object")
                                   ),
                                   #### trivariate #####
                                   tabPanel("Trivariate", 
                                            uiOutput("ui_trivariate_select"),
                                            a_options("trivariate"),
                                            uiOutput("ui_triviariate_customize"),
                                            br(),
                                            threejs::scatterplotThreeOutput("trivariate_plot_out", height = "400px"),
                                            helpText(style = "font-size: 12px;", "Use your mouse and trackpad to rotate the plot and zoom in or out.")
                                   ),
                                   #### density #####
                                   tabPanel("Density",
                                            a_options("density"),
                                            uiOutput("ui_density_customize"),
                                            plotOutput("density_plot_out", height = "250px"),
                                            hr(),
                                            downloadButton("download_density", "Save as ggplot2 object")
                                   ),
                                   #### histogram #####
                                   tabPanel("Histogram", 
                                            a_options("hist"),
                                            uiOutput("ui_hist_customize"),
                                            plotOutput("hist_plot_out", height = "250px"),
                                            hr(),
                                            downloadButton("download_histogram", "Save as ggplot2 object")
                                   )
                                   
                      ) # End navlist
             ), # End EXPLORE
             
             #### MENU: More ####
             navbarMenu(title = "More",
                        
                        #### PAGE: Model Code ####
                        tabPanel(title = "Model Code",
                                 fluidRow(
                                   column(10, offset = 2,
                                          h4("Model Code"),
                                          tags$textarea(id="model_code", 
                                                        wrap = "off",
                                                        cols = 80,
                                                        readonly = TRUE,
                                                        object@model_code)
                                   )
                                 )
                        ), # End Model Code
                        
                        #### PAGE: Notepad ####
                        tabPanel(title = "Notepad",
                                 uiOutput("ui_notepad")
                        ), # End Notepad
                        
                        #### PAGE: About ####
                        tabPanel(title = "About", uiOutput("ui_about")), 
                        
                        #### PAGE: Help ####
                        tabPanel(title = "Help",
                                 br(),br(),
                                 div(class = "home-links",
                                     actionLink(class = "help-links-active", 
                                                inputId = "toggle_help_glossary", 
                                                label = h4("Toggle Help/Glossary"))
                                 ),
                                 uiOutput("ui_help"),
                                 uiOutput("ui_glossary")
                        )
                        
             ), # End navbarMenu
             
             #### QUIT ####
             tabPanel(strong(style = "color: #dadada;", "Quit"), 
                      value = "quit", icon = icon("close"),
                      h1("Thanks for using ShinyStan."),
                      br(),br(),
                      h5("It's safe to close this browser window.")
             )
  ) # End navbarPage
) # End tagList
