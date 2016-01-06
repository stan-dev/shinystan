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

object <- get(".shinystan_temp_object", envir = shinystan:::.sso_env)
source("global_utils.R", local = TRUE)
rm(object)
gc()

# corner_link <- HTML(paste0('<a href=',
#                            shQuote(paste0("http://mc-stan.org",sep='')), 
#                            '>', 'Stan', '</a>'))

# Begin shinyUI -----------------------------------------------------------
# _________________________________________________________________________
tagList(
  tags$noscript(style = "color: orange; font-size: 30px; text-align: center;", 
                "Please enable JavaScript to use ShinyStan."),
  shinyjs::useShinyjs(),
  includeCSS("css/ShinyStan.css"),
  navbarPage(save_and_close, id = "nav", #title = NULL,
             windowTitle = "ShinyStan", collapsible = TRUE, 
             inverse = FALSE, position = "fixed-top",
             theme = shinythemes::shinytheme("flatly"),
             
             #### HOME PAGE ####
             tabPanel(title = strong(style = "color: #B2011D;", "ShinyStan"),
                      value = "home",
                      logo_and_name(),
                      div(class = "home-links",
                          div(id = "model-name",
                              br(),
                              h2(paste("Model:")),
                              h4(.model_name)
                          )
                      ),
                      br(),br(),br(),br(),
                      includeHTML("html/home_page_links.html")
             ),
             
             #### PAGE: DIAGNOSE ####
             tabPanel(title = "Diagnose", icon = icon("medkit"),
                      tabsetPanel(
                        #### hmc/nuts plots ####
                        tabPanel("NUTS (plots)",
                                 source(file.path("ui_files", "diagnostics_customize.R"), local = TRUE)$value,
                                 navlistPanel(id = "diagnostics_navlist",
                                              tabPanel("By model parameter", source(file.path("ui_files", "diagnostics_by_parameter.R"), local = TRUE)$value),
                                              tabPanel("Sample information", source(file.path("ui_files", "diagnostics_sample.R"), local = TRUE)$value),
                                              tabPanel("Treedepth information", source(file.path("ui_files", "diagnostics_treedepth.R"), local = TRUE)$value),
                                              tabPanel("N divergent information", source(file.path("ui_files", "diagnostics_ndivergent.R"), local = TRUE)$value),
                                              tabPanel("Step size information", source(file.path("ui_files", "diagnostics_stepsize.R"), local = TRUE)$value),
                                              tabPanel("Help", source(file.path("ui_files", "diagnostics_help.R"), local = TRUE)$value),
                                              well = FALSE,
                                              widths = c(2, 10)
                                 )
                        ),
                        #### hmc/nuts stats ####
                        tabPanel("HMC/NUTS (stats)",
                                 h2("Summary of sampler parameters"),
                                 a_glossary("open_glossary_from_nuts_table"),
                                 br(),
                                 source(file.path("ui_files", "sampler_stats_customize.R"), local = TRUE)$value,
                                 DT::dataTableOutput("sampler_summary"),
                                 br()
                        ),
                        #### rhat, n_eff, mcse ####
                        tabPanel("\\(\\hat{R}, n_{eff}, \\text{se}_{mean}\\)", 
                                 source(file.path("ui_files", "rhat_neff_mcse_layout.R"), local = TRUE)$value
                        ),
                        #### autocorrelation ####
                        tabPanel("Autocorrelation", 
                                 source(file.path("ui_files", "autocorr_customize.R"), local = TRUE)$value,
                                 wellPanel(
                                   fluidRow(
                                     column(8, selectizeInput("ac_params", width = "100%", label = h5("Select or enter parameter names"), 
                                                              choices = .param_list_with_groups, multiple = TRUE)),
                                     column(3, offset = 1, a_options("autocorr"))
                                   )
                                 ),
                                 plotOutput("autocorr_plot_out")
                        ),
                        #### ppcheck ####
                        tabPanel(title = "PPcheck", 
                                 h2("Graphical posterior predictive checks"),
                                 h6("Experimental feature"),
                                 source(file.path("ui_files", if (.from_rstanarm) "pp_navlist_rstanarm.R" else "pp_navlist.R"), local = TRUE)$value,
                                 br()
                        )
                      ) # End tabsetPanel
             ), # End DIAGNOSE
             
             #### PAGE: ESTIMATE ####
             tabPanel(title = "Estimate", icon = icon("stats", lib = "glyphicon"),
                      withMathJax(),
                      
                      tabsetPanel(
                        #### multiparameter plot ####
                        tabPanel("Parameters plot",
                                 wellPanel(
                                   fluidRow(
                                     column(6, uiOutput("ui_multiparam_selectize")),
                                     column(3, offset = 1, 
                                            sliderInput("param_plot_ci_level", h5("Credible interval"), 
                                                        width = "75%", ticks = FALSE, min = 50, max = 95, 
                                                        value = 50, step = 5, post = "%")),
                                     column(2, a_options("multiparam"))
                                   ),
                                   fluidRow(
                                     column(1, actionButton("param_plot_regex", label = "Search", class = "regex-go")),
                                     column(3, textInput("params_to_plot_regex", label = NULL, value = "Add parameters by regex search")),
                                     column(5, textOutput("invalid_regex"))
                                   )
                                 ),
                                 source(file.path("ui_files", "multiparam_customize.R"), local = TRUE)$value,
                                 plotOutput("multiparam_plot_out", width = "90%")
                        ),
                        #### posterior summary statistics ####
                        tabPanel("Posterior summary statistics",
                                 source(file.path("ui_files", "table_customize.R"), local = TRUE)$value,
                                 div(DT::dataTableOutput("all_summary_out"), 
                                     style = "overflow-x: auto")
                        ),
                        #### LaTex tables ####
                        tabPanel("Generate LaTeX table",
                                 br(),
                                 sidebarLayout(
                                   mainPanel = source(file.path("ui_files", "table_latex_main.R"), local = TRUE)$value,
                                   sidebarPanel = source(file.path("ui_files", "table_latex_sidebar.R"), local = TRUE)$value
                                 )
                        )
                      ) # End tabsetPanel
             ), # End ESTIMATE
             
             #### PAGE: EXPLORE ####
             tabPanel(title = "Explore", icon = icon("eye-open", lib = "glyphicon"),
                      fluidRow(
                        column(3, selectizeInput(inputId = "param", label = h4("Select parameter"), 
                                                 choices = .param_list, 
                                                 selected = .param_list[1], 
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
                                            source(file.path("ui_files", "dynamic_trace_helptext.R"), local = TRUE)$value
                                   ),
                                   #### bivariate #####
                                   tabPanel("Bivariate",
                                            selectizeInput("bivariate_param_y", label = strong(style = "color: #006DCC;", "y-axis"), 
                                                           choices = rev(.param_list), 
                                                           selected = rev(.param_list)[1], multiple = FALSE),
                                            a_options("bivariate"),
                                            source(file.path("ui_files", "bivariate_customize.R"), local = TRUE)$value,
                                            plotOutput("bivariate_plot_out", height = "350px"),
                                            helpText(style = "font-size: 11px", "For Stan models using the NUTS algorithm, red points indicate iterations that encountered a divergent transition.",  
                                                     "Yellow points indicate a transition that hit the maximum treedepth",
                                                     "rather than terminated its evolution normally."),
                                            hr(),
                                            downloadButton("download_bivariate", "ggplot2",  class = "plot-download"),
                                            downloadButton('save_pdf_bivariate', "pdf", class = "plot-download pdf-download")
                                   ),
                                   #### trivariate #####
                                   tabPanel("Trivariate", 
                                            source(file.path("ui_files", "trivariate_select.R"), local = TRUE)$value,
                                            a_options("trivariate"),
                                            source(file.path("ui_files", "trivariate_customize.R"), local = TRUE)$value,
                                            br(),
                                            threejs::scatterplotThreeOutput("trivariate_plot_out", height = "400px"),
                                            helpText(style = "font-size: 12px;", "Use your mouse and trackpad to rotate the plot and zoom in or out.")
                                   ),
                                   #### density #####
                                   tabPanel("Density",
                                            a_options("density"),
                                            source(file.path("ui_files", "density_customize.R"), local = TRUE)$value,
                                            plotOutput("density_plot_out", height = "250px"),
                                            hr(),
                                            downloadButton("download_density", "ggplot2",  class = "plot-download"),
                                            downloadButton('save_pdf_density', "pdf", class = "plot-download pdf-download")
                                   ),
                                   #### histogram #####
                                   tabPanel("Histogram", 
                                            a_options("hist"),
                                            source(file.path("ui_files", "hist_customize.R"), local = TRUE)$value,
                                            plotOutput("hist_plot_out", height = "250px"),
                                            hr(),
                                            downloadButton("download_histogram", "ggplot2",  class = "plot-download"),
                                            downloadButton('save_pdf_histogram', "pdf", class = "plot-download pdf-download")
                                   )
                      ) # End navlist
             ), # End EXPLORE
             
             #### MENU: More ####
             navbarMenu(title = "More",
                        
                        #### model code ####
                        tabPanel(title = "Model Code", 
                                 source(file.path("ui_files", "model_code.R"), local = TRUE)$value
                        ), 
                        #### notepad ####
                        tabPanel(title = "Notepad",
                                 source(file.path("ui_files", "notepad.R"), local = TRUE)$value
                        ),
                        #### about ####
                        tabPanel(title = "About", 
                                 logo_and_name(),
                                 div(style = "margin-top: 75px;",
                                     source(file.path("ui_files", "about.R"), local = TRUE)$value
                                 )
                        ),
                        #### glossary ####
                        tabPanel(title = "Glossary",
                                 div(style = "background-color: white;",
                                     h1(style = "text-align: center;", "Glossary"),
                                     source(file.path("ui_files", "glossary.R"), local = TRUE)$value,
                                     hr(),
                                     stan_manual()
                                 )
                        ),
                        #### help ####
                        tabPanel(title = "Help",
                                 h1(style = "text-align: center;", "Help"),
                                 source(file.path("ui_files", "help.R"), local = TRUE)$value
                        )
             ) # End navbarMenu
  ) # End navbarPage
) # End tagList
