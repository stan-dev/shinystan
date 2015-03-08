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


pkgs <- c("shiny", "shinyBS", "ggplot2", "gtools", "plyr", "reshape2", "dygraphs", "xts", "threejs", "xtable")
invisible(lapply(X = pkgs, FUN = library, character.only = TRUE))

# load the helper functions
source("helper_functions/shinyStan_helpers.R", local = TRUE)

# load pp_check plot_names and plot_descriptions
source("pp_check/plot_names_descriptions.R", local = TRUE)

# give shinystan_object shorter name
object <- shinystan_object
show_model_name <- h4(style = "padding: 0px 0px 10px 10px; color: #428bca;", paste("Model name:", object@model_name))

# Begin shinyUI -----------------------------------------------------------
# _________________________________________________________________________


navbarPage(title = strong(style = "color: #f9dd67;", "shinyStan"),
           windowTitle = "shinyStan", collapsible = FALSE, id = "nav",
           inverse = TRUE, header = show_model_name,
           
           #### TAB: Estimates ####
           tabPanel(title = "Estimate", icon = icon("stats", lib = "glyphicon"),
                    withMathJax(),
                    
                    tabsetPanel(
                      #### multiparameter plot ####
                      tabPanel("Parameters plot", icon = icon("bar-chart-o", "fa-2x"),
                               wellPanel(
                                 fluidRow(
                                   uiOutput("ui_multiparam_selectize"),
                                   column(3, offset = 1, sliderInput("param_plot_ci_level", h5("Credible interval"), ticks = FALSE, min = 50, max = 95, value = 50, step = 5, post = "%"))
                                 ),
                                 tags$div(class = "pull-right",style = "line-height: 150%;", checkboxInput("multiparam_options", label = span(style = "font-size: 12px;","Show/hide customization panel"), value = TRUE))
                               ),
                               conditionalPanel(condition = "input.multiparam_options == true",
                                                uiOutput("ui_multiparam_customize")),
                               plotOutput("plot_param_vertical_out", width = "90%")
                      ),
                      #### Posterior summary statistics ####
                      tabPanel("Posterior summary statistics", icon = icon("table", "fa-2x"),                                
                               fluidRow(
                                 column(3,
                                        br(),
                                        bsCollapse(
                                          bsCollapsePanel(title = "View Table Options", id = "stats_table_options_collapse",
                                                          #                                                             bsButton("btn_open_glossary", "Open glossary", style = "link"),
                                                          actionLink("btn_open_glossary", "Open glossary", icon = icon("book", lib = "glyphicon")),
                                                          uiOutput("glossary_modal"),
                                                          numericInput("stats_digits", label = h5(style = "color: black;", "Decimal places"), value = 1, min = 0, max = 7, step = 1),
                                                          checkboxGroupInput("stats_columns", label = h5(style = "color: black;", "Columns"),
                                                                             choices = c("Rhat", "Effective sample size (n_eff)" = "n_eff", "Posterior mean" = "mean", "Posterior standard deviation" = "sd", "Monte Carlo uncertainty (se_mean)" = "se_mean", "Quantile: 2.5%" = "2.5%", "Quantile: 25%" = "25%", "Quantile: 50%" = "50%", "Quantile: 75%" = "75%", "Quantile: 97.5%" = "97.5%"),
                                                                             selected = c("Rhat", "n_eff", "mean", "sd", "2.5%", "50%", "97.5%")),
                                                          
                                                          downloadButton("download_all_summary", "Save"),
                                                          actionButton("tex_options", "LaTeX", icon = icon("print", lib = "glyphicon")),
                                                          bsModal("tex", title = "Print LaTeX table", trigger = "tex_options",
                                                                  helpText("The LaTeX table will print in the R console and can be pasted into a .tex file"),
                                                                  selectizeInput("tex_params", width = "100%", label = h5("Select or enter parameter names"), choices = .make_param_list_with_groups(object), multiple = TRUE,
                                                                                 options = list(placeholder = "Leave blank for all parameters")),
                                                                  checkboxInput("tex_booktabs", "Booktabs", value = TRUE),
                                                                  checkboxInput("tex_long", "Longtable", value = FALSE),
                                                                  hr(),
                                                                  actionButton("tex_go", "Print LaTeX")
                                                          )
                                          )
                                        )
                                 ),
                                 column(9, br(), dataTableOutput("all_summary_out")
                                 )
                               )
                      )
                    ) # End tabsetPanel
           ), # End Estimates
           
           #### TAB: Convergence ####
           tabPanel(title = "Diagnose", icon = icon("medkit"),
                    tabsetPanel(
                      #### sampler parameters ####
                      tabPanel("HMC/NUTS", icon = icon("table", "fa-2x"),
                               #                                  bsButton("btn_open_glossary_nuts", "Open glossary", style = "link", size = "large"),
                               actionLink("btn_open_glossary_nuts", "Open glossary", icon = icon("book", lib = "glyphicon")),
                               uiOutput("glossary_modal_nuts"),
                               h3("Summary of sampler parameters"),
                               fluidRow(
                                 column(3, radioButtons("sampler_warmup", label = h5("Warmup period"),
                                                        choices = list(Include = "include", Omit = "omit"),
                                                        inline = TRUE
                                 )),
                                 column(5, radioButtons("sampler_report", label = h5("Report average, maximum, or minimum values"),
                                                        choices = list(Average = "average", Maximum = "maximum", Minimum = "minimum"),
                                                        inline = TRUE
                                 )),
                                 column(2, numericInput("sampler_digits", label = h5("Decimals"), value = 4, min = 0, max = 10, step = 1))
                               ),
                               dataTableOutput("sampler_summary")
                      ),
                      #### Rhat, ESS, MCSE, diagnostics ####
                      tabPanel("\\((\\hat{R}, n_{eff}, \\text{se}_{mean}) \\text{ diagnostics} \\)", icon = icon("bar-chart-o", "fa-2x"),
                               fluidRow(
                                 column(6, 
                                        actionLink("btn_open_glossary_copy", "Open glossary", icon = icon("book", lib = "glyphicon")),
                                        uiOutput("glossary_modal_copy")
                                 ),
                                 column(6, tags$div(class = "pull-right",style = "line-height: 150%;", checkboxInput("warnings_options", label = span(style = "font-size: 12px;","Show/hide customization panel"), value = TRUE)))
                               ),
                               fluidRow(
                                 column(4, h4("\\(n_{eff} / N\\)", align = "center")),
                                 column(4, h4("\\(\\text{se}_{mean} / sd\\)", align = "center")),
                                 column(4, h4("\\(\\hat{R}\\)", align = "center"))
                               ),
                               fluidRow(
                                 column(4, plotOutput("n_eff_plot_out", height = "250px")),
                                 column(4, plotOutput("mcse_over_sd_plot_out", height = "250px")),
                                 column(4, plotOutput("rhat_plot_out", height = "250px"))
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
                                 column(4, div(style = "color: #428bca;", textOutput("n_eff_warnings"))),
                                 column(4, div(style = "color: #428bca;", textOutput("mcse_over_sd_warnings"))),
                                 column(4, div(style = "color: #428bca;", textOutput("rhat_warnings")))
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
                                 selectizeInput("ac_params", width = "50%", label = h5("Select or enter parameter names"), choices = .make_param_list_with_groups(object), multiple = TRUE),
                                 tags$div(class = "pull-right",style = "line-height: 150%;", checkboxInput("ac_options", label = span(style = "font-size: 12px;","Show/hide customization panel"), value = TRUE))
                               ),
                               plotOutput("autocorr_plot_out")
                      ),
                      #### multiparameter trace plots ####
                      tabPanel("Trace", icon = icon("bar-chart-o", "fa-2x"),
                               wellPanel(
                                 fluidRow(
                                   column(6,selectizeInput("multi_trace_params", width = '100%', label = h5("Select or enter parameter names"), choices = .make_param_list_with_groups(object), multiple = TRUE)),
                                   column(6, sliderInput("multi_xzoom", width = "100%",label = h5("Control range of iterations"), min = 0, max = object@nIter, step = 1, value = c(object@nWarmup + 1, object@nIter)))
                                 ),
                                 tags$div(class = "pull-right",style = "line-height: 150%;", checkboxInput("multi_trace_options", label = span(style = "font-size: 12px;","Show/hide customization panel"), value = TRUE))
                               ),
                                 conditionalPanel(condition = "input.multi_trace_options == true",
                                                  uiOutput("ui_multi_trace_customize")),
                                 plotOutput("multi_trace_plot_out"),
                                 br()
                        ),
                      #### PPcheck ####
                      tabPanel(title = "PPcheck", icon = icon("bar-chart-o", "fa-2x"),
                               h1("Graphical posterior predictive checks"),
                               navlistPanel(id = "pp_navlist", widths = c(4,8), well = FALSE,  
                                            "Data",
                                            tabPanel("Select data",
                                                     uiOutput("ui_pp_data")
                                            ),
                                            "Plots",
                                            tabPanel("Distribution of observed data vs replications",
                                                     uiOutput("ui_pp_hists_rep_vs_obs")
                                            ),
                                            tabPanel("Distributions of test statistics",
                                                     uiOutput("ui_pp_hists_test_statistics")
                                            ),
                                            tabPanel("Scatters",
                                                     uiOutput("ui_pp_scatters")
                                            ),
                                            tabPanel("Histogram of residuals",
                                                     uiOutput("ui_pp_hist_resids")
                                            ),
                                            "About",
                                            tabPanel("About graphical posterior predictive checking",
                                                     uiOutput("ui_pp_about")
                                            ),
                                            tabPanel("Tutorial",
                                                     includeMarkdown("pp_check/pp_check_tutorial.md")
                                            )
                                            
                               )
                      )
                      ) # End tabsetPanel
             ), # End Convergence
             
             #### TAB: Explore Parameters ####
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
                                            bsCollapse(
                                              bsCollapsePanel(title = "View Options", id = "multiview_collapse",
                                                              checkboxInput("multiview_warmup", label = strong(style = "color: white;", "Include warmup"), value = FALSE),
                                                              hr(),
                                                              downloadButton("download_multiview", "Save as ggplot2 objects")
                                              )
                                            ),
                                            fluidRow(
                                              column(6, h5("Density")),
                                              column(6, h5("Autocorrelation"))
                                            ),
                                            fluidRow(
                                              column(6, plotOutput("multiview_density", height = "150")),
                                              column(6, plotOutput("multiview_autocorr", height = "150"))
                                            ),
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
                                                             plotOutput("density_plot_out")
                                                             ),
                                            conditionalPanel(condition = "input.distribution == 'Histogram'",
                                                             uiOutput("ui_hist_customize"),
                                                             plotOutput("hist_plot_out")
                                                             )
                                            
                                   ),
                                   #### trivariate plot #####
                                   tabPanel("Dynamic 3D scatterplot", # icon = icon("spinner", "fa-spin"),
                                            uiOutput("ui_triviariate_customize"),
                                            fluidRow(
                                              column(3, selectizeInput("trivariate_param_x", label = h5(style = "color: #428bca;", "x-axis"), choices = .make_param_list(object), multiple = FALSE)),
                                              column(3, selectizeInput("trivariate_param_y", label = h5(style = "color: #428bca;", "y-axis"), choices = .make_param_list(object), multiple = FALSE)),
                                              column(3, selectizeInput("trivariate_param_z", label = h5(style = "color: #428bca;", "z-axis"), choices = rev(.make_param_list(object)), multiple = FALSE))
                                            ),
                                            br(),
                                            threejs::scatterplotThreeOutput("trivariate_plot_out"),
                                            br()
                                   ),
                                   #### bivariate plot #####
                                   tabPanel("Bivariate",
                                            uiOutput("ui_bivariate_customize"),
                                            selectizeInput("bivariate_param_y", label = h5(style = "color: #428bca;", "y-axis"), choices = rev(.make_param_list(object)), multiple = FALSE),
                                            plotOutput("bivariate_plot_out")
                                   )
                                   
                      )
           ),
           #### MENU: More ####
           navbarMenu(title = "More",
                      
                      #### TAB: Model Code ####
                      tabPanel(title = "Model Code", # h4(style = "padding: 0px;","Model Code"),
                               h4("Model Code"),
                               tags$textarea(id="model_code", object@model_code)
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
                               tags$textarea(id="user_model_info", rows=20, cols=60, object@user_model_info),
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
                               h4("shinyStan"),
                               htmlOutput("ui_credits"),
                               br(),
                               actionLink("citation_modal", "Citing shinyStan"),
                               uiOutput("ui_cite"),
                               br(),
                               h4("Stan & RStan"),
                               a("Stan Development Team", href="http://mc-stan.org/team.html"),
                               hr(),
                               uiOutput("ui_about")
                      ), # END TAB: About
                      #### TAB: Help ####
                      tabPanel(title = "Help",
                               br(),
                               a(style = "color: maroon; font-size: 15px;", "Click here to report a bug, request a new feature, or ask us a question.", href = "https://github.com/stan-dev/shinystan/issues"),
                               br(),br(),
                               h3("shinyStan help"),
                               p("More coming soon."),
                               uiOutput("help_navlist"), # output navlist with help files
                               br(),
                               hr(),
                               h3("Stan help"),
                               a("Stan website", href = "http://mc-stan.org"),
                               br(),
                               a("Stan users google group", href = "https://groups.google.com/forum/#!forum/stan-users")
                      ) # END Help
           ), # END navbarMenu
           #### QUIT ####
           tabPanel(tags$div(style = "color: #f9dd67;", "Quit"), value = "quit",
                    h1("Thanks for using shinyStan."),
                    br(),br(),
                    h5("It's safe to close this browser window.")
           ),
           
           #### includeCSS ####
           includeCSS("shinyStan.css"),
           tags$style(type="text/css", "#autocorr_plot_out.recalculating, #plot_param_vertical_out.recalculating { opacity: 1.0; }"),
           tags$head(tags$style(".table .alignRight {text-align:right;}"))
) # END navbarPage

