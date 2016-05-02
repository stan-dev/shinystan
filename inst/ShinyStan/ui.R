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

source_value <- function(file) source(file, local = TRUE)$value
source("global_utils.R", local = TRUE)
if (exists("object")) 
  rm(object)
gc()


# Begin shinyUI -----------------------------------------------------------
# _________________________________________________________________________
tagList(
  tags$noscript(style = "color: orange; font-size: 30px; text-align: center;",
                "Please enable JavaScript to use ShinyStan."), 
  shinyjs::useShinyjs(),
  includeCSS("css/ShinyStan.css"),
  
  navbarPage(
    save_and_close,
    id = "nav",
    windowTitle = "ShinyStan", # title = NULL,
    collapsible = TRUE,
    inverse = FALSE,
    position = "fixed-top",
    theme = shinythemes::shinytheme("flatly"),
    
    
    #### HOME PAGE ####
    tabPanel(
      title = strong(style = "color: #B2011D;", "ShinyStan"),
      value = "home",
      logo_and_name(),
      div(class = "home-links",
          div(id = "model-name",
              br(),
              h2(paste("Model:")),
              h4(.model_name))),
      br(), br(),br(),br(),
      includeHTML("html/home_page_links.html")
    ),
    
    #### PAGE: DIAGNOSE ####
    tabPanel(
      title = "Diagnose",
      icon = icon("medkit"),
      tabsetPanel(
        id = "diagnose_tabset",
        #### hmc/nuts plots ####
        tabPanel(
          title = "NUTS (plots)",
          source_value(file.path("ui_files", "diagnostics_customize.R")),
          navlistPanel(
            id = "diagnostics_navlist",
            tabPanel("By model parameter", source_value(
              file.path("ui_files", "diagnostics_by_parameter.R")
            )),
            tabPanel("Sample information", source_value(
              file.path("ui_files", "diagnostics_sample.R")
            )),
            tabPanel("Treedepth information", source_value(
              file.path("ui_files", "diagnostics_treedepth.R")
            )),
            tabPanel("Divergence information", source_value(
              file.path("ui_files", "diagnostics_ndivergent.R")
            )),
            tabPanel("Step size information", source_value(
              file.path("ui_files", "diagnostics_stepsize.R")
            )),
            tabPanel("Help", source_value(
              file.path("ui_files", "diagnostics_help.R")
            )),
            well = FALSE,
            widths = c(2, 10)
          )
        ),
        #### hmc/nuts stats ####
        tabPanel(
          title = "HMC/NUTS (stats)",
          h2("Summary of sampler parameters"),
          a_glossary("open_glossary_from_nuts_table"),
          br(),
          source_value(file.path("ui_files", "sampler_stats_customize.R")),
          DT::dataTableOutput("sampler_summary"),
          br()
        ),
        #### rhat, n_eff, mcse ####
        tabPanel(
          title = "\\(\\hat{R}, n_{eff}, \\text{se}_{mean}\\)",
          source_value(file.path("ui_files", "rhat_neff_mcse_layout.R"))
        ),
        #### autocorrelation ####
        tabPanel(
          title = "Autocorrelation",
          source_value(file.path("ui_files", "autocorr_customize.R")),
          wellPanel(fluidRow(
            column(
              8,
              selectizeInput(
                "ac_params",
                width = "100%",
                label = h5("Select or enter parameter names"),
                choices = .param_list_with_groups,
                multiple = TRUE
              )
            ),
            column(3, offset = 1, a_options("autocorr"))
          )),
          plotOutput("autocorr_plot_out")
        ),
        #### ppcheck ####
        tabPanel(
          title = "PPcheck",
          h2("Graphical posterior predictive checks"),
          h6("Experimental feature"),
          source_value(file.path("ui_files", if (.has_rstanarm_ppcs)
            "pp_navlist_rstanarm.R" else "pp_navlist.R")),
          br()
        )
      ) # End tabsetPanel
    ), # End DIAGNOSE
    
    #### PAGE: ESTIMATE ####
    tabPanel(
      title = "Estimate",
      icon = icon("stats", lib = "glyphicon"),
      withMathJax(),
      
      tabsetPanel(
        id = "estimate_tabset", 
        #### multiparameter plot ####
        tabPanel(
          title = "Parameters plot",
          wellPanel(
            fluidRow(
              column(6, uiOutput("ui_multiparam_selectize")),
              column(
                3,
                offset = 1,
                sliderInput(
                  "param_plot_ci_level",
                  h5("Credible interval"),
                  width = "75%",
                  ticks = FALSE,
                  min = 50,
                  max = 95,
                  value = 50,
                  step = 5,
                  post = "%"
                )
              ),
              column(2, a_options("multiparam"))
            ),
            fluidRow(column(
              3,
              offset = 1,
              span(id = "params_to_plot_regex_label",
                   "Add parameters by regex search")
            )),
            fluidRow(
              column(1, actionButton("param_plot_regex", label = "Search", class = "regex-go")
              ),
              column(3, textInput("params_to_plot_regex", label = NULL, value = "")
              ),
              column(5, textOutput("invalid_regex"))
            )
          ),
          source_value(file.path("ui_files", "multiparam_customize.R")),
          plotOutput("multiparam_plot_out", width = "90%"),
          br()
        ),
        #### posterior summary statistics ####
        tabPanel(
          "Posterior summary statistics",
          source_value(file.path("ui_files", "table_customize.R")),
          div(DT::dataTableOutput("all_summary_out"), 
              style = "overflow-x: auto")
        ),
        #### LaTex tables ####
        tabPanel(
          "Generate LaTeX table",
          br(),
          sidebarLayout(
            mainPanel = source_value(file.path("ui_files", "table_latex_main.R")),
            sidebarPanel = source_value(file.path("ui_files", "table_latex_sidebar.R"))
          )
        )
      ) # End tabsetPanel
    ), # End ESTIMATE
    
    #### PAGE: EXPLORE ####
    tabPanel(
      title = "Explore",
      icon = icon("eye-open", lib = "glyphicon"),
      fluidRow(
        column(
          3,
          selectizeInput(
            inputId = "param",
            label = h4("Select parameter"),
            choices = .param_list,
            selected = .param_list[1],
            multiple = FALSE
          )
        ),
        column(7, offset = 1, DT::dataTableOutput("parameter_summary_out"))
      ),
      navlistPanel(
        well = FALSE,
        widths = c(3, 9),
        #### multiview ####
        tabPanel(
          title = "Multiview",
          icon = icon("th-large", lib = "glyphicon"),
          checkboxInput(
            "multiview_warmup",
            label = strong("Include warmup"),
            value = FALSE
          ),
          splitLayout(h5("Kernel Density Estimate"), h5("Autocorrelation")),
          splitLayout(
            plotOutput("multiview_density_out", height = "150"),
            plotOutput("multiview_autocorr_out", height = "150"),
            cellArgs = list(class = "plot_hover_shadow")
          ),
          h5("Trace"),
          dygraphs::dygraphOutput("multiview_trace_out", height = "200px"),
          source_value(file.path("ui_files", "dynamic_trace_helptext.R"))
        ),
        #### bivariate #####
        tabPanel(
          title = "Bivariate",
          selectizeInput(
            "bivariate_param_y",
            label = strong(style = "color: #006DCC;", "y-axis"),
            choices = rev(.param_list),
            selected = rev(.param_list)[1],
            multiple = FALSE
          ),
          a_options("bivariate"),
          source_value(file.path("ui_files", "bivariate_customize.R")),
          plotOutput("bivariate_plot_out", height = "350px"),
          helpText(
            style = "font-size: 11px",
            "For Stan models using the NUTS algorithm, red points indicate iterations that encountered a divergent transition.",
            "Yellow points indicate a transition that hit the maximum treedepth",
            "rather than terminated its evolution normally."
          ),
          hr(),
          downloadButton("download_bivariate", "ggplot2",  class = "plot-download"),
          downloadButton('save_pdf_bivariate', "pdf", class = "plot-download pdf-download")
        ),
        #### trivariate #####
        tabPanel(
          title = "Trivariate",
          source_value(file.path("ui_files", "trivariate_select.R")),
          a_options("trivariate"),
          source_value(file.path("ui_files", "trivariate_customize.R")),
          br(),
          threejs::scatterplotThreeOutput("trivariate_plot_out", height = "400px"),
          helpText(
            style = "font-size: 12px;",
            "Use your mouse and trackpad to rotate the plot and zoom in or out."
          )
        ),
        #### density #####
        tabPanel(
          title = "Density",
          a_options("density"),
          source_value(file.path("ui_files", "density_customize.R")),
          plotOutput("density_plot_out", height = "250px"),
          hr(),
          downloadButton("download_density", "ggplot2",  class = "plot-download"),
          downloadButton('save_pdf_density', "pdf", class = "plot-download pdf-download")
        ),
        #### histogram #####
        tabPanel(
          title = "Histogram",
          a_options("hist"),
          source_value(file.path("ui_files", "hist_customize.R")),
          plotOutput("hist_plot_out", height = "250px"),
          hr(),
          downloadButton("download_histogram", "ggplot2",  class = "plot-download"),
          downloadButton('save_pdf_histogram', "pdf", class = "plot-download pdf-download")
        )
      ) # End navlist
    ), # End EXPLORE
             
    #### MENU: More ####
    navbarMenu(
      title = "More",
      #### model code ####
      tabPanel(
        title = "Model Code",
        source_value(file.path("ui_files", "model_code.R"))
      ),
      #### notepad ####
      tabPanel(
        title = "Notepad",
        source_value(file.path("ui_files", "notepad.R"))
      ),
      #### about ####
      tabPanel(
        title = "About",
        logo_and_name(),
        div(style = "margin-top: 75px;",
            source_value(file.path("ui_files", "about.R")))
      ),
      #### glossary ####
      tabPanel(
        title = "Glossary",
        div(
          style = "background-color: white;",
          h1(style = "text-align: center;", "Glossary"),
          source_value(file.path("ui_files", "glossary.R")),
          hr(),
          stan_manual()
        )
      ),
      #### help ####
      tabPanel(
        title = "Help",
        h1(style = "text-align: center;", "Help"),
        source_value(file.path("ui_files", "help.R"))
      )
    ) # End navbarMenu
  ) # End navbarPage
) # End tagList

# End shinyUI -------------------------------------------------------------
# -------------------------------------------------------------------------
