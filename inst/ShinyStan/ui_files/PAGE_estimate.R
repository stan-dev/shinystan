tabsetPanel(
  id = "estimate_tabset",
  
  #### multiparameter plot ####
  tabPanel(
    title = "Parameters plot",
    wellPanel(
      fluidRow(
        column(width = 6,
               uiOutput("ui_multiparam_selectize")),
        column(
          width = 3,
          offset = 1,
          sliderInput(
            "param_plot_ci_level",
            h5("Posterior interval"),
            width = "75%",
            ticks = FALSE,
            min = 50,
            max = 95,
            value = 50,
            step = 5,
            post = "%"
          )
        ),
        column(width = 2,
               a_options("multiparam"))
      ),
      fluidRow(column(
        width = 3,
        offset = 1,
        span(id = "params_to_plot_regex_label",
             "Add parameters by regex search")
      )),
      fluidRow(
        column(
          width = 1,
          actionButton("param_plot_regex", label = "Search", class = "regex-go")
        ),
        column(
          width = 3,
          textInput("params_to_plot_regex", label = NULL, value = "")
        ),
        column(width = 5,
               textOutput("invalid_regex"))
      )
    ),
    source_ui("multiparam_customize.R"),
    plotOutput("multiparam_plot_out", width = "90%"),
    br()
  ),
  
  #### posterior summary statistics ####
  tabPanel(
    "Posterior summary statistics",
    source_ui("table_customize.R"),
    div(DT::dataTableOutput("all_summary_out"),
        style = "overflow-x: auto")
  ),
  
  #### LaTex tables ####
  tabPanel(
    "Generate LaTeX table",
    br(),
    sidebarLayout(
      mainPanel = source_ui("table_latex_main.R"),
      sidebarPanel = source_ui("table_latex_sidebar.R")
    )
  )
)