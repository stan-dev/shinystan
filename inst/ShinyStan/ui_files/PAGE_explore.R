tagList(
  fluidRow(
    column(
      width = 3,
      selectizeInput(
        inputId = "param",
        label = h4("Select parameter"),
        choices = .param_list,
        selected = .param_list[1],
        multiple = FALSE
      )
    ),
    column(
      width = 7, 
      offset = 1, 
      DT::dataTableOutput("parameter_summary_out")
    )
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
      source_ui("dynamic_trace_helptext.R")
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
      source_ui("bivariate_customize.R"),
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
      source_ui("trivariate_select.R"),
      a_options("trivariate"),
      source_ui("trivariate_customize.R"),
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
      source_ui("density_customize.R"),
      plotOutput("density_plot_out", height = "250px"),
      hr(),
      downloadButton("download_density", "ggplot2",  class = "plot-download"),
      downloadButton('save_pdf_density', "pdf", class = "plot-download pdf-download")
    ),
    
    #### histogram #####
    tabPanel(
      title = "Histogram",
      a_options("hist"),
      source_ui("hist_customize.R"),
      plotOutput("hist_plot_out", height = "250px"),
      hr(),
      downloadButton("download_histogram", "ggplot2",  class = "plot-download"),
      downloadButton('save_pdf_histogram', "pdf", class = "plot-download pdf-download")
    )
  )
)