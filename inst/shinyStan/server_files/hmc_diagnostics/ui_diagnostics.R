help_interval <- helpText(style = "font-size: 11px;","Highlighted interval shows \\(\\bar{x} \\pm sd(x)\\)")
help_lines <- helpText(style = "font-size: 11px;","Lines are mean (solid) and median (dashed)")
help_max_td <- helpText(style = "font-size: 11px;", "Horizontal line indicates the max_treedepth setting")
help_points <- helpText(style = "font-size: 11px;", "Red indicates which (if any) iterations encountered a divergent transition.",
                        "Orange indicates a transition hitting the maximum treedepth.")  
help_dynamic <- helpText(style = "font-size: 11px;", "Use your mouse to highlight areas in the plot to zoom into. You can also use the sliders. Double-click to reset.")
output$diagnostics_warnings_text <- renderText({
  divs <- sum(ndivergent_pw()[,-1])
  hits <- sum(treedepth_pw()[,-1] == MISC$max_td)
  d <- divs > 0
  h <- hits > 0
  if (d && h) msg <- paste("Diverging error:", divs, "iterations.",
                           "Maximum treedepth reached:", hits, "iterations.")
  else if (d && !h) msg <- paste("Diverging error:", divs, "iterations.")
  else if (!d && h) msg <- paste("Maximum treedepth reached:", hits, "iterations.")
  else msg <- NULL
  msg
})

output$ui_diagnostics_customize <- renderUI({
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
                                                label = NULL, multiple = FALSE,
                                                choices = .make_param_list(object), 
                                                selected = .make_param_list(object)[1]))
             ),
      column(3, conditionalPanel(condition = "input.diagnostics_navlist == 'By model parameter'", 
                                 textInput("diagnostic_param_transform", label = NULL, value = "x"))),
      column(2, conditionalPanel(condition = "input.diagnostics_navlist == 'By model parameter'", 
                                 actionButton("diagnostic_param_transform_go", "Transform"))
      )
    ),
    fluidRow(
      column(1, strong(style = "color: red;", "Warnings:")),
      column(6, helpText(style = "color: red; font-size: 13px;", 
                         textOutput("diagnostics_warnings_text")))
    )
  )
})


output$ui_diagnostics_parameter <- renderUI({
  div(
    withMathJax(),
    fluidRow(
      column(7, 
             help_dynamic,
             # plotOutput("p_trace_out", height = "150px")
             dygraphs::dygraphOutput("dynamic_trace_plot_diagnostic_out", 
                                     height = "150px")
             ),
      column(5, 
             help_lines,
             plotOutput("p_hist_out", height = "200px"))
    ),
    help_points,
    fluidRow(
      column(6, 
             plotOutput("param_vs_lp_out", height = "200px"),
             plotOutput("param_vs_stepsize_out", height = "200px")
      ),
      column(6,
             plotOutput("param_vs_accept_stat_out", height = "200px"),
             plotOutput("param_vs_treedepth_out", height = "200px"))
    )
  )
})

output$ui_diagnostics_sample <- renderUI({
  div(
    withMathJax(),
    fluidRow(
      column(7,
             fluidRow(
               column(6,
                      help_interval,
                      plotOutput("lp_trace_out", height = "200px"),
                      plotOutput("accept_stat_trace_out", height = "200px")
               ),
               column(6, 
                      help_lines,
                      plotOutput("lp_hist_out", height = "200px"),
                      plotOutput("accept_stat_hist_out", height = "200px")
               )
             )
      ),
      column(5,
             help_points,
             plotOutput("accept_stat_vs_lp_out", height = "400px"))
    )
  )
})

output$ui_diagnostics_ndivergent <- renderUI({
  fluidRow(
    column(7,
           plotOutput("ndivergent_trace_out", height = "200px"),
           plotOutput("ndivergent_vs_lp_out", height = "200px")
           
    ),
    column(5, 
           plotOutput("ndivergent_vs_accept_stat_out", height = "400px")
    )
  )
})
output$ui_diagnostics_treedepth <- renderUI({
  div(
    withMathJax(),
    fluidRow(
      column(7,
             help_max_td, help_interval,
             plotOutput("treedepth_trace_out", height = "150px"),
             plotOutput("treedepth_vs_lp_out", height = "150px")
      ),
      column(5, plotOutput("treedepth_vs_accept_stat_out", height = "400px"))
    ),
    br(),br(),
    splitLayout( 
      plotOutput("treedepth_ndivergent_hist_out", height = "125px"),
      plotOutput("treedepth_ndivergent0_hist_out", height = "125px"),
      plotOutput("treedepth_ndivergent1_hist_out", height = "125px")
    ),
    br()
  )
})

output$ui_diagnostics_stepsize <- renderUI({
  fluidRow(
    column(7,
           plotOutput("stepsize_trace_out", height = "200px"),
           plotOutput("stepsize_vs_lp_out", height = "200px")
    ),
    column(5, plotOutput("stepsize_vs_accept_stat_out", height = "400px")
    )
  )
})

