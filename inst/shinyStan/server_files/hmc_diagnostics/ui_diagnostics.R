help_interval <- helpText(style = "font-size: 11px;","Highlighted interval shows \\(\\bar{x} \\pm sd(x)\\)")
help_lines <- helpText(style = "font-size: 11px;","Lines are mean (solid) and median (dashed)")
help_max_td <- helpText(style = "font-size: 11px;", "Red line indicates the max_treedepth setting")
help_points <- helpText(style = "font-size: 11px;", "Red indicates a divergent transition.",
                        "Yellow indicates a transition hitting the maximum treedepth.")  
output$ui_diagnostics_parameter <- renderUI({
  div(
    withMathJax(),
    fluidRow(
      column(7, 
             help_interval,
             plotOutput("p_trace_out", height = "150px")),
      column(5, 
             help_lines,
             plotOutput("p_hist_out", height = "150px"))
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
                      plotOutput("lp_trace_out", height = "150px"),
                      plotOutput("accept_stat_trace_out", height = "150px")
               ),
               column(6, 
                      help_lines,
                      plotOutput("lp_hist_out", height = "150px"),
                      plotOutput("accept_stat_hist_out", height = "150px")
               )
             )
      ),
      column(5,
             help_points,
             plotOutput("accept_stat_vs_lp_out", height = "300px"))
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
      column(5, plotOutput("treedepth_vs_accept_stat_out", height = "300px"))
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
    column(5, 
           plotOutput("stepsize_vs_accept_stat_out", height = "400px")
    )
  )
})

