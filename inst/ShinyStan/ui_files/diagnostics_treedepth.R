# treedepth
div(
  class = "diagnostics-navlist-tabpanel",
  fluidRow(
    column(
      width = 7,
      help_dynamic,
      dygraphOutput_175px("dynamic_trace_diagnostic_treedepth_out"),
      br(), br(),
      plotOutput("treedepth_vs_lp_out", height = "150px")
    ),
    column(width = 5, plotOutput_400px("treedepth_vs_accept_stat_out"))
  ),
  splitLayout(
    plotOutput("treedepth_ndivergent_hist_out", height = "125px"),
    plotOutput("treedepth_ndivergent0_hist_out", height = "125px"),
    plotOutput("treedepth_ndivergent1_hist_out", height = "125px")
  ),
  br()
)
