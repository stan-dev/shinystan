# stepsize ----------------------------------------------------------------
fluidRow(
  column(7, help_dynamic,
         dygraphOutput_175px("dynamic_trace_diagnostic_stepsize_out"), 
         br(),br(),
         plotOutput("stepsize_vs_lp_out", height = "150px")),
  column(5, plotOutput_400px("stepsize_vs_accept_stat_out"))
)