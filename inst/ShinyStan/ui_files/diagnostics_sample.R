# sample (accept_stat, lp)
div(class = "diagnostics-navlist-tabpanel",
    fluidRow(
      column(
        width = 7,
        fluidRow(
          column(
            width = 6,
            help_dynamic,
            dygraphOutput_175px("dynamic_trace_diagnostic_lp_out"),
            br(),
            dygraphOutput_175px("dynamic_trace_diagnostic_accept_stat_out")
          ),
          column(
            width = 6,
            help_lines,
            plotOutput_200px("lp_hist_out"),
            br(),
            plotOutput_200px("accept_stat_hist_out")
          )
        )), 
      column(
        width = 5,
        help_points,
        plotOutput_400px("accept_stat_vs_lp_out")
      )
    ))