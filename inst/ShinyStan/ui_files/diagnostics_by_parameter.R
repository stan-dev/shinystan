# model parameter
div(class = "diagnostics-navlist-tabpanel",
    fluidRow(
      column(
        width = 7,
        help_dynamic,
        dygraphOutput_175px("dynamic_trace_diagnostic_parameter_out")
      ),
      column(width = 5, help_lines, plotOutput_200px("p_hist_out"))
    ),
    help_points,
    fluidRow(
      column(
        width = 6,
        plotOutput_200px("param_vs_lp_out"),
        plotOutput_200px("param_vs_stepsize_out")
      ),
      column(
        width = 6,
        plotOutput_200px("param_vs_accept_stat_out"),
        plotOutput_200px("param_vs_treedepth_out")
      )
    ),
    br()
)