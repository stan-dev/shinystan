# Energy

# fluidRow(
#   column(
#     width = 7,
#     help_dynamic,
#     dygraphOutput_175px("dynamic_trace_diagnostic_ndivergent_out"),
#     br(), br(),
#     plotOutput("ndivergent_vs_lp_out", height = "150px")
#   ),
#   column(width = 5, plotOutput_400px("ndivergent_vs_accept_stat_out"))
# )

plotOutput("energy_hist_out")
