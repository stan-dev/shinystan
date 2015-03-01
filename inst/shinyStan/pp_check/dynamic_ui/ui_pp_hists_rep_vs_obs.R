output$ui_pp_hists_rep_vs_obs <- renderUI({
  div(
    h5(withMathJax(plot_descriptions["plot_hists_rep_vs_obs"])),
#     bsCollapse(
#       bsCollapsePanel(title = "Making the plot", id = "make_plot1",
#                       h5("Making the plot: "),
#                       withMathJax("1) Draw \\(S\\) sets of values \\(\\theta^{[s]} \\) from the posterior \\(p(\\theta \\mid y)\\)"),
#                       br(),
#                       withMathJax("2) For each of the \\(S\\) draws from the posterior simulate a vector \\(y^{rep}\\) from the posterior predictive distribution"), 
#                       br(),
#                       withMathJax("3) Plots histogram of \\(y\\) alongside histograms of the replications")
#       )
#     ),
#     tags$style(type = "text/css", "#make_plot1 .panel-body{background-color: white;}"),
fluidRow(
  column(2, radioButtons("pp_hists_rep_vs_obs_type", label = "Plot type", choices = list(Histogram = "histogram", Density = "density"), inline = FALSE)),
  column(2, 
  conditionalPanel(condition = "input.pp_hists_rep_vs_obs_type == 'density'",
                   checkboxInput("pp_hists_rep_vs_obs_overlay", "Overlay densities", value = FALSE)
                   )),
  column(4, actionButton("resample_hist_go", label = "Show different replications", icon = icon("refresh")))
),
    
    plotOutput("pp_hists_rep_vs_obs_out")
    
  )
})
