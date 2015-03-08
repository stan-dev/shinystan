output$ui_pp_hists_test_statistics <- renderUI({
  div(
    br(),
    h5(withMathJax(plot_descriptions["plot_test_statistics"])),
    #     bsCollapse(
    #       bsCollapsePanel(title = "Making the plot", id = "make_plot4",
    #                       withMathJax("1) Draw \\(S\\) sets of values \\(\\theta^{[s]} \\) from the posterior \\(p(\\theta \\mid y)\\)"),
    #                       br(),
    #                       withMathJax("2) For each of the \\(S\\) simulations from the posterior draw a vector \\(y^{rep}\\) from the posterior predictive distribution"), 
    #                       br(),
    #                       withMathJax("3) For each of the \\(S\\) replications compute the value of the test statistic"),
    #                       br(),
    #                       withMathJax("4) Plot a histogram of the \\(S \\) values of the test statistic"),
    #                       br(),
    #                       withMathJax("5) Add a line showing the value of the test statistic computed from the observed data")
    #       )
    #     ),
    #     tags$style(type = "text/css", "#make_plot4 .panel-body{background-color: white;}"),
    radioButtons("pp_hists_test_statistics_type", label = "Plot type", choices = list(Histogram = "histogram", Density = "density"), inline = TRUE),
    fluidRow(
      column(6, plotOutput("pp_hists_test_statistics_mean_out", height = "200px")),
      column(6, plotOutput("pp_hists_test_statistics_sd_out", height = "200px"))
    ),
    fluidRow(
      column(6, plotOutput("pp_hists_test_statistics_min_out", height = "200px")),
      column(6, plotOutput("pp_hists_test_statistics_max_out", height = "200px"))
    )
  )
  
})