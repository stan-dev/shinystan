output$ui_pp_hists_test_statistics <- renderUI({
  div(
    br(),
    h4(withMathJax(plot_descriptions["plot_test_statistics"])),
    helpText("The blue lines show \\(T(y)\\), the value of the statistic computed from the observed data."),
    radioButtons("pp_hists_test_statistics_type", label = "", choices = list(Histograms = "histogram", Densities = "density"), inline = TRUE),
    fluidRow(
      column(6, plotOutput("pp_hists_test_statistics_mean_out", height = "200px")),
      column(6, plotOutput("pp_hists_test_statistics_sd_out", height = "200px"))
    ),
    br(),
    fluidRow(
      column(6, plotOutput("pp_hists_test_statistics_min_out", height = "200px")),
      column(6, plotOutput("pp_hists_test_statistics_max_out", height = "200px"))
    ),
    br()
  )
  
})