output$ui_pp_hists_test_statistics <- renderUI({
  div(
    br(),
    h5(withMathJax(plot_descriptions["plot_test_statistics"])),
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