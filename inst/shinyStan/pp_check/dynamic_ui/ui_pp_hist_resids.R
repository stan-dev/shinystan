output$ui_pp_hist_resids <- renderUI({
  div(
    h5(withMathJax(plot_descriptions["plot_hist_resids"])),
    bsCollapse(
      bsCollapsePanel(title = "Making the plot", id = "make_plot3",
                      withMathJax("1) Draw a single set of values \\(\\theta^\\star \\) from the posterior \\(p(\\theta \\mid y)\\)"),
                      br(),
                      withMathJax("2) Given \\(\\theta^\\star \\), draw a vector \\(y^{rep} \\) from the posterior predictive distribution"), 
                      br(),
                      withMathJax("3) Plot histogram of the residuals \\(r = y - y^{rep}\\)"),
                      br(),
                      withMathJax("4) Add density curve for normal distribution with mean \\(\\mu = \\text{mean}(r)\\) and standard deviation \\( \\sigma = \\text{sd}(r)\\) ")
      )
    ),
    tags$style(type = "text/css", "#make_plot3 .panel-body{background-color: white;}"),
    plotOutput("pp_hist_resids_out"),
    actionButton("resample_resids_go", label = "Show a different replication", icon = icon("refresh"))
    )
})