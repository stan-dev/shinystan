output$ui_pp_dens_rep_vs_obs <- renderUI({
  div(
    h5(withMathJax(plot_descriptions["plot_dens_rep_vs_obs"])),
    bsCollapse(
      bsCollapsePanel(title = "Making the plot", id = "make_plot2",
                      withMathJax("1) Draw \\(S\\) sets of values \\(\\theta^{[s]}\\) from the posterior \\(p(\\theta \\mid y)\\)"),
                      br(),
                      withMathJax("2) For each of the \\(S\\) draws of \\(\\theta\\) simulate a vector \\(y^{rep}\\) from the posterior predictive distribution"), 
                      br(),
                      withMathJax("3) Plot kernel density estimate of \\(y\\) and the \\(S\\) replications")
      )
    ),
    tags$style(type = "text/css", "#make_plot2 .panel-body{background-color: white;}"),
    plotOutput("pp_dens_rep_vs_obs_out"),
    actionButton("resample_dens_go", label = "Show different replications", icon = icon("refresh"))
  )
})