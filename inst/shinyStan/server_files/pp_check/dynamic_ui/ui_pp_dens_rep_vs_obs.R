output$ui_pp_dens_rep_vs_obs <- renderUI({
  div(
    br(),
    h5(withMathJax(plot_descriptions["plot_dens_rep_vs_obs"])),
    plotOutput("pp_dens_rep_vs_obs_out"),
    actionButton("resample_dens_go", label = "Show different replications", icon = icon("refresh"))
  )
})