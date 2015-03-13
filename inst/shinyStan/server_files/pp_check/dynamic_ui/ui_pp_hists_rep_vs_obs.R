output$ui_pp_hists_rep_vs_obs <- renderUI({
  div(
    br(),
    h4(withMathJax(plot_descriptions["plot_hists_rep_vs_obs"])),
    fluidRow(
      column(5, radioButtons("pp_hists_rep_vs_obs_type", label = "", choices = list(Histograms = "histogram", Densities = "density"), inline = TRUE)),
      column(4, 
             conditionalPanel(condition = "input.pp_hists_rep_vs_obs_type == 'density'",
                              radioButtons("pp_hists_rep_vs_obs_overlay", label = "", choices = list(Separate = FALSE, Overlay = TRUE), selected = FALSE, inline = TRUE)
             ))
    ),
    plotOutput("pp_hists_rep_vs_obs_out"),
    actionButton("resample_hist_go", label = "Show different replications", icon = icon("refresh")),
    br()
  )
})
