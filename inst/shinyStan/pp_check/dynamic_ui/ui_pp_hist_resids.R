output$ui_pp_hist_resids <- renderUI({
  div(
    br(),
    h5(withMathJax(plot_descriptions["plot_hist_resids"])),
    plotOutput("pp_hist_resids_out"),
    actionButton("resample_resids_go", label = "Show a different replication", icon = icon("refresh"))
    )
})