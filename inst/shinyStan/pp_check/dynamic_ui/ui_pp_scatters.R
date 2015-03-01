output$ui_pp_scatters <- renderUI({
  div(
    h5(withMathJax(plot_descriptions["plot_obs_vs_avg_y_rep"])),
    plotOutput("pp_y_vs_avg_rep_out", height = "250px"),
    h5(withMathJax(plot_descriptions["plot_avg_rep_vs_avg_resid_rep"])),
    plotOutput("pp_avg_rep_vs_avg_resid_rep_out", height = "250px")
  )
})
