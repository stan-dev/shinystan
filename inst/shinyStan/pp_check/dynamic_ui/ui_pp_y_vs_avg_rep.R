output$ui_pp_y_vs_avg_rep <- renderUI({
  div(
    h5(withMathJax(plot_descriptions["plot_obs_vs_avg_y_rep"])),
    plotOutput("pp_y_vs_avg_rep_out")
  )
})