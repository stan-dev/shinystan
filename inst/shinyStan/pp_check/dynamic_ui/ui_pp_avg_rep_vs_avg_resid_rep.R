output$ui_pp_avg_rep_vs_avg_resid_rep <- renderUI({
  div(
    h5(withMathJax(plot_descriptions["plot_avg_rep_vs_avg_resid_rep"])),
    plotOutput("pp_avg_rep_vs_avg_resid_rep_out")
    )
})