output$ui_pp_scatters <- renderUI({
  div(
    br(),
    h5(withMathJax(plot_descriptions["plot_obs_vs_avg_y_rep"])),
    checkboxInput("pp_zoom_to_zero", "Zoom to include (0,0)", value = FALSE),
    plotOutput("pp_y_vs_avg_rep_out", height = "250px"),
    h5(withMathJax(plot_descriptions["plot_avg_rep_vs_avg_resid_rep"])),
    plotOutput("pp_avg_rep_vs_avg_resid_rep_out", height = "250px"),
    br()
  )
})
