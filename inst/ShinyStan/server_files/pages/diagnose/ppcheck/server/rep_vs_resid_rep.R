pp_avg_rep_vs_avg_resid_rep <- reactive({
  pp_tests()
  y <- get_y()
  yrep <- get_yrep()
  rowMeans_resids <- rowMeans(y - yrep)
  rowMeans_yrep <- rowMeans(yrep)
  do.call(".pp_avg_rep_vs_avg_resid_rep", args = list(
    rowMeans_yrep = rowMeans_yrep,
    rowMeans_resids = rowMeans_resids
  ))
})

output$pp_avg_rep_vs_avg_resid_rep_out <- renderPlot({
  pp_avg_rep_vs_avg_resid_rep()
}, bg = "transparent")
