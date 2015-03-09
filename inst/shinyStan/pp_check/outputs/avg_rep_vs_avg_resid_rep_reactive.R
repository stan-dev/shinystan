pp_avg_rep_vs_avg_resid_rep <- reactive({
  pp_tests()
  y <- get(input$y_name)
  y_rep <- y_rep()
  rowMeans_resids <- rowMeans(y - y_rep)
  rowMeans_y_rep <- rowMeans(y_rep)
  
  
  do.call(".pp_avg_rep_vs_avg_resid_rep", args = list(
    rowMeans_y_rep = rowMeans_y_rep,
    rowMeans_resids = rowMeans_resids
  ))
})

