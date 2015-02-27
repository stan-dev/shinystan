pp_hists_rep_vs_obs <- reactive({
  tests()
  y <- get(input$y_name)
  y_rep <- y_rep()
  sample_ids <- sample_ids_for_hist()
  y_rep_samp <- y_rep[sample_ids, ]
  rownames(y_rep_samp) <- paste0("y_rep_", sample_ids)
  
  graphs <- .pp_hists_rep_vs_obs(y = y, y_rep_samp = y_rep_samp)
  do.call(gridExtra::grid.arrange, c(graphs, ncol = 3))
})