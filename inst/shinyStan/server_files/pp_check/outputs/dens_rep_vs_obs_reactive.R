pp_dens_rep_vs_obs <- reactive({
  pp_tests()
  y <- get(input$y_name)
  y_rep <- y_rep()
  # sample_ids <- sample_ids_for_dens()
  sample_ids <- sample_ids_for_hist()
  y_rep_samp <- y_rep[sample_ids, ]
  
  max_y_rep_dens <- apply(y_rep, 1, function(x) density(x)$y)
  y_lim <- c(0, max(density(y)$y, max_y_rep_dens))
  x_lim <- range(c(y, y_rep))
  do.call(".pp_dens_rep_vs_obs", args = list(
    y = y, 
    y_rep_samp = y_rep_samp,
    y_lim = y_lim,
    x_lim = x_lim
  ))
})