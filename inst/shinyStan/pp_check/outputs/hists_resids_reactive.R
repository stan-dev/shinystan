pp_hist_resids <- reactive({
  tests()
  y <- get(input$y_name)
  y_rep <- y_rep()
  s <- sample_id_for_resids()
  resids <- y - y_rep[s, ]
  names(resids) <- paste0("resids(y_rep_",s,")")
  
  
  do.call(".pp_hist_resids", args = list(
    resids = resids
  ))
})