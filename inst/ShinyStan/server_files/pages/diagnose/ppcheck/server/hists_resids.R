pp_hist_resids <- reactive({
  pp_tests()
  s <- sample_id_for_resids()
  resids <- get_y() - get_yrep()[s,]
  names(resids) <- paste0("resids(yrep_", s, ")")
  do.call(".pp_hist_resids", args = list(resids = resids))
})

output$pp_hist_resids_out <- renderPlot({
  x <- suppressMessages(pp_hist_resids())
  suppress_and_print(x)
}, bg = "transparent")
