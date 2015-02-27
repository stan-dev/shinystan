pp_hists_test_statistics_mean <- reactive({
  tests()
  y <- get(input$y_name)
  y_rep <- y_rep()
  mean_y <- mean(y)
  mean_y_rep <- apply(y_rep, 1, mean)
  
  do.call(".pp_hists_test_statistics", args = list(
    stat_y = mean_y,
    stat_y_rep = mean_y_rep,
    which = "mean"
    ))
})

pp_hists_test_statistics_sd <- reactive({
  tests()
  y <- get(input$y_name)
  y_rep <- y_rep()
  sd_y <- sd(y)
  sd_y_rep <- apply(y_rep, 1, sd)
  
  do.call(".pp_hists_test_statistics", args = list(
    stat_y = sd_y,
    stat_y_rep = sd_y_rep,
    which = "sd"
  ))
})

pp_hists_test_statistics_min <- reactive({
  tests()
  y <- get(input$y_name)
  y_rep <- y_rep()  
  min_y <- min(y)
  min_y_rep <- apply(y_rep, 1, min)

  do.call(".pp_hists_test_statistics", args = list(
    stat_y = min_y,
    stat_y_rep = min_y_rep,
    which = "min"
  ))
})

pp_hists_test_statistics_max <- reactive({
  tests()
  y <- get(input$y_name)
  y_rep <- y_rep()
  max_y <- max(y)
  max_y_rep <- apply(y_rep, 1, max)
  
  do.call(".pp_hists_test_statistics", args = list(
    stat_y = max_y,
    stat_y_rep = max_y_rep,
    which = "max"
  ))
})