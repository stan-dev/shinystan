pp_hists_test_statistics_mean <- reactive({
  tests()
  y <- get(input$y_name)
  y_rep <- y_rep()
  mean_y <- mean(y)
  mean_y_rep <- apply(y_rep, 1, mean)
  
  do.call(".pp_hists_test_statistics", args = list(
    stat_y = mean_y,
    stat_y_rep = mean_y_rep,
    which = "mean",
    geom = input$pp_hists_test_statistics_type
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
    which = "sd",
    geom = input$pp_hists_test_statistics_type
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
    which = "min",
    geom = input$pp_hists_test_statistics_type
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
    which = "max",
    geom = input$pp_hists_test_statistics_type
  ))
})

pp_hists_test_statistics_mean <- reactive({
  tests()
  y <- get(input$y_name)
  y_rep <- y_rep()
  mean_y <- mean(y)
  mean_y_rep <- apply(y_rep, 1, mean)
  
  do.call(".pp_hists_test_statistics", args = list(
    stat_y = mean_y,
    stat_y_rep = mean_y_rep,
    which = "mean",
    geom = input$pp_hists_test_statistics_type
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
    which = "sd",
    geom = input$pp_hists_test_statistics_type
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
    which = "min",
    geom = input$pp_hists_test_statistics_type
  ))
})

# pp_hists_test_statistics_custom1 <- reactive({
#   tests()
#   validate(need(input$pp_test_statistics_fun1, message = ""))
#   y <- get(input$y_name)
#   y_rep <- y_rep()
#   
#   fun <- input$pp_test_statistics_fun1
#   if (grepl("function", fun)) {
#     f <- eval(parse(text = fun))
#     stat_y <- f(y)
#     stat_y_rep <- apply(y_rep, 1, FUN = f)
#   } else {
#     stat_y <- do.call(fun, args = list(y))
#     stat_y_rep <- apply(y_rep, 1, paste(fun))
#   }
#   
#   do.call(".pp_hists_test_statistics", args = list(
#     stat_y = stat_y,
#     stat_y_rep = stat_y_rep,
#     which = "f",
#     geom = input$pp_hists_test_statistics_type
#   ))
# })
# pp_hists_test_statistics_custom2 <- reactive({
#   tests()
#   if (is.null(input$pp_test_statistics_fun2) | is.na(input$pp_test_statistics_fun2)) {
#     return(last_plot())
#   }
#   y <- get(input$y_name)
#   y_rep <- y_rep()
#   stat_y <- do.call(input$pp_test_statistics_fun2, args = list(y))
#   stat_y_rep <- apply(y_rep, 1, paste(input$pp_test_statistics_fun2))
#   
#   do.call(".pp_hists_test_statistics", args = list(
#     stat_y = stat_y,
#     stat_y_rep = stat_y_rep,
#     which = paste(input$pp_test_statistics_fun2),
#     geom = input$pp_hists_test_statistics_type
#   ))
# })