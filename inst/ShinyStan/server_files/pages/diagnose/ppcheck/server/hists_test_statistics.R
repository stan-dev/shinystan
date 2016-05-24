pp_hists_test_statistics_mean <- reactive({
  pp_tests()
  y <- get_y()
  yrep <- get_yrep()
  mean_y <- mean(y)
  mean_yrep <- apply(yrep, 1, mean)
  do.call(".pp_hists_test_statistics", args = list(
    stat_y = mean_y,
    stat_yrep = mean_yrep,
    which = "mean",
    geom = input$pp_hists_test_statistics_type
  ))
})
pp_hists_test_statistics_sd <- reactive({
  pp_tests()
  y <- get_y()
  yrep <- get_yrep()
  sd_y <- sd(y)
  sd_yrep <- apply(yrep, 1, sd)
  do.call(".pp_hists_test_statistics", args = list(
    stat_y = sd_y,
    stat_yrep = sd_yrep,
    which = "sd",
    geom = input$pp_hists_test_statistics_type
  ))
})
pp_hists_test_statistics_min <- reactive({
  pp_tests()
  y <- get_y()
  yrep <- get_yrep()  
  min_y <- min(y)
  min_yrep <- apply(yrep, 1, min)
  do.call(".pp_hists_test_statistics", args = list(
    stat_y = min_y,
    stat_yrep = min_yrep,
    which = "min",
    geom = input$pp_hists_test_statistics_type
  ))
})
pp_hists_test_statistics_max <- reactive({
  pp_tests()
  y <- get_y()
  yrep <- get_yrep()
  max_y <- max(y)
  max_yrep <- apply(yrep, 1, max)
  do.call(".pp_hists_test_statistics", args = list(
    stat_y = max_y,
    stat_yrep = max_yrep,
    which = "max",
    geom = input$pp_hists_test_statistics_type
  ))
})

pp_test_stats <- c("mean", "sd", "min", "max")
for (i in seq_along(pp_test_stats)) {
  local({
    fn <- paste0("pp_hists_test_statistics_", pp_test_stats[i])
    output[[paste0(fn,"_out")]] <- renderPlot({
      x <- suppressMessages(do.call(fn, list()))
      suppress_and_print(x)
    }, bg = "transparent")
  })
}

# pp_hists_test_statistics_custom1 <- reactive({
#   tests()
#   validate(need(input$pp_test_statistics_fun1, message = ""))
#   y <- get_y()
#   yrep <- get_yrep()
#   
#   fun <- input$pp_test_statistics_fun1
#   if (grepl("function", fun)) {
#     f <- eval(parse(text = fun))
#     stat_y <- f(y)
#     stat_yrep <- apply(yrep, 1, FUN = f)
#   } else {
#     stat_y <- do.call(fun, args = list(y))
#     stat_yrep <- apply(yrep, 1, paste(fun))
#   }
#   
#   do.call(".pp_hists_test_statistics", args = list(
#     stat_y = stat_y,
#     stat_yrep = stat_yrep,
#     which = "f",
#     geom = input$pp_hists_test_statistics_type
#   ))
# })
# pp_hists_test_statistics_custom2 <- reactive({
#   tests()
#   if (is.null(input$pp_test_statistics_fun2) | is.na(input$pp_test_statistics_fun2)) {
#     return(last_plot())
#   }
#   y <- get_y()
#   yrep <- get_yrep()
#   stat_y <- do.call(input$pp_test_statistics_fun2, args = list(y))
#   stat_yrep <- apply(yrep, 1, paste(input$pp_test_statistics_fun2))
#   
#   do.call(".pp_hists_test_statistics", args = list(
#     stat_y = stat_y,
#     stat_yrep = stat_yrep,
#     which = paste(input$pp_test_statistics_fun2),
#     geom = input$pp_hists_test_statistics_type
#   ))
# })