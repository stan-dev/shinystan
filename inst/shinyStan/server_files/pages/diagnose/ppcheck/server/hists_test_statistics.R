# This file is part of shinyStan
# Copyright (C) 2015 Jonah Sol Gabry & Stan Development Team
#
# shinyStan is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
# 
# shinyStan is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# this program; if not, see <http://www.gnu.org/licenses/>.



pp_hists_test_statistics_mean <- reactive({
  pp_tests()
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
  pp_tests()
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
  pp_tests()
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
  pp_tests()
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