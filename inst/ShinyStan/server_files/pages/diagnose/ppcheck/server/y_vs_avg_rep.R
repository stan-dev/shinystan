pp_y_vs_avg_rep <- reactive({
  pp_tests()
  y <- get_y()
  yrep <- get_yrep()
  zoom <- input$pp_zoom_to_zero
  do.call(".pp_y_vs_avg_rep", args = list(
    y = y, 
    colMeans_yrep = colMeans(yrep),
    zoom_to_zero = zoom
  ))
})

output$pp_y_vs_avg_rep_out <- renderPlot({
  pp_y_vs_avg_rep()
}, bg = "transparent")
