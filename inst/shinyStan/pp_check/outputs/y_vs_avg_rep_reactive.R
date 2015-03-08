pp_y_vs_avg_rep <- reactive({
    tests()
    y <- get(input$y_name)
    y_rep <- y_rep()
    zoom <- input$pp_zoom_to_zero
    do.call(".pp_y_vs_avg_rep", args = list(
      y = y, 
      colMeans_y_rep = colMeans(y_rep),
      zoom_to_zero = zoom
      ))
})