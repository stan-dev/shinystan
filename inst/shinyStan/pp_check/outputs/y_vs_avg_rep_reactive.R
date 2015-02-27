pp_y_vs_avg_rep <- reactive({
    tests()
    y <- get(input$y_name)
    y_rep <- y_rep()
    
    do.call(".pp_y_vs_avg_rep", args = list(
      y = y, 
      colMeans_y_rep = colMeans(y_rep)
      ))
})