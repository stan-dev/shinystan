save_settings_density <- reactive({
  list(
    fill_color  = input$dens_fill_color,
    line_color  = input$dens_line_color,
    point_est   = input$dens_point_est,
    CI          = input$dens_ci,
    y_breaks    = input$dens_y_breaks,
    x_breaks    = input$dens_x_breaks
  )
})
