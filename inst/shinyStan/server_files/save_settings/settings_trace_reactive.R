save_settings_trace <- reactive({
  list(
    palette     = input$trace_palette,
    rect        = input$trace_rect,
    rect_color  = input$trace_rect_color,
    rect_alpha  = input$trace_rect_alpha,
    style       = input$trace_style
  )
})
