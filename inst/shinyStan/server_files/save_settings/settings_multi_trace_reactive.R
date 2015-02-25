save_settings_multi_trace <- reactive({
  list(
    palette     = input$multi_trace_palette,
    rect        = input$multi_trace_rect,
    rect_color  = input$multi_trace_rect_color,
    rect_alpha  = input$multi_trace_rect_alpha,
    layout      = input$multi_trace_layout
  )
})
