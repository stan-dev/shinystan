output$ui_dynamic_trace_helptext <- renderUI({
  tags$div(
    h5(style = "color: #337ab7;", "Controlling the dynamic trace plot"),
    helpText(style = "color: white; font-size: 12px;", "Use your mouse to highlight areas in the plot to zoom into. To zoom back out just double-click.",
             "You can also use the range selector below the graph for panning and zooming.",
             "The number in the small black box in the bottom left corner controls the", em("roll period."),
             "If you specify a roll period of N the resulting graph will be a moving average,", 
             "with each plotted point representing the average of N points in the data.")
  )
})
