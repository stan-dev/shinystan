output$ui_dynamic_trace_customize <- renderUI({
  bsCollapse(
    bsCollapsePanel(title = "View Options", id = "dynamic_trace_collapse",
                    fluidRow(
                      column(3, numericInput("dynamic_trace_chain", label = h5(style = "color: white;", "Chain (0 = all)"), min = 0, max = object@nChains, step = 1, value = 0)),
                      column(3, radioButtons("dynamic_trace_stack", label = h5("Lines"), choices = list(Normal = "normal", Stacked = "stacked"), selected = "normal", inline = FALSE)),
                      column(3, radioButtons("dynamic_trace_grid", label = h5("Grid"), choices = list(Show = "show", Hide = "hide"), selected = "hide", inline = FALSE)),
                      column(3, radioButtons("dynamic_trace_warmup_shade", label = h5("Warmup shading"), choices = list(Show = "show", Hide = "hide"), selected = "show", inline = FALSE))
                    ),
                    hr(),
                    h5(style = "color: white;", "Controlling the dynamic trace plot"),
                    helpText(style = "color: white; font-size: 12px;", "Use your mouse to highlight areas in the plot to zoom into. To zoom back out just double-click.",
                             "You can also use the range selector below the graph for panning and zooming.",
                             "The number in the small black box in the bottom left corner controls the", em("roll period."),
                             "If you specify a roll period of N the resulting graph will be a moving average,", 
                             "with each plotted point representing the average of N points in the data.")
    )
  )
})
